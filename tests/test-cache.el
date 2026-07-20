;;; test-cache.el --- Sync/cache logic tests for patchwel-cache.el -*- lexical-binding: t; -*-
(require 'ert)

;; -- pure functions, no server/db needed ------------------------------------

(ert-deftest patchwork-cache-test-series-state-precedence ()
  (should (equal (patchwork-cache--series-state nil) "new"))
  (should (equal (patchwork-cache--series-state (list (list :state "new"))) "new"))
  (should (equal (patchwork-cache--series-state
                  (list (list :state "new") (list :state "new")))
                 "new"))
  (should (equal (patchwork-cache--series-state
                  (list (list :state "rejected") (list :state "new")))
                 "rejected"))
  (should (equal (patchwork-cache--series-state
                  (list (list :state "changes-requested") (list :state "new")))
                 "changes-requested"))
  (should (equal (patchwork-cache--series-state
                  (list (list :state "under-review") (list :state "new")))
                 "under-review"))
  (should (equal (patchwork-cache--series-state
                  (list (list :state "rejected") (list :state "changes-requested")))
                 "rejected")))

(ert-deftest patchwork-cache-test-parse-server-time-treats-naive-as-utc ()
  (let ((naive (patchwork-parse-server-time "2026-01-01T00:00:00"))
        (explicit (patchwork-parse-server-time "2026-01-01T00:00:00Z")))
    (should (equal naive explicit))))

;; -- since= format cascade (unit-level, via patchwork-cache--fetch-with-since) --

(ert-deftest patchwork-cache-test-since-cascade-succeeds-on-second-format ()
  (patchwork-test-with-mock-server port
    (patchwork-test-control port "set-since-mode" '((mode . "naive")))
    (let ((server (patchwork-test-server-plist port)))
      (should (listp (patchwork-cache--fetch-with-since
                      server (current-time)
                      (lambda (s) (patchwork-api-list-series server nil `(("since" . ,s)))))))
      (should (>= (length (patchwork-test-control-log port)) 2)))))

(ert-deftest patchwork-cache-test-since-explicit-format-skips-cascade-and-propagates ()
  (patchwork-test-with-mock-server port
    (patchwork-test-control port "set-since-mode" '((mode . "date")))
    (let ((server (patchwork-test-server-plist port :since-format 'naive)))
      (should-error
       (patchwork-cache--fetch-with-since
        server (current-time)
        (lambda (s) (patchwork-api-list-series server nil `(("since" . ,s)))))
       :type 'patchwork-api-http-error)
      ;; only one attempt was made -- the explicit format, no cascade retries
      (should (= (length (patchwork-test-control-log port)) 1)))))

(ert-deftest patchwork-cache-test-since-all-formats-rejected-resignals-last-error ()
  (patchwork-test-with-mock-server port
    (patchwork-test-control port "set-since-mode" '((mode . "reject-all")))
    (let ((server (patchwork-test-server-plist port)))
      (should-error
       (patchwork-cache--fetch-with-since
        server (current-time)
        (lambda (s) (patchwork-api-list-series server nil `(("since" . ,s)))))
       :type 'patchwork-api-http-error))))

;; -- incremental sync events handling ---------------------------------------

(ert-deftest patchwork-cache-test-incremental-events-404-falls-back-to-window ()
  (patchwork-test-with-mock-server port
    (patchwork-test-with-temp-db
      (let ((server (patchwork-test-server-plist port)))
        (patchwork-test-control port "set-since-mode" '((mode . "no-events-api")))
        (patchwork-cache--sync-incremental
         server nil "2020-01-01T00:00:00Z"
         (time-subtract (current-time) (days-to-time 30)))
        (should (> (length (patchwork-db-query-series)) 0))))))

(ert-deftest patchwork-cache-test-incremental-events-500-propagates-without-fallback ()
  (patchwork-test-with-mock-server port
    (patchwork-test-with-temp-db
      (let ((server (patchwork-test-server-plist port)))
        (patchwork-test-control port "set-status" '((path . "/api/events/") (status . 500)))
        (should-error
         (patchwork-cache--sync-incremental
          server nil "2020-01-01T00:00:00Z"
          (time-subtract (current-time) (days-to-time 30)))
         :type 'patchwork-api-http-error)))))

;; -- full sync / cache-sync integration --------------------------------------

(defmacro patchwork-cache-test--with-synced-mock (server-var &rest body)
  "Bind SERVER-VAR to a mock server plist, `patchwork-servers' to just
that one server, and a scratch `patchwork-git-temp-dir', run BODY."
  (declare (indent 1))
  `(patchwork-test-with-mock-server port
     (patchwork-test-with-temp-db
       (let* ((,server-var (patchwork-test-server-plist port))
              (patchwork-servers (list ,server-var))
              (patchwork-git-temp-dir (make-temp-file "patchwork-test-mboxdir" t)))
         (unwind-protect
             (progn ,@body)
           (delete-directory patchwork-git-temp-dir t))))))

(ert-deftest patchwork-cache-test-full-sync-populates-every-table ()
  (patchwork-cache-test--with-synced-mock server
    (patchwork-cache-sync)
    (should (= (length (patchwork-db-query-series)) 3))
    (let ((s1002 (patchwork-db-get-series (plist-get server :url) 1002)))
      (should (equal (plist-get s1002 :state) "under-review")))
    (should (= (length (patchwork-db-get-series-patches (plist-get server :url) 1002)) 2))
    (should (patchwork-db-get-comment (plist-get server :url) 3001))
    (should (= (length (patchwork-db-get-checks (plist-get server :url) 2003)) 2))))

(ert-deftest patchwork-cache-test-sync-series-bypasses-window-and-project ()
  (patchwork-test-with-mock-server port
    (patchwork-test-with-temp-db
      (let ((server (patchwork-test-server-plist port)))
        (let ((got (patchwork-cache-sync-series server 1001)))
          (should (equal (plist-get got :name) "A single-patch series")))
        (should (patchwork-db-get-series (plist-get server :url) 1001))))))

(ert-deftest patchwork-cache-test-ttl-staleness-skips-then-force-bypasses ()
  (patchwork-cache-test--with-synced-mock server
    (patchwork-cache-sync)
    (let ((count-after-first (length (patchwork-test-control-log port))))
      (patchwork-cache-sync)
      (should (= (length (patchwork-test-control-log port)) count-after-first))
      (patchwork-cache-sync t)
      (should (> (length (patchwork-test-control-log port)) count-after-first)))))

(ert-deftest patchwork-cache-test-per-server-error-isolation ()
  (patchwork-test-with-mock-server port
    (patchwork-test-with-temp-db
      (let* ((good-server (patchwork-test-server-plist port))
             (bad-server (list :url "http://127.0.0.1:1/api" :token nil :projects nil))
             (patchwork-servers (list bad-server good-server))
             (patchwork-git-temp-dir (make-temp-file "patchwork-test-mboxdir" t))
             (patchwork-sync-timeout 1))
        (unwind-protect
            (progn
              (patchwork-cache-sync)
              (should (> (length (patchwork-db-query-series)) 0)))
          (delete-directory patchwork-git-temp-dir t))))))

;; -- prune-on-terminal-state-transition --------------------------------------

(ert-deftest patchwork-cache-test-prune-on-terminal-transition ()
  (patchwork-cache-test--with-synced-mock server
    (patchwork-cache-sync)
    (let ((mbox-file (patchwork-git-download-patch server 2002)))
      (should (file-exists-p mbox-file))
      ;; non-terminal -> non-terminal: untouched
      (patchwork-test-control port "set-patch-state"
                                '((patch_id . 2002) (state . "under-review")))
      (patchwork-cache-sync t)
      (should (file-exists-p mbox-file))
      ;; non-terminal -> terminal: pruned
      (patchwork-test-control port "set-patch-state"
                                '((patch_id . 2002) (state . "accepted")))
      (patchwork-cache-sync t)
      (should-not (file-exists-p mbox-file))
      ;; already-terminal -> terminal: no-op, no error
      (patchwork-cache-sync t)
      (should-not (file-exists-p mbox-file)))))

(ert-deftest patchwork-cache-test-prune-disabled-via-nil ()
  (patchwork-cache-test--with-synced-mock server
    (let ((patchwork-prune-on-terminal-states nil))
      (patchwork-cache-sync)
      (let ((mbox-file (patchwork-git-download-patch server 2003)))
        (patchwork-test-control port "set-patch-state"
                                  '((patch_id . 2003) (state . "rejected")))
        (patchwork-cache-sync t)
        (should (file-exists-p mbox-file))))))

(provide 'test-cache)

;;; test-cache.el ends here
