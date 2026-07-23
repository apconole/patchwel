;;; test-pending-changes.el --- offline-tolerant state/delegate updates -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:
(require 'ert)

(defmacro patchwork-pending-test--with-server (server-var &rest body)
  "Bind SERVER-VAR to a fresh mock-server plist, `patchwork-servers' to
just that one server, run an initial `patchwork-cache-sync' so the
local `patches'/`series' tables are populated, then run BODY inside a
temp db."
  (declare (indent 1))
  `(patchwork-test-with-mock-server port
     (patchwork-test-with-temp-db
       (let* ((,server-var (patchwork-test-server-plist port))
              (patchwork-servers (list ,server-var)))
         (patchwork-cache-sync)
         ,@body))))

(ert-deftest patchwork-pending-test-set-patch-state-applies-immediately ()
  (patchwork-pending-test--with-server server
    (should (eq (patchwork-cache-set-patch-state server 2001 "accepted") 'applied))
    (should (equal (plist-get (patchwork-db-get-patch (plist-get server :url) 2001) :state)
                    "accepted"))
    (should-not (patchwork-db-get-pending-change (plist-get server :url) 2001 "state"))))

(ert-deftest patchwork-pending-test-set-patch-delegate-applies-immediately ()
  (patchwork-pending-test--with-server server
    (should (eq (patchwork-cache-set-patch-delegate server 2001 "carol") 'applied))
    (should (equal (plist-get (patchwork-db-get-patch (plist-get server :url) 2001) :delegate)
                    "carol"))
    (should-not (patchwork-db-get-pending-change (plist-get server :url) 2001 "delegate"))))

(ert-deftest patchwork-pending-test-5xx-queues ()
  (patchwork-pending-test--with-server server
    (patchwork-test-control port "set-status" '((path . "/api/patches/2001/") (status . 500)))
    (should (eq (patchwork-cache-set-patch-state server 2001 "rejected") 'queued))
    ;; local cache untouched
    (should-not (equal (plist-get (patchwork-db-get-patch (plist-get server :url) 2001) :state)
                        "rejected"))
    (let ((pending (patchwork-db-get-pending-change (plist-get server :url) 2001 "state")))
      (should pending)
      (should (equal (plist-get pending :desired-value) "rejected"))
      (should (equal (plist-get pending :observed-value) "new")))))

(ert-deftest patchwork-pending-test-timeout-queues ()
  (patchwork-pending-test--with-server server
    (let ((patchwork-sync-timeout 0.2))
      (patchwork-test-control port "set-delay" '((path . "/api/patches/2001/") (seconds . 2)))
      (should (eq (patchwork-cache-set-patch-state server 2001 "rejected") 'queued))
      (should (patchwork-db-get-pending-change (plist-get server :url) 2001 "state")))))

(ert-deftest patchwork-pending-test-connection-refused-queues ()
  ;; Point at a closed local port -- nothing is listening there, so the
  ;; request fails outright (not via any mock-server machinery).
  (patchwork-test-with-temp-db
    (let* ((server (patchwork-test-server-plist 1)) ; port 1: never a Patchwork server
           (patchwork-servers (list server)))
      (patchwork-db-insert-patch
       (list :server-url (plist-get server :url) :id 9001 :series-id 9001 :project-id 1
             :state "new" :submitter "Alice" :delegate nil :name "x" :date "2026-01-01"
             :series-position 1 :check-state "pending" :updated-at "2026-01-01"))
      (should (eq (patchwork-cache-set-patch-state server 9001 "rejected") 'queued))
      (should (patchwork-db-get-pending-change (plist-get server :url) 9001 "state")))))

(ert-deftest patchwork-pending-test-4xx-propagates-not-queued ()
  (patchwork-pending-test--with-server server
    (patchwork-test-control port "set-status" '((path . "/api/patches/2001/") (status . 404)))
    (should-error (patchwork-cache-set-patch-state server 2001 "rejected")
                  :type 'patchwork-api-http-error)
    (should-not (patchwork-db-get-pending-change (plist-get server :url) 2001 "state"))))

(ert-deftest patchwork-pending-test-drain-applies-when-observed-value-still-matches ()
  (patchwork-pending-test--with-server server
    (patchwork-test-control port "set-status" '((path . "/api/patches/2001/") (status . 500)))
    (patchwork-cache-set-patch-state server 2001 "rejected")
    (patchwork-test-control port "set-status" '((path . "/api/patches/2001/") (status . nil)))
    (patchwork-cache--drain-pending-changes server)
    (should-not (patchwork-db-get-pending-change (plist-get server :url) 2001 "state"))
    (should (equal (plist-get (patchwork-db-get-patch (plist-get server :url) 2001) :state)
                    "rejected"))))

(ert-deftest patchwork-pending-test-drain-discards-on-conflict ()
  (patchwork-pending-test--with-server server
    (patchwork-test-control port "set-status" '((path . "/api/patches/2001/") (status . 500)))
    (patchwork-cache-set-patch-state server 2001 "rejected")
    ;; the patch's real server-side (and now locally re-synced) value
    ;; changed to something else in the meantime
    (patchwork-test-control port "set-patch-state" '((patch_id . 2001) (state . "accepted")))
    (patchwork-cache-sync t)
    ;; a full resync should already have drained it via patchwork-cache-sync;
    ;; assert directly against the drain function too for a focused check
    (should-not (patchwork-db-get-pending-change (plist-get server :url) 2001 "state"))
    (should (equal (plist-get (patchwork-db-get-patch (plist-get server :url) 2001) :state)
                    "accepted"))))

(ert-deftest patchwork-pending-test-drain-skips-untracked-local-patch ()
  (patchwork-pending-test--with-server server
    (patchwork-db-queue-pending-change (plist-get server :url) 987654 "state" "rejected" "new")
    (patchwork-cache--drain-pending-changes server)
    (should (patchwork-db-get-pending-change (plist-get server :url) 987654 "state"))))

(ert-deftest patchwork-pending-test-series-bulk-set-state-tolerates-one-failure ()
  (patchwork-pending-test--with-server server
    (patchwork-test-control port "set-status" '((path . "/api/patches/2003/") (status . 404)))
    (patchwork-cache-set-series-state server 1002 "rejected")
    (should (equal (plist-get (patchwork-db-get-patch (plist-get server :url) 2002) :state)
                    "rejected"))
    (should-not (equal (plist-get (patchwork-db-get-patch (plist-get server :url) 2003) :state)
                        "rejected"))))

(ert-deftest patchwork-pending-test-series-bulk-set-delegate ()
  (patchwork-pending-test--with-server server
    (patchwork-cache-set-series-delegate server 1002 "bob")
    (should (equal (plist-get (patchwork-db-get-patch (plist-get server :url) 2002) :delegate) "bob"))
    (should (equal (plist-get (patchwork-db-get-patch (plist-get server :url) 2003) :delegate) "bob"))))

(ert-deftest patchwork-pending-test-series-pending-value-nil-when-none-pending ()
  (patchwork-pending-test--with-server server
    (should-not (patchwork-cache-series-pending-value (plist-get server :url) 1002 "state"))))

(ert-deftest patchwork-pending-test-series-pending-value-agrees ()
  (patchwork-pending-test--with-server server
    (patchwork-test-control port "set-status" '((path . "/api/patches/2002/") (status . 500)))
    (patchwork-test-control port "set-status" '((path . "/api/patches/2003/") (status . 500)))
    (patchwork-cache-set-series-state server 1002 "rejected")
    (should (equal (patchwork-cache-series-pending-value (plist-get server :url) 1002 "state")
                    "rejected"))))

(ert-deftest patchwork-pending-test-series-pending-value-mixed-on-partial ()
  (patchwork-pending-test--with-server server
    (patchwork-test-control port "set-status" '((path . "/api/patches/2002/") (status . 500)))
    ;; patch 2003 stays reachable and applies immediately -- only 2002 queues
    (patchwork-cache-set-series-state server 1002 "rejected")
    (should (eq (patchwork-cache-series-pending-value (plist-get server :url) 1002 "state")
                'mixed))))

(ert-deftest patchwork-pending-test-per-server-not-per-project-drain-timing ()
  ;; A server with two projects: queue a change against a patch in
  ;; project 1, then run a full sync. The drain must happen once, after
  ;; BOTH projects have synced -- not right after the first project's
  ;; loop iteration, which would run before project 2's own sync had a
  ;; chance to (harmlessly, but incidentally) touch anything.
  (patchwork-test-with-mock-server port
    (patchwork-test-with-temp-db
      (let* ((server (patchwork-test-server-plist port :projects '("testproj" "otherproj")))
             (patchwork-servers (list server))
             (drain-call-count 0))
        (patchwork-cache-sync)
        (patchwork-test-control port "set-status" '((path . "/api/patches/2001/") (status . 500)))
        (patchwork-cache-set-patch-state server 2001 "rejected")
        (patchwork-test-control port "set-status" '((path . "/api/patches/2001/") (status . nil)))
        (let ((counter (lambda (&rest _) (setq drain-call-count (1+ drain-call-count)))))
          (advice-add 'patchwork-cache--drain-pending-changes :before counter)
          (unwind-protect
              (progn
                (patchwork-cache-sync t)
                (should (= drain-call-count 1))
                (should-not (patchwork-db-get-pending-change (plist-get server :url) 2001 "state")))
            (advice-remove 'patchwork-cache--drain-pending-changes counter)))))))

(provide 'test-pending-changes)

;;; test-pending-changes.el ends here
