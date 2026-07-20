;;; test-api.el --- HTTP/JSON layer tests for patchwel-api.el -*- lexical-binding: t; -*-
(require 'ert)

(ert-deftest patchwork-api-test-get-single-resource ()
  (patchwork-test-with-mock-server port
    (let ((server (patchwork-test-server-plist port)))
      (let ((patch (patchwork-api-get-patch server 2002)))
        (should (equal (plist-get patch :name) "[PATCH 1/2] first of two"))
        (should (plist-get patch :diff))
        (should (plist-get patch :headers))))))

(ert-deftest patchwork-api-test-pagination-concatenates-pages ()
  (patchwork-test-with-mock-server port
    (let ((server (patchwork-test-server-plist port)))
      ;; 3 series fixtures, force 1 per page so 3 pages are needed
      (let ((series (patchwork-api-list-series server nil '(("per_page" . 1)))))
        (should (= (length series) 3))
        (should (equal (sort (mapcar (lambda (s) (plist-get s :id)) series) #'<)
                       '(1001 1002 1003)))))))

(ert-deftest patchwork-api-test-http-error-status ()
  (patchwork-test-with-mock-server port
    (patchwork-test-control port "set-status" '((path . "/api/series/") (status . 500)))
    (let ((server (patchwork-test-server-plist port)))
      (let ((err (should-error (patchwork-api-list-series server)
                                :type 'patchwork-api-http-error)))
        (should (= (nth 1 err) 500))))))

(ert-deftest patchwork-api-test-timeout ()
  (patchwork-test-with-mock-server port
    (patchwork-test-control port "set-delay" '((path . "/api/series/") (seconds . 2)))
    (let ((server (patchwork-test-server-plist port))
          (patchwork-sync-timeout 1))
      (should-error (patchwork-api-list-series server)
                    :type 'patchwork-api-timeout-error))))

(ert-deftest patchwork-api-test-per-server-sync-timeout-override ()
  ;; A server whose events endpoint is consistently slower than the
  ;; global default needs a longer timeout of its own, without raising
  ;; it for every other server -- :sync-timeout is exactly that override.
  (patchwork-test-with-mock-server port
    (patchwork-test-control port "set-delay" '((path . "/api/events/") (seconds . 2)))
    (let ((patchwork-sync-timeout 1))
      (let ((default-server (patchwork-test-server-plist port)))
        (should-error (patchwork-api-list-events default-server "proj" nil)
                      :type 'patchwork-api-timeout-error))
      (let ((overridden-server (patchwork-test-server-plist port :sync-timeout 5)))
        (should (patchwork-api-list-events overridden-server "proj" nil))))))

(ert-deftest patchwork-api-test-challenge-no-hook ()
  (patchwork-test-with-mock-server port
    (patchwork-test-control port "set-challenge" '((path . "/api/series/") (on . t)))
    (let ((server (patchwork-test-server-plist port))
          (patchwork-api-challenge-hook nil))
      (let ((err (should-error (patchwork-api-list-series server) :type 'error)))
        (should (string-match-p "anti-bot" (error-message-string err)))))))

(ert-deftest patchwork-api-test-challenge-hook-returns-nil ()
  (patchwork-test-with-mock-server port
    (patchwork-test-control port "set-challenge" '((path . "/api/series/") (on . t)))
    (let ((server (patchwork-test-server-plist port))
          (patchwork-api-challenge-hook (list (lambda (_s _u _b) nil))))
      (let ((err (should-error (patchwork-api-list-series server) :type 'error)))
        (should (string-match-p "anti-bot" (error-message-string err)))))))

(ert-deftest patchwork-api-test-challenge-hook-returns-new-buffer ()
  (patchwork-test-with-mock-server port
    (patchwork-test-control port "set-challenge" '((path . "/api/series/") (on . t)))
    (let* ((substitute-buffer nil)
           (server (patchwork-test-server-plist port))
           (patchwork-api-challenge-hook
            (list (lambda (_server _url _buffer)
                    (setq substitute-buffer (generate-new-buffer " *fake-solved*"))
                    (with-current-buffer substitute-buffer
                      (insert "HTTP/1.1 200 OK\n\n[]"))
                    substitute-buffer))))
      (should (equal (patchwork-api-list-series server) '()))
      ;; process-response kills the substitute buffer itself once done,
      ;; since it isn't `eq' to the original challenge-page buffer
      (should (not (buffer-live-p substitute-buffer))))))

(ert-deftest patchwork-api-test-challenge-hook-mutates-same-buffer ()
  (patchwork-test-with-mock-server port
    (patchwork-test-control port "set-challenge" '((path . "/api/series/") (on . t)))
    (let* ((server (patchwork-test-server-plist port))
           (patchwork-api-challenge-hook
            (list (lambda (_server _url buffer)
                    (with-current-buffer buffer
                      (goto-char (point-min))
                      (search-forward "\n\n")
                      (delete-region (point) (point-max))
                      (insert "[]"))
                    buffer))))
      (should (equal (patchwork-api-list-series server) '())))))

(ert-deftest patchwork-api-test-non-html-non-json-body-no-anti-bot-annotation ()
  ;; a plain-text (non-HTML) non-JSON body should still error, but without
  ;; the "anti-bot" annotation, since only an HTML-looking body gets it
  (let ((buffer (generate-new-buffer " *plain-text-response*")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (insert "HTTP/1.1 200 OK\n\nnot json, not html either"))
          (let ((err (should-error
                      (patchwork-api--process-response
                       (patchwork-test-server-plist 1) "http://x" buffer)
                      :type 'error)))
            (should-not (string-match-p "anti-bot" (error-message-string err)))))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest patchwork-api-test-user-agent-default-and-override ()
  (patchwork-test-with-mock-server port
    (let ((server (patchwork-test-server-plist port)))
      (patchwork-api-list-series server)
      (let* ((log (patchwork-test-control-log port))
             (last (car (last log))))
        (should (equal (plist-get (plist-get last :headers) :User-Agent)
                       patchwork-user-agent))))
    (let ((server (patchwork-test-server-plist port :user-agent "my-agent/1.0")))
      (patchwork-api-list-series server)
      (let* ((log (patchwork-test-control-log port))
             (last (car (last log))))
        (should (equal (plist-get (plist-get last :headers) :User-Agent)
                       "my-agent/1.0"))))))

(ert-deftest patchwork-api-test-authorization-header-present-iff-token ()
  (patchwork-test-with-mock-server port
    (let ((server (patchwork-test-server-plist port)))
      (patchwork-api-list-series server)
      (let* ((log (patchwork-test-control-log port))
             (last (car (last log))))
        (should-not (plist-get (plist-get last :headers) :Authorization))))
    (let ((server (patchwork-test-server-plist port :token "sekret")))
      (patchwork-api-list-series server)
      (let* ((log (patchwork-test-control-log port))
             (last (car (last log))))
        (should (equal (plist-get (plist-get last :headers) :Authorization)
                       "Token sekret"))))))

(ert-deftest patchwork-api-test-set-delegate-and-state ()
  (patchwork-test-with-mock-server port
    (let ((server (patchwork-test-server-plist port)))
      (patchwork-api-set-delegate server 2002 "bob")
      (should (equal (plist-get (patchwork-api-get-patch server 2002) :delegate) "bob"))
      (patchwork-api-set-state server 2002 "accepted")
      (should (equal (plist-get (patchwork-api-get-patch server 2002) :state) "accepted")))))

(provide 'test-api)

;;; test-api.el ends here
