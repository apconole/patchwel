;;; test-mail.el --- mail-reply composition tests for patchwel-mail.el -*- lexical-binding: t; -*-
(require 'ert)

(ert-deftest patchwork-mail-test-wide-reply-recipients-excludes-self-and-author ()
  (let ((user-mail-address "me@example.com"))
    (let* ((comment (list :submitter-email "author@example.com"
                           :to "list@example.com, Author <author@example.com>"
                           :cc "Me <me@example.com>, Other <other@example.com>"))
           (recipients (patchwork-mail--wide-reply-recipients comment)))
      (should (equal (car recipients) "author@example.com"))
      ;; author excluded even though present in original To; self excluded
      ;; from Cc; "Other" remains
      (should (string-match-p "list@example.com" (cdr recipients)))
      (should (string-match-p "other@example.com" (cdr recipients)))
      (should-not (string-match-p "author@example.com" (cdr recipients)))
      (should-not (string-match-p "me@example.com" (cdr recipients))))))

(ert-deftest patchwork-mail-test-wide-reply-recipients-requires-author-email ()
  (should-error (patchwork-mail--wide-reply-recipients (list :submitter-email nil))))

(ert-deftest patchwork-mail-test-reply-subject-idempotent ()
  (should (equal (patchwork-mail--reply-subject "a patch") "Re: a patch"))
  (should (equal (patchwork-mail--reply-subject "Re: a patch") "Re: a patch"))
  (should (equal (patchwork-mail--reply-subject nil) "Re: ")))

(ert-deftest patchwork-mail-test-reply-references-chain ()
  (should (equal (patchwork-mail--reply-references
                   (list :references "<a@x> <b@x>" :msgid "<c@x>"))
                 "<a@x> <b@x> <c@x>"))
  (should (equal (patchwork-mail--reply-references (list :references nil :msgid "<c@x>"))
                 "<c@x>"))
  (should (equal (patchwork-mail--reply-references (list :references "<a@x>" :msgid nil))
                 "<a@x>"))
  (should (null (patchwork-mail--reply-references (list :references nil :msgid nil)))))

(ert-deftest patchwork-mail-test-quote-content-adds-one-level ()
  (should (equal (patchwork-mail--quote-content "line one\nline two")
                 "> line one\n> line two"))
  (should (equal (patchwork-mail--quote-content "> already quoted")
                 "> > already quoted")))

(ert-deftest patchwork-mail-test-reply-to-comment-composes-full-buffer ()
  (let ((user-mail-address "me@example.com"))
    (let ((comment (list :author "Bob Reviewer" :submitter-email "bob@example.com"
                          :subject "a patch" :msgid "<comment-1@x>"
                          :references "<patch-1@x>"
                          :to "list@example.com" :cc "Me <me@example.com>"
                          :content "> old quote\nnew reply text")))
      (patchwork-mail-reply-to-comment comment)
      (unwind-protect
          (progn
            (should (derived-mode-p 'message-mode))
            (should (equal (message-fetch-field "To") "bob@example.com"))
            (should (equal (message-fetch-field "Subject") "Re: a patch"))
            (should (equal (message-fetch-field "In-Reply-To") "<comment-1@x>"))
            (should (equal (message-fetch-field "References") "<patch-1@x> <comment-1@x>"))
            (save-excursion
              (message-goto-body)
              (should (search-forward "Bob Reviewer <bob@example.com> writes:" nil t))
              (should (search-forward "> > old quote" nil t))
              (should (search-forward "> new reply text" nil t))))
        (kill-buffer)))))

(ert-deftest patchwork-mail-test-compose-hook-runs-in-composed-buffer ()
  (let* ((user-mail-address "me@example.com")
         (hook-buffer nil)
         (patchwork-mail-compose-hook
          (list (lambda ()
                  (setq hook-buffer (current-buffer))
                  (message-add-header
                   (format-time-string "Gcc: nnfolder+archive:sent.%Y-%m"))))))
    (let ((comment (list :author "Bob Reviewer" :submitter-email "bob@example.com"
                          :subject "a patch" :msgid "<comment-1@x>"
                          :content "reply text")))
      (patchwork-mail-reply-to-comment comment)
      (unwind-protect
          (progn
            (should (eq hook-buffer (current-buffer)))
            (should (equal (message-fetch-field "Gcc")
                            (format-time-string "nnfolder+archive:sent.%Y-%m"))))
        (kill-buffer)))))

(ert-deftest patchwork-mail-test-reply-to-patch-prefers-cached-msgid ()
  (patchwork-test-with-temp-db
    (let ((user-mail-address "me@example.com")
          (server (list :url "http://x" :token nil :projects nil)))
      (patchwork-db-insert-patch
       (list :server-url "http://x" :id 1 :series-id 1 :project-id 1 :state "new"
             :submitter "Alice" :name "a patch" :date "2026-01-01" :series-position 1
             :content "commit body" :submitter-email "alice@example.com"
             :msgid "<patch-1@x>" :to "list@example.com" :cc nil
             :references nil :in-reply-to nil))
      (cl-letf (((symbol-function 'patchwork-api-get-patch)
                 (lambda (&rest _) (error "should not live-fetch when cache has msgid"))))
        (patchwork-mail-reply-to-patch server 1)
        (unwind-protect
            (should (equal (message-fetch-field "To") "alice@example.com"))
          (kill-buffer))))))

(ert-deftest patchwork-mail-test-reply-to-patch-falls-back-to-live-fetch ()
  (patchwork-test-with-temp-db
    (let ((user-mail-address "me@example.com")
          (server (list :url "http://x" :token nil :projects nil)))
      ;; cached patch with no msgid -- cache from before headers were tracked
      (patchwork-db-insert-patch
       (list :server-url "http://x" :id 1 :series-id 1 :project-id 1 :state "new"
             :submitter "Alice" :name "a patch" :date "2026-01-01" :series-position 1))
      (cl-letf (((symbol-function 'patchwork-api-get-patch)
                 (lambda (&rest _)
                   (list :submitter (list :name "Alice" :email "alice@example.com")
                         :content "commit body" :msgid "<patch-1@x>" :name "a patch"
                         :headers (list :To "list@example.com")))))
        (patchwork-mail-reply-to-patch server 1)
        (unwind-protect
            (should (equal (message-fetch-field "In-Reply-To") "<patch-1@x>"))
          (kill-buffer))))))

(provide 'test-mail)

;;; test-mail.el ends here
