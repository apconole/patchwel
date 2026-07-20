;;; test-git-review.el --- apply-as-commits/review/branch-strategy tests -*- lexical-binding: t; -*-
(require 'ert)

(defun patchwork-git-review-test--current-branch (repo)
  (with-temp-buffer
    (call-process "git" nil t nil "-C" repo "branch" "--show-current")
    (string-trim (buffer-string))))

(defun patchwork-git-review-test--log-oneline (repo)
  (with-temp-buffer
    (call-process "git" nil t nil "-C" repo "log" "--oneline")
    (buffer-string)))

(defun patchwork-git-review-test--write-mbox-to-temp (content)
  (let ((f (make-temp-file "patchwork-test-mbox")))
    (write-region content nil f)
    f))

;; -- git am --3way happy/conflict paths --------------------------------------

(ert-deftest patchwork-git-test-apply-series-as-commits-happy-path ()
  (patchwork-test-with-temp-repo repo
    (patchwork-test-with-temp-repo source-repo
      (patchwork-test-with-temp-db
        (let* ((mbox1 (patchwork-test-make-patch-fixture source-repo "line one\n" "patch one"))
               (mbox2 (patchwork-test-make-patch-fixture source-repo "line two\n" "patch two"))
               (server (list :url "http://x" :token nil :projects nil)))
          (patchwork-db-insert-project (plist-get server :url) 1 "Proj" "proj")
          (patchwork-db-upsert-series
           (list :server-url (plist-get server :url) :id 1 :project-id 1 :project-slug "proj"
                 :name "s" :submitter "Alice" :version 1 :total 1 :submitted-at "2026-01-01"
                 :state "new" :assignee nil :comment-count 0 :ack-count 0 :review-count 0
                 :test-count 0 :fixes-count 0 :check-success 0 :check-warning 0 :check-fail 0
                 :url "" :updated-at "2026-01-01"))
          (patchwork-db-insert-patch
           (list :server-url (plist-get server :url) :id 1 :series-id 1 :project-id 1
                 :state "new" :submitter "Alice" :name "p1" :date "2026-01-01"
                 :series-position 1))
          (patchwork-db-insert-patch
           (list :server-url (plist-get server :url) :id 2 :series-id 1 :project-id 1
                 :state "new" :submitter "Alice" :name "p2" :date "2026-01-02"
                 :series-position 2))
          (cl-letf (((symbol-function 'patchwork-git-download-patch)
                     (lambda (_server id)
                       (patchwork-git-review-test--write-mbox-to-temp
                        (if (= id 1) mbox1 mbox2)))))
            (let ((revs (patchwork-apply-series-as-commits server 1 repo)))
              (should revs)
              (should (equal (cdr revs) (patchwork-test-head-rev repo)))
              (should (= (length (split-string
                                   (patchwork-git-review-test--log-oneline repo) "\n" t))
                         3)))))))))

(ert-deftest patchwork-git-test-apply-series-as-commits-conflict-then-continue ()
  (patchwork-test-with-temp-repo repo
    (patchwork-test-with-temp-repo source-repo
      (patchwork-test-with-temp-db
        (let* ((mbox1 (patchwork-test-make-patch-fixture source-repo "line one\n" "patch one"))
               (server (list :url "http://x" :token nil :projects nil)))
          (patchwork-db-insert-project (plist-get server :url) 1 "Proj" "proj")
          (patchwork-db-upsert-series
           (list :server-url (plist-get server :url) :id 1 :project-id 1 :project-slug "proj"
                 :name "s" :submitter "Alice" :version 1 :total 1 :submitted-at "2026-01-01"
                 :state "new" :assignee nil :comment-count 0 :ack-count 0 :review-count 0
                 :test-count 0 :fixes-count 0 :check-success 0 :check-warning 0 :check-fail 0
                 :url "" :updated-at "2026-01-01"))
          (patchwork-db-insert-patch
           (list :server-url (plist-get server :url) :id 1 :series-id 1 :project-id 1
                 :state "new" :submitter "Alice" :name "p1" :date "2026-01-01"
                 :series-position 1))
          ;; diverge target repo so patch 1 cannot apply cleanly
          (write-region "diverged\n" nil (expand-file-name "file.txt" repo))
          (patchwork-test--git repo "commit" "-q" "-am" "diverge")
          (cl-letf (((symbol-function 'patchwork-git-download-patch)
                     (lambda (_server _id)
                       (patchwork-git-review-test--write-mbox-to-temp mbox1))))
            (should-not (patchwork-apply-series-as-commits server 1 repo))
            (should (get-buffer (patchwork-git--am-buffer-name repo)))
            (should (patchwork-git--am-conflicted-files repo))
            ;; resolve by hand and continue
            (write-region "diverged\nline one\n" nil (expand-file-name "file.txt" repo))
            (patchwork-test--git repo "add" "file.txt")
            (patchwork-git-am-continue repo)
            (should (string-match-p "patch one"
                                    (patchwork-git-review-test--log-oneline repo)))))))))

(ert-deftest patchwork-git-test-apply-series-as-commits-conflict-then-abort ()
  (patchwork-test-with-temp-repo repo
    (patchwork-test-with-temp-repo source-repo
      (patchwork-test-with-temp-db
        (let* ((mbox1 (patchwork-test-make-patch-fixture source-repo "line one\n" "patch one"))
               (server (list :url "http://x" :token nil :projects nil))
               (before-rev (patchwork-test-head-rev repo)))
          (patchwork-db-insert-project (plist-get server :url) 1 "Proj" "proj")
          (patchwork-db-upsert-series
           (list :server-url (plist-get server :url) :id 1 :project-id 1 :project-slug "proj"
                 :name "s" :submitter "Alice" :version 1 :total 1 :submitted-at "2026-01-01"
                 :state "new" :assignee nil :comment-count 0 :ack-count 0 :review-count 0
                 :test-count 0 :fixes-count 0 :check-success 0 :check-warning 0 :check-fail 0
                 :url "" :updated-at "2026-01-01"))
          (patchwork-db-insert-patch
           (list :server-url (plist-get server :url) :id 1 :series-id 1 :project-id 1
                 :state "new" :submitter "Alice" :name "p1" :date "2026-01-01"
                 :series-position 1))
          (write-region "diverged\n" nil (expand-file-name "file.txt" repo))
          (patchwork-test--git repo "commit" "-q" "-am" "diverge")
          (setq before-rev (patchwork-test-head-rev repo))
          (cl-letf (((symbol-function 'patchwork-git-download-patch)
                     (lambda (_server _id)
                       (patchwork-git-review-test--write-mbox-to-temp mbox1))))
            (should-not (patchwork-apply-series-as-commits server 1 repo))
            (patchwork-git-am-abort repo)
            (should (equal (patchwork-test-head-rev repo) before-rev))))))))

(ert-deftest patchwork-git-test-apply-series-as-commits-conflict-then-skip ()
  (patchwork-test-with-temp-repo repo
    (patchwork-test-with-temp-repo source-repo
      (patchwork-test-with-temp-db
        (let* ((mbox1 (patchwork-test-make-patch-fixture source-repo "line one\n" "patch one"))
               (server (list :url "http://x" :token nil :projects nil)))
          (patchwork-db-insert-project (plist-get server :url) 1 "Proj" "proj")
          (patchwork-db-upsert-series
           (list :server-url (plist-get server :url) :id 1 :project-id 1 :project-slug "proj"
                 :name "s" :submitter "Alice" :version 1 :total 1 :submitted-at "2026-01-01"
                 :state "new" :assignee nil :comment-count 0 :ack-count 0 :review-count 0
                 :test-count 0 :fixes-count 0 :check-success 0 :check-warning 0 :check-fail 0
                 :url "" :updated-at "2026-01-01"))
          (patchwork-db-insert-patch
           (list :server-url (plist-get server :url) :id 1 :series-id 1 :project-id 1
                 :state "new" :submitter "Alice" :name "p1" :date "2026-01-01"
                 :series-position 1))
          (write-region "diverged\n" nil (expand-file-name "file.txt" repo))
          (patchwork-test--git repo "commit" "-q" "-am" "diverge")
          (let ((before-rev (patchwork-test-head-rev repo)))
            (cl-letf (((symbol-function 'patchwork-git-download-patch)
                       (lambda (_server _id)
                         (patchwork-git-review-test--write-mbox-to-temp mbox1))))
              (should-not (patchwork-apply-series-as-commits server 1 repo))
              (patchwork-git-am-skip repo)
              ;; skipping the only patch leaves HEAD where it started
              (should (equal (patchwork-test-head-rev repo) before-rev)))))))))

;; -- review backend dispatch --------------------------------------------------

(ert-deftest patchwork-git-test-review-backend-magit-called-with-range ()
  (let (called-with)
    (unwind-protect
        (progn
          (fset 'magit-log-setup-buffer
                (lambda (revs args files &optional _locked _focus)
                  (setq called-with (list revs args files))))
          (patchwork-review-backend-magit "/tmp/some-repo" "aaa" "bbb")
          (should (equal (car called-with) '("aaa..bbb"))))
      (fmakunbound 'magit-log-setup-buffer))))

(ert-deftest patchwork-git-test-review-backends-prefers-magit-when-available ()
  (let (magit-called vc-called)
    (unwind-protect
        (progn
          (fset 'magit-log-setup-buffer (lambda (&rest _) (setq magit-called t)))
          (cl-letf (((symbol-function 'patchwork-review-backend-vc)
                     (lambda (&rest _) (setq vc-called t) t)))
            (run-hook-with-args-until-success
             'patchwork-review-backends "/tmp/some-repo" "aaa" "bbb")
            (should magit-called)
            (should-not vc-called)))
      (fmakunbound 'magit-log-setup-buffer))))

(ert-deftest patchwork-git-test-review-backend-vc-opens-log-view ()
  (patchwork-test-with-temp-repo repo
    (write-region "more\n" nil (expand-file-name "file.txt" repo) t)
    (patchwork-test--git repo "commit" "-q" "-am" "second commit")
    (let ((before-rev (patchwork-test--git-first-commit repo))
          (after-rev (patchwork-test-head-rev repo)))
      (should (patchwork-review-backend-vc repo before-rev after-rev))
      (should (get-buffer "*vc-change-log*"))
      (with-current-buffer "*vc-change-log*"
        (should (eq major-mode 'vc-git-log-view-mode))))))

(defun patchwork-test--git-first-commit (repo)
  (with-temp-buffer
    (call-process "git" nil t nil "-C" repo "rev-list" "--max-parents=0" "HEAD")
    (string-trim (buffer-string))))

;; -- branch strategies ---------------------------------------------------

(ert-deftest patchwork-git-test-branch-strategy-noop-without-configuration ()
  (patchwork-test-with-temp-repo repo
    (let ((patchwork-project-branch-strategies nil)
          (before (patchwork-git-review-test--current-branch repo)))
      (patchwork-git--maybe-checkout-branch "http://x" "proj" 42 repo)
      (should (equal (patchwork-git-review-test--current-branch repo) before)))))

(ert-deftest patchwork-git-test-branch-strategy-creates-and-checks-out-branch ()
  (patchwork-test-with-temp-repo repo
    (let ((patchwork-project-branch-strategies
           '((("http://x" . "proj") . "HEAD:review_%i"))))
      (patchwork-git--maybe-checkout-branch "http://x" "proj" 42 repo)
      (should (equal (patchwork-git-review-test--current-branch repo) "review_42")))))

(ert-deftest patchwork-git-test-branch-strategy-errors-on-existing-branch ()
  (patchwork-test-with-temp-repo repo
    (patchwork-test--git repo "branch" "review_42")
    (let ((patchwork-project-branch-strategies
           '((("http://x" . "proj") . "HEAD:review_%i"))))
      (should-error (patchwork-git--maybe-checkout-branch "http://x" "proj" 42 repo)))))

(ert-deftest patchwork-git-test-parse-branch-strategy-nil-is-nil ()
  (should (null (patchwork-parse-branch-strategy nil 42))))

(ert-deftest patchwork-git-test-parse-branch-strategy-substitutes-series-id ()
  (should (equal (patchwork-parse-branch-strategy "upstream/main:review_%i" 514346)
                 (cons "upstream/main" "review_514346"))))

(ert-deftest patchwork-git-test-parse-branch-strategy-malformed-errors ()
  (should-error (patchwork-parse-branch-strategy "no-colon-here" 1)))

;; -- per-project git tree lookup ----------------------------------------

(ert-deftest patchwork-git-test-project-git-tree-prompts-then-remembers ()
  (let ((patchwork-project-git-trees nil))
    (cl-letf (((symbol-function 'read-directory-name) (lambda (&rest _) "/tmp/some-repo"))
              ((symbol-function 'y-or-n-p) (lambda (&rest _) t))
              ((symbol-function 'customize-save-variable) (lambda (&rest _) nil)))
      (should (equal (patchwork-project-git-tree "http://x" "proj") "/tmp/some-repo")))
    (should (equal (cdr (assoc (cons "http://x" "proj") patchwork-project-git-trees))
                   "/tmp/some-repo"))
    ;; already configured: no prompt needed this time
    (cl-letf (((symbol-function 'read-directory-name)
               (lambda (&rest _) (error "should not have prompted"))))
      (should (equal (patchwork-project-git-tree "http://x" "proj") "/tmp/some-repo")))))

(ert-deftest patchwork-git-test-project-git-tree-declines-save ()
  (let ((patchwork-project-git-trees nil))
    (cl-letf (((symbol-function 'read-directory-name) (lambda (&rest _) "/tmp/some-repo"))
              ((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
      (should (equal (patchwork-project-git-tree "http://x" "proj") "/tmp/some-repo")))
    (should (null patchwork-project-git-trees))))

(provide 'test-git-review)

;;; test-git-review.el ends here
