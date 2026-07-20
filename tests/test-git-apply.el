;;; test-git-apply.el --- working-tree-only apply tests for patchwel-git.el -*- lexical-binding: t; -*-
(require 'ert)

(defmacro patchwork-git-test--with-server-and-repo (server-var repo-var &rest body)
  "Bind SERVER-VAR to a mock server plist and REPO-VAR to a scratch git
repository, with `patchwork-git-temp-dir' pointed at a scratch cache
directory, run BODY."
  (declare (indent 2))
  `(patchwork-test-with-mock-server port
     (patchwork-test-with-temp-repo ,repo-var
       (let ((,server-var (patchwork-test-server-plist port))
             (patchwork-git-temp-dir (make-temp-file "patchwork-test-mboxdir" t)))
         (unwind-protect
             (progn ,@body)
           (delete-directory patchwork-git-temp-dir t))))))

(ert-deftest patchwork-git-test-apply-patch-success ()
  (patchwork-git-test--with-server-and-repo server repo
    (let ((before-rev (patchwork-test-head-rev repo)))
      (should (patchwork-apply-patch server 2001 repo))
      (should (file-exists-p (expand-file-name "patch-2001.txt" repo)))
      ;; working-tree only: no commit was created
      (should (equal (patchwork-test-head-rev repo) before-rev))
      (with-temp-buffer
        (call-process "git" nil t nil "-C" repo "status" "--porcelain")
        (should (string-match-p "^\\?\\? patch-2001.txt" (buffer-string)))))))

(ert-deftest patchwork-git-test-apply-patch-conflict-produces-reject-file ()
  ;; Build a real modify-file.txt patch against one scratch repo (so its
  ;; diff context expects file.txt's original "base\n" content), then
  ;; apply it to a second scratch repo whose file.txt has since diverged
  ;; -- a hunk-context mismatch, which `git apply --reject' can leave a
  ;; real .rej file for (unlike a whole-file "already exists" conflict).
  (patchwork-test-with-temp-repo target-repo
    (patchwork-test-with-temp-repo source-repo
      (let* ((patchwork-git-temp-dir (make-temp-file "patchwork-test-mboxdir" t))
             (mbox (patchwork-test-make-patch-fixture
                    source-repo "line appended in source\n" "modify file.txt")))
        (unwind-protect
            (progn
              (write-region "totally different content\n" nil
                            (expand-file-name "file.txt" target-repo))
              (patchwork-test--git target-repo "commit" "-q" "-am" "diverge")
              (cl-letf (((symbol-function 'patchwork-git-download-patch)
                         (lambda (_server _id)
                           (let ((f (expand-file-name "fixture.patch" target-repo)))
                             (write-region mbox nil f)
                             f)))
                        ((symbol-function 'find-file) (lambda (&rest _) nil)))
                (should-not (patchwork-apply-patch
                             (list :url "http://x" :token nil :projects nil)
                             999 target-repo)))
              (should (file-exists-p (expand-file-name "file.txt.rej" target-repo))))
          (delete-directory patchwork-git-temp-dir t))))))

(ert-deftest patchwork-git-test-apply-series-applies-every-patch ()
  (patchwork-git-test--with-server-and-repo server repo
    (patchwork-test-with-temp-db
      (patchwork-db-insert-project (plist-get server :url) 1 "Proj" "testproj")
      (patchwork-db-insert-patch
       (list :server-url (plist-get server :url) :id 2002 :series-id 1002
             :project-id 1 :state "new" :submitter "Alice" :name "p1"
             :date "2026-01-01" :series-position 1))
      (patchwork-db-insert-patch
       (list :server-url (plist-get server :url) :id 2003 :series-id 1002
             :project-id 1 :state "new" :submitter "Alice" :name "p2"
             :date "2026-01-02" :series-position 2))
      (should (patchwork-apply-series server 1002 repo))
      (should (file-exists-p (expand-file-name "patch-2002.txt" repo)))
      (should (file-exists-p (expand-file-name "patch-2003.txt" repo))))))

(ert-deftest patchwork-git-test-undo-patch ()
  (patchwork-git-test--with-server-and-repo server repo
    (should (patchwork-apply-patch server 2001 repo))
    (should (file-exists-p (expand-file-name "patch-2001.txt" repo)))
    (should (patchwork-undo-patch server 2001 repo))
    (should-not (file-exists-p (expand-file-name "patch-2001.txt" repo)))))

(provide 'test-git-apply)

;;; test-git-apply.el ends here
