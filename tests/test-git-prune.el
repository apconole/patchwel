;;; test-git-prune.el --- cached patch mbox pruning tests -*- lexical-binding: t; -*-
(require 'ert)

(defmacro patchwork-git-prune-test--with-cache-dir (dir-var &rest body)
  (declare (indent 1))
  `(let ((,dir-var (make-temp-file "patchwork-test-prunedir" t)))
     (unwind-protect (progn ,@body)
       (delete-directory ,dir-var t))))

(defun patchwork-git-prune-test--touch (file &optional days-ago)
  "Create FILE with placeholder content, backdating its mtime by
DAYS-AGO days (0 for \"now\")."
  (write-region "placeholder\n" nil file)
  (when (and days-ago (> days-ago 0))
    (let ((time (time-subtract (current-time) (days-to-time days-ago))))
      (set-file-times file time))))

(ert-deftest patchwork-git-prune-test-age-based-only-deletes-old-files ()
  (patchwork-git-prune-test--with-cache-dir dir
    (let ((patchwork-git-temp-dir dir)
          (patchwork-patch-cache-max-age-days 30)
          (old-file (expand-file-name "x-1.patch" dir))
          (new-file (expand-file-name "x-2.patch" dir))
          (other-file (expand-file-name "other.txt" dir)))
      (patchwork-git-prune-test--touch old-file 40)
      (patchwork-git-prune-test--touch new-file 0)
      (patchwork-git-prune-test--touch other-file 40)
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_p) t)))
        (patchwork-git-prune-patches))
      (should-not (file-exists-p old-file))
      (should (file-exists-p new-file))
      (should (file-exists-p other-file)))))

(ert-deftest patchwork-git-prune-test-all-prefix-arg-deletes-regardless-of-age ()
  (patchwork-git-prune-test--with-cache-dir dir
    (let ((patchwork-git-temp-dir dir)
          (new-file (expand-file-name "x-1.patch" dir)))
      (patchwork-git-prune-test--touch new-file 0)
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_p) t)))
        (patchwork-git-prune-patches t))
      (should-not (file-exists-p new-file)))))

(ert-deftest patchwork-git-prune-test-declining-confirmation-prunes-nothing ()
  (patchwork-git-prune-test--with-cache-dir dir
    (let ((patchwork-git-temp-dir dir)
          (old-file (expand-file-name "x-1.patch" dir)))
      (patchwork-git-prune-test--touch old-file 40)
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_p) nil)))
        (patchwork-git-prune-patches))
      (should (file-exists-p old-file)))))

(ert-deftest patchwork-git-prune-test-nothing-eligible-no-prompt ()
  (patchwork-git-prune-test--with-cache-dir dir
    (let ((patchwork-git-temp-dir dir)
          (new-file (expand-file-name "x-1.patch" dir))
          (prompted nil))
      (patchwork-git-prune-test--touch new-file 0)
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_p) (setq prompted t) t)))
        (patchwork-git-prune-patches))
      (should-not prompted)
      (should (file-exists-p new-file)))))

(provide 'test-git-prune)

;;; test-git-prune.el ends here
