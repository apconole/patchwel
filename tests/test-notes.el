;;; test-notes.el --- per-patch/per-series notes editing workflow -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:
(require 'ert)

(defmacro patchwork-notes-test--with-seeded-series (&rest body)
  "Seed a temp db with one project/series/patch, run BODY, then kill
any leftover *patchwork-note:* or detail/listing buffers."
  (declare (indent 0))
  `(patchwork-test-with-temp-db
     (patchwork-db-insert-project "http://x" 1 "Proj" "proj")
     (patchwork-db-upsert-series
      (list :server-url "http://x" :id 1 :project-id 1 :project-slug "proj"
            :name "s" :submitter "Alice" :version 1 :total 1 :submitted-at "2026-01-01"
            :state "new" :assignee nil :comment-count 0 :ack-count 0 :review-count 0
            :test-count 0 :fixes-count 0 :check-success 0 :check-warning 0 :check-fail 0
            :url "" :updated-at "2026-01-01"))
     (patchwork-db-insert-patch
      (list :server-url "http://x" :id 100 :series-id 1 :project-id 1 :state "new"
            :submitter "Alice" :delegate nil :name "a patch" :date "2026-01-01"
            :series-position 1 :check-state "success" :updated-at "2026-01-01"))
     (unwind-protect
         (progn ,@body)
       (dolist (buf (buffer-list))
         (when (string-prefix-p "*patchwork-note:" (buffer-name buf))
           (kill-buffer buf)))
       (when (get-buffer "*patchwork-series-x-1*")
         (kill-buffer "*patchwork-series-x-1*"))
       (when (get-buffer patchwork-series-buffer-name)
         (kill-buffer patchwork-series-buffer-name)))))

(ert-deftest patchwork-notes-test-edit-buffer-prefilled-and-mode ()
  (patchwork-notes-test--with-seeded-series
    (patchwork-db-set-note "http://x" "patch" 100 "existing note")
    (patchwork-notes--edit "http://x" "patch" 100 "patch 100" #'ignore)
    (with-current-buffer "*patchwork-note: patch 100*"
      (should (derived-mode-p 'patchwork-notes-edit-mode))
      (should (equal (buffer-string) "existing note")))))

(ert-deftest patchwork-notes-test-edit-buffer-empty-when-no-existing-note ()
  (patchwork-notes-test--with-seeded-series
    (patchwork-notes--edit "http://x" "patch" 100 "patch 100" #'ignore)
    (with-current-buffer "*patchwork-note: patch 100*"
      (should (equal (buffer-string) "")))))

(ert-deftest patchwork-notes-test-save-persists-kills-buffer-and-refreshes ()
  (patchwork-notes-test--with-seeded-series
    (let ((refreshed nil))
      (patchwork-notes--edit "http://x" "patch" 100 "patch 100"
                              (lambda () (setq refreshed t)))
      (with-current-buffer "*patchwork-note: patch 100*"
        (insert "a new note")
        (patchwork-notes-edit-save))
      (should (equal (patchwork-db-get-note "http://x" "patch" 100) "a new note"))
      (should-not (get-buffer "*patchwork-note: patch 100*"))
      (should refreshed))))

(ert-deftest patchwork-notes-test-save-blank-deletes-existing-note ()
  (patchwork-notes-test--with-seeded-series
    (patchwork-db-set-note "http://x" "patch" 100 "will be cleared")
    (patchwork-notes--edit "http://x" "patch" 100 "patch 100" #'ignore)
    (with-current-buffer "*patchwork-note: patch 100*"
      (erase-buffer)
      (insert "   \n  ")
      (patchwork-notes-edit-save))
    (should-not (patchwork-db-get-note "http://x" "patch" 100))))

(ert-deftest patchwork-notes-test-cancel-discards-without-saving ()
  (patchwork-notes-test--with-seeded-series
    (patchwork-notes--edit "http://x" "patch" 100 "patch 100" #'ignore)
    (with-current-buffer "*patchwork-note: patch 100*"
      (insert "should not be saved")
      (patchwork-notes-edit-cancel))
    (should-not (get-buffer "*patchwork-note: patch 100*"))
    (should-not (patchwork-db-get-note "http://x" "patch" 100))))

(ert-deftest patchwork-notes-test-series-entry-point-from-listing ()
  (patchwork-notes-test--with-seeded-series
    (let ((patchwork-default-state-filter nil))
      (patchwork-show-series t))
    (with-current-buffer patchwork-series-buffer-name
      (goto-char (point-min))
      (search-forward "Alice")
      (beginning-of-line)
      (patchwork-series-edit-note-at-point))
    (with-current-buffer "*patchwork-note: series 1*"
      (insert "series-level note")
      (patchwork-notes-edit-save))
    (should (equal (patchwork-db-get-note "http://x" "series" 1) "series-level note"))
    ;; the listing buffer's Nt column should now show the marker
    (with-current-buffer patchwork-series-buffer-name
      (goto-char (point-min))
      (should (search-forward "  *  " nil t)))))

(ert-deftest patchwork-notes-test-detail-entry-points ()
  (patchwork-notes-test--with-seeded-series
    (patchwork-view-series-details "http://x" 1)
    (with-current-buffer "*patchwork-series-x-1*"
      (goto-char (point-min))
      (search-forward "a patch")
      (beginning-of-line)
      (patchwork-series-detail-edit-note-at-point))
    (with-current-buffer "*patchwork-note: patch 100*"
      (insert "patch-level note")
      (patchwork-notes-edit-save))
    (should (equal (patchwork-db-get-note "http://x" "patch" 100) "patch-level note"))
    (with-current-buffer "*patchwork-series-x-1*"
      (should (save-excursion (goto-char (point-min)) (search-forward "[note]" nil t))))
    (with-current-buffer "*patchwork-series-x-1*"
      (patchwork-series-detail-edit-series-note))
    (with-current-buffer "*patchwork-note: series 1*"
      (insert "series-level note via detail buffer")
      (patchwork-notes-edit-save))
    (should (equal (patchwork-db-get-note "http://x" "series" 1)
                    "series-level note via detail buffer"))
    (with-current-buffer "*patchwork-series-x-1*"
      (should (save-excursion (goto-char (point-min)) (search-forward "Notes:" nil t))))))

(provide 'test-notes)

;;; test-notes.el ends here
