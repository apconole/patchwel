;;; test-ui-render.el --- buffer rendering/navigation tests -*- lexical-binding: t; -*-
(require 'ert)

(defmacro patchwork-ui-test--with-seeded-listing (&rest body)
  "Seed a temp db with two series across two projects, render the
listing buffer (no sync), run BODY, then kill the buffer."
  (declare (indent 0))
  `(patchwork-test-with-temp-db
     (patchwork-db-insert-project "http://x" 1 "Proj One" "proj1")
     (patchwork-db-insert-project "http://x" 2 "Proj Two" "proj2")
     (patchwork-db-upsert-series
      (list :server-url "http://x" :id 1001 :project-id 1 :project-slug "proj1"
            :name "First series" :submitter "Alice" :version 1 :total 1
            :submitted-at (patchwork-cache--iso-datetime (current-time))
            :state "new" :assignee nil :comment-count 0 :ack-count 0 :review-count 0
            :test-count 0 :fixes-count 0 :check-success 0 :check-warning 0 :check-fail 0
            :url "" :updated-at ""))
     (patchwork-db-upsert-series
      (list :server-url "http://x" :id 1002 :project-id 2 :project-slug "proj2"
            :name "Second series" :submitter "Bob" :version 1 :total 1
            :submitted-at (patchwork-cache--iso-datetime (current-time))
            :state "new" :assignee nil :comment-count 0 :ack-count 0 :review-count 0
            :test-count 0 :fixes-count 0 :check-success 0 :check-warning 0 :check-fail 0
            :url "" :updated-at ""))
     (unwind-protect
         (let ((patchwork-default-state-filter nil))
           (patchwork-show-series t)
           ,@body)
       (when (get-buffer patchwork-series-buffer-name)
         (kill-buffer patchwork-series-buffer-name)))))

(ert-deftest patchwork-ui-test-show-series-renders-grouped-listing ()
  (patchwork-ui-test--with-seeded-listing
    (with-current-buffer patchwork-series-buffer-name
      (should (derived-mode-p 'patchwork-series-mode))
      (goto-char (point-min))
      (should (search-forward "First series" nil t))
      (should (search-forward "Second series" nil t)))))

(ert-deftest patchwork-ui-test-collapse-then-expand-all ()
  (patchwork-ui-test--with-seeded-listing
    (with-current-buffer patchwork-series-buffer-name
      (patchwork-series-collapse-all)
      (goto-char (point-min))
      (should-not (search-forward "First series" nil t))
      (patchwork-series-expand-all)
      (goto-char (point-min))
      (should (search-forward "First series" nil t)))))

(ert-deftest patchwork-ui-test-series-next-moves-across-rows ()
  (patchwork-ui-test--with-seeded-listing
    (with-current-buffer patchwork-series-buffer-name
      (goto-char (point-min))
      (let (found-a found-b)
        (while (and (not found-b) (patchwork-series-next))
          (let ((entry (patchwork-series-at-point)))
            (when entry
              (unless found-a (setq found-a entry))
              (setq found-b entry))))
        (should found-a)
        (should found-b)))))

(ert-deftest patchwork-ui-test-series-detail-renders-checks-as-buttons ()
  (patchwork-test-with-temp-db
    (patchwork-db-insert-project "http://x" 1 "Proj" "proj")
    (patchwork-db-upsert-series
     (list :server-url "http://x" :id 1 :project-id 1 :project-slug "proj"
           :name "s" :submitter "Alice" :version 1 :total 1 :submitted-at "2026-01-01"
           :state "new" :assignee nil :comment-count 0 :ack-count 0 :review-count 0
           :test-count 0 :fixes-count 0 :check-success 1 :check-warning 0 :check-fail 0
           :url "" :updated-at "2026-01-01"))
    (patchwork-db-insert-patch
     (list :server-url "http://x" :id 100 :series-id 1 :project-id 1 :state "new"
           :submitter "Alice" :delegate nil :name "a patch" :date "2026-01-01"
           :series-position 1 :check-state "success" :updated-at "2026-01-01"))
    (patchwork-db-insert-check
     (list :server-url "http://x" :id 1 :patch-id 100 :reporter "ci" :state "success"
           :context "build" :description "build passed"
           :target-url "http://ci.example.com/1" :date "2026-01-01"))
    (unwind-protect
        (progn
          (patchwork-view-series-details "http://x" 1)
          (with-current-buffer "*patchwork-series-x-1*"
            (patchwork-series-detail-expand-all)
            (goto-char (point-min))
            (search-forward "http://ci.example.com/1")
            (backward-char 1)
            (should (button-at (point)))
            (should (equal (button-label (button-at (point))) "http://ci.example.com/1"))
            (let (browsed)
              (cl-letf (((symbol-function 'browse-url) (lambda (url) (setq browsed url))))
                (push-button (point)))
              (should (equal browsed "http://ci.example.com/1")))))
      (when (get-buffer "*patchwork-series-x-1*")
        (kill-buffer "*patchwork-series-x-1*")))))

(provide 'test-ui-render)

;;; test-ui-render.el ends here
