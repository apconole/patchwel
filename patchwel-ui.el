;;; patchwel-ui.el --- Buffers for browsing Patchwork series -*- lexical-binding: t; -*-
;;; Code:

(require 'tabulated-list)
(require 'patchwel-config)
(require 'patchwel-db)
(require 'patchwel-cache)
(require 'patchwel-git)

(defun patchwork-series--format-date (date)
  "Return the calendar-date portion of DATE, an ISO-8601 timestamp or nil."
  (cond ((null date) "")
        ((>= (length date) 10) (substring date 0 10))
        (t date)))

(defvar-local patchwork-series--filter nil
  "Current filter for this series listing buffer.
A plist with keys :states (list of state strings, or nil for all),
:server (a server URL, or nil for all), :project (a project slug, or
nil for all), and :author (a case-insensitive substring, or nil for
all).  Set on buffer creation from `patchwork-default-state-filter';
change at runtime with `patchwork-series-set-filter' or
`patchwork-series-reset-filter'.")

(defun patchwork-series--matches-filter-p (series filter)
  "Return non-nil if SERIES passes every dimension of FILTER."
  (and (or (null (plist-get filter :states))
           (member (plist-get series :state) (plist-get filter :states)))
       (or (null (plist-get filter :server))
           (equal (plist-get series :server-url) (plist-get filter :server)))
       (or (null (plist-get filter :project))
           (equal (plist-get series :project-slug) (plist-get filter :project)))
       (or (null (plist-get filter :author))
           (and (plist-get series :submitter)
                (string-match-p (regexp-quote (downcase (plist-get filter :author)))
                                 (downcase (plist-get series :submitter)))))))

(defun patchwork-series--filter-description (filter)
  "Return a short human-readable summary of FILTER for the mode line."
  (let (parts)
    (when (plist-get filter :states)
      (push (string-join (plist-get filter :states) "/") parts))
    (when (plist-get filter :server)
      (push (format "server:%s" (patchwork-server-slug (list :url (plist-get filter :server))))
            parts))
    (when (plist-get filter :project)
      (push (format "project:%s" (plist-get filter :project)) parts))
    (when (plist-get filter :author)
      (push (format "author:%s" (plist-get filter :author)) parts))
    (if parts (string-join (nreverse parts) " ") "all")))

(defun patchwork-series--entry (series)
  "Build a `tabulated-list-entries' element for SERIES.
The entry id is a (SERVER-URL . SERIES-ID) cons, since series ids are
only unique within a single server."
  (let ((server-url (plist-get series :server-url))
        (id (plist-get series :id)))
    (list (cons server-url id)
          (vector
           (patchwork-server-slug (list :url server-url))
           (or (plist-get series :project-slug) "")
           (number-to-string id)
           (or (plist-get series :name) "")
           (or (plist-get series :submitter) "")
           (patchwork-series--format-date (plist-get series :submitted-at))
           (number-to-string (or (plist-get series :comment-count) 0))
           (number-to-string (or (plist-get series :ack-count) 0))
           (number-to-string (or (plist-get series :review-count) 0))
           (number-to-string (or (plist-get series :test-count) 0))
           (number-to-string (or (plist-get series :fixes-count) 0))
           (number-to-string (or (plist-get series :check-success) 0))
           (number-to-string (or (plist-get series :check-warning) 0))
           (number-to-string (or (plist-get series :check-fail) 0))
           (or (plist-get series :assignee) "")
           (or (plist-get series :state) "")))))

(defun patchwork-series--populate ()
  "Refresh `tabulated-list-entries' from the local cache, applying the
buffer's current filter, and update the mode line to show it."
  (setq tabulated-list-entries
        (mapcar #'patchwork-series--entry
                (seq-filter (lambda (s) (patchwork-series--matches-filter-p s patchwork-series--filter))
                            (patchwork-db-query-series))))
  (setq mode-line-process (format " [%s]" (patchwork-series--filter-description patchwork-series--filter))))

(defun patchwork-series-at-point ()
  "Return the (SERVER-URL . SERIES-ID) entry id at point, or nil."
  (tabulated-list-get-id))

(defun patchwork-series-refresh (&optional force)
  "Re-sync from every configured Patchwork server and redraw the listing.
With a prefix argument, FORCE a sync even if the cache is fresh."
  (interactive "P")
  (patchwork-cache-sync force)
  (patchwork-series--populate)
  (tabulated-list-print t))

(defun patchwork-series-view-at-point ()
  "Open the detail buffer for the series at point."
  (interactive)
  (let ((entry (patchwork-series-at-point)))
    (if entry
        (patchwork-view-series-details (car entry) (cdr entry))
      (message "No series on this line"))))

(defun patchwork-series-apply-at-point ()
  "Apply every patch in the series at point to a chosen git repository."
  (interactive)
  (let ((entry (patchwork-series-at-point)))
    (if entry
        (let ((server (or (patchwork-servers-find (car entry))
                           (error "Unknown Patchwork server: %s" (car entry)))))
          (patchwork-apply-series server (cdr entry)
                                   (read-directory-name "Git repository directory: ")))
      (message "No series on this line"))))

(defun patchwork-series-set-filter ()
  "Interactively edit this buffer's state/server/project/author filter.
Each prompt is pre-filled with the current value; clear the input to
remove that dimension (show every value for it)."
  (interactive)
  (let* ((current patchwork-series--filter)
         (states-str (read-string
                      "Filter states (comma-separated, empty = all): "
                      (and (plist-get current :states)
                           (string-join (plist-get current :states) ","))))
         (server (completing-read
                  "Filter server (empty = all): "
                  (mapcar (lambda (s) (plist-get s :url)) patchwork-servers)
                  nil nil (plist-get current :server)))
         (project (read-string "Filter project (empty = all): " (plist-get current :project)))
         (author (read-string "Filter author substring (empty = all): " (plist-get current :author))))
    (setq patchwork-series--filter
          (list :states (unless (string-empty-p states-str)
                          (split-string states-str "[ \t]*,[ \t]*" t))
                :server (unless (string-empty-p server) server)
                :project (unless (string-empty-p project) project)
                :author (unless (string-empty-p author) author)))
    (patchwork-series--populate)
    (tabulated-list-print t)
    (message "Filter: %s" (patchwork-series--filter-description patchwork-series--filter))))

(defun patchwork-series-reset-filter ()
  "Reset this buffer's filter back to `patchwork-default-state-filter'."
  (interactive)
  (setq patchwork-series--filter (list :states patchwork-default-state-filter))
  (patchwork-series--populate)
  (tabulated-list-print t)
  (message "Filter reset to default: %s"
           (patchwork-series--filter-description patchwork-series--filter)))

(defvar patchwork-series-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'patchwork-series-view-at-point)
    (define-key map "g" #'patchwork-series-refresh)
    (define-key map "a" #'patchwork-series-apply-at-point)
    (define-key map "f" #'patchwork-series-set-filter)
    (define-key map "F" #'patchwork-series-reset-filter)
    (define-key map "q" #'quit-window)
    map)
  "Keymap for `patchwork-series-mode'.")

(define-derived-mode patchwork-series-mode tabulated-list-mode "Patchwork-Series"
  "Major mode listing cached Patchwork series and their review status."
  (setq tabulated-list-format
        [("Server" 12 t)
         ("Project" 10 t)
         ("ID" 6 t)
         ("Title" 36 t)
         ("Author" 14 t)
         ("Submitted" 12 t)
         ("Cmts" 5 t)
         ("Ack" 4 t)
         ("Rev" 4 t)
         ("Test" 5 t)
         ("Fix" 4 t)
         ("Succ" 5 t)
         ("Warn" 5 t)
         ("Fail" 5 t)
         ("Assignee" 12 t)
         ("State" 18 t)])
  (setq tabulated-list-padding 1)
  (setq tabulated-list-sort-key (cons "Submitted" t))
  (tabulated-list-init-header))

;;;###autoload
(defun patchwork-show-series ()
  "Display cached Patchwork series, from every configured server, in the
main listing buffer.  Syncs from the Patchwork API first (subject to
`patchwork-cache-ttl').  Shows only `patchwork-default-state-filter'
states the first time this buffer is created; re-invoking this command
on an already-open buffer just refreshes its data and keeps whatever
filter is currently active (see `patchwork-series-set-filter')."
  (interactive)
  (let ((buffer (get-buffer-create patchwork-series-buffer-name)))
    (patchwork-cache-sync)
    (with-current-buffer buffer
      (unless (derived-mode-p 'patchwork-series-mode)
        (patchwork-series-mode)
        (setq patchwork-series--filter (list :states patchwork-default-state-filter)))
      (patchwork-series--populate)
      (tabulated-list-print t))
    (switch-to-buffer buffer)))

(defvar-local patchwork-series-detail--server-url nil
  "Server URL that the current detail buffer is showing a series from.")

(defvar-local patchwork-series-detail--id nil
  "Series id that the current detail buffer is showing.")

(defun patchwork-view-series-details (server-url series-id)
  "Show a detail buffer for SERIES-ID on SERVER-URL.
Displays metadata, tag/check counters, and its patches."
  (let* ((series (patchwork-db-get-series server-url series-id))
         (patches (patchwork-db-get-series-patches server-url series-id))
         (buffer (get-buffer-create
                  (format "*patchwork-series-%s-%s*"
                          (patchwork-server-slug (list :url server-url))
                          series-id))))
    (unless series
      (error "No cached series %s on %s" series-id server-url))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "%s\n" (or (plist-get series :name) "(untitled series)")))
        (insert (make-string (min 70 (max 8 (length (or (plist-get series :name) ""))))
                              ?=))
        (insert "\n\n")
        (insert (format "Server:      %s\n" server-url))
        (insert (format "Project:     %s\n" (or (plist-get series :project-slug) "")))
        (insert (format "Author:      %s\n" (or (plist-get series :submitter) "")))
        (insert (format "Submitted:   %s\n"
                        (patchwork-series--format-date (plist-get series :submitted-at))))
        (insert (format "Assignee:    %s\n" (or (plist-get series :assignee) "unassigned")))
        (insert (format "State:       %s\n" (or (plist-get series :state) "")))
        (insert (format "Comments:    %d\n" (or (plist-get series :comment-count) 0)))
        (insert (format "Tags:        Ack %d  Review %d  Tested %d  Fixes %d\n"
                        (or (plist-get series :ack-count) 0)
                        (or (plist-get series :review-count) 0)
                        (or (plist-get series :test-count) 0)
                        (or (plist-get series :fixes-count) 0)))
        (insert (format "Checks:      Success %d  Warning %d  Failure %d\n"
                        (or (plist-get series :check-success) 0)
                        (or (plist-get series :check-warning) 0)
                        (or (plist-get series :check-fail) 0)))
        (insert "\n--- Patches ---\n")
        (dolist (patch patches)
          (insert (format "%3d. #%-8d [%-16s] %s\n"
                          (or (plist-get patch :series-position) 0)
                          (plist-get patch :id)
                          (or (plist-get patch :state) "")
                          (or (plist-get patch :name) "")))
          (dolist (comment (patchwork-db-get-comments server-url (plist-get patch :id)))
            (insert (format "       [%s] %s: %s\n"
                            (patchwork-series--format-date (plist-get comment :date))
                            (or (plist-get comment :author) "")
                            (car (split-string (or (plist-get comment :content) "") "\n")))))))
      (goto-char (point-min))
      (patchwork-series-detail-mode)
      (setq patchwork-series-detail--server-url server-url)
      (setq patchwork-series-detail--id series-id))
    (switch-to-buffer buffer)))

(defvar patchwork-series-detail-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" #'patchwork-series-detail-refresh)
    (define-key map "a" #'patchwork-series-detail-apply)
    (define-key map "q" #'quit-window)
    map)
  "Keymap for `patchwork-series-detail-mode'.")

(define-derived-mode patchwork-series-detail-mode special-mode "Patchwork-Series-Detail"
  "Major mode for viewing details of a single Patchwork series.")

(defun patchwork-series-detail-refresh ()
  "Refresh the current series detail buffer from the local cache."
  (interactive)
  (when patchwork-series-detail--id
    (patchwork-view-series-details patchwork-series-detail--server-url
                                    patchwork-series-detail--id)))

(defun patchwork-series-detail-apply ()
  "Apply every patch in the series shown in this buffer to a chosen repository."
  (interactive)
  (when patchwork-series-detail--id
    (let ((server (or (patchwork-servers-find patchwork-series-detail--server-url)
                       (error "Unknown Patchwork server: %s"
                              patchwork-series-detail--server-url))))
      (patchwork-apply-series server patchwork-series-detail--id
                               (read-directory-name "Git repository directory: ")))))

(provide 'patchwel-ui)

;;; patchwel-ui.el ends here
