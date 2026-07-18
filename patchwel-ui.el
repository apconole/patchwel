;;; patchwel-ui.el --- Buffers for browsing Patchwork series -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

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

(defconst patchwork-series--row-format
  "  %-6s %-34.34s %-14.14s %-10s %5s %4s %4s %5s %4s %5s %5s %5s %-12.12s %-16s"
  "Format string for one series row and for the column header row.
Columns: id, title, author, submitted, comments, ack, review, test,
fixes, check-success, check-warning, check-fail, assignee, state.
Server and project aren't columns here since each group's header line
already names them once for every series underneath it.")

(defun patchwork-series--header-row-string ()
  "Return the column header line matching `patchwork-series--row-format'."
  (format patchwork-series--row-format
          "ID" "Title" "Author" "Submitted" "Cmts" "Ack" "Rev" "Test" "Fix"
          "Succ" "Warn" "Fail" "Assignee" "State"))

(defun patchwork-series--row-string (series)
  "Format SERIES as one aligned display row."
  (format patchwork-series--row-format
          (plist-get series :id)
          (or (plist-get series :name) "")
          (or (plist-get series :submitter) "")
          (patchwork-series--format-date (plist-get series :submitted-at))
          (or (plist-get series :comment-count) 0)
          (or (plist-get series :ack-count) 0)
          (or (plist-get series :review-count) 0)
          (or (plist-get series :test-count) 0)
          (or (plist-get series :fixes-count) 0)
          (or (plist-get series :check-success) 0)
          (or (plist-get series :check-warning) 0)
          (or (plist-get series :check-fail) 0)
          (or (plist-get series :assignee) "")
          (or (plist-get series :state) "")))

(defvar-local patchwork-series--collapsed nil
  "Hash table of (SERVER-URL . PROJECT-SLUG) group key -> non-nil if
that group is currently collapsed.  Persists across `g' refreshes of
the same buffer so a user's collapse/expand choices aren't lost.")

(defun patchwork-series--grouped ()
  "Return cached series matching the buffer's filter, grouped and sorted.
Result is an alist of ((SERVER-URL . PROJECT-SLUG) . SERIES-LIST),
groups ordered by server then project, each SERIES-LIST ordered newest
submitted first."
  (let ((matching (seq-filter (lambda (s) (patchwork-series--matches-filter-p s patchwork-series--filter))
                               (patchwork-db-query-series)))
        (groups (make-hash-table :test #'equal))
        (order nil))
    (dolist (s matching)
      (let ((key (cons (plist-get s :server-url) (plist-get s :project-slug))))
        (unless (gethash key groups) (push key order))
        (puthash key (cons s (gethash key groups)) groups)))
    (mapcar (lambda (key)
              (cons key
                    (sort (gethash key groups)
                          (lambda (a b) (string> (or (plist-get a :submitted-at) "")
                                                  (or (plist-get b :submitted-at) ""))))))
            (sort order
                  (lambda (a b)
                    (let ((sa (patchwork-server-slug (list :url (car a))))
                          (sb (patchwork-server-slug (list :url (car b)))))
                      (if (string= sa sb)
                          (string< (or (cdr a) "") (or (cdr b) ""))
                        (string< sa sb))))))))

(defun patchwork-series--render ()
  "Redraw the series listing buffer: grouped by server/project, with
collapsible `[+]'/`[-]' group headers, preserving the current line."
  (unless (hash-table-p patchwork-series--collapsed)
    (setq patchwork-series--collapsed (make-hash-table :test #'equal)))
  (let ((inhibit-read-only t)
        (line (line-number-at-pos)))
    (erase-buffer)
    (insert (patchwork-series--header-row-string) "\n")
    (insert (make-string (length (patchwork-series--header-row-string)) ?-) "\n")
    (dolist (group (patchwork-series--grouped))
      (let* ((key (car group))
             (series-list (cdr group))
             (collapsed (gethash key patchwork-series--collapsed))
             (label (format "[%s] %s | %s  (%d)"
                            (if collapsed "+" "-")
                            (patchwork-server-slug (list :url (car key)))
                            (or (cdr key) "(no project)")
                            (length series-list))))
        (insert (propertize label 'patchwork-group key 'face 'bold) "\n")
        (unless collapsed
          (dolist (s series-list)
            (insert (propertize (patchwork-series--row-string s)
                                 'patchwork-series-entry
                                 (cons (plist-get s :server-url) (plist-get s :id)))
                    "\n")))))
    (goto-char (point-min))
    (forward-line (1- line))
    (setq mode-line-process (format " [%s]" (patchwork-series--filter-description patchwork-series--filter)))))

(defun patchwork-series-at-point ()
  "Return the (SERVER-URL . SERIES-ID) of the series row at point, or nil."
  (get-text-property (line-beginning-position) 'patchwork-series-entry))

(defun patchwork-series-group-at-point ()
  "Return the (SERVER-URL . PROJECT-SLUG) of the group header at point, or nil."
  (get-text-property (line-beginning-position) 'patchwork-group))

(defun patchwork-series-refresh (&optional force)
  "Re-sync from every configured Patchwork server and redraw the listing.
With a prefix argument, do a full resync (`patchwork-sync-lookback-days'
worth) rather than the usual incremental events-based sync, even if
the cache is still fresh -- use this to recover from any gap in event
coverage instead of waiting for the next periodic full sync."
  (interactive "P")
  (patchwork-cache-sync force)
  (patchwork-series--render))

(defun patchwork-series-toggle-group (key)
  "Toggle the collapsed/expanded state of the group KEY and redraw."
  (unless patchwork-series--collapsed
    (setq patchwork-series--collapsed (make-hash-table :test #'equal)))
  (if (gethash key patchwork-series--collapsed)
      (remhash key patchwork-series--collapsed)
    (puthash key t patchwork-series--collapsed))
  (patchwork-series--render))

(defun patchwork-series-dwim ()
  "View the series at point, or toggle the group header at point."
  (interactive)
  (let ((entry (patchwork-series-at-point))
        (group (patchwork-series-group-at-point)))
    (cond
     (entry (patchwork-view-series-details (car entry) (cdr entry)))
     (group (patchwork-series-toggle-group group))
     (t (message "Nothing on this line")))))

(defun patchwork-series-expand-all ()
  "Expand every group in the series listing buffer."
  (interactive)
  (setq patchwork-series--collapsed (make-hash-table :test #'equal))
  (patchwork-series--render))

(defun patchwork-series-collapse-all ()
  "Collapse every group in the series listing buffer."
  (interactive)
  (setq patchwork-series--collapsed (make-hash-table :test #'equal))
  (dolist (group (patchwork-series--grouped))
    (puthash (car group) t patchwork-series--collapsed))
  (patchwork-series--render))

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
    (patchwork-series--render)
    (message "Filter: %s" (patchwork-series--filter-description patchwork-series--filter))))

(defun patchwork-series-reset-filter ()
  "Reset this buffer's filter back to `patchwork-default-state-filter'."
  (interactive)
  (setq patchwork-series--filter (list :states patchwork-default-state-filter))
  (patchwork-series--render)
  (message "Filter reset to default: %s"
           (patchwork-series--filter-description patchwork-series--filter)))

(defvar patchwork-series-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'patchwork-series-dwim)
    (define-key map (kbd "TAB") #'patchwork-series-dwim)
    (define-key map "g" #'patchwork-series-refresh)
    (define-key map "a" #'patchwork-series-apply-at-point)
    (define-key map "f" #'patchwork-series-set-filter)
    (define-key map "F" #'patchwork-series-reset-filter)
    (define-key map "+" #'patchwork-series-expand-all)
    (define-key map "-" #'patchwork-series-collapse-all)
    (define-key map "q" #'quit-window)
    map)
  "Keymap for `patchwork-series-mode'.")

(define-derived-mode patchwork-series-mode special-mode "Patchwork-Series"
  "Major mode listing cached Patchwork series, grouped by server and
project under collapsible `[+]'/`[-]' headers.  Press RET or TAB on a
group header to toggle it, or on a series row to view its details.")

;;;###autoload
(defun patchwork-show-series ()
  "Display cached Patchwork series, from every configured server, in the
main listing buffer, grouped by server and project.  Syncs from the
Patchwork API first (subject to `patchwork-cache-ttl').  Shows only
`patchwork-default-state-filter' states the first time this buffer is
created; re-invoking this command on an already-open buffer just
refreshes its data and keeps whatever filter and group collapse state
are currently active (see `patchwork-series-set-filter')."
  (interactive)
  (let ((buffer (get-buffer-create patchwork-series-buffer-name)))
    (patchwork-cache-sync)
    (with-current-buffer buffer
      (unless (derived-mode-p 'patchwork-series-mode)
        (patchwork-series-mode)
        (setq patchwork-series--filter (list :states patchwork-default-state-filter)))
      (patchwork-series--render))
    (switch-to-buffer buffer)))

(defvar-local patchwork-series-detail--server-url nil
  "Server URL that the current detail buffer is showing a series from.")

(defvar-local patchwork-series-detail--id nil
  "Series id that the current detail buffer is showing.")

(defvar-local patchwork-series-detail--expanded-comments nil
  "Hash table of comment id -> non-nil if that comment is expanded to
show its full text, in the current detail buffer.  Persists across
`g' refreshes of the same buffer.")

(defun patchwork-series-detail--comment-at-point ()
  "Return the comment id at point in a detail buffer, or nil."
  (get-text-property (line-beginning-position) 'patchwork-comment-id))

(defun patchwork-view-series-details (server-url series-id)
  "Show a detail buffer for SERIES-ID on SERVER-URL.
Displays metadata, tag/check counters, its patches, and each patch's
comments -- collapsed to a one-line summary by default; RET or TAB on
a comment line toggles it open to show the full text."
  (let* ((series (patchwork-db-get-series server-url series-id))
         (patches (patchwork-db-get-series-patches server-url series-id))
         (buffer (get-buffer-create
                  (format "*patchwork-series-%s-%s*"
                          (patchwork-server-slug (list :url server-url))
                          series-id))))
    (unless series
      (error "No cached series %s on %s" series-id server-url))
    (with-current-buffer buffer
      (unless (hash-table-p patchwork-series-detail--expanded-comments)
        (setq patchwork-series-detail--expanded-comments (make-hash-table :test #'eql)))
      (let ((inhibit-read-only t)
            (line (line-number-at-pos)))
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
            (let* ((comment-id (plist-get comment :id))
                   (expanded (gethash comment-id patchwork-series-detail--expanded-comments))
                   (content (or (plist-get comment :content) ""))
                   (summary (car (split-string content "\n"))))
              (insert (propertize
                       (format "       [%s] [%s] %s: %s\n"
                               (if expanded "-" "+")
                               (patchwork-series--format-date (plist-get comment :date))
                               (or (plist-get comment :author) "")
                               summary)
                       'patchwork-comment-id comment-id))
              (when expanded
                (dolist (content-line (split-string content "\n"))
                  (insert (format "         %s\n" content-line)))
                (insert "\n")))))
        (goto-char (point-min))
        (forward-line (1- line)))
      (unless (derived-mode-p 'patchwork-series-detail-mode)
        (patchwork-series-detail-mode))
      (setq patchwork-series-detail--server-url server-url)
      (setq patchwork-series-detail--id series-id))
    (switch-to-buffer buffer)))

(defun patchwork-series-detail-toggle-comment ()
  "Toggle the comment at point between collapsed and fully expanded."
  (interactive)
  (let ((comment-id (patchwork-series-detail--comment-at-point)))
    (if (null comment-id)
        (message "No comment on this line")
      (unless (hash-table-p patchwork-series-detail--expanded-comments)
        (setq patchwork-series-detail--expanded-comments (make-hash-table :test #'eql)))
      (if (gethash comment-id patchwork-series-detail--expanded-comments)
          (remhash comment-id patchwork-series-detail--expanded-comments)
        (puthash comment-id t patchwork-series-detail--expanded-comments))
      (patchwork-view-series-details patchwork-series-detail--server-url
                                      patchwork-series-detail--id))))

(defvar patchwork-series-detail-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'patchwork-series-detail-toggle-comment)
    (define-key map (kbd "TAB") #'patchwork-series-detail-toggle-comment)
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
