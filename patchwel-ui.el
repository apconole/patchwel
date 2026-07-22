;;; patchwel-ui.el --- Buffers for browsing Patchwork series -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'diff-mode)
(require 'button)
(require 'browse-url)
(require 'patchwel-config)
(require 'patchwel-db)
(require 'patchwel-cache)
(require 'patchwel-git)
(require 'patchwel-mail)

(defun patchwork-series--format-date (date)
  "Return the calendar-date portion of DATE, an ISO-8601 timestamp or nil."
  (cond ((null date) "")
        ((>= (length date) 10) (substring date 0 10))
        (t date)))

(defface patchwork-series-mine-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for a series assigned to one of `patchwork-my-identities'."
  :group 'patchwork)

(defface patchwork-series-stale-face
  '((t :inherit warning))
  "Face for a series old enough (`patchwork-series-stale-days') with no
comments yet -- likely needs a nudge."
  :group 'patchwork)

(defface patchwork-series-old-face
  '((t :inherit shadow))
  "Face for a series older than `patchwork-series-old-days', regardless
of comment activity."
  :group 'patchwork)

(defface patchwork-series-title-face
  '((t :inherit font-lock-function-name-face))
  "Face for a series' title column in the listing buffer."
  :group 'patchwork)

(defface patchwork-series-date-face
  '((t :inherit font-lock-comment-face))
  "Face for a series' submitted-date column in the listing buffer."
  :group 'patchwork)

(defface patchwork-patch-line-face
  '((t :inherit font-lock-keyword-face))
  "Face for a patch's summary line in a series detail buffer,
distinguishing it at a glance from the comment lines beneath it."
  :group 'patchwork)

(defface patchwork-check-success-face
  '((t :inherit success))
  "Face for a CI check line whose state is \"success\"."
  :group 'patchwork)

(defface patchwork-check-warning-face
  '((t :inherit warning))
  "Face for a CI check line whose state is \"warning\"."
  :group 'patchwork)

(defface patchwork-check-fail-face
  '((t :inherit error))
  "Face for a CI check line whose state is \"fail\"."
  :group 'patchwork)

(defun patchwork-series-detail--check-face (state)
  "Return the face for a check's STATE, or nil for an unrecognized one."
  (cond ((equal state "success") 'patchwork-check-success-face)
        ((equal state "warning") 'patchwork-check-warning-face)
        ((equal state "fail") 'patchwork-check-fail-face)))

(defface patchwork-comment-quote-face
  '((t :inherit font-lock-comment-face))
  "Face for quoted lines (any nesting depth) within an expanded
comment's body, distinguishing them from the actual reply text."
  :group 'patchwork)

(defface patchwork-comment-reply-face
  '((t :inherit default))
  "Face for non-quoted lines -- the actual reply text -- within an
expanded comment's body."
  :group 'patchwork)

(defun patchwork-series-detail--propertize-comment-line (line)
  "Return LINE propertized as quoted or reply text, whichever it looks
like: a leading `>' (at any nesting depth, ignoring leading
whitespace) marks a quoted line."
  (propertize line 'face
              (if (string-match-p "\\`[ \t]*>" line)
                  'patchwork-comment-quote-face
                'patchwork-comment-reply-face)))

(defun patchwork-series--age-days (series)
  "Return SERIES' age in days since it was submitted, or nil if its
submitted-at date is missing or unparseable."
  (let ((submitted (plist-get series :submitted-at)))
    (when submitted
      (let ((parsed (ignore-errors (patchwork-parse-server-time submitted))))
        (when parsed
          (/ (float-time (time-subtract (current-time) parsed)) 86400.0))))))

(defun patchwork-series--rule-mine (series)
  "Highlight rule: SERIES is assigned to one of `patchwork-my-identities'."
  (let ((assignee (plist-get series :assignee)))
    (when (and patchwork-my-identities assignee)
      (let ((assignee (downcase assignee)))
        (when (seq-some (lambda (id) (string-match-p (regexp-quote (downcase id)) assignee))
                         patchwork-my-identities)
          'patchwork-series-mine-face)))))

(defun patchwork-series--rule-stale (series)
  "Highlight rule: SERIES is old (`patchwork-series-stale-days') with no
comments yet."
  (let ((age (patchwork-series--age-days series)))
    (when (and age (>= age patchwork-series-stale-days)
               (= (or (plist-get series :comment-count) 0) 0))
      'patchwork-series-stale-face)))

(defun patchwork-series--rule-old (series)
  "Highlight rule: SERIES is older than `patchwork-series-old-days'."
  (let ((age (patchwork-series--age-days series)))
    (when (and age (>= age patchwork-series-old-days))
      'patchwork-series-old-face)))

(defcustom patchwork-series-highlight-rules
  '(patchwork-series--rule-mine
    patchwork-series--rule-stale
    patchwork-series--rule-old)
  "Functions used to highlight a row in the series listing buffer.
Each is called with a series plist and should return a face, or nil.
Applied in order; the first to return non-nil wins.  Extend this list
with your own predicate functions for custom highlighting -- e.g. one
that checks `plist-get series :state' or any other field."
  :type '(repeat function)
  :group 'patchwork)

(defun patchwork-series--row-face (series)
  "Return the face to use for SERIES' row, per
`patchwork-series-highlight-rules', or nil for none."
  (seq-some (lambda (rule) (funcall rule series)) patchwork-series-highlight-rules))

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

(defconst patchwork-series--column-formats
  '("%-6s" "%-34.34s" "%-14.14s" "%-10s" "%5s" "%4s" "%4s" "%5s" "%4s"
    "%5s" "%5s" "%5s" "%-12.12s" "%-16s")
  "Per-column format specs, in order, shared by
`patchwork-series--header-row-string' and `patchwork-series--row-string'
so their widths can never drift apart.  Columns: id, title, author,
submitted, comments, ack, review, test, fixes, check-success,
check-warning, check-fail, assignee, state.  Server and project aren't
columns here since each group's header line already names them once
for every series underneath it.")

(defun patchwork-series--format-columns (values &optional faces)
  "Format VALUES (one per `patchwork-series--column-formats' entry) into
a single row string, left-padded and joined by single spaces.  FACES,
if given, is a parallel list of faces (or nil) applied to each
formatted column individually."
  (concat "  "
          (mapconcat
           #'identity
           (seq-mapn (lambda (fmt value face)
                       (let ((text (format fmt value)))
                         (if face (propertize text 'face face) text)))
                     patchwork-series--column-formats
                     values
                     (or faces (make-list (length patchwork-series--column-formats) nil)))
           " ")))

(defun patchwork-series--header-row-string ()
  "Return the column header line."
  (patchwork-series--format-columns
   '("ID" "Title" "Author" "Submitted" "Cmts" "Ack" "Rev" "Test" "Fix"
     "Succ" "Warn" "Fail" "Assignee" "State")))

(defun patchwork-series--row-string (series)
  "Format SERIES as one aligned display row.
The title and submitted-date columns get their own faces
\(`patchwork-series-title-face'/`patchwork-series-date-face'); if any
`patchwork-series-highlight-rules' rule matches, its face is then
layered over the whole row (filling in gaps left by the per-column
faces, via `add-face-text-property' with APPEND so it doesn't clobber
them)."
  (let* ((row (patchwork-series--format-columns
               (list (plist-get series :id)
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
                     (or (plist-get series :state) ""))
               (list nil 'patchwork-series-title-face nil 'patchwork-series-date-face
                     nil nil nil nil nil nil nil nil nil nil)))
         (row-face (patchwork-series--row-face series)))
    (when row-face
      (add-face-text-property 0 (length row) row-face t row))
    row))

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

(defun patchwork-series--server-at-point ()
  "Return the server-url context at point: from the group header if
point is on one, or from the series entry if point is on a series
row.  nil if point is on neither."
  (or (car (patchwork-series-group-at-point))
      (car (patchwork-series-at-point))))

(defun patchwork-series-next ()
  "Move point to the next series row."
  (interactive)
  (let ((start (point)))
    (forward-line 1)
    (while (and (not (eobp)) (not (patchwork-series-at-point)))
      (forward-line 1))
    (if (patchwork-series-at-point)
        t
      (goto-char start)
      (message "No next series")
      nil)))

(defun patchwork-series-prev ()
  "Move point to the previous series row."
  (interactive)
  (let ((start (point)))
    (forward-line -1)
    (while (and (not (bobp)) (not (patchwork-series-at-point)))
      (forward-line -1))
    (if (patchwork-series-at-point)
        t
      (goto-char start)
      (message "No previous series")
      nil)))

(defun patchwork-series-next-server ()
  "Move point to the next group header for a server different from the
one at point -- i.e. skip over any remaining project groups under the
current server."
  (interactive)
  (let ((current (patchwork-series--server-at-point))
        (start (point)))
    (forward-line 1)
    (while (and (not (eobp))
                (let ((group (patchwork-series-group-at-point)))
                  (not (and group (not (equal (car group) current))))))
      (forward-line 1))
    (let ((group (patchwork-series-group-at-point)))
      (if (and group (not (equal (car group) current)))
          t
        (goto-char start)
        (message "No next server")
        nil))))

(defun patchwork-series-prev-server ()
  "Move point to the previous group header for a server different from
the one at point."
  (interactive)
  (let ((current (patchwork-series--server-at-point))
        (start (point)))
    (forward-line -1)
    (while (and (not (bobp))
                (let ((group (patchwork-series-group-at-point)))
                  (not (and group (not (equal (car group) current))))))
      (forward-line -1))
    (let ((group (patchwork-series-group-at-point)))
      (if (and group (not (equal (car group) current)))
          t
        (goto-char start)
        (message "No previous server")
        nil))))

(defun patchwork-series-refresh (&optional force)
  "Re-sync from every configured Patchwork server and redraw the listing.
With a prefix argument, do a full resync (`patchwork-sync-lookback-days'
worth) rather than the usual incremental events-based sync, even if
the cache is still fresh -- use this to recover from any gap in event
coverage instead of waiting for the next periodic full sync."
  (interactive "P")
  (patchwork-cache-sync force)
  (patchwork-series--render))

(defun patchwork-series-redisplay ()
  "Redraw the series listing buffer from whatever is already cached,
without contacting any Patchwork server at all -- unlike
`patchwork-series-refresh' (\\`g'), which always at least checks
whether a sync is due (subject to `patchwork-cache-ttl').  Useful when
a separate process (e.g. `patchwork-cron-sync.el' via crontab) is
already keeping the cache fresh on its own schedule, and you just want
this buffer to reflect that without the interactive session ever
touching the network on its own."
  (interactive)
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

(defun patchwork-series-review-at-point ()
  "Apply the series at point as commits and open a review view for them.
See `patchwork-review-series'."
  (interactive)
  (let ((entry (patchwork-series-at-point)))
    (if entry
        (let ((server (or (patchwork-servers-find (car entry))
                           (error "Unknown Patchwork server: %s" (car entry)))))
          (patchwork-review-series server (cdr entry)))
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
    (define-key map "l" #'patchwork-series-redisplay)
    (define-key map "G" #'patchwork-fetch-series)
    (define-key map "a" #'patchwork-series-apply-at-point)
    (define-key map "R" #'patchwork-series-review-at-point)
    (define-key map "f" #'patchwork-series-set-filter)
    (define-key map "F" #'patchwork-series-reset-filter)
    (define-key map "n" #'patchwork-series-next)
    (define-key map "p" #'patchwork-series-prev)
    (define-key map "N" #'patchwork-series-next-server)
    (define-key map "P" #'patchwork-series-prev-server)
    (define-key map "+" #'patchwork-series-expand-all)
    (define-key map "-" #'patchwork-series-collapse-all)
    (define-key map "q" #'quit-window)
    map)
  "Keymap for `patchwork-series-mode'.
n/p move to the next/previous series row; N/P move to the next/
previous server, skipping over any remaining project groups under the
current one.")

(define-derived-mode patchwork-series-mode special-mode "Patchwork-Series"
  "Major mode listing cached Patchwork series, grouped by server and
project under collapsible `[+]'/`[-]' headers.  Press RET or TAB on a
group header to toggle it, or on a series row to view its details.")

;;;###autoload
(defun patchwork-show-series (&optional skip-sync)
  "Display cached Patchwork series, from every configured server, in the
main listing buffer, grouped by server and project.  Syncs from the
Patchwork API first (subject to `patchwork-cache-ttl').  Shows only
`patchwork-default-state-filter' states the first time this buffer is
created; re-invoking this command on an already-open buffer just
refreshes its data and keeps whatever filter and group collapse state
are currently active (see `patchwork-series-set-filter').

With a prefix argument, skip the sync entirely and show whatever is
already cached, right now -- useful if a background process (e.g. a
crontab running patchwork-cron-sync.el) is already keeping the cache
warm, syncing is otherwise just taking too long to wait on right now,
or you simply want to control when this goes over the network
yourself.  Sync later, from within the buffer, with `g'/`C-u g'."
  (interactive "P")
  (let ((buffer (get-buffer-create patchwork-series-buffer-name)))
    (unless skip-sync
      (patchwork-cache-sync))
    (with-current-buffer buffer
      (unless (derived-mode-p 'patchwork-series-mode)
        (patchwork-series-mode)
        (setq patchwork-series--filter (list :states patchwork-default-state-filter)))
      (patchwork-series--render))
    (switch-to-buffer buffer)))

;;;###autoload
(defun patchwork-fetch-series (server-url series-id)
  "Fetch and cache a specific series by id from SERVER-URL, regardless
of `patchwork-sync-lookback-days' or the main listing buffer's usual
sync range, then open its detail buffer.  Useful for a series you
already know the id of -- e.g. found by browsing the Patchwork web UI
directly -- that's too old, or otherwise outside the range, for a
routine sync to ever reach on its own."
  (interactive
   (list (completing-read "Patchwork server: "
                           (mapcar (lambda (s) (plist-get s :url)) patchwork-servers)
                           nil t)
         (read-number "Series id: ")))
  (let ((server (or (patchwork-servers-find server-url)
                     (error "Unknown Patchwork server: %s" server-url))))
    (patchwork-cache-sync-series server series-id)
    (patchwork-view-series-details server-url series-id)))

(defvar-local patchwork-series-detail--server-url nil
  "Server URL that the current detail buffer is showing a series from.")

(defvar-local patchwork-series-detail--id nil
  "Series id that the current detail buffer is showing.")

(defvar-local patchwork-series-detail--expanded-comments nil
  "Hash table of comment id -> non-nil if that comment is expanded to
show its full text, in the current detail buffer.  Persists across
`g' refreshes of the same buffer.")

(defvar-local patchwork-series-detail--expanded-patches nil
  "Hash table of patch id -> non-nil if that patch is expanded to show
its commit message and diff, in the current detail buffer.  Persists
across `g' refreshes of the same buffer.")

(defun patchwork-series-detail--comment-at-point ()
  "Return the comment id at point in a detail buffer, or nil."
  (get-text-property (line-beginning-position) 'patchwork-comment-id))

(defun patchwork-series-detail--patch-at-point ()
  "Return the patch id at point in a detail buffer, or nil."
  (get-text-property (line-beginning-position) 'patchwork-patch-id))

(defun patchwork-series-detail--goto-next-with-property (prop)
  "Move point to the next line (strictly after the current one) where
text property PROP is non-nil.  Message and leave point alone if there
is no such line."
  (let ((start (point)))
    (forward-line 1)
    (while (and (not (eobp)) (not (get-text-property (line-beginning-position) prop)))
      (forward-line 1))
    (if (get-text-property (line-beginning-position) prop)
        t
      (goto-char start)
      (message "No next %s" (if (eq prop 'patchwork-patch-id) "patch" "comment"))
      nil)))

(defun patchwork-series-detail--goto-prev-with-property (prop)
  "Move point to the previous line (strictly before the current one)
where text property PROP is non-nil.  Message and leave point alone if
there is no such line."
  (let ((start (point)))
    (forward-line -1)
    (while (and (not (bobp)) (not (get-text-property (line-beginning-position) prop)))
      (forward-line -1))
    (if (get-text-property (line-beginning-position) prop)
        t
      (goto-char start)
      (message "No previous %s" (if (eq prop 'patchwork-patch-id) "patch" "comment"))
      nil)))

(defun patchwork-series-detail-next-patch ()
  "Move point to the next patch's summary line."
  (interactive)
  (patchwork-series-detail--goto-next-with-property 'patchwork-patch-id))

(defun patchwork-series-detail-prev-patch ()
  "Move point to the previous patch's summary line."
  (interactive)
  (patchwork-series-detail--goto-prev-with-property 'patchwork-patch-id))

(defun patchwork-series-detail-next-comment ()
  "Move point to the next comment's summary line."
  (interactive)
  (patchwork-series-detail--goto-next-with-property 'patchwork-comment-id))

(defun patchwork-series-detail-prev-comment ()
  "Move point to the previous comment's summary line."
  (interactive)
  (patchwork-series-detail--goto-prev-with-property 'patchwork-comment-id))

(defun patchwork-series-detail--toggle-hash (hash-var key)
  "Toggle KEY's presence in the hash table held by buffer-local
variable HASH-VAR (a symbol), lazily creating it if needed, then
redraw this detail buffer."
  (unless (hash-table-p (symbol-value hash-var))
    (set hash-var (make-hash-table :test #'eql)))
  (let ((table (symbol-value hash-var)))
    (if (gethash key table)
        (remhash key table)
      (puthash key t table)))
  (patchwork-view-series-details patchwork-series-detail--server-url
                                  patchwork-series-detail--id))

(defun patchwork-series-detail--propertize-diff-line (line)
  "Return LINE propertized with the diff-mode face matching its role,
if any (file/hunk headers, added/removed lines)."
  (cond
   ((string-prefix-p "diff --git" line) (propertize line 'face 'diff-header))
   ((or (string-prefix-p "+++" line) (string-prefix-p "---" line))
    (propertize line 'face 'diff-file-header))
   ((string-prefix-p "@@" line) (propertize line 'face 'diff-hunk-header))
   ((string-prefix-p "+" line) (propertize line 'face 'diff-added))
   ((string-prefix-p "-" line) (propertize line 'face 'diff-removed))
   (t line)))

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
      (unless (hash-table-p patchwork-series-detail--expanded-patches)
        (setq patchwork-series-detail--expanded-patches (make-hash-table :test #'eql)))
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
          (let* ((patch-id (plist-get patch :id))
                 (expanded (gethash patch-id patchwork-series-detail--expanded-patches)))
            (insert (propertize
                     (format "%s %3d. #%-8d [%-16s] %s\n"
                             (if expanded "[-]" "[+]")
                             (or (plist-get patch :series-position) 0)
                             patch-id
                             (or (plist-get patch :state) "")
                             (or (plist-get patch :name) ""))
                     'patchwork-patch-id patch-id
                     'face 'patchwork-patch-line-face))
            (when expanded
              (let ((content (plist-get patch :content))
                    (diff (plist-get patch :diff)))
                (if (not (or content diff))
                    (insert "     (full patch content not cached yet; C-u g fetches it from the server)\n")
                  (when content
                    (insert "     --- Commit message ---\n")
                    (dolist (content-line (split-string content "\n"))
                      (insert (format "     %s\n" content-line))))
                  (when diff
                    (insert "     --- Diff ---\n")
                    (dolist (diff-line (split-string diff "\n"))
                      (insert "     " (patchwork-series-detail--propertize-diff-line diff-line) "\n"))))
                (let ((checks (patchwork-db-get-checks server-url patch-id)))
                  (when checks
                    (insert "     --- Checks ---\n")
                    (dolist (check checks)
                      (insert (propertize
                               (format "     [%s] %s: %s\n"
                                       (or (plist-get check :state) "")
                                       (or (plist-get check :context) "")
                                       (or (plist-get check :description) ""))
                               'face (patchwork-series-detail--check-face
                                      (plist-get check :state))))
                      (when (plist-get check :target-url)
                        (insert "         ")
                        (insert-text-button
                         (plist-get check :target-url)
                         'action (lambda (button) (browse-url (button-label button)))
                         'follow-link t
                         'help-echo "mouse-2, RET: browse this check's report")
                        (insert "\n")))))
                (insert "\n"))))
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
                  (insert "         "
                          (patchwork-series-detail--propertize-comment-line content-line)
                          "\n"))
                (insert "\n")))))
        (goto-char (point-min))
        (forward-line (1- line)))
      (unless (derived-mode-p 'patchwork-series-detail-mode)
        (patchwork-series-detail-mode))
      (setq patchwork-series-detail--server-url server-url)
      (setq patchwork-series-detail--id series-id))
    (switch-to-buffer buffer)))

(defun patchwork-series-detail-toggle-at-point ()
  "Toggle whatever is at point between collapsed and expanded: a
comment (its full text) or a patch (its commit message and diff)."
  (interactive)
  (let ((comment-id (patchwork-series-detail--comment-at-point))
        (patch-id (patchwork-series-detail--patch-at-point)))
    (cond
     (comment-id
      (patchwork-series-detail--toggle-hash 'patchwork-series-detail--expanded-comments
                                             comment-id))
     (patch-id
      (patchwork-series-detail--toggle-hash 'patchwork-series-detail--expanded-patches
                                             patch-id))
     (t (message "Nothing to expand/collapse on this line")))))

(defun patchwork-series-detail-reply-at-point ()
  "Reply, as a wide-reply mail message, to the comment or patch at point.
On a comment line, replies to that comment using cached data. On a
patch line, fetches that patch's full detail live (its mail headers
aren't part of the cached list-view data) and replies to it."
  (interactive)
  (let ((comment-id (patchwork-series-detail--comment-at-point))
        (patch-id (patchwork-series-detail--patch-at-point)))
    (cond
     (comment-id
      (let ((comment (patchwork-db-get-comment patchwork-series-detail--server-url
                                                comment-id)))
        (if comment
            (patchwork-mail-reply-to-comment comment)
          (message "No cached comment %s" comment-id))))
     (patch-id
      (let ((server (or (patchwork-servers-find patchwork-series-detail--server-url)
                         (error "Unknown Patchwork server: %s"
                                patchwork-series-detail--server-url))))
        (patchwork-mail-reply-to-patch server patch-id)))
     (t (message "Nothing to reply to on this line")))))

(defun patchwork-series-detail-expand-all ()
  "Expand every patch and comment in this series detail buffer."
  (interactive)
  (setq patchwork-series-detail--expanded-patches (make-hash-table :test #'eql))
  (setq patchwork-series-detail--expanded-comments (make-hash-table :test #'eql))
  (dolist (patch (patchwork-db-get-series-patches patchwork-series-detail--server-url
                                                   patchwork-series-detail--id))
    (puthash (plist-get patch :id) t patchwork-series-detail--expanded-patches)
    (dolist (comment (patchwork-db-get-comments patchwork-series-detail--server-url
                                                 (plist-get patch :id)))
      (puthash (plist-get comment :id) t patchwork-series-detail--expanded-comments)))
  (patchwork-view-series-details patchwork-series-detail--server-url
                                  patchwork-series-detail--id))

(defun patchwork-series-detail-collapse-all ()
  "Collapse every patch and comment in this series detail buffer."
  (interactive)
  (setq patchwork-series-detail--expanded-patches (make-hash-table :test #'eql))
  (setq patchwork-series-detail--expanded-comments (make-hash-table :test #'eql))
  (patchwork-view-series-details patchwork-series-detail--server-url
                                  patchwork-series-detail--id))

(defvar patchwork-series-detail-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'patchwork-series-detail-toggle-at-point)
    (define-key map (kbd "TAB") #'patchwork-series-detail-toggle-at-point)
    (define-key map "r" #'patchwork-series-detail-reply-at-point)
    (define-key map "g" #'patchwork-series-detail-refresh)
    (define-key map "a" #'patchwork-series-detail-apply)
    (define-key map "R" #'patchwork-series-detail-review)
    (define-key map "n" #'patchwork-series-detail-next-patch)
    (define-key map "p" #'patchwork-series-detail-prev-patch)
    (define-key map "N" #'patchwork-series-detail-next-comment)
    (define-key map "P" #'patchwork-series-detail-prev-comment)
    (define-key map "+" #'patchwork-series-detail-expand-all)
    (define-key map "-" #'patchwork-series-detail-collapse-all)
    (define-key map "q" #'quit-window)
    map)
  "Keymap for `patchwork-series-detail-mode'.
n/p move to the next/previous patch; N/P move to the next/previous
comment; +/- expand/collapse everything in this buffer.")

(define-derived-mode patchwork-series-detail-mode special-mode "Patchwork-Series-Detail"
  "Major mode for viewing details of a single Patchwork series.")

(defun patchwork-series-detail-refresh (&optional force)
  "Refresh the current series detail buffer.
With a prefix argument, first re-fetch this series from the server
\(via `patchwork-cache-sync-series', regardless of
`patchwork-sync-lookback-days') rather than just re-rendering whatever
is already cached -- use this to pull in real updates to the series
you're currently looking at, without leaving this buffer."
  (interactive "P")
  (when patchwork-series-detail--id
    (when force
      (let ((server (or (patchwork-servers-find patchwork-series-detail--server-url)
                         (error "Unknown Patchwork server: %s"
                                patchwork-series-detail--server-url))))
        (patchwork-cache-sync-series server patchwork-series-detail--id)))
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

(defun patchwork-series-detail-review ()
  "Apply the series shown in this buffer as commits and open a review view.
See `patchwork-review-series'."
  (interactive)
  (when patchwork-series-detail--id
    (let ((server (or (patchwork-servers-find patchwork-series-detail--server-url)
                       (error "Unknown Patchwork server: %s"
                              patchwork-series-detail--server-url))))
      (patchwork-review-series server patchwork-series-detail--id))))

(provide 'patchwel-ui)

;;; patchwel-ui.el ends here
