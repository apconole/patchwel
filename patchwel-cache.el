;;; patchwel-cache.el --- Sync Patchwork API data into the local cache -*- lexical-binding: t; -*-
;;; Code:

(require 'time-date)
(require 'patchwel-config)
(require 'patchwel-db)
(require 'patchwel-api)

(defconst patchwork-cache--tag-patterns
  '(("ack" . "^Acked-by:")
    ("review" . "^Reviewed-by:")
    ("test" . "^Tested-by:")
    ("fixes" . "^Fixes:"))
  "Mapping of tag name (see `patchwork-tag-names') to the trailer regexp
used to count its occurrences in comment text.")

(defun patchwork-cache-is-stale (key)
  "Return non-nil if the cache entry for KEY is missing or older than
`patchwork-cache-ttl' seconds."
  (let* ((cached-at (patchwork-db-get-sync-meta key))
         (cached-time (when cached-at (ignore-errors (date-to-time cached-at)))))
    (if cached-time
        (> (- (float-time) (float-time cached-time)) patchwork-cache-ttl)
      t)))

(defun patchwork-cache--iso (time)
  "Format the Lisp TIME value as an ISO-8601 UTC timestamp.
Uses an explicit +00:00 offset rather than a Z suffix, since not every
Patchwork deployment's date parser accepts the latter (some `since='
filters have 500'd on it)."
  (format-time-string "%Y-%m-%d" time t))

(defun patchwork-cache--after-cutoff-p (date-str cutoff-time)
  "Return non-nil if DATE-STR is at or after CUTOFF-TIME.
An unparseable DATE-STR is kept rather than silently dropped."
  (let ((parsed (ignore-errors (date-to-time date-str))))
    (or (null parsed) (not (time-less-p parsed cutoff-time)))))

(defun patchwork-cache--get (obj &rest keys)
  "Walk plist OBJ through KEYS, returning nil at the first missing key."
  (let ((cur obj))
    (dolist (key keys cur)
      (setq cur (and (listp cur) (plist-get cur key))))))

(defun patchwork-cache--person-name (person)
  "Return a human-readable name for a submitter/delegate PERSON value.
PERSON may be a plist with :name/:email, a bare string, or nil."
  (cond ((null person) nil)
        ((stringp person) person)
        ((plist-get person :name) (plist-get person :name))
        ((plist-get person :email) (plist-get person :email))
        ((plist-get person :username) (plist-get person :username))
        (t nil)))

(defun patchwork-cache--upsert-project-from (server obj)
  "Upsert the project embedded in OBJ (a series or patch payload from SERVER).
Return a (id . slug) cons, or nil if OBJ has no project information."
  (let ((project (patchwork-cache--get obj :project)))
    (when project
      (let ((id (plist-get project :id))
            (name (plist-get project :name))
            (slug (or (plist-get project :link_name) (plist-get project :name))))
        (when id
          (patchwork-db-insert-project (plist-get server :url) id name slug)
          (cons id slug))))))

(defun patchwork-cache--count-tags (comments)
  "Count trailer tags across COMMENTS, a list of comment plists.
Returns a plist keyed by the entries in `patchwork-tag-names'."
  (let ((counts (mapcar (lambda (tag) (cons tag 0)) patchwork-tag-names)))
    (dolist (comment comments)
      (let ((content (or (plist-get comment :content) "")))
        (dolist (tag patchwork-tag-names)
          (let ((pattern (cdr (assoc tag patchwork-cache--tag-patterns))))
            (when pattern
              (let ((start 0) (n 0))
                (while (string-match pattern content start)
                  (setq n (1+ n))
                  (setq start (match-end 0)))
                (when (> n 0)
                  (setcdr (assoc tag counts) (+ n (cdr (assoc tag counts)))))))))))
    (list :ack-count (or (cdr (assoc "ack" counts)) 0)
          :review-count (or (cdr (assoc "review" counts)) 0)
          :test-count (or (cdr (assoc "test" counts)) 0)
          :fixes-count (or (cdr (assoc "fixes" counts)) 0))))

(defun patchwork-cache--count-checks (checks)
  "Tally CHECKS (a list of check plists) into success/warning/fail counts."
  (let ((success 0) (warning 0) (fail 0))
    (dolist (check checks)
      (pcase (plist-get check :state)
        ("success" (setq success (1+ success)))
        ("warning" (setq warning (1+ warning)))
        ("fail" (setq fail (1+ fail)))))
    (list :check-success success :check-warning warning :check-fail fail)))

(defun patchwork-cache--series-state (patches)
  "Derive an aggregate state for a series from its PATCHES plists."
  (let ((states (delete-dups (mapcar (lambda (p) (plist-get p :state)) patches))))
    (cond ((null states) "new")
          ((= (length states) 1) (car states))
          ((member "rejected" states) "rejected")
          ((member "changes-requested" states) "changes-requested")
          ((member "under-review" states) "under-review")
          (t (car states)))))

(defun patchwork-cache--series-assignee (patches)
  "Derive an aggregate assignee for a series from its PATCHES plists."
  (or (seq-some (lambda (p) (plist-get p :delegate)) patches)
      "unassigned"))

(defun patchwork-cache--sync-patch (server patch-json series-id project-id position)
  "Sync a single PATCH-JSON from SERVER belonging to SERIES-ID at series POSITION.
Returns a plist describing the cached patch, including freshly synced
comment and check totals."
  (let* ((server-url (plist-get server :url))
         (id (plist-get patch-json :id))
         (comments (ignore-errors (patchwork-api-list-comments server id)))
         (checks (ignore-errors (patchwork-api-list-checks server id)))
         (check-counts (patchwork-cache--count-checks checks))
         (patch (list :server-url server-url
                       :id id
                       :series-id series-id
                       :project-id project-id
                       :state (plist-get patch-json :state)
                       :submitter (patchwork-cache--person-name
                                   (plist-get patch-json :submitter))
                       :delegate (patchwork-cache--person-name
                                  (plist-get patch-json :delegate))
                       :name (plist-get patch-json :name)
                       :date (plist-get patch-json :date)
                       :series-position position
                       :check-state (plist-get patch-json :check)
                       :updated-at (plist-get patch-json :date))))
    (patchwork-db-insert-patch patch)
    (dolist (comment-json comments)
      (patchwork-db-insert-comment
       (list :server-url server-url
             :id (plist-get comment-json :id)
             :patch-id id
             :author (patchwork-cache--person-name
                      (plist-get comment-json :submitter))
             :date (plist-get comment-json :date)
             :content (or (plist-get comment-json :content) ""))))
    (append patch (list :comments comments) check-counts)))

(defun patchwork-cache--sync-one-series (server series-json)
  "Sync SERIES-JSON from SERVER and all of its patches, comments, and checks."
  (let* ((server-url (plist-get server :url))
         (series-id (plist-get series-json :id))
         (project (patchwork-cache--upsert-project-from server series-json))
         (project-id (car project))
         (project-slug (cdr project))
         (patches-json (ignore-errors
                         (patchwork-api-list-patches server nil `(("series" . ,series-id)))))
         (position 0)
         (synced (mapcar (lambda (patch-json)
                            (setq position (1+ position))
                            (patchwork-cache--sync-patch
                             server patch-json series-id project-id position))
                          patches-json))
         (tag-counts (patchwork-cache--count-tags
                      (apply #'append (mapcar (lambda (p) (plist-get p :comments)) synced))))
         (check-totals (list :check-success (apply #'+ (mapcar (lambda (p) (plist-get p :check-success)) synced))
                              :check-warning (apply #'+ (mapcar (lambda (p) (plist-get p :check-warning)) synced))
                              :check-fail (apply #'+ (mapcar (lambda (p) (plist-get p :check-fail)) synced))))
         (comment-count (apply #'+ (mapcar (lambda (p) (length (plist-get p :comments))) synced))))
    (patchwork-db-upsert-series
     (append
      (list :server-url server-url
            :id series-id
            :project-id project-id
            :project-slug project-slug
            :name (plist-get series-json :name)
            :submitter (patchwork-cache--person-name (plist-get series-json :submitter))
            :version (plist-get series-json :version)
            :total (or (plist-get series-json :total) (length patches-json))
            :submitted-at (plist-get series-json :date)
            :state (patchwork-cache--series-state synced)
            :assignee (patchwork-cache--series-assignee synced)
            :comment-count comment-count)
      tag-counts
      check-totals
      (list :url (plist-get series-json :web_url)
            :updated-at (plist-get series-json :date))))))

(defun patchwork-cache--sync-window (server project cutoff-time)
  "Sync PROJECT on SERVER, limited to series at or after CUTOFF-TIME.
Used for a server/project's first-ever sync, and as the fallback when
SERVER has no events API to drive an incremental sync."
  (let ((cutoff-str (patchwork-cache--iso cutoff-time)))
    (dolist (series-json (patchwork-api-list-series server project `(("since" . ,cutoff-str))))
      (when (patchwork-cache--after-cutoff-p (plist-get series-json :date) cutoff-time)
        (patchwork-cache--sync-one-series server series-json)))))

(defun patchwork-cache--event-series-ids (server events)
  "Return the deduplicated series ids affected by EVENTS from SERVER.
Events that reference a series directly contribute that id for free;
events that only reference a patch require one extra lookup to learn
which series that patch belongs to."
  (let (ids)
    (dolist (event events)
      (let ((series (plist-get event :series))
            (patch (plist-get event :patch)))
        (cond
         (series (push (plist-get series :id) ids))
         (patch
          (let ((patch-json (ignore-errors
                              (patchwork-api-get-patch server (plist-get patch :id)))))
            (dolist (s (plist-get patch-json :series))
              (push (plist-get s :id) ids)))))))
    (delete-dups ids)))

(defun patchwork-cache--sync-incremental (server project since cutoff-time)
  "Sync PROJECT on SERVER using events since SINCE (an ISO-8601 string).
Only series touched by those events are re-fetched.  Falls back to
`patchwork-cache--sync-window' (bounded by CUTOFF-TIME) if SERVER has
no events API."
  (let ((events (condition-case nil
                    (patchwork-api-list-events server project since)
                  (error :unsupported))))
    (if (eq events :unsupported)
        (patchwork-cache--sync-window server project cutoff-time)
      (dolist (series-id (patchwork-cache--event-series-ids server events))
        (let ((series-json (ignore-errors (patchwork-api-get-series server series-id))))
          (when series-json
            (patchwork-cache--sync-one-series server series-json)))))))

(defun patchwork-cache--sync-server-project (server project force)
  "Sync PROJECT (or every project, if nil) on SERVER into the local cache.
Skips the network round-trip when a sync happened within
`patchwork-cache-ttl' seconds, unless FORCE is non-nil.  The very first
sync for this SERVER/PROJECT pulls only `patchwork-sync-lookback-days'
of history; later syncs fetch just what changed since the previous
sync via the Patchwork events API."
  (let* ((key (format "series-%s-%s" (plist-get server :url) (or project "all")))
         (last-sync (patchwork-db-get-sync-meta key)))
    (when (or force (patchwork-cache-is-stale key))
      (message "Fetching Patchwork data from %s%s..."
               (plist-get server :url)
               (if project (format " (%s)" project) ""))
      (let ((cutoff-time (time-subtract (current-time)
                                         (* patchwork-sync-lookback-days 86400))))
        (if last-sync
            (patchwork-cache--sync-incremental server project last-sync cutoff-time)
          (patchwork-cache--sync-window server project cutoff-time)))
      (patchwork-db-set-sync-meta key (patchwork-cache--iso (current-time))))))

(defun patchwork-cache-sync (&optional force)
  "Sync every configured `patchwork-servers' entry (and its projects).
With FORCE non-nil, sync even if the cache is still fresh.  A server or
project that errors out (network failure, HTTP error, etc.) is skipped
with a warning rather than aborting the rest of the sync, so one bad
server never prevents the others' cached data from showing.  Returns
all cached series afterwards, including any left over from previous
successful syncs of a server that just failed."
  (dolist (server patchwork-servers)
    (dolist (project (patchwork-server-projects server))
      (condition-case err
          (patchwork-cache--sync-server-project server project force)
        (error
         (message "Patchwork: failed to sync %s%s: %s"
                  (plist-get server :url)
                  (if project (format " (%s)" project) "")
                  (error-message-string err))))))
  (patchwork-db-query-series))

(provide 'patchwel-cache)

;;; patchwel-cache.el ends here
