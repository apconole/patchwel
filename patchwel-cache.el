;;; patchwel-cache.el --- Sync Patchwork API data into the local cache -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'time-date)
(require 'patchwel-config)
(require 'patchwel-db)
(require 'patchwel-api)
(require 'patchwel-git)

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

(defun patchwork-cache--iso-datetime (time)
  "Format the Lisp TIME value as a full ISO-8601 UTC timestamp with a
Z suffix (YYYY-MM-DDTHH:mm:ssZ).  Used both for local bookkeeping
\(`sync_meta' storage, `patchwork-cache-is-stale') and as the `since='
value sent to the server.  Confirmed against a real Patchwork server
(patchwork.ozlabs.org) to filter `/series/' and `/events/' correctly
in this exact form.  Earlier attempts used a \"+00:00\" offset instead
of \"Z\"; that form arrived at the server mangled (almost certainly
its \"+\" being decoded as a literal space, per the
application/x-www-form-urlencoded convention) even though it looked
correctly percent-encoded on this end -- \"Z\" has no such character
to corrupt.  A bare date (no time-of-day) was also tried as a
workaround and seemed safe, but turned out to be silently accepted
and then ignored by the server (200 OK, always an empty result) --
not an error, just quietly useless, which is worse."
  (format-time-string "%Y-%m-%dT%H:%M:%SZ" time t))

(defun patchwork-cache--fetch-with-since (server cutoff-time fetch-fn)
  "Call FETCH-FN, a function of one string argument (a `since=' value)
derived from CUTOFF-TIME.  If SERVER has an explicit :since-format
configured (see `patchwork-servers'), use exactly that format and let
any error propagate normally -- the point of setting it is to skip
paying for a known-wrong attempt.  Otherwise try each format in
`patchwork-since-format-strings' order, moving to the next whenever a
format is rejected with an HTTP error; if every format fails,
re-signals the last error rather than silently returning nothing."
  (let ((explicit (patchwork-server-since-format server)))
    (if explicit
        (funcall fetch-fn (patchwork-since-format-string cutoff-time explicit))
      (let ((formats (mapcar #'car patchwork-since-format-strings))
            result done)
        (while (and formats (not done))
          (condition-case err
              (progn
                (setq result (funcall fetch-fn
                                       (patchwork-since-format-string cutoff-time (car formats))))
                (setq done t))
            (patchwork-api-http-error
             (setq formats (cdr formats))
             (unless formats (signal (car err) (cdr err))))))
        result))))

(defun patchwork-parse-server-time (date-str)
  "Parse DATE-STR, a raw Patchwork API date field, as a UTC Lisp time value.
Patchwork's JSON date fields (e.g. \"2026-07-18T14:41:06.870773\")
omit any timezone marker even though the value is actually UTC.
Parsing that as-is via `date-to-time' would interpret it using the
local system timezone instead, silently skewing every comparison
against it by the local UTC offset -- e.g. an event from 8am UTC could
look like it happened at noon or 4am depending where this Emacs
happens to be running.  If DATE-STR already ends in an explicit
offset or \"Z\", it is parsed as-is."
  (if (string-match-p "\\(Z\\|[+-][0-9][0-9]:?[0-9][0-9]\\)\\'" date-str)
      (date-to-time date-str)
    (date-to-time (concat date-str "Z"))))

(defun patchwork-cache--after-cutoff-p (date-str cutoff-time)
  "Return non-nil if DATE-STR is at or after CUTOFF-TIME.
An unparseable DATE-STR is kept rather than silently dropped."
  (let ((parsed (ignore-errors (patchwork-parse-server-time date-str))))
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

(defun patchwork-cache--header-value (headers key)
  "Return HEADERS's KEY as a single string, or nil if absent.
Patchwork occasionally reports a raw mail header as a list of repeated
values (e.g. multiple Received: lines) rather than a single string;
join those with \", \" so callers always get a plain string."
  (let ((value (plist-get headers key)))
    (cond ((null value) nil)
          ((stringp value) value)
          ((listp value) (string-join value ", "))
          (t (format "%s" value)))))

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

(defun patchwork-cache--maybe-prune-patch (server old-state new-patch)
  "Delete NEW-PATCH's cached mbox file if it just transitioned into a
terminal state (see `patchwork-prune-on-terminal-states'): OLD-STATE,
its state before this sync (nil if it wasn't cached before), is not
terminal, but its new state is."
  (let ((new-state (plist-get new-patch :state)))
    (when (and patchwork-prune-on-terminal-states
               (member new-state patchwork-prune-on-terminal-states)
               (not (member old-state patchwork-prune-on-terminal-states)))
      (let ((file (patchwork-git--patch-file server (plist-get new-patch :id))))
        (when (file-exists-p file)
          (delete-file file)
          (message "Patch %s moved to state %s; pruned cached mbox %s."
                   (plist-get new-patch :id) new-state file))))))

(defun patchwork-cache--sync-patch (server patch-json series-id project-id position)
  "Sync a single PATCH-JSON from SERVER belonging to SERIES-ID at series POSITION.
Returns a plist describing the cached patch, including freshly synced
comment and check totals."
  (let* ((server-url (plist-get server :url))
         (id (plist-get patch-json :id))
         (old-state (plist-get (patchwork-db-get-patch server-url id) :state))
         (comments (ignore-errors (patchwork-api-list-comments server id)))
         (checks (ignore-errors (patchwork-api-list-checks server id)))
         (check-counts (patchwork-cache--count-checks checks))
         ;; The list view (patch-json, above) doesn't carry mail headers,
         ;; the commit-message body, or the diff -- only the single-patch
         ;; detail endpoint does.  Fetch it now so a later mail reply to
         ;; this patch, or expanding it in the series detail buffer,
         ;; doesn't need its own live round-trip; best-effort, since
         ;; losing this one extra call shouldn't fail the whole sync (the
         ;; mail layer falls back to a live fetch if these fields are nil).
         (detail (ignore-errors (patchwork-api-get-patch server id)))
         (detail-headers (plist-get detail :headers))
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
                       :content (plist-get detail :content)
                       :diff (plist-get detail :diff)
                       :submitter-email (plist-get (plist-get detail :submitter) :email)
                       :msgid (plist-get detail :msgid)
                       :to (patchwork-cache--header-value detail-headers :To)
                       :cc (patchwork-cache--header-value detail-headers :Cc)
                       :references (patchwork-cache--header-value detail-headers :References)
                       :in-reply-to (patchwork-cache--header-value detail-headers :In-Reply-To)
                       :updated-at (plist-get patch-json :date))))
    (patchwork-db-insert-patch patch)
    (patchwork-cache--maybe-prune-patch server old-state patch)
    (dolist (comment-json comments)
      (let ((headers (plist-get comment-json :headers)))
        (patchwork-db-insert-comment
         (list :server-url server-url
               :id (plist-get comment-json :id)
               :patch-id id
               :author (patchwork-cache--person-name
                        (plist-get comment-json :submitter))
               :date (plist-get comment-json :date)
               :content (or (plist-get comment-json :content) "")
               :msgid (plist-get comment-json :msgid)
               :subject (plist-get comment-json :subject)
               :submitter-email (plist-get (plist-get comment-json :submitter) :email)
               :to (patchwork-cache--header-value headers :To)
               :cc (patchwork-cache--header-value headers :Cc)
               :references (patchwork-cache--header-value headers :References)
               :in-reply-to (patchwork-cache--header-value headers :In-Reply-To)))))
    (dolist (check-json checks)
      (patchwork-db-insert-check
       (list :server-url server-url
             :id (plist-get check-json :id)
             :patch-id id
             :reporter (patchwork-cache--person-name (plist-get check-json :user))
             :state (plist-get check-json :state)
             :context (plist-get check-json :context)
             :description (plist-get check-json :description)
             :target-url (plist-get check-json :target_url)
             :date (plist-get check-json :date))))
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
SERVER has no events API to drive an incremental sync.  The client-side
`patchwork-cache--after-cutoff-p' check is kept as a defensive
double-check even though the server-side `since=' filter has been
confirmed to work correctly on its own -- on most deployments; see
`patchwork-cache--fetch-with-since' for why a single format can't be
assumed to work everywhere."
  (dolist (series-json
           (patchwork-cache--fetch-with-since
            server cutoff-time
            (lambda (since-str)
              (patchwork-api-list-series server project `(("since" . ,since-str))))))
    (when (patchwork-cache--after-cutoff-p (plist-get series-json :date) cutoff-time)
      (patchwork-cache--sync-one-series server series-json))))

(defun patchwork-cache--event-series-ids (server events)
  "Return the deduplicated series ids affected by EVENTS from SERVER.
An event's series/patch/cover reference lives under its :payload, not
the event's top level (e.g. a patch-state-changed event looks like
\(:category \"patch-state-changed\" :payload (:patch (:id ...)
:previous_state ... :current_state ...))).  Confirmed against real
Patchwork event payloads (patchwork.ozlabs.org, patches.dpdk.org):

  series-created, series-completed  -> payload has :series directly
  patch-completed                   -> payload has both :series and
                                        :patch; :series is used
  patch-created, patch-delegated,
  patch-relation-changed,
  check-created, patch-comment-created
                                     -> payload has only :patch; its
                                        series membership is looked up
                                        via `patchwork-api-get-patch'
  cover-comment-created, cover-created
                                     -> payload has only :cover; its
                                        series membership is looked up
                                        via `patchwork-api-get-cover'.
                                        cover-created's lookup is
                                        usually redundant, since it
                                        fires alongside a
                                        series-created event for the
                                        same series, but there's no
                                        cheap way to distinguish the
                                        two categories from shape
                                        alone, and the extra lookup is
                                        harmless."
  (let (ids)
    (dolist (event events)
      (let* ((payload (plist-get event :payload))
             (series (plist-get payload :series))
             (patch (plist-get payload :patch))
             (cover (plist-get payload :cover)))
        (cond
         (series (push (plist-get series :id) ids))
         (patch
          (let ((patch-json (ignore-errors
                              (patchwork-api-get-patch server (plist-get patch :id)))))
            (dolist (s (plist-get patch-json :series))
              (push (plist-get s :id) ids))))
         (cover
          (let ((cover-json (ignore-errors
                              (patchwork-api-get-cover server (plist-get cover :id)))))
            (dolist (s (plist-get cover-json :series))
              (push (plist-get s :id) ids)))))))
    (delete-dups ids)))

(defun patchwork-cache--event-matches-project-p (event project)
  "Return non-nil if EVENT belongs to PROJECT (a project slug/link_name),
using EVENT's own top-level :project field -- present in real
Patchwork event payloads independently of any `project=' the request
was scoped by.  PROJECT nil (\"every project\") always matches."
  (or (null project)
      (equal (plist-get (plist-get event :project) :link_name) project)))

(defun patchwork-cache--sync-incremental (server project since cutoff-time)
  "Sync PROJECT on SERVER using events since SINCE (a full-precision
ISO-8601 string, as stored in `sync_meta').  The actual `since=' value
sent may be a coarser format than SINCE if the server rejects finer
ones with an HTTP error -- see `patchwork-cache--fetch-with-since'.  A
client-side `patchwork-cache--after-cutoff-p' re-check against SINCE's
real precision is kept regardless, so a coarser request doesn't cause
already-processed events to be reprocessed.  Only series touched by
the surviving events are re-fetched.

If SERVER has :events-omit-project set (see `patchwork-servers'), the
`/events/' request never sends `project=' at all -- every project's
events on SERVER come back together, and PROJECT is instead applied as
a client-side filter via `patchwork-cache--event-matches-project-p'
(using each event's own :project field, no extra requests needed).
Some deployments 502 on this endpoint specifically when `project=' is
present; this sidesteps that.  Leave the flag unset (the default) on a
server hosting many unrelated projects, since PROJECT is normally sent
server-side precisely to avoid fetching everyone else's event history
too.

Falls back to a full `patchwork-cache--sync-window' (bounded by
CUTOFF-TIME) only when SERVER genuinely has no events API, signaled by
the endpoint itself returning HTTP 404.  Any other failure (timeout,
5xx, connection refused, ...) is a transient condition rather than
proof the endpoint is missing, so it is re-signaled and left to the
caller's per-server error handling instead of triggering a full resync
that would just pile more requests onto an already-struggling network
or server."
  (condition-case err
      (let* ((since-time (date-to-time since))
             (omit-project (patchwork-server-events-omit-project-p server))
             (query-project (unless omit-project project))
             (events (seq-filter
                      (lambda (event)
                        (and (patchwork-cache--after-cutoff-p (plist-get event :date) since-time)
                             (or (not omit-project)
                                 (patchwork-cache--event-matches-project-p event project))))
                      (patchwork-cache--fetch-with-since
                       server since-time
                       (lambda (since-str)
                         (patchwork-api-list-events server query-project since-str))))))
        (dolist (series-id (patchwork-cache--event-series-ids server events))
          (let ((series-json (ignore-errors (patchwork-api-get-series server series-id))))
            (when series-json
              (patchwork-cache--sync-one-series server series-json)))))
    (patchwork-api-http-error
     (if (eql (nth 1 err) 404)
         (patchwork-cache--sync-window server project cutoff-time)
       (signal (car err) (cdr err))))))

(defun patchwork-cache--sync-server-project (server project force)
  "Sync PROJECT (or every project, if nil) on SERVER into the local cache.
Skips the network round-trip when a sync happened within
`patchwork-cache-ttl' seconds, unless FORCE is non-nil.  The very first
sync for this SERVER/PROJECT, and any FORCEd sync, pulls a full
`patchwork-cache--sync-window' over `patchwork-sync-lookback-days' of
history; other syncs fetch just what changed since the previous sync
via the Patchwork events API.  FORCE therefore means a genuine full
resync (e.g. to recover from a gap in event coverage), not merely
\"ignore the TTL and do the usual incremental sync\"."
  (let* ((key (format "series-%s-%s" (plist-get server :url) (or project "all")))
         (last-sync (patchwork-db-get-sync-meta key)))
    (when (or force (patchwork-cache-is-stale key))
      (message "Fetching Patchwork data from %s%s..."
               (plist-get server :url)
               (if project (format " (%s)" project) ""))
      (let ((cutoff-time (time-subtract (current-time)
                                         (* patchwork-sync-lookback-days 86400))))
        (if (and last-sync (not force))
            (patchwork-cache--sync-incremental server project last-sync cutoff-time)
          (patchwork-cache--sync-window server project cutoff-time)))
      (patchwork-db-set-sync-meta key (patchwork-cache--iso-datetime (current-time))))))

(defun patchwork-cache-sync (&optional force)
  "Sync every configured `patchwork-servers' entry (and its projects).
With FORCE non-nil, sync even if the cache is still fresh, and do a
full resync of `patchwork-sync-lookback-days' rather than the usual
incremental events-based sync -- useful for recovering from any gap in
event coverage rather than waiting for the next periodic full sync.
A server or project that errors out (network failure, HTTP error,
etc.) is skipped with a warning rather than aborting the rest of the
sync, so one bad server never prevents the others' cached data from
showing.  Once every one of a server's configured projects has been
attempted, retries any of that server's queued offline state/delegate
changes (`patchwork-cache--drain-pending-changes') -- deliberately once
per *server*, not per project, since a queued change isn't scoped to
one project and draining after only the first of several projects has
synced would compare it against other, not-yet-refreshed projects'
stale local data.  Returns all cached series afterwards, including any
left over from previous successful syncs of a server that just failed."
  (dolist (server patchwork-servers)
    (dolist (project (patchwork-server-projects server))
      (condition-case err
          (patchwork-cache--sync-server-project server project force)
        (error
         (message "Patchwork: failed to sync %s%s: %s"
                  (plist-get server :url)
                  (if project (format " (%s)" project) "")
                  (error-message-string err)))))
    (condition-case err
        (patchwork-cache--drain-pending-changes server)
      (error
       (message "Patchwork: failed to drain queued changes for %s: %s"
                (plist-get server :url) (error-message-string err)))))
  (patchwork-db-query-series))

;; -- offline-tolerant patch/series state and delegate updates --------------

(defvar patchwork-cache--draining-p nil
  "Non-nil, bound to the server plist currently being drained, while
`patchwork-cache--drain-pending-changes' is running for it.  Interactive
set commands check this (via `patchwork-cache--set-patch-field') and
decline to proceed rather than race a background drain touching the
same patch, since both would otherwise be free to run at the same time
\(a blocking synchronous HTTP call can still let a sync timer fire
reentrantly).")

(defun patchwork-cache--queueable-error-p (err)
  "Return non-nil if ERR (a condition-case error value, (SYMBOL . DATA))
should be queued for later retry rather than signaled to the caller.
Queueable: `patchwork-api-timeout-error'; a `patchwork-api-http-error'
with a nil (unparseable) or >=500 status (a genuinely transient-looking
server problem, not a rejection); any other error at all (connection
refused, DNS failure, a non-JSON/challenge-page response, which signals
a plain `error').  NOT queueable: a `patchwork-api-http-error' with a
4xx status -- retrying a permission or not-found rejection blindly is
pointless."
  (pcase (car err)
    ('patchwork-api-http-error
     (let ((status (nth 1 err)))
       (or (null status) (>= status 500))))
    (_ t)))

(defun patchwork-cache--set-patch-field (server patch-id field new-value api-fn column)
  "Shared implementation setting FIELD (the string \"state\" or
\"delegate\") to NEW-VALUE for PATCH-ID on SERVER by calling API-FN as
\(API-FN SERVER PATCH-ID NEW-VALUE\), updating COLUMN (the matching
keyword, :state or :delegate) in the local `patches' row on success.
On a `patchwork-cache--queueable-error-p' failure, queues the change
instead of signaling it (see `patchwork-db-queue-pending-change'),
using the patch's current cached COLUMN value as what was \"observed\"
at request time, for later conflict detection.  Any other failure
\(e.g. a 4xx rejection) propagates normally.  Returns `applied',
`queued', or `busy' (a drain for this server is already running)."
  (let* ((server-url (plist-get server :url)))
    (if (and patchwork-cache--draining-p
              (equal (plist-get patchwork-cache--draining-p :url) server-url))
        (progn
          (message "patchwork: a background sync is draining queued changes for %s right now; try again in a moment"
                    server-url)
          'busy)
      (let* ((patch (patchwork-db-get-patch server-url patch-id))
             (observed (plist-get patch column)))
        (condition-case err
            (progn
              (funcall api-fn server patch-id new-value)
              (when patch
                (patchwork-db-insert-patch (plist-put (copy-sequence patch) column new-value)))
              (patchwork-db-delete-pending-change server-url patch-id field)
              (message "patchwork: %s set to %s for patch %s" field new-value patch-id)
              'applied)
          (error
           (if (patchwork-cache--queueable-error-p err)
               (progn
                 (patchwork-db-queue-pending-change server-url patch-id field new-value observed)
                 (message "patchwork: network unavailable, queued %s change (%s) for patch %s"
                          field new-value patch-id)
                 'queued)
             (signal (car err) (cdr err)))))))))

(defun patchwork-cache-set-patch-state (server patch-id new-state)
  "Set PATCH-ID's state to NEW-STATE on SERVER, queuing it locally
instead of failing outright if the server can't be reached right now --
see `patchwork-cache--set-patch-field'."
  (patchwork-cache--set-patch-field server patch-id "state" new-state
                                     #'patchwork-api-set-state :state))

(defun patchwork-cache-set-patch-delegate (server patch-id new-delegate)
  "Set PATCH-ID's delegate to NEW-DELEGATE on SERVER, queuing it locally
instead of failing outright if the server can't be reached right now --
see `patchwork-cache--set-patch-field'.  NEW-DELEGATE must already be
whatever form the server's API actually expects (a user id or username,
depending on deployment) -- the locally cached delegate is a resolved
display name for display purposes only, and there is no name-to-id
lookup here."
  (patchwork-cache--set-patch-field server patch-id "delegate" new-delegate
                                     #'patchwork-api-set-delegate :delegate))

(defun patchwork-cache-set-series-state (server series-id new-state)
  "Set NEW-STATE on every patch currently in SERIES-ID on SERVER.
Patchwork has no native per-series state -- this simply loops
`patchwork-cache-set-patch-state' over the series' patches; one patch's
failure is reported but doesn't stop the rest from being attempted."
  (dolist (patch (patchwork-db-get-series-patches (plist-get server :url) series-id))
    (condition-case err
        (patchwork-cache-set-patch-state server (plist-get patch :id) new-state)
      (error
       (message "patchwork: failed to set state for patch %s: %s"
                (plist-get patch :id) (error-message-string err))))))

(defun patchwork-cache-set-series-delegate (server series-id new-delegate)
  "Set NEW-DELEGATE on every patch currently in SERIES-ID on SERVER.
See `patchwork-cache-set-series-state' and
`patchwork-cache-set-patch-delegate'."
  (dolist (patch (patchwork-db-get-series-patches (plist-get server :url) series-id))
    (condition-case err
        (patchwork-cache-set-patch-delegate server (plist-get patch :id) new-delegate)
      (error
       (message "patchwork: failed to set delegate for patch %s: %s"
                (plist-get patch :id) (error-message-string err))))))

(defun patchwork-cache-patch-pending-value (server-url patch-id field)
  "Return the queued desired value for FIELD (\"state\" or \"delegate\")
on PATCH-ID on SERVER-URL, or nil if nothing is queued for it."
  (plist-get (patchwork-db-get-pending-change server-url patch-id field) :desired-value))

(defun patchwork-cache-series-pending-value (server-url series-id field)
  "Return the queued desired value for FIELD across every patch in
SERIES-ID on SERVER-URL: nil if none are pending, the value if every
one of the series' patches that has a pending FIELD change agrees on
it AND every patch has one, or the symbol `mixed' if they disagree or
only some of the series' patches have one queued -- this can happen
after a partial bulk failure, or if one patch was changed
individually, and should never be silently collapsed to one
arbitrarily-chosen patch's value."
  (let ((values nil)
        (any-missing nil))
    (dolist (patch (patchwork-db-get-series-patches server-url series-id))
      (let ((v (patchwork-cache-patch-pending-value server-url (plist-get patch :id) field)))
        (if v (push v values) (setq any-missing t))))
    (cond
     ((null values) nil)
     ((and (not any-missing) (= (length (delete-dups (copy-sequence values))) 1))
      (car values))
     (t 'mixed))))

(defun patchwork-cache--drain-pending-changes (server)
  "Retry every currently-queued change for SERVER now that a sync has
just succeeded against it.  For each: if the patch is no longer
tracked locally at all, leave it queued (skip it) rather than risk
applying a stale value against something nobody's watched since;
otherwise compare the patch's current cached value for that field
against what was observed when the change was queued -- if it still
matches, apply the change for real; if it has changed server-side in
the meantime, discard the queued change and report the conflict
rather than applying it (does not keep it around for later review --
see the design note in TODO.org)."
  (let* ((server-url (plist-get server :url))
         (patchwork-cache--draining-p server))
    (dolist (pending (patchwork-db-get-pending-changes server-url))
      (let* ((patch-id (plist-get pending :patch-id))
             (field (plist-get pending :field))
             (column (intern (concat ":" field)))
             (patch (patchwork-db-get-patch server-url patch-id)))
        (cond
         ((null patch)
          (message "patchwork: patch %s no longer tracked locally; leaving its queued %s change as-is"
                   patch-id field))
         ((equal (plist-get patch column) (plist-get pending :observed-value))
          (let ((api-fn (if (equal field "state") #'patchwork-api-set-state #'patchwork-api-set-delegate)))
            (condition-case err
                (progn
                  (funcall api-fn server patch-id (plist-get pending :desired-value))
                  (patchwork-db-insert-patch
                   (plist-put (copy-sequence patch) column (plist-get pending :desired-value)))
                  (patchwork-db-delete-pending-change server-url patch-id field)
                  (message "patchwork: applied queued %s change (%s) for patch %s"
                           field (plist-get pending :desired-value) patch-id))
              (error
               (unless (patchwork-cache--queueable-error-p err)
                 (patchwork-db-delete-pending-change server-url patch-id field))
               (message "patchwork: retry of queued %s change for patch %s failed: %s"
                        field patch-id (error-message-string err))))))
         (t
          (message "patchwork: queued %s change for patch %s is stale (was %s, now %s) -- discarding, not applying"
                   field patch-id (plist-get pending :observed-value) (plist-get patch column))
          (patchwork-db-delete-pending-change server-url patch-id field)))))))

(defun patchwork-cache-sync-series (server series-id)
  "Fetch and cache SERIES-ID from SERVER directly -- its patches,
comments, and checks, same as a routine sync would -- regardless of
`patchwork-sync-lookback-days' or which project it belongs to (a
series' own data already says that).  Useful for pulling in a specific
series that falls outside the range any routine sync would ever
reach, e.g. one you already know the id of from browsing the
Patchwork web UI directly.  Returns the cached series plist."
  (patchwork-cache--sync-one-series server (patchwork-api-get-series server series-id))
  (patchwork-db-get-series (plist-get server :url) series-id))

(provide 'patchwel-cache)

;;; patchwel-cache.el ends here
