;;; patchwel-db.el --- Local SQLite cache for patchwel -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'sqlite)
(require 'patchwel-config)

(defvar patchwork-db--connection nil
  "Cached connection to the local Patchwork SQLite database.")

(defconst patchwork-db-schema-version 8
  "Bump whenever `patchwork-db-schema' changes shape.
A mismatch causes the local cache tables to be dropped and recreated,
since the database only ever holds re-fetchable cache data -- with the
deliberate exception of `checks', `pending_changes', and `notes' (see
`patchwork-db-init'), which are not.")

(defconst patchwork-db-schema
  '("CREATE TABLE IF NOT EXISTS projects (
       server_url TEXT NOT NULL,
       id INTEGER NOT NULL,
       name TEXT,
       slug TEXT,
       updated_at TEXT,
       PRIMARY KEY (server_url, id))"
    "CREATE TABLE IF NOT EXISTS series (
       server_url TEXT NOT NULL,
       id INTEGER NOT NULL,
       project_id INTEGER,
       project_slug TEXT,
       name TEXT,
       submitter TEXT,
       version INTEGER,
       total INTEGER,
       submitted_at TEXT,
       state TEXT,
       assignee TEXT,
       comment_count INTEGER DEFAULT 0,
       ack_count INTEGER DEFAULT 0,
       review_count INTEGER DEFAULT 0,
       test_count INTEGER DEFAULT 0,
       fixes_count INTEGER DEFAULT 0,
       check_success INTEGER DEFAULT 0,
       check_warning INTEGER DEFAULT 0,
       check_fail INTEGER DEFAULT 0,
       url TEXT,
       updated_at TEXT,
       cached_at TEXT,
       PRIMARY KEY (server_url, id))"
    "CREATE TABLE IF NOT EXISTS patches (
       server_url TEXT NOT NULL,
       id INTEGER NOT NULL,
       series_id INTEGER,
       project_id INTEGER,
       state TEXT,
       submitter TEXT,
       delegate TEXT,
       name TEXT,
       date TEXT,
       series_position INTEGER,
       check_state TEXT,
       content TEXT,
       diff TEXT,
       submitter_email TEXT,
       msgid TEXT,
       to_header TEXT,
       cc_header TEXT,
       references_header TEXT,
       in_reply_to_header TEXT,
       updated_at TEXT,
       cached_at TEXT,
       PRIMARY KEY (server_url, id))"
    "CREATE TABLE IF NOT EXISTS comments (
       server_url TEXT NOT NULL,
       id INTEGER NOT NULL,
       patch_id INTEGER,
       author TEXT,
       date TEXT,
       content TEXT,
       msgid TEXT,
       subject TEXT,
       submitter_email TEXT,
       to_header TEXT,
       cc_header TEXT,
       references_header TEXT,
       in_reply_to_header TEXT,
       PRIMARY KEY (server_url, id))"
    "CREATE TABLE IF NOT EXISTS checks (
       server_url TEXT NOT NULL,
       id INTEGER NOT NULL,
       patch_id INTEGER,
       reporter TEXT,
       state TEXT,
       context TEXT,
       description TEXT,
       target_url TEXT,
       date TEXT,
       PRIMARY KEY (server_url, id))"
    "CREATE TABLE IF NOT EXISTS sync_meta (
       key TEXT PRIMARY KEY,
       value TEXT,
       updated_at TEXT)"
    "CREATE TABLE IF NOT EXISTS pending_changes (
       server_url TEXT NOT NULL,
       patch_id INTEGER NOT NULL,
       field TEXT NOT NULL,
       desired_value TEXT,
       observed_value TEXT,
       queued_at TEXT,
       PRIMARY KEY (server_url, patch_id, field))"
    "CREATE TABLE IF NOT EXISTS notes (
       server_url TEXT NOT NULL,
       target_type TEXT NOT NULL,
       target_id INTEGER NOT NULL,
       content TEXT,
       updated_at TEXT,
       PRIMARY KEY (server_url, target_type, target_id))"
    "CREATE INDEX IF NOT EXISTS idx_patches_series ON patches(server_url, series_id)"
    "CREATE INDEX IF NOT EXISTS idx_patches_project ON patches(server_url, project_id)"
    "CREATE INDEX IF NOT EXISTS idx_series_project ON series(server_url, project_slug)"
    "CREATE INDEX IF NOT EXISTS idx_comments_patch ON comments(server_url, patch_id)"
    "CREATE INDEX IF NOT EXISTS idx_checks_patch ON checks(server_url, patch_id)")
  "SQL statements used to create the local cache schema.")

(defun patchwork-db-connection ()
  "Return the open connection to `patchwork-local-db-file', opening it if needed.
Sets WAL journaling and a busy timeout so a background updater (e.g. a
cron job running `emacs --batch' against the same file) and an
interactive Emacs session can read and write concurrently without
\"database is locked\" errors."
  (unless (and patchwork-db--connection (sqlitep patchwork-db--connection))
    (make-directory (file-name-directory patchwork-local-db-file) t)
    (setq patchwork-db--connection (sqlite-open patchwork-local-db-file))
    (sqlite-execute patchwork-db--connection "PRAGMA journal_mode=WAL")
    (sqlite-execute patchwork-db--connection "PRAGMA busy_timeout=5000")
    (patchwork-db-init patchwork-db--connection))
  patchwork-db--connection)

(defun patchwork-db-init (&optional db)
  "Create the local cache schema in DB, migrating if the schema is outdated.
`checks', `pending_changes', and `notes' are deliberately never in the
drop list below: unlike the rest of the cache, they hold state that
cannot be re-fetched from the server (a queued offline change, or a
hand-authored note), so an unrelated future schema bump must not
silently discard them."
  (let* ((db (or db (patchwork-db-connection)))
         (version (or (caar (sqlite-select db "PRAGMA user_version")) 0)))
    (when (< version patchwork-db-schema-version)
      (dolist (table '("comments" "patches" "series" "projects" "sync_meta"))
        (sqlite-execute db (format "DROP TABLE IF EXISTS %s" table))))
    (dolist (stmt patchwork-db-schema)
      (sqlite-execute db stmt))
    (sqlite-execute db (format "PRAGMA user_version = %d" patchwork-db-schema-version))
    db))

(defun patchwork-db-close ()
  "Close the cached database connection, if open."
  (when (and patchwork-db--connection (sqlitep patchwork-db--connection))
    (sqlite-close patchwork-db--connection))
  (setq patchwork-db--connection nil))

;; -- projects ---------------------------------------------------------

(defun patchwork-db-insert-project (server-url id name slug)
  "Insert or update project ID on SERVER-URL with NAME and SLUG."
  (sqlite-execute
   (patchwork-db-connection)
   "INSERT INTO projects (server_url, id, name, slug, updated_at) VALUES (?, ?, ?, ?, ?)
    ON CONFLICT(server_url, id) DO UPDATE SET name = excluded.name,
                                              slug = excluded.slug,
                                              updated_at = excluded.updated_at"
   (list server-url id name slug (current-time-string))))

;; -- series -------------------------------------------------------------

(defconst patchwork-db--series-columns
  "server_url, id, project_id, project_slug, name, submitter, version, total,
   submitted_at, state, assignee, comment_count, ack_count, review_count,
   test_count, fixes_count, check_success, check_warning, check_fail, url,
   updated_at, cached_at")

(defun patchwork-db--series-row-to-plist (row)
  "Convert a ROW returned from the series table into a plist."
  (pcase-let ((`(,server-url ,id ,project-id ,project-slug ,name ,submitter
                 ,version ,total ,submitted-at ,state ,assignee ,comment-count
                 ,ack-count ,review-count ,test-count ,fixes-count
                 ,check-success ,check-warning ,check-fail ,url
                 ,updated-at ,cached-at)
                row))
    (list :server-url server-url :id id :project-id project-id
          :project-slug project-slug :name name :submitter submitter
          :version version :total total :submitted-at submitted-at
          :state state :assignee assignee :comment-count comment-count
          :ack-count ack-count :review-count review-count
          :test-count test-count :fixes-count fixes-count
          :check-success check-success :check-warning check-warning
          :check-fail check-fail :url url :updated-at updated-at
          :cached-at cached-at)))

(defun patchwork-db-upsert-series (series)
  "Insert or update SERIES, a plist as produced by
`patchwork-cache--sync-one-series'."
  (sqlite-execute
   (patchwork-db-connection)
   (format "INSERT INTO series (%s) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
            ON CONFLICT(server_url, id) DO UPDATE SET
              project_id = excluded.project_id,
              project_slug = excluded.project_slug,
              name = excluded.name,
              submitter = excluded.submitter,
              version = excluded.version,
              total = excluded.total,
              submitted_at = excluded.submitted_at,
              state = excluded.state,
              assignee = excluded.assignee,
              comment_count = excluded.comment_count,
              ack_count = excluded.ack_count,
              review_count = excluded.review_count,
              test_count = excluded.test_count,
              fixes_count = excluded.fixes_count,
              check_success = excluded.check_success,
              check_warning = excluded.check_warning,
              check_fail = excluded.check_fail,
              url = excluded.url,
              updated_at = excluded.updated_at,
              cached_at = excluded.cached_at"
           patchwork-db--series-columns)
   (list (plist-get series :server-url)
         (plist-get series :id)
         (plist-get series :project-id)
         (plist-get series :project-slug)
         (plist-get series :name)
         (plist-get series :submitter)
         (plist-get series :version)
         (plist-get series :total)
         (plist-get series :submitted-at)
         (plist-get series :state)
         (plist-get series :assignee)
         (plist-get series :comment-count)
         (plist-get series :ack-count)
         (plist-get series :review-count)
         (plist-get series :test-count)
         (plist-get series :fixes-count)
         (plist-get series :check-success)
         (plist-get series :check-warning)
         (plist-get series :check-fail)
         (plist-get series :url)
         (plist-get series :updated-at)
         (current-time-string))))

(defun patchwork-db-query-series (&optional server-url project-slug)
  "Return cached series, newest first.
When SERVER-URL is given, restrict to that server.  When PROJECT-SLUG is
also given, further restrict to that project."
  (let* ((db (patchwork-db-connection))
         (base (format "SELECT %s FROM series" patchwork-db--series-columns))
         (rows (cond
                ((and server-url project-slug)
                 (sqlite-select db
                                 (concat base " WHERE server_url = ? AND project_slug = ? ORDER BY submitted_at DESC")
                                 (list server-url project-slug)))
                (server-url
                 (sqlite-select db
                                 (concat base " WHERE server_url = ? ORDER BY submitted_at DESC")
                                 (list server-url)))
                (t
                 (sqlite-select db (concat base " ORDER BY submitted_at DESC"))))))
    (mapcar #'patchwork-db--series-row-to-plist rows)))

(defun patchwork-db-get-series (server-url series-id)
  "Return the cached series plist for SERIES-ID on SERVER-URL, or nil."
  (let ((rows (sqlite-select
               (patchwork-db-connection)
               (format "SELECT %s FROM series WHERE server_url = ? AND id = ?"
                       patchwork-db--series-columns)
               (list server-url series-id))))
    (when rows
      (patchwork-db--series-row-to-plist (car rows)))))

;; -- patches --------------------------------------------------------------

(defconst patchwork-db--patch-columns
  "server_url, id, series_id, project_id, state, submitter, delegate, name,
   date, series_position, check_state, content, diff, submitter_email,
   msgid, to_header, cc_header, references_header, in_reply_to_header,
   updated_at, cached_at")

(defun patchwork-db--patch-row-to-plist (row)
  "Convert a ROW returned from the patches table into a plist."
  (pcase-let ((`(,server-url ,id ,series-id ,project-id ,state ,submitter
                 ,delegate ,name ,date ,series-position ,check-state
                 ,content ,diff ,submitter-email ,msgid ,to ,cc ,references
                 ,in-reply-to ,updated-at ,cached-at)
                row))
    (list :server-url server-url :id id :series-id series-id
          :project-id project-id :state state :submitter submitter
          :delegate delegate :name name :date date
          :series-position series-position :check-state check-state
          :content content :diff diff :submitter-email submitter-email
          :msgid msgid :to to :cc cc :references references
          :in-reply-to in-reply-to :updated-at updated-at :cached-at cached-at)))

(defun patchwork-db-insert-patch (patch)
  "Insert or update PATCH, a plist with patch fields including :server-url.
:content, :diff, :submitter-email, :msgid, :to, :cc, :references, and
:in-reply-to are optional -- populated when the sync layer fetched
this patch's full detail (the list view used for routine syncing
doesn't carry the commit message, diff, or mail headers), left nil
otherwise."
  (sqlite-execute
   (patchwork-db-connection)
   (format "INSERT INTO patches (%s) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
            ON CONFLICT(server_url, id) DO UPDATE SET
              series_id = excluded.series_id,
              project_id = excluded.project_id,
              state = excluded.state,
              submitter = excluded.submitter,
              delegate = excluded.delegate,
              name = excluded.name,
              date = excluded.date,
              series_position = excluded.series_position,
              check_state = excluded.check_state,
              content = excluded.content,
              diff = excluded.diff,
              submitter_email = excluded.submitter_email,
              msgid = excluded.msgid,
              to_header = excluded.to_header,
              cc_header = excluded.cc_header,
              references_header = excluded.references_header,
              in_reply_to_header = excluded.in_reply_to_header,
              updated_at = excluded.updated_at,
              cached_at = excluded.cached_at"
           patchwork-db--patch-columns)
   (list (plist-get patch :server-url)
         (plist-get patch :id)
         (plist-get patch :series-id)
         (plist-get patch :project-id)
         (plist-get patch :state)
         (plist-get patch :submitter)
         (plist-get patch :delegate)
         (plist-get patch :name)
         (plist-get patch :date)
         (plist-get patch :series-position)
         (plist-get patch :check-state)
         (plist-get patch :content)
         (plist-get patch :diff)
         (plist-get patch :submitter-email)
         (plist-get patch :msgid)
         (plist-get patch :to)
         (plist-get patch :cc)
         (plist-get patch :references)
         (plist-get patch :in-reply-to)
         (plist-get patch :updated-at)
         (current-time-string))))

(defun patchwork-db-get-series-patches (server-url series-id)
  "Return cached patches on SERVER-URL belonging to SERIES-ID, ordered by position."
  (mapcar #'patchwork-db--patch-row-to-plist
          (sqlite-select
           (patchwork-db-connection)
           (format "SELECT %s FROM patches
                    WHERE server_url = ? AND series_id = ? ORDER BY series_position ASC"
                   patchwork-db--patch-columns)
           (list server-url series-id))))

(defun patchwork-db-get-patch (server-url patch-id)
  "Return the cached patch plist for PATCH-ID on SERVER-URL, or nil."
  (let ((rows (sqlite-select
               (patchwork-db-connection)
               (format "SELECT %s FROM patches WHERE server_url = ? AND id = ?"
                       patchwork-db--patch-columns)
               (list server-url patch-id))))
    (when rows
      (patchwork-db--patch-row-to-plist (car rows)))))

;; -- comments ---------------------------------------------------------------

(defconst patchwork-db--comment-columns
  "server_url, id, patch_id, author, date, content, msgid, subject,
   submitter_email, to_header, cc_header, references_header,
   in_reply_to_header")

(defun patchwork-db--comment-row-to-plist (row)
  "Convert a ROW returned from the comments table into a plist."
  (pcase-let ((`(,server-url ,id ,pid ,author ,date ,content ,msgid ,subject
                 ,submitter-email ,to ,cc ,references ,in-reply-to)
                row))
    (list :server-url server-url :id id :patch-id pid :author author
          :date date :content content :msgid msgid :subject subject
          :submitter-email submitter-email :to to :cc cc
          :references references :in-reply-to in-reply-to)))

(defun patchwork-db-insert-comment (comment)
  "Insert or update COMMENT, a plist with comment fields including :server-url."
  (sqlite-execute
   (patchwork-db-connection)
   (format "INSERT INTO comments (%s) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
            ON CONFLICT(server_url, id) DO UPDATE SET
              patch_id = excluded.patch_id,
              author = excluded.author,
              date = excluded.date,
              content = excluded.content,
              msgid = excluded.msgid,
              subject = excluded.subject,
              submitter_email = excluded.submitter_email,
              to_header = excluded.to_header,
              cc_header = excluded.cc_header,
              references_header = excluded.references_header,
              in_reply_to_header = excluded.in_reply_to_header"
           patchwork-db--comment-columns)
   (list (plist-get comment :server-url)
         (plist-get comment :id)
         (plist-get comment :patch-id)
         (plist-get comment :author)
         (plist-get comment :date)
         (plist-get comment :content)
         (plist-get comment :msgid)
         (plist-get comment :subject)
         (plist-get comment :submitter-email)
         (plist-get comment :to)
         (plist-get comment :cc)
         (plist-get comment :references)
         (plist-get comment :in-reply-to))))

(defun patchwork-db-get-comments (server-url patch-id)
  "Return cached comments on SERVER-URL for PATCH-ID, ordered by date."
  (mapcar #'patchwork-db--comment-row-to-plist
          (sqlite-select
           (patchwork-db-connection)
           (format "SELECT %s FROM comments WHERE server_url = ? AND patch_id = ? ORDER BY date ASC"
                   patchwork-db--comment-columns)
           (list server-url patch-id))))

(defun patchwork-db-get-comment (server-url comment-id)
  "Return the cached comment plist for COMMENT-ID on SERVER-URL, or nil."
  (let ((rows (sqlite-select
               (patchwork-db-connection)
               (format "SELECT %s FROM comments WHERE server_url = ? AND id = ?"
                       patchwork-db--comment-columns)
               (list server-url comment-id))))
    (when rows
      (patchwork-db--comment-row-to-plist (car rows)))))

;; -- checks ---------------------------------------------------------------

(defconst patchwork-db--check-columns
  "server_url, id, patch_id, reporter, state, context, description,
   target_url, date")

(defun patchwork-db--check-row-to-plist (row)
  "Convert a ROW returned from the checks table into a plist."
  (pcase-let ((`(,server-url ,id ,pid ,reporter ,state ,context ,description
                 ,target-url ,date)
                row))
    (list :server-url server-url :id id :patch-id pid :reporter reporter
          :state state :context context :description description
          :target-url target-url :date date)))

(defun patchwork-db-insert-check (check)
  "Insert or update CHECK, a plist with check fields including :server-url."
  (sqlite-execute
   (patchwork-db-connection)
   (format "INSERT INTO checks (%s) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
            ON CONFLICT(server_url, id) DO UPDATE SET
              patch_id = excluded.patch_id,
              reporter = excluded.reporter,
              state = excluded.state,
              context = excluded.context,
              description = excluded.description,
              target_url = excluded.target_url,
              date = excluded.date"
           patchwork-db--check-columns)
   (list (plist-get check :server-url)
         (plist-get check :id)
         (plist-get check :patch-id)
         (plist-get check :reporter)
         (plist-get check :state)
         (plist-get check :context)
         (plist-get check :description)
         (plist-get check :target-url)
         (plist-get check :date))))

(defun patchwork-db-get-checks (server-url patch-id)
  "Return cached checks on SERVER-URL for PATCH-ID, ordered by date."
  (mapcar #'patchwork-db--check-row-to-plist
          (sqlite-select
           (patchwork-db-connection)
           (format "SELECT %s FROM checks WHERE server_url = ? AND patch_id = ? ORDER BY date ASC"
                   patchwork-db--check-columns)
           (list server-url patch-id))))

;; -- pending changes (offline-queued state/delegate updates) ---------------

(defconst patchwork-db--pending-change-columns
  "server_url, patch_id, field, desired_value, observed_value, queued_at")

(defun patchwork-db--pending-change-row-to-plist (row)
  "Convert a ROW returned from the pending_changes table into a plist."
  (pcase-let ((`(,server-url ,patch-id ,field ,desired ,observed ,queued-at) row))
    (list :server-url server-url :patch-id patch-id :field field
          :desired-value desired :observed-value observed :queued-at queued-at)))

(defun patchwork-db-queue-pending-change (server-url patch-id field desired-value observed-value)
  "Queue (or replace any existing queued) change of FIELD (a string,
\"state\" or \"delegate\") on PATCH-ID on SERVER-URL to DESIRED-VALUE.
OBSERVED-VALUE is the value FIELD had locally at the time this change
was requested, used later to detect whether it changed server-side in
the meantime before the queued change is ever applied.  Only one
pending change per (SERVER-URL, PATCH-ID, FIELD) is kept -- a newer
request for the same patch+field replaces the older queued one."
  (sqlite-execute
   (patchwork-db-connection)
   (format "INSERT INTO pending_changes (%s) VALUES (?, ?, ?, ?, ?, ?)
            ON CONFLICT(server_url, patch_id, field) DO UPDATE SET
              desired_value = excluded.desired_value,
              observed_value = excluded.observed_value,
              queued_at = excluded.queued_at"
           patchwork-db--pending-change-columns)
   (list server-url patch-id field desired-value observed-value (current-time-string))))

(defun patchwork-db-get-pending-change (server-url patch-id field)
  "Return the queued change plist for (SERVER-URL, PATCH-ID, FIELD), or nil."
  (let ((rows (sqlite-select
               (patchwork-db-connection)
               (format "SELECT %s FROM pending_changes
                        WHERE server_url = ? AND patch_id = ? AND field = ?"
                       patchwork-db--pending-change-columns)
               (list server-url patch-id field))))
    (when rows
      (patchwork-db--pending-change-row-to-plist (car rows)))))

(defun patchwork-db-get-pending-changes (&optional server-url)
  "Return all queued changes, or only those on SERVER-URL if given."
  (mapcar #'patchwork-db--pending-change-row-to-plist
          (if server-url
              (sqlite-select
               (patchwork-db-connection)
               (format "SELECT %s FROM pending_changes WHERE server_url = ?"
                       patchwork-db--pending-change-columns)
               (list server-url))
            (sqlite-select
             (patchwork-db-connection)
             (format "SELECT %s FROM pending_changes" patchwork-db--pending-change-columns)))))

(defun patchwork-db-delete-pending-change (server-url patch-id field)
  "Delete the queued change for (SERVER-URL, PATCH-ID, FIELD), if any."
  (sqlite-execute
   (patchwork-db-connection)
   "DELETE FROM pending_changes WHERE server_url = ? AND patch_id = ? AND field = ?"
   (list server-url patch-id field)))

;; -- notes ----------------------------------------------------------------

(defun patchwork-db-set-note (server-url target-type target-id content)
  "Set the note for TARGET-TYPE (a string, \"patch\" or \"series\") and
TARGET-ID on SERVER-URL to CONTENT.  If CONTENT is nil or blank
\(`string-blank-p'\), the note is deleted instead of storing an empty
row -- this is also how a note gets removed, there is no separate
delete keybinding."
  (if (or (null content) (string-blank-p content))
      (patchwork-db-delete-note server-url target-type target-id)
    (sqlite-execute
     (patchwork-db-connection)
     "INSERT INTO notes (server_url, target_type, target_id, content, updated_at)
      VALUES (?, ?, ?, ?, ?)
      ON CONFLICT(server_url, target_type, target_id) DO UPDATE SET
        content = excluded.content, updated_at = excluded.updated_at"
     (list server-url target-type target-id content (current-time-string)))))

(defun patchwork-db-get-note (server-url target-type target-id)
  "Return the note content for TARGET-TYPE/TARGET-ID on SERVER-URL, or nil."
  (caar (sqlite-select
         (patchwork-db-connection)
         "SELECT content FROM notes WHERE server_url = ? AND target_type = ? AND target_id = ?"
         (list server-url target-type target-id))))

(defun patchwork-db-delete-note (server-url target-type target-id)
  "Delete the note for TARGET-TYPE/TARGET-ID on SERVER-URL, if any."
  (sqlite-execute
   (patchwork-db-connection)
   "DELETE FROM notes WHERE server_url = ? AND target_type = ? AND target_id = ?"
   (list server-url target-type target-id)))

;; -- purging a project's cached data -------------------------------------

(defun patchwork-db--cached-project-slugs (server-url)
  "Return the distinct project slugs with any cached series on SERVER-URL."
  (mapcar #'car
          (sqlite-select
           (patchwork-db-connection)
           "SELECT DISTINCT project_slug FROM series
            WHERE server_url = ? AND project_slug IS NOT NULL"
           (list server-url))))

(defun patchwork-db-purge-project (server-url project-slug)
  "Delete every cached series, patch, comment, check, queued pending
change, and note for PROJECT-SLUG on SERVER-URL, plus its own
`projects' row -- e.g. to clean up a project that got polled by
mistake (a misconfigured sync) without disturbing any other server or
project's cached data.  Scoped via `series.project_slug' (not
`projects.id'), so this also cleans up data cached for a project that
was queried directly and never got its own `projects' row.  Prompts
for confirmation, reporting how many series/patches would be removed,
before deleting anything."
  (interactive
   (let* ((server-url (completing-read
                        "Purge cached data for server: "
                        (mapcar (lambda (s) (plist-get s :url)) patchwork-servers)
                        nil t))
          (slug (completing-read
                 "Project slug to purge: "
                 (patchwork-db--cached-project-slugs server-url)
                 nil t)))
     (list server-url slug)))
  (let* ((db (patchwork-db-connection))
         (series-ids (mapcar #'car
                              (sqlite-select
                               db "SELECT id FROM series WHERE server_url = ? AND project_slug = ?"
                               (list server-url project-slug))))
         (patch-ids (and series-ids
                          (mapcar #'car
                                  (sqlite-select
                                   db (format "SELECT id FROM patches WHERE server_url = ? AND series_id IN (%s)"
                                              (mapconcat #'number-to-string series-ids ","))
                                   (list server-url))))))
    (if (null series-ids)
        (message "Nothing cached for project %s on %s" project-slug server-url)
      (when (or (not (called-interactively-p 'interactive))
                (yes-or-no-p (format "Purge %d cached series (%d patches) for project %s on %s? "
                                      (length series-ids) (length patch-ids)
                                      project-slug server-url)))
        (dolist (pid patch-ids)
          (sqlite-execute db "DELETE FROM comments WHERE server_url = ? AND patch_id = ?"
                           (list server-url pid))
          (sqlite-execute db "DELETE FROM checks WHERE server_url = ? AND patch_id = ?"
                           (list server-url pid))
          (sqlite-execute db "DELETE FROM pending_changes WHERE server_url = ? AND patch_id = ?"
                           (list server-url pid))
          (patchwork-db-delete-note server-url "patch" pid))
        (dolist (sid series-ids)
          (patchwork-db-delete-note server-url "series" sid))
        (sqlite-execute db (format "DELETE FROM patches WHERE server_url = ? AND series_id IN (%s)"
                                    (mapconcat #'number-to-string series-ids ","))
                         (list server-url))
        (sqlite-execute db "DELETE FROM series WHERE server_url = ? AND project_slug = ?"
                         (list server-url project-slug))
        (sqlite-execute db "DELETE FROM projects WHERE server_url = ? AND slug = ?"
                         (list server-url project-slug))
        (message "Purged project %s on %s: %d series, %d patch(es) removed."
                 project-slug server-url (length series-ids) (length patch-ids))))))

;; -- sync metadata ------------------------------------------------------

(defun patchwork-db-get-sync-meta (key)
  "Return the stored timestamp string for KEY, or nil."
  (caar (sqlite-select
         (patchwork-db-connection)
         "SELECT value FROM sync_meta WHERE key = ?"
         (list key))))

(defun patchwork-db-set-sync-meta (key value)
  "Set sync metadata KEY to VALUE."
  (sqlite-execute
   (patchwork-db-connection)
   "INSERT INTO sync_meta (key, value, updated_at) VALUES (?, ?, ?)
    ON CONFLICT(key) DO UPDATE SET value = excluded.value,
                                   updated_at = excluded.updated_at"
   (list key value (current-time-string))))

(provide 'patchwel-db)

;;; patchwel-db.el ends here
