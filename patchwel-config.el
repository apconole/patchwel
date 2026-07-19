;;; patchwel-config.el --- Configuration for patchwel -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'url-parse)

(defgroup patchwork nil
  "Interface with Patchwork patch tracking system."
  :group 'development
  :prefix "patchwork-")

(defcustom patchwork-servers
  '((:url "https://patchwork.ozlabs.org/api" :token nil :projects nil))
  "List of Patchwork servers to sync and browse.
Each entry is a plist with:

  :url           Base API URL for the server, no trailing slash.
  :token         API token for this server, or nil for anonymous/read-only
                 access.
  :projects      List of project slugs or ids to sync on this server, or
                 nil to sync every project visible to this server/token.
  :since-format  Optional; a key into `patchwork-since-format-strings'
                 (one of z, naive, date) forcing which `since=' timestamp
                 format to send to this server, skipping the normal
                 try-each-format cascade.  Different Patchwork deployments
                 parse `since=' differently (see
                 `patchwork-since-format-strings'); if you already know
                 which one a given server wants, set this to avoid paying
                 for a failed attempt with the wrong format on every
                 first-ever/forced sync.  Leave nil to keep auto-detecting.
  :user-agent    Optional User-Agent string to send to this server instead
                 of `patchwork-user-agent'.  Useful if a server admin has
                 exempted a specific string from anti-bot protection for
                 you.  Leave nil to use the default."
  :type '(repeat
          (list :tag "Server"
                (const :format "" :url)
                (string :tag "URL")
                (const :format "" :token)
                (choice :tag "Token" (const :tag "None" nil) string)
                (const :format "" :projects)
                (repeat :tag "Projects" string)
                (const :format "" :since-format)
                (choice :tag "Since format"
                        (const :tag "Auto (try common formats)" nil)
                        (const :tag "UTC datetime with Z suffix" z)
                        (const :tag "Naive datetime, no timezone" naive)
                        (const :tag "Date only" date))
                (const :format "" :user-agent)
                (choice :tag "User-Agent" (const :tag "Default" nil) string)))
  :group 'patchwork)

(defcustom patchwork-user-agent "curl/8.7.1"
  "Default User-Agent string sent with every Patchwork API request.
Emacs's own default (something like \"URL/Emacs Emacs/30.2...\") reads
as automated-client traffic, and at least one deployment fronted by an
anti-bot system (e.g. Anubis) has been observed serving a JavaScript
challenge page instead of a real API response for it.  Identifying as
a plain curl client is a more neutral choice that doesn't misrepresent
this as an interactive browser -- deliberately never set this to an
actual browser's User-Agent string.  Override per server with
:user-agent in `patchwork-servers', e.g. if a
server admin has exempted a specific string for you."
  :type 'string
  :group 'patchwork)

(defconst patchwork-since-format-strings
  '((z . "%Y-%m-%dT%H:%M:%SZ")
    (naive . "%Y-%m-%dT%H:%M:%S")
    (date . "%Y-%m-%d"))
  "Known `since=' timestamp formats, keyed by a short symbol, in the
order tried when a server's format isn't already known (see
:since-format in `patchwork-servers').  There is no one format
confirmed to work across every Patchwork deployment:
patchwork.ozlabs.org and patches.dpdk.org both want a Z-suffixed UTC
datetime (z); patchwork.kernel.org 500s on any timezone marker at all
and only accepts a naive datetime with none (naive); a bare date
(date) is the coarsest fallback.")

(defun patchwork-since-format-string (time &optional format)
  "Format the Lisp TIME value as a `since=' string using FORMAT, a key
in `patchwork-since-format-strings', defaulting to its Z-suffixed
entry when FORMAT is nil."
  (format-time-string (alist-get (or format 'z) patchwork-since-format-strings)
                       time t))

(defun patchwork-server-since-format (server)
  "Return SERVER's configured :since-format symbol, or nil to use the
default try-each-format cascade."
  (plist-get server :since-format))

(defcustom patchwork-local-db-file
  (expand-file-name "~/.cache/patchwel/patchwork.db")
  "Path to local SQLite cache database."
  :type 'file
  :group 'patchwork)

(defcustom patchwork-cache-ttl 300
  "Cache time-to-live in seconds before a re-sync is attempted."
  :type 'integer
  :group 'patchwork)

(defcustom patchwork-sync-lookback-days 30
  "How many days of history to sync on a server/project's first-ever sync.
Later syncs use the Patchwork events API to fetch only what changed
since the previous sync, regardless of this window; it only bounds how
far back the very first sync goes, and how far a fallback resync (used
when a server has no events API) reaches."
  :type 'integer
  :group 'patchwork)

(defcustom patchwork-page-size 100
  "Number of results to request per page from the Patchwork API."
  :type 'integer
  :group 'patchwork)

(defcustom patchwork-sync-timeout 5
  "Seconds to wait for a single Patchwork API HTTP request before giving up.
Applies to every individual request (series/patches/comments/checks/
events/etc.), not to a whole sync as one unit; a sync that makes many
requests can still take longer than this in total.  Combined with the
per-server error isolation in `patchwork-cache-sync', a slow or
unreachable server will time out and be skipped rather than blocking
the rest of the sync."
  :type 'integer
  :group 'patchwork)

(defcustom patchwork-series-buffer-name "*patchwork-series*"
  "Name of the main series listing buffer."
  :type 'string
  :group 'patchwork)

(defcustom patchwork-tag-names '("ack" "review" "test" "fixes")
  "Trailer tags to count when summarizing series activity.
Each entry is matched case-insensitively against comment text using
patterns such as \"Acked-by:\", \"Reviewed-by:\", \"Tested-by:\" and
\"Fixes:\"."
  :type '(repeat string)
  :group 'patchwork)

(defcustom patchwork-default-state-filter '("new" "assigned" "under-review")
  "States shown in the series listing buffer by default.
A series' aggregate state (see `patchwork-cache--series-state') must
match one of these strings to be shown until the buffer's filter is
changed at runtime with `patchwork-series-set-filter'.  Set to nil to
show every state by default.  Note that Patchwork's built-in state
names vary by deployment/project configuration; adjust this list to
match whatever your server(s) actually use."
  :type '(repeat string)
  :group 'patchwork)

(defun patchwork-server-slug (server)
  "Return a filesystem/buffer-name-safe identifier for SERVER's URL.
Includes the port when the URL specifies one, since two distinct
servers commonly share a host (e.g. local testing, or several
Patchwork instances proxied off different ports on one machine)."
  (let* ((url (plist-get server :url))
         (parsed (url-generic-parse-url url))
         (host (or (url-host parsed) url))
         (port (url-portspec parsed)))
    (replace-regexp-in-string "[^A-Za-z0-9.-]+" "-"
                               (if port (format "%s-%s" host port) host))))

(defun patchwork-servers-find (server-url)
  "Return the server plist in `patchwork-servers' whose :url is SERVER-URL."
  (seq-find (lambda (server) (equal (plist-get server :url) server-url))
            patchwork-servers))

(defun patchwork-server-projects (server)
  "Return the list of project filters to sync for SERVER.
A single nil entry means \"sync every project\"."
  (or (plist-get server :projects) (list nil)))

(defun patchwork-server-user-agent (server)
  "Return the User-Agent string to send to SERVER: its own :user-agent
override if set, otherwise `patchwork-user-agent'."
  (or (plist-get server :user-agent) patchwork-user-agent))

(provide 'patchwel-config)

;;; patchwel-config.el ends here
