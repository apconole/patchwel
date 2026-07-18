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

  :url      Base API URL for the server, no trailing slash.
  :token    API token for this server, or nil for anonymous/read-only access.
  :projects List of project slugs or ids to sync on this server, or nil to
            sync every project visible to this server/token."
  :type '(repeat
          (list :tag "Server"
                (const :format "" :url)
                (string :tag "URL")
                (const :format "" :token)
                (choice :tag "Token" (const :tag "None" nil) string)
                (const :format "" :projects)
                (repeat :tag "Projects" string)))
  :group 'patchwork)

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

(provide 'patchwel-config)

;;; patchwel-config.el ends here
