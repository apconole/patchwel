;;; patchwork-cron-sync.el --- headless sync entry point for cron -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Standalone entry point for `emacs --batch -l patchwork-cron-sync.el',
;; so a crontab entry can keep the local Patchwork cache warm without an
;; interactive Emacs session open.  Deliberately does NOT load the user's
;; normal init file: init files often assume a display, take a while to
;; load (package.el bootstrapping, use-package with deferred packages,
;; etc.), or do things that don't make sense headless.
;;
;; This file is pure functional logic ONLY -- it deliberately contains no
;; server configuration of your own, so `git pull' can always update it
;; freely.  Your actual `patchwork-servers' (and any other `patchwork-*'
;; settings) belong in `patchwork-cron-config.el', a sibling file this
;; script loads that is NOT tracked by git (see .gitignore) -- copy
;; `patchwork-cron-config.el.example' to that path once and edit your
;; copy; editing THIS file instead is exactly the mistake that made
;; every future `git pull' here conflict with your own settings.
;;
;; Usage, single process, every configured server synced serially (as
;; before -- fine for a small number of servers/projects):
;;
;;   */5 * * * * /usr/bin/emacs --batch -l ~/.emacs.d/patchwork-cron-sync.el >> ~/.cache/patchwel/sync.log 2>&1
;;
;; Usage, one server per SERVER-URL argument (everything after Emacs's own
;; `--' marker on the command line): syncs only that one server, letting a
;; wrapper run several of these concurrently instead of serially -- see
;; `patchwork-cron-sync-parallel.sh', which drives exactly that by first
;; asking this same file (via `--list-servers') for the configured server
;; URLs, so the list only ever lives in one place:
;;
;;   emacs --batch -l ~/.emacs.d/patchwork-cron-sync.el -- https://patchwork.ozlabs.org/api
;;
;; Running more often than `patchwork-cache-ttl' is harmless: the sync is
;; a no-op (no network traffic) until the cache is actually due for
;; another look, so cron's interval just controls the worst-case latency
;; before new upstream activity shows up, not how often requests fire.
;; The local db is opened in WAL mode with a busy timeout specifically so
;; several of these processes (one per server) can write to it
;; concurrently without "database is locked" errors.

;;; Code:

(add-to-list 'load-path (expand-file-name "." (file-name-directory load-file-name)))

(require 'patchwel)

;; Your configuration lives in a sibling file this script loads, not in
;; this one -- see the Commentary above for why.  Its location can be
;; overridden via $PATCHWORK_CRON_CONFIG, e.g. to keep it in a private
;; dotfiles repo instead of right next to this script.
(defvar patchwork-cron-config-file
  (or (getenv "PATCHWORK_CRON_CONFIG")
      (expand-file-name "patchwork-cron-config.el" (file-name-directory load-file-name)))
  "Path to the untracked local configuration file loaded by
patchwork-cron-sync.el (see its Commentary section).")

(if (file-exists-p patchwork-cron-config-file)
    (load patchwork-cron-config-file)
  (error (concat "patchwork-cron-sync: no config file at %s -- copy "
                 "patchwork-cron-config.el.example there (or point "
                 "$PATCHWORK_CRON_CONFIG elsewhere) and edit it first")
         patchwork-cron-config-file))

;; `command-line-args-left' is not just a read-only view of what's left
;; on the command line -- it's the actual mutable queue Emacs's own
;; top-level argument loop keeps popping from once this `-l' script
;; returns.  It still contains the literal "--" marker at this point
;; (Emacs only treats "--" as "stop parsing my own options" when its
;; own loop reaches that position, not before a `-l' script runs), so
;; read a locally-filtered copy for our own use below rather than
;; mutating the global -- and once we're done with it, set the global
;; to nil so Emacs's own loop has nothing left to trip over (otherwise
;; it resumes past "--" and tries to parse our own arguments, e.g.
;; "--list-servers", as one of ITS OWN unrecognized options).
(let ((args (seq-remove (lambda (a) (string= a "--")) command-line-args-left)))
  (setq command-line-args-left nil)
  (cond
   ;; --list-servers: print each configured server's :url, one per line,
   ;; and exit immediately -- no sync, no db access at all.  This is the
   ;; single source of truth patchwork-cron-sync-parallel.sh reads from,
   ;; so the server list is never duplicated between this file and that
   ;; wrapper script.
   ((member "--list-servers" args)
    (dolist (server patchwork-servers)
      (princ (concat (plist-get server :url) "\n"))))

   ;; A server URL was given: restrict this run to only that one server,
   ;; so several of these processes (one per server) can run at once
   ;; instead of one process working through all of them serially.
   (args
    (let* ((target-url (car args))
           (patchwork-servers (seq-filter (lambda (s) (equal (plist-get s :url) target-url))
                                          patchwork-servers)))
      (unless patchwork-servers
        (error "patchwork-cron-sync: no configured server matches %s" target-url))
      (condition-case err
          (progn
            (patchwork-cache-sync)
            (message "patchwork-cron-sync: sync of %s complete" target-url))
        (error
         (message "patchwork-cron-sync: %s failed: %s" target-url (error-message-string err))))
      (patchwork-db-close)))

   ;; No arguments: sync every configured server serially, exactly as
   ;; before -- unchanged default behavior for a single cron job.
   (t
    (condition-case err
        (progn
          (patchwork-cache-sync)
          (message "patchwork-cron-sync: sync complete"))
      (error
       (message "patchwork-cron-sync: failed: %s" (error-message-string err))))
    (patchwork-db-close))))

;;; patchwork-cron-sync.el ends here
