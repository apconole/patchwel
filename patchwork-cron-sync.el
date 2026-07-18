;;; patchwork-cron-sync.el --- headless sync entry point for cron -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Standalone config for `emacs --batch -l patchwork-cron-sync.el', so a
;; crontab entry can keep the local Patchwork cache warm without an
;; interactive Emacs session open.  Deliberately does NOT load the user's
;; normal init file: init files often assume a display, take a while to
;; load (package.el bootstrapping, use-package with deferred packages,
;; etc.), or do things that don't make sense headless.  Instead, this file
;; is a minimal, self-contained duplicate of just the `patchwork-*'
;; settings needed for a sync -- keep it in sync with whatever you have in
;; your real init.el (same `patchwork-servers' etc.), or `load' a config
;; file that both this script and your init.el share.
;;
;; Usage: adjust the `add-to-list' and `setq' forms below for your setup,
;; then add a crontab entry such as:
;;
;;   */5 * * * * /usr/bin/emacs --batch -l ~/.emacs.d/patchwork-cron-sync.el >> ~/.cache/patchwel/sync.log 2>&1
;;
;; Running more often than `patchwork-cache-ttl' is harmless: the sync is
;; a no-op (no network traffic) until the cache is actually due for
;; another look, so cron's interval just controls the worst-case latency
;; before new upstream activity shows up, not how often requests fire.

;;; Code:

(add-to-list 'load-path (expand-file-name "." (file-name-directory load-file-name)))

(require 'patchwel)

;; Mirror your interactive `patchwork-servers' (and any other customized
;; `patchwork-*' variables -- `patchwork-local-db-file', `patchwork-sync-timeout',
;; `patchwork-sync-lookback-days', etc.) here so this headless run targets
;; the same cache file and the same servers/projects/tokens.
(setq patchwork-servers
      '((:url "https://patchwork.ozlabs.org/api" :token nil :projects nil)))

(condition-case err
    (progn
      (patchwork-cache-sync)
      (message "patchwork-cron-sync: sync complete"))
  (error
   (message "patchwork-cron-sync: failed: %s" (error-message-string err))))

(patchwork-db-close)

;;; patchwork-cron-sync.el ends here
