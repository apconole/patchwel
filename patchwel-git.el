;;; patchwel-git.el --- Apply Patchwork patches/series to a git tree -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'patchwel-config)
(require 'patchwel-db)
(require 'patchwel-api)
(require 'vc)
(require 'button)

(defconst patchwork-git-temp-dir
  (expand-file-name "~/.cache/patchwel/patchwork-patches")
  "Directory where downloaded patch mbox files are cached.")

(defcustom patchwork-patch-cache-max-age-days 30
  "Age in days after which a downloaded patch mbox file in
`patchwork-git-temp-dir' is eligible for pruning by
`patchwork-git-prune-patches', based on the file's last-modified time."
  :type 'integer
  :group 'patchwork)

(defun patchwork-git-prune-patches (&optional all)
  "Delete cached patch mbox files from `patchwork-git-temp-dir'.
Every applied (or downloaded-for-mbox) patch is cached there
indefinitely -- `patchwork-git-download-patch' never cleans up after
itself -- so this is the way to reclaim that space.  Only files older
than `patchwork-patch-cache-max-age-days' are deleted; with a prefix
arg (ALL non-nil), every cached patch file is deleted regardless of
age.  Prompts for confirmation before deleting anything, then reports
how many files and how much disk space were freed."
  (interactive "P")
  (let* ((now (float-time))
         (max-age-secs (* patchwork-patch-cache-max-age-days 86400))
         (files (when (file-directory-p patchwork-git-temp-dir)
                  (directory-files patchwork-git-temp-dir t "\\.patch\\'")))
         (to-delete (if all
                        files
                      (seq-filter
                       (lambda (f)
                         (> (- now (float-time (file-attribute-modification-time
                                                 (file-attributes f))))
                            max-age-secs))
                       files))))
    (if (null to-delete)
        (message "No cached patch files to prune%s."
                 (if all "" (format " (older than %d day(s))"
                                     patchwork-patch-cache-max-age-days)))
      (let ((total-size (apply #'+ (mapcar #'file-attribute-size
                                            (mapcar #'file-attributes to-delete)))))
        (if (yes-or-no-p (format "Delete %d cached patch file(s) (%s)? "
                                  (length to-delete)
                                  (file-size-human-readable total-size)))
            (progn
              (dolist (f to-delete) (delete-file f))
              (message "Pruned %d cached patch file(s), freeing %s."
                        (length to-delete) (file-size-human-readable total-size)))
          (message "Nothing pruned."))))))

(defun patchwork-git--read-server ()
  "Prompt for one of the configured `patchwork-servers' and return its plist."
  (let ((url (completing-read "Patchwork server: "
                               (mapcar (lambda (s) (plist-get s :url)) patchwork-servers)
                               nil t)))
    (or (patchwork-servers-find url)
        (error "Unknown Patchwork server: %s" url))))

(defun patchwork-git--patch-file (server patch-id)
  "Return the cache path for PATCH-ID's mbox file on SERVER."
  (expand-file-name (format "%s-%s.patch" (patchwork-server-slug server) patch-id)
                     patchwork-git-temp-dir))

(defun patchwork-git-download-patch (server patch-id)
  "Download the mbox file for PATCH-ID on SERVER into `patchwork-git-temp-dir'.
Returns the path to the downloaded file."
  (make-directory patchwork-git-temp-dir t)
  (let* ((patch (patchwork-api-get-patch server patch-id))
         (mbox-url (plist-get patch :mbox))
         (dest (patchwork-git--patch-file server patch-id)))
    (unless mbox-url
      (error "Patch %s has no mbox URL" patch-id))
    (with-temp-file dest
      (insert (patchwork-api-fetch-raw server mbox-url)))
    dest))

(defun patchwork-apply-patch (server patch-id repo-dir &optional args)
  "Download and apply PATCH-ID from SERVER to the git repository at REPO-DIR.
ARGS defaults to (\"--reject\"), so a patch that doesn't apply cleanly
leaves a .rej file to review instead of just failing outright (git
apply rejects the combination of --reject with --3way, so this can't
also attempt a 3-way merge; see `patchwork-apply-series-as-commits'
for a `git am --3way'-based alternative that can).  Returns non-nil on
success."
  (interactive
   (list (patchwork-git--read-server)
         (read-number "Patch id: ")
         (read-directory-name "Git repository directory: ")))
  (let* ((args (or args '("--reject")))
         (patch-file (patchwork-git-download-patch server patch-id)))
    (message "Applying patch %s to %s..." patch-id repo-dir)
    (let ((exit-code (apply #'call-process "git" nil nil nil
                             "-C" repo-dir "apply"
                             (append args (list patch-file)))))
      (if (zerop exit-code)
          (progn
            (message "Patch %s applied successfully." patch-id)
            t)
        (let ((reject-files (directory-files repo-dir nil "\\.rej\\'")))
          (if reject-files
              (progn
                (message "Patch %s had conflicts; opening %s"
                         patch-id (car reject-files))
                (find-file (expand-file-name (car reject-files) repo-dir)))
            (message "Patch %s failed to apply (exit code %d)." patch-id exit-code)))
        nil))))

(defun patchwork-apply-series (server series-id repo-dir)
  "Apply every patch in SERIES-ID on SERVER to REPO-DIR, in order.
If the series' project has a branch-apply strategy configured (see
`patchwork-project-branch-strategy'), a new branch is created and
checked out in REPO-DIR for this series first; with no strategy
configured, patches are applied to REPO-DIR as-is.
Returns non-nil if every patch applied successfully."
  (interactive
   (let ((server (patchwork-git--read-server)))
     (list server
           (read-number "Series id: ")
           (read-directory-name "Git repository directory: "))))
  (let* ((series (patchwork-db-get-series (plist-get server :url) series-id))
         (patches (patchwork-db-get-series-patches (plist-get server :url) series-id))
         (success t))
    (when series
      (patchwork-git--maybe-checkout-branch (plist-get server :url)
                                            (plist-get series :project-slug)
                                            series-id repo-dir))
    (dolist (patch patches)
      (unless (patchwork-apply-patch server (plist-get patch :id) repo-dir)
        (setq success nil)))
    (if success
        (message "Series %s applied successfully." series-id)
      (message "Series %s had one or more patches that failed to apply." series-id))
    success))

(defun patchwork-git--checkout-new-branch (repo-dir base target)
  "Create and check out TARGET in REPO-DIR based on BASE.
Signals an error (rather than reusing or resetting it) if TARGET
already exists."
  (with-temp-buffer
    (let ((exit-code (call-process "git" nil t nil "-C" repo-dir
                                    "checkout" "-b" target base)))
      (unless (zerop exit-code)
        (error "Could not create branch %s from %s in %s: %s"
               target base repo-dir (string-trim (buffer-string)))))))

(defun patchwork-git--maybe-checkout-branch (server-url project-slug series-id repo-dir)
  "Check out a new branch in REPO-DIR for SERIES-ID if PROJECT-SLUG on
SERVER-URL has a branch-apply strategy configured (see
`patchwork-project-branch-strategy'), otherwise do nothing so the
series applies to whatever is currently checked out."
  (let ((strategy (patchwork-parse-branch-strategy
                    (patchwork-project-branch-strategy server-url project-slug)
                    series-id)))
    (when strategy
      (patchwork-git--checkout-new-branch repo-dir (car strategy) (cdr strategy))
      (message "Created and checked out branch %s from %s in %s"
               (cdr strategy) (car strategy) repo-dir))))

(defun patchwork-git--head-rev (repo-dir)
  "Return the current HEAD commit hash in REPO-DIR, or nil if it has none."
  (with-temp-buffer
    (if (zerop (call-process "git" nil t nil "-C" repo-dir "rev-parse" "HEAD"))
        (string-trim (buffer-string))
      nil)))

(defun patchwork-git--am-buffer-name (repo-dir)
  "Return the name of the `git am' status buffer for REPO-DIR."
  (format "*patchwork-am: %s*" (file-name-nondirectory (directory-file-name repo-dir))))

(defun patchwork-git--am-conflicted-files (repo-dir)
  "Return files with unresolved conflicts in REPO-DIR mid-`git am', or nil."
  (with-temp-buffer
    (call-process "git" nil t nil "-C" repo-dir "diff" "--name-only" "--diff-filter=U")
    (split-string (buffer-string) "\n" t)))

(defun patchwork-git--am-current-patch (repo-dir)
  "Return the raw patch email currently blocking `git am' in REPO-DIR, or nil."
  (with-temp-buffer
    (when (zerop (call-process "git" nil t nil "-C" repo-dir "am" "--show-current-patch"))
      (buffer-string))))

(defun patchwork-git--show-am-status (repo-dir &optional am-output)
  "Open a buffer summarizing the current `git am' state in REPO-DIR: the
patch currently blocking it, any conflicted files (each a button that
opens the file), and, if given, AM-OUTPUT -- the captured stdout/
stderr of the `git am'/`--continue'/`--skip'/`--abort' call that led
to this state."
  (let ((buffer (get-buffer-create (patchwork-git--am-buffer-name repo-dir))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "git am state in %s\n" repo-dir))
        (when (and am-output (not (string-empty-p am-output)))
          (insert "\n--- output ---\n")
          (insert am-output))
        (let ((current-patch (patchwork-git--am-current-patch repo-dir)))
          (if current-patch
              (progn
                (insert "\n--- patch currently blocking `git am' ---\n")
                (insert current-patch))
            (insert "\nNo patch currently blocking `git am' in this repository.\n")))
        (let ((conflicted (patchwork-git--am-conflicted-files repo-dir)))
          (when conflicted
            (insert "\n--- conflicted files (RET/mouse-2 to open) ---\n")
            (dolist (file conflicted)
              (insert "  ")
              (insert-text-button
               file
               'action (lambda (button)
                         (find-file (expand-file-name (button-label button) repo-dir)))
               'follow-link t
               'help-echo "mouse-2, RET: open this file")
              (insert "\n"))))
        (insert (format "\nWhen ready: `M-x patchwork-git-am-continue', `M-x patchwork-git-am-skip', or `M-x patchwork-git-am-abort', each prompting for the repository directory (%s here).\n"
                         repo-dir))
        (goto-char (point-min))
        (special-mode)))
    (pop-to-buffer buffer)))

(defun patchwork-git--run-am-subcommand (repo-dir subcommand)
  "Run \"git am SUBCOMMAND\" in REPO-DIR and show its result.
SUBCOMMAND is a string such as \"--continue\", \"--skip\", or \"--abort\"."
  (let ((buffer (generate-new-buffer " *patchwork-am-output*")))
    (unwind-protect
        (let ((exit-code (call-process "git" nil buffer nil "-C" repo-dir "am" subcommand))
              (output (with-current-buffer buffer (buffer-string))))
          (patchwork-git--show-am-status repo-dir output)
          (if (zerop exit-code)
              (message "git am %s succeeded in %s." subcommand repo-dir)
            (message "git am %s failed in %s; see %s."
                     subcommand repo-dir (patchwork-git--am-buffer-name repo-dir))))
      (kill-buffer buffer))))

(defun patchwork-git-am-status (repo-dir)
  "Show the current `git am' state (blocking patch, conflicted files) in
REPO-DIR, without running any `git am' subcommand.  Useful to look
back at a failure after the fact, e.g. from a later Emacs session."
  (interactive (list (read-directory-name "Git repository directory: ")))
  (patchwork-git--show-am-status repo-dir))

(defun patchwork-git-am-continue (repo-dir)
  "Run \"git am --continue\" in REPO-DIR, after resolving conflicts by
hand, and show the result."
  (interactive (list (read-directory-name "Git repository directory: ")))
  (patchwork-git--run-am-subcommand repo-dir "--continue"))

(defun patchwork-git-am-skip (repo-dir)
  "Run \"git am --skip\" in REPO-DIR and show the result."
  (interactive (list (read-directory-name "Git repository directory: ")))
  (patchwork-git--run-am-subcommand repo-dir "--skip"))

(defun patchwork-git-am-abort (repo-dir)
  "Run \"git am --abort\" in REPO-DIR and show the result."
  (interactive (list (read-directory-name "Git repository directory: ")))
  (patchwork-git--run-am-subcommand repo-dir "--abort"))

(defun patchwork-apply-series-as-commits (server series-id repo-dir)
  "Apply every patch in SERIES-ID on SERVER to REPO-DIR as real commits.
Unlike `patchwork-apply-series' (which uses `git apply' against the
working tree only, and never commits), this downloads each patch's
mbox and hands the whole set to a single `git am --3way' call, so
every patch becomes its own commit with its original author, date,
and commit message preserved -- exactly as if it had been applied and
committed by hand.  This is what lets the result be browsed
afterwards with `vc'/magit/etc., all of which need real commits to
walk.  `--3way' is what lets a failing patch fall back to a real
merge with conflict markers to resolve, rather than simply refusing
to apply.

Before applying, if the series' project has a branch-apply strategy
configured (see `patchwork-project-branch-strategy'), a new branch is
created and checked out in REPO-DIR for this series first, so
BEFORE-REV/AFTER-REV (and everything applied) refer to that branch
rather than whatever was previously checked out; with no strategy
configured, the series is applied to REPO-DIR as-is.

Returns a (BEFORE-REV . AFTER-REV) cons of the HEAD commit hash
before and after applying, or nil if `git am' failed (in which case
REPO-DIR is left mid-`am' and a `*patchwork-am: ...*' buffer is shown
with `git am''s output, the patch it was blocked on, and any
conflicted files, each a button to open it directly -- resolve them
there and run `patchwork-git-am-continue', or `patchwork-git-am-abort'
to give up; both work directly from Emacs, no shell needed)."
  (interactive
   (let* ((server (patchwork-git--read-server))
          (series-id (read-number "Series id: "))
          (series (or (patchwork-db-get-series (plist-get server :url) series-id)
                      (error "No cached series %s on %s" series-id (plist-get server :url)))))
     (list server series-id
           (patchwork-project-git-tree (plist-get server :url)
                                        (plist-get series :project-slug)))))
  (let ((patches (patchwork-db-get-series-patches (plist-get server :url) series-id))
        (series (or (patchwork-db-get-series (plist-get server :url) series-id)
                    (error "No cached series %s on %s" series-id (plist-get server :url)))))
    (unless patches
      (error "No cached patches for series %s" series-id))
    (patchwork-git--maybe-checkout-branch (plist-get server :url)
                                          (plist-get series :project-slug)
                                          series-id repo-dir)
    (let ((before-rev (patchwork-git--head-rev repo-dir))
          (patch-files (mapcar (lambda (patch)
                                  (patchwork-git-download-patch server (plist-get patch :id)))
                                patches))
          (am-buffer (generate-new-buffer " *patchwork-am-output*")))
      (message "Applying series %s to %s via git am..." series-id repo-dir)
      (unwind-protect
          (let ((exit-code (apply #'call-process "git" nil am-buffer nil
                                   "-C" repo-dir "am" "--3way" patch-files))
                (output (with-current-buffer am-buffer (buffer-string))))
            (if (zerop exit-code)
                (let ((after-rev (patchwork-git--head-rev repo-dir)))
                  (message "Series %s applied to %s as %d commit(s)."
                           series-id repo-dir (length patches))
                  (cons before-rev after-rev))
              (patchwork-git--show-am-status repo-dir output)
              (message "git am failed applying series %s in %s; see %s."
                       series-id repo-dir (patchwork-git--am-buffer-name repo-dir))
              nil))
        (kill-buffer am-buffer)))))

(defvar patchwork-review-backends
  '(patchwork-review-backend-magit
    patchwork-review-backend-vc)
  "Functions tried, in order, to open a browsable view of a series' just-
applied commits, via `run-hook-with-args-until-success'.  Each is
called with (REPO-DIR BEFORE-REV AFTER-REV) -- the git repository and
the HEAD commit hashes from just before and just after applying, as
returned by `patchwork-apply-series-as-commits' -- and should return
non-nil if it opened a view, or nil to let the next backend try.

Add your own function to the front of this list to prefer a different
review tool (e.g. code-review.el, forge); the built-in backends only
depend on `magit' and Emacs's own `vc'/`log-view', so at least one of
them works with no extra setup.  Note that `git-timemachine' walks a
single file's revision history rather than a range of commits across
a whole tree, so it doesn't fit this per-series-range hook -- once a
series is applied, it can still be used as normal on any file touched
by it (\\[git-timemachine] in that file's buffer).")

(defun patchwork-review-backend-magit (repo-dir before-rev after-rev)
  "Open magit's log buffer for BEFORE-REV..AFTER-REV in REPO-DIR.
Returns non-nil (having opened the buffer) if magit is loaded, nil
otherwise so the next backend in `patchwork-review-backends' can try.

Uses `magit-log-setup-buffer' (the function every interactive
magit-log-* command funnels into) directly with an explicit revision
range, rather than any single interactive command, since none of
magit's own log commands take a range as a plain argument the way
this needs."
  (when (fboundp 'magit-log-setup-buffer)
    (let ((default-directory (file-name-as-directory repo-dir)))
      (funcall #'magit-log-setup-buffer
               (list (format "%s..%s" before-rev after-rev)) nil nil)
      t)))

(defun patchwork-review-backend-vc (repo-dir before-rev after-rev)
  "Open a `log-view-mode' buffer over the commits from BEFORE-REV to
AFTER-REV in REPO-DIR, via `vc-print-log-internal'.  This only needs
Emacs's built-in `vc', so it works with no extra packages; its
`log-view-mode' buffer supports the usual n/p to move between commits
and RET to view a commit's diff.

Calls `vc-print-log-internal' directly with the Git backend rather
than going through `vc-print-root-log'/`vc-deduce-backend', which
infer the backend from the *current* buffer's major mode or
`vc-mode' and so fail outside a buffer already visiting a file under
version control -- exactly the case right after applying a series to
a tree with no such buffer open yet."
  (when (fboundp 'vc-print-log-internal)
    (let* ((default-directory (file-name-as-directory repo-dir))
           (count (with-temp-buffer
                    (call-process "git" nil t nil "rev-list" "--count"
                                  (format "%s..%s" before-rev after-rev))
                    (max 1 (string-to-number (string-trim (buffer-string)))))))
      (vc-print-log-internal 'Git (list repo-dir) after-rev nil count)
      t)))

(defun patchwork-review-series (server series-id)
  "Apply every patch in SERIES-ID on SERVER as commits, to its project's
configured git tree (see `patchwork-project-git-tree'), then open a
browsable view of the newly applied commits via
`patchwork-review-backends'."
  (interactive
   (list (patchwork-git--read-server) (read-number "Series id: ")))
  (let* ((server-url (plist-get server :url))
         (series (or (patchwork-db-get-series server-url series-id)
                     (error "No cached series %s on %s" series-id server-url)))
         (repo-dir (patchwork-project-git-tree server-url (plist-get series :project-slug)))
         (revs (patchwork-apply-series-as-commits server series-id repo-dir)))
    (when revs
      (unless (run-hook-with-args-until-success
               'patchwork-review-backends repo-dir (car revs) (cdr revs))
        (message "Series %s applied to %s; no backend in `patchwork-review-backends' could open a view."
                 series-id repo-dir)))))

(defun patchwork-undo-patch (server patch-id repo-dir)
  "Reverse-apply the cached patch file for PATCH-ID on SERVER against REPO-DIR."
  (interactive
   (list (patchwork-git--read-server)
         (read-number "Patch id: ")
         (read-directory-name "Git repository directory: ")))
  (let ((patch-file (patchwork-git--patch-file server patch-id)))
    (unless (file-exists-p patch-file)
      (error "No cached patch file for %s; apply it first" patch-id))
    (let ((exit-code (call-process "git" nil nil nil "-C" repo-dir "apply" "-R" patch-file)))
      (if (zerop exit-code)
          (message "Patch %s undone." patch-id)
        (message "Failed to undo patch %s (exit code %d)." patch-id exit-code))
      (zerop exit-code))))

(provide 'patchwel-git)

;;; patchwel-git.el ends here
