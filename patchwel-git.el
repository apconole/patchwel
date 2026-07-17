;;; patchwel-git.el --- Apply Patchwork patches/series to a git tree -*- lexical-binding: t; -*-
;;; Code:

(require 'patchwel-config)
(require 'patchwel-db)
(require 'patchwel-api)

(defconst patchwork-git-temp-dir
  (expand-file-name "~/.cache/patchwel/patchwork-patches")
  "Directory where downloaded patch mbox files are cached.")

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
ARGS defaults to (\"--3way\" \"--reject\").  Returns non-nil on success."
  (interactive
   (list (patchwork-git--read-server)
         (read-number "Patch id: ")
         (read-directory-name "Git repository directory: ")))
  (let* ((args (or args '("--3way" "--reject")))
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
Returns non-nil if every patch applied successfully."
  (interactive
   (let ((server (patchwork-git--read-server)))
     (list server
           (read-number "Series id: ")
           (read-directory-name "Git repository directory: "))))
  (let ((patches (patchwork-db-get-series-patches (plist-get server :url) series-id))
        (success t))
    (dolist (patch patches)
      (unless (patchwork-apply-patch server (plist-get patch :id) repo-dir)
        (setq success nil)))
    (if success
        (message "Series %s applied successfully." series-id)
      (message "Series %s had one or more patches that failed to apply." series-id))
    success))

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
