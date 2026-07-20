;;; test-helpers.el --- Shared ERT fixtures for patchwel's test suite -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'json)
(require 'url)
(require 'cl-lib)
(require 'patchwel)

;; -- mock Patchwork server ---------------------------------------------

(defconst patchwork-test--server-script
  (expand-file-name "mock_patchwork_server.py"
                     (file-name-directory (or load-file-name buffer-file-name)))
  "Path to the mock Patchwork server script.")

(defvar patchwork-test--mock-server-process nil
  "The mock server process for the current batch Emacs run, or nil.
Memoized per test *file* (one Emacs batch process per file) rather
than per `ert-deftest', since spawning the Python process is the
expensive part; use `patchwork-test-with-mock-server' for per-test
state isolation via a `/_control/reset' call instead.")

(defvar patchwork-test--mock-server-port nil
  "Port the current mock server process is listening on, or nil.")

(defun patchwork-test--kill-mock-server ()
  "Kill the memoized mock server process, if any."
  (when (and patchwork-test--mock-server-process
             (process-live-p patchwork-test--mock-server-process))
    (delete-process patchwork-test--mock-server-process)))

(defun patchwork-test--ensure-mock-server ()
  "Start the mock server if not already running for this Emacs process,
and return its port."
  (unless (and patchwork-test--mock-server-process
               (process-live-p patchwork-test--mock-server-process))
    (let ((buf (generate-new-buffer " *patchwork-mock-server*")))
      (setq patchwork-test--mock-server-process
            (start-process "patchwork-mock-server" buf
                            "python3" patchwork-test--server-script))
      (let ((deadline (+ (float-time) 10))
            (port nil))
        (while (and (not port) (< (float-time) deadline))
          (with-current-buffer buf
            (goto-char (point-min))
            (when (re-search-forward "^PORT=\\([0-9]+\\)" nil t)
              (setq port (string-to-number (match-string 1)))))
          (unless port (accept-process-output nil 0.05)))
        (unless port
          (error "Mock Patchwork server did not report a port within 10s"))
        (setq patchwork-test--mock-server-port port))
      (add-hook 'kill-emacs-hook #'patchwork-test--kill-mock-server)))
  patchwork-test--mock-server-port)

(defmacro patchwork-test-with-mock-server (port-var &rest body)
  "Ensure the mock server is running, bind PORT-VAR to its port, call
`/_control/reset' on it (so each use of this macro starts from a known
state, standing in for ERT's lack of native per-test fixtures), then
run BODY."
  (declare (indent 1))
  `(let ((,port-var (patchwork-test--ensure-mock-server)))
     (patchwork-test-control ,port-var "reset")
     ,@body))

(defun patchwork-test-mock-server-url (port)
  "Return the base API URL (with /api suffix) for the mock server on PORT."
  (format "http://127.0.0.1:%d/api" port))

(defun patchwork-test-server-plist (port &rest overrides)
  "Return a `patchwork-servers'-shaped plist pointed at the mock server
on PORT, with OVERRIDES (a plist) taking precedence over the defaults."
  (append overrides
          (list :url (patchwork-test-mock-server-url port) :token nil :projects nil)))

(defun patchwork-test--http (method url &optional payload)
  "Perform a synchronous METHOD request to URL, JSON-encoding PAYLOAD (an
alist) as the body if given, and return (STATUS . BODY-STRING)."
  (let* ((url-request-method method)
         (url-request-extra-headers (when payload '(("Content-Type" . "application/json"))))
         (url-request-data (when payload (encode-coding-string (json-encode payload) 'utf-8)))
         (buffer (url-retrieve-synchronously url t t 10)))
    (unless buffer (error "No response from %s" url))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-min))
          (let ((status (and (looking-at "HTTP/[0-9.]+ \\([0-9]+\\)")
                              (string-to-number (match-string 1)))))
            (search-forward "\n\n")
            (cons status (buffer-substring-no-properties (point) (point-max)))))
      (kill-buffer buffer))))

(defun patchwork-test-control (port endpoint &optional payload)
  "POST PAYLOAD (an alist) to /_control/ENDPOINT on the mock server at
PORT. Signals an error if the response isn't 200."
  (let* ((url (format "http://127.0.0.1:%d/_control/%s" port endpoint))
         (result (patchwork-test--http "POST" url (or payload '((dummy . t))))))
    (unless (eql (car result) 200)
      (error "Mock server control call %s failed: %S" endpoint result))
    (car result)))

(defun patchwork-test-control-log (port)
  "Return the mock server's request log (list of alists) via GET
/_control/log on PORT."
  (let* ((url (format "http://127.0.0.1:%d/_control/log" port))
         (result (patchwork-test--http "GET" url)))
    (json-parse-string (cdr result) :object-type 'plist :array-type 'list)))

;; -- temp sqlite db -------------------------------------------------------

(defmacro patchwork-test-with-temp-db (&rest body)
  "Bind `patchwork-local-db-file' to a fresh temp path, force-close and
nil any pre-existing `patchwork-db--connection' (a module-global cache
that a plain `let' on the file path alone does not reset), run BODY,
then unconditionally close and delete the temp db and its -wal/-shm
siblings."
  (declare (indent 0))
  `(let ((patchwork-local-db-file (make-temp-file "patchwork-test-db")))
     (setq patchwork-db--connection nil)
     (unwind-protect
         (progn ,@body)
       (ignore-errors (patchwork-db-close))
       (setq patchwork-db--connection nil)
       (dolist (suffix '("" "-wal" "-shm"))
         (ignore-errors (delete-file (concat patchwork-local-db-file suffix)))))))

;; -- temp git repo ---------------------------------------------------------

(defun patchwork-test--git (repo-dir &rest args)
  "Run git ARGS in REPO-DIR, signaling an error on non-zero exit."
  (with-temp-buffer
    (let ((exit-code (apply #'call-process "git" nil t nil "-C" repo-dir args)))
      (unless (zerop exit-code)
        (error "git %S failed (exit %d) in %s: %s"
               args exit-code repo-dir (buffer-string))))))

(defmacro patchwork-test-with-temp-repo (repo-var &rest body)
  "Create a temp git repository with a throwaway local user.name/
user.email and one initial commit (so branch-strategy BASE refs and
`git am' have something to apply onto), bind REPO-VAR to its path, run
BODY, then delete the directory."
  (declare (indent 1))
  `(let ((,repo-var (make-temp-file "patchwork-test-repo" t)))
     (unwind-protect
         (progn
           (patchwork-test--git ,repo-var "init" "-q")
           (patchwork-test--git ,repo-var "config" "user.email" "test@example.com")
           (patchwork-test--git ,repo-var "config" "user.name" "Test User")
           (write-region "base\n" nil (expand-file-name "file.txt" ,repo-var))
           (patchwork-test--git ,repo-var "add" "file.txt")
           (patchwork-test--git ,repo-var "commit" "-q" "-m" "initial commit")
           ,@body)
       (delete-directory ,repo-var t))))

(defun patchwork-test-make-patch-fixture (repo-dir file-content commit-message)
  "In REPO-DIR (from `patchwork-test-with-temp-repo'), append FILE-CONTENT
to file.txt, commit it with COMMIT-MESSAGE, and return real
`git format-patch -1 --stdout' output against its parent -- never
hand-rolled mbox text, so it round-trips through real `git am'."
  (write-region file-content nil (expand-file-name "file.txt" repo-dir) t)
  (patchwork-test--git repo-dir "commit" "-q" "-am" commit-message)
  (with-temp-buffer
    (call-process "git" nil t nil "-C" repo-dir "format-patch" "-1" "--stdout" "HEAD")
    (buffer-string)))

(defun patchwork-test-head-rev (repo-dir)
  "Return REPO-DIR's current HEAD commit hash."
  (with-temp-buffer
    (call-process "git" nil t nil "-C" repo-dir "rev-parse" "HEAD")
    (string-trim (buffer-string))))

(provide 'test-helpers)

;;; test-helpers.el ends here
