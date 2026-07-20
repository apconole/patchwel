;;; patchwel-api.el --- HTTP client for the Patchwork REST API -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'url)
(require 'json)
(require 'mail-parse)
(require 'patchwel-config)

(define-error 'patchwork-api-error "Patchwork API error")
(define-error 'patchwork-api-http-error
  "Patchwork API returned a non-2xx HTTP status" 'patchwork-api-error)
(define-error 'patchwork-api-timeout-error
  "Patchwork API request timed out" 'patchwork-api-error)

(defun patchwork-api--build-query (params)
  "Build a query string from the PARAMS alist."
  (if (null params)
      ""
    (concat "?"
            (mapconcat
             (lambda (kv)
               (format "%s=%s"
                       (url-hexify-string (format "%s" (car kv)))
                       (url-hexify-string (format "%s" (cdr kv)))))
             params
             "&"))))

(defun patchwork-api--headers (server)
  "Return the HTTP headers used for every request to SERVER.
Deliberately does not include User-Agent: passing one via headers
here doesn't replace url.el's own auto-generated one, it gets appended
after it (\"URL/Emacs Emacs/30.2 (...),curl/8.7.1\", observed against a
header-echoing test service) -- worse than not trying at all.  The
`url-user-agent' dynamic variable is the mechanism that actually
replaces it; see callers of this function, which bind it around the
request."
  (append '(("Accept" . "application/json")
            ("Content-Type" . "application/json"))
          (when (plist-get server :token)
            `(("Authorization" . ,(format "Token %s" (plist-get server :token)))))))

(defun patchwork-api--next-link (buffer)
  "Return the \"next\" pagination URL advertised in BUFFER's Link header, if any."
  (with-current-buffer buffer
    (save-excursion
      (save-restriction
        (goto-char (point-min))
        (let ((header-end (or (and (search-forward "\n\n" nil t) (point))
                               (point-max))))
          (narrow-to-region (point-min) header-end)
          (let ((link (mail-fetch-field "Link")))
            (when (and link (string-match "<\\([^>]+\\)>; *rel=\"next\"" link))
              (match-string 1 link))))))))

(defvar patchwork-api-challenge-hook nil
  "Hook run when a Patchwork server's response looks like an anti-bot
challenge page (e.g. Anubis's JavaScript proof-of-work gate) rather
than the expected JSON.  Emacs cannot solve such a challenge on its
own; this exists purely as an extension point so a dedicated module
could plug in later.

Each function is called with (SERVER URL BUFFER), where BUFFER holds
the complete raw HTTP response (headers and body) that looked like a
challenge, and SERVER/URL identify what was being requested.  Called
via `run-hook-with-args-until-success': the first function to return a
buffer wins -- that buffer must hold a substitute HTTP response (same
raw \"status line, headers, blank line, body\" shape as BUFFER) to use
instead, e.g. after solving the challenge and re-issuing the request
some other way.  A function that can't handle this particular response
should return nil.  If no function handles it (including when this
hook is empty, its default), the original \"non-JSON content\" error is
signaled as before.")

(defun patchwork-api--looks-like-challenge-p (start)
  "Return non-nil if the current buffer doesn't look like JSON starting
at START -- i.e. this might be a non-API challenge page (e.g. Anubis)
rather than the expected response."
  (save-excursion
    (goto-char start)
    (skip-chars-forward " \t\r\n")
    (not (memq (char-after) '(?\[ ?\{)))))

(defun patchwork-api--signal-non-json-error (start)
  "Signal a clear error for the non-JSON content at START, rather than
letting `json-parse-buffer' fail with a cryptic byte-position error."
  (save-excursion
    (goto-char start)
    (skip-chars-forward " \t\r\n")
    (let ((snippet (buffer-substring-no-properties
                    (point) (min (point-max) (+ (point) 200)))))
      (error "Patchwork API returned non-JSON content%s: %s"
             (if (string-match-p "<!doctype html\\|<html" snippet)
                 " (looks like an HTML page -- possibly an anti-bot challenge such as Anubis, which Emacs cannot solve)"
               "")
             snippet))))

(defun patchwork-api--process-response (server url buffer)
  "Process BUFFER, a completed `url-retrieve' response for URL on
SERVER, into (BODY . NEXT-LINK), or signal an error.  If BUFFER looks
like a non-JSON challenge page, gives `patchwork-api-challenge-hook' a
chance to supply a substitute response to process instead before
giving up."
  (with-current-buffer buffer
    (goto-char (point-min))
    (unless (looking-at "HTTP/[0-9.]+ 2[0-9][0-9]")
      (let* ((status-line (buffer-substring-no-properties (point) (line-end-position)))
             (status (and (string-match "HTTP/[0-9.]+ \\([0-9]+\\)" status-line)
                          (string-to-number (match-string 1 status-line)))))
        (signal 'patchwork-api-http-error (list status url status-line))))
    (let ((next (patchwork-api--next-link buffer)))
      (search-forward "\n\n")
      (if (patchwork-api--looks-like-challenge-p (point))
          (let ((solved (run-hook-with-args-until-success
                         'patchwork-api-challenge-hook server url buffer)))
            (if solved
                (unwind-protect
                    (patchwork-api--process-response server url solved)
                  (unless (eq solved buffer) (kill-buffer solved)))
              (patchwork-api--signal-non-json-error (point))))
        (cons (json-parse-buffer :object-type 'plist :array-type 'list
                                  :null-object nil :false-object nil)
              next)))))

(defun patchwork-api--request-once (server url method params timeout)
  "Perform a single HTTP request to URL on SERVER and return (BODY . NEXT-LINK).
Gives up after TIMEOUT seconds."
  (let* ((url-request-method (upcase (symbol-name method)))
         (url-request-extra-headers (patchwork-api--headers server))
         (url-user-agent (patchwork-server-user-agent server))
         (url-request-data
          (when (and params (not (memq method '(get))))
            (encode-coding-string (json-encode params) 'utf-8)))
         (buffer (url-retrieve-synchronously url t t timeout)))
    (unless buffer
      (signal 'patchwork-api-timeout-error (list url timeout)))
    (unwind-protect
        (patchwork-api--process-response server url buffer)
      (kill-buffer buffer))))

(defun patchwork-api-request (server path &optional method params timeout)
  "Make a request to PATH on SERVER using METHOD and PARAMS.
SERVER is a plist as found in `patchwork-servers'.  METHOD defaults to
`get'.  For GET requests, PARAMS is encoded as a query string; for
other methods it is sent as a JSON body.  When the response is a
paginated list, all pages are fetched and concatenated.  TIMEOUT
defaults to `patchwork-sync-timeout'; pass a larger value for requests
expected to be slower (see `patchwork-detail-fetch-timeout')."
  (let* ((method (or method 'get))
         (timeout (or timeout patchwork-sync-timeout))
         (url (concat (plist-get server :url) path
                       (if (eq method 'get) (patchwork-api--build-query params) ""))))
    (let ((result (patchwork-api--request-once server url method params timeout))
          (accumulated nil))
      (push (car result) accumulated)
      (let ((next (cdr result)))
        (while next
          (let ((page (patchwork-api--request-once server next 'get nil timeout)))
            (push (car page) accumulated)
            (setq next (cdr page)))))
      (setq accumulated (nreverse accumulated))
      (if (and (cdr accumulated) (listp (car accumulated)))
          (apply #'append accumulated)
        (car accumulated)))))

(defun patchwork-api-fetch-raw (server url &optional timeout)
  "Fetch URL on SERVER and return its response body as a plain string (not JSON).
TIMEOUT defaults to `patchwork-detail-fetch-timeout', since this is
used to download a patch's full mbox."
  (let* ((timeout (or timeout patchwork-detail-fetch-timeout))
         (url-request-extra-headers (patchwork-api--headers server))
         (url-user-agent (patchwork-server-user-agent server))
         (buffer (url-retrieve-synchronously url t t timeout)))
    (unless buffer
      (signal 'patchwork-api-timeout-error (list url timeout)))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-min))
          (unless (looking-at "HTTP/[0-9.]+ 2[0-9][0-9]")
            (let* ((status-line (buffer-substring-no-properties (point) (line-end-position)))
                   (status (and (string-match "HTTP/[0-9.]+ \\([0-9]+\\)" status-line)
                                (string-to-number (match-string 1 status-line)))))
              (signal 'patchwork-api-http-error (list status url status-line))))
          (search-forward "\n\n")
          (buffer-substring-no-properties (point) (point-max)))
      (kill-buffer buffer))))

(defun patchwork-api-list-series (server &optional project params)
  "List series on SERVER, optionally scoped to PROJECT, with extra query PARAMS."
  (patchwork-api-request
   server "/series/" 'get
   (append (when project `(("project" . ,project)))
           `(("per_page" . ,patchwork-page-size))
           params)
   (patchwork-server-sync-timeout server)))

(defun patchwork-api-get-series (server series-id)
  "Get detailed information for SERIES-ID on SERVER."
  (patchwork-api-request server (format "/series/%s/" series-id) 'get nil
                          patchwork-detail-fetch-timeout))

(defun patchwork-api-get-cover (server cover-id)
  "Get detailed information for the cover letter COVER-ID on SERVER.
Like a patch, its response includes a :series field (a list of series
references) naming which series it belongs to."
  (patchwork-api-request server (format "/covers/%s/" cover-id) 'get nil
                          patchwork-detail-fetch-timeout))

(defun patchwork-api-list-patches (server &optional project params)
  "List patches on SERVER, optionally scoped to PROJECT, with extra query PARAMS."
  (patchwork-api-request
   server "/patches/" 'get
   (append (when project `(("project" . ,project)))
           `(("per_page" . ,patchwork-page-size))
           params)
   (patchwork-server-sync-timeout server)))

(defun patchwork-api-get-patch (server patch-id)
  "Get detailed information for PATCH-ID on SERVER."
  (patchwork-api-request server (format "/patches/%s/" patch-id) 'get nil
                          patchwork-detail-fetch-timeout))

(defun patchwork-api-list-comments (server patch-id)
  "List comments for PATCH-ID on SERVER."
  (patchwork-api-request server (format "/patches/%s/comments/" patch-id) 'get nil
                          (patchwork-server-sync-timeout server)))

(defun patchwork-api-list-checks (server patch-id)
  "List CI checks for PATCH-ID on SERVER."
  (patchwork-api-request server (format "/patches/%s/checks/" patch-id) 'get nil
                          (patchwork-server-sync-timeout server)))

(defun patchwork-api-list-events (server &optional project since params)
  "List events on SERVER, optionally scoped to PROJECT.
When SINCE (an ISO-8601 timestamp string) is given, only events at or
after that time are requested.  Signals an error if SERVER has no
events API (e.g. an older Patchwork instance); callers that need to
fall back gracefully should catch that with `condition-case'."
  (patchwork-api-request
   server "/events/" 'get
   (append (when project `(("project" . ,project)))
           (when since `(("since" . ,since)))
           `(("per_page" . ,patchwork-page-size))
           params)
   (patchwork-server-sync-timeout server)))

(defun patchwork-api-set-delegate (server patch-id delegate)
  "Set the assignee/DELEGATE for PATCH-ID on SERVER."
  (patchwork-api-request
   server (format "/patches/%s/" patch-id)
   'patch
   `((delegate . ,delegate))))

(defun patchwork-api-set-state (server patch-id state)
  "Set the STATE for PATCH-ID on SERVER."
  (patchwork-api-request
   server (format "/patches/%s/" patch-id)
   'patch
   `((state . ,state))))

(provide 'patchwel-api)

;;; patchwel-api.el ends here
