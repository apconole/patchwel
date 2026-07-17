;;; patchwel-api.el --- HTTP client for the Patchwork REST API -*- lexical-binding: t; -*-
;;; Code:

(require 'url)
(require 'json)
(require 'mail-parse)
(require 'patchwel-config)

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
  "Return the HTTP headers used for every request to SERVER."
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

(defun patchwork-api--request-once (server url method params)
  "Perform a single HTTP request to URL on SERVER and return (BODY . NEXT-LINK)."
  (let* ((url-request-method (upcase (symbol-name method)))
         (url-request-extra-headers (patchwork-api--headers server))
         (url-request-data
          (when (and params (not (memq method '(get))))
            (encode-coding-string (json-encode params) 'utf-8)))
         (buffer (url-retrieve-synchronously url t t patchwork-sync-timeout)))
    (unless buffer
      (error "Patchwork API request to %s timed out after %ss" url patchwork-sync-timeout))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-min))
          (unless (looking-at "HTTP/[0-9.]+ 2[0-9][0-9]")
            (error "Patchwork API error for %s: %s" url
                   (buffer-substring-no-properties (point) (line-end-position))))
          (let ((next (patchwork-api--next-link buffer)))
            (search-forward "\n\n")
            (cons (json-parse-buffer :object-type 'plist :array-type 'list
                                      :null-object nil :false-object nil)
                  next)))
      (kill-buffer buffer))))

(defun patchwork-api-request (server path &optional method params)
  "Make a request to PATH on SERVER using METHOD and PARAMS.
SERVER is a plist as found in `patchwork-servers'.  METHOD defaults to
`get'.  For GET requests, PARAMS is encoded as a query string; for
other methods it is sent as a JSON body.  When the response is a
paginated list, all pages are fetched and concatenated."
  (let* ((method (or method 'get))
         (url (concat (plist-get server :url) path
                       (if (eq method 'get) (patchwork-api--build-query params) ""))))
    (let ((result (patchwork-api--request-once server url method params))
          (accumulated nil))
      (push (car result) accumulated)
      (let ((next (cdr result)))
        (while next
          (let ((page (patchwork-api--request-once server next 'get nil)))
            (push (car page) accumulated)
            (setq next (cdr page)))))
      (setq accumulated (nreverse accumulated))
      (if (and (cdr accumulated) (listp (car accumulated)))
          (apply #'append accumulated)
        (car accumulated)))))

(defun patchwork-api-fetch-raw (server url)
  "Fetch URL on SERVER and return its response body as a plain string (not JSON)."
  (let* ((url-request-extra-headers (patchwork-api--headers server))
         (buffer (url-retrieve-synchronously url t t patchwork-sync-timeout)))
    (unless buffer
      (error "Request to %s timed out after %ss" url patchwork-sync-timeout))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-min))
          (unless (looking-at "HTTP/[0-9.]+ 2[0-9][0-9]")
            (error "Error fetching %s: %s" url
                   (buffer-substring-no-properties (point) (line-end-position))))
          (search-forward "\n\n")
          (buffer-substring-no-properties (point) (point-max)))
      (kill-buffer buffer))))

(defun patchwork-api-list-series (server &optional project params)
  "List series on SERVER, optionally scoped to PROJECT, with extra query PARAMS."
  (patchwork-api-request
   server "/series/" 'get
   (append (when project `(("project" . ,project)))
           `(("per_page" . ,patchwork-page-size))
           params)))

(defun patchwork-api-get-series (server series-id)
  "Get detailed information for SERIES-ID on SERVER."
  (patchwork-api-request server (format "/series/%s/" series-id) 'get))

(defun patchwork-api-list-patches (server &optional project params)
  "List patches on SERVER, optionally scoped to PROJECT, with extra query PARAMS."
  (patchwork-api-request
   server "/patches/" 'get
   (append (when project `(("project" . ,project)))
           `(("per_page" . ,patchwork-page-size))
           params)))

(defun patchwork-api-get-patch (server patch-id)
  "Get detailed information for PATCH-ID on SERVER."
  (patchwork-api-request server (format "/patches/%s/" patch-id) 'get))

(defun patchwork-api-list-comments (server patch-id)
  "List comments for PATCH-ID on SERVER."
  (patchwork-api-request server (format "/patches/%s/comments/" patch-id) 'get))

(defun patchwork-api-list-checks (server patch-id)
  "List CI checks for PATCH-ID on SERVER."
  (patchwork-api-request server (format "/patches/%s/checks/" patch-id) 'get))

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
           params)))

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
