;;; patchwel-mail.el --- Reply to Patchwork comments as mail -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'message)
(require 'patchwel-db)
(require 'patchwel-api)

(defun patchwork-mail--normalize-header (value)
  "Collapse folded whitespace (newline + continuation indent) in VALUE
into single spaces, or return nil unchanged."
  (and value (replace-regexp-in-string "[ \t]*\n[ \t]*" " " value)))

(defun patchwork-mail--split-addresses (value)
  "Split header VALUE into a list of trimmed address entries."
  (when value
    (mapcar #'string-trim
            (split-string (patchwork-mail--normalize-header value) "," t "[ \t]+"))))

(defun patchwork-mail--address-matches-p (entry needle)
  "Return non-nil if address ENTRY textually contains NEEDLE."
  (and needle (not (string-empty-p needle))
       (string-match-p (regexp-quote (downcase needle)) (downcase entry))))

(defun patchwork-mail--wide-reply-recipients (comment)
  "Return (TO . CC) strings for a wide reply to COMMENT.
TO is the comment's author; CC is the original To/Cc addresses (the
rest of the thread's participants), minus the new To address and
minus `user-mail-address' so you don't Cc yourself."
  (let* ((to (or (plist-get comment :submitter-email)
                 (error "No email address available for this message's author")))
         (candidates (append (patchwork-mail--split-addresses (plist-get comment :to))
                              (patchwork-mail--split-addresses (plist-get comment :cc))))
         (cc (seq-uniq
              (seq-remove (lambda (addr)
                            (or (patchwork-mail--address-matches-p addr to)
                                (patchwork-mail--address-matches-p addr user-mail-address)))
                          candidates))))
    (cons to (string-join cc ", "))))

(defun patchwork-mail--reply-subject (subject)
  "Return SUBJECT prefixed with \"Re: \", unless already so prefixed."
  (if (and subject (string-match-p "\\`Re:" subject))
      subject
    (format "Re: %s" (or subject ""))))

(defun patchwork-mail--reply-references (comment)
  "Return the References header for a reply to COMMENT: its own
References chain (if any) plus its own Message-ID appended, so the
new message threads one level deeper."
  (let ((refs (plist-get comment :references))
        (msgid (plist-get comment :msgid)))
    (patchwork-mail--normalize-header
     (cond ((and refs msgid) (concat refs " " msgid))
           (msgid msgid)
           (t refs)))))

(defun patchwork-mail--quote-content (content)
  "Quote CONTENT one level deeper, prefixing every line with \"> \"."
  (mapconcat (lambda (line) (concat "> " line))
             (split-string content "\n")
             "\n"))

(defun patchwork-mail-reply-to-comment (comment)
  "Compose a wide-reply `message-mode' buffer replying to COMMENT.
COMMENT is a plist as returned by `patchwork-db-get-comment'.  To/Cc,
In-Reply-To, and References are populated the same way a Gnus wide
reply to the original email would, and COMMENT's content is inserted
quoted one level deeper than it already was."
  (let* ((recipients (patchwork-mail--wide-reply-recipients comment))
         (to (car recipients))
         (cc (cdr recipients))
         (subject (patchwork-mail--reply-subject (plist-get comment :subject)))
         (in-reply-to (plist-get comment :msgid))
         (references (patchwork-mail--reply-references comment))
         (other-headers (delq nil
                               (list (and (not (string-empty-p cc)) (cons "Cc" cc))))))
    (message-mail to subject other-headers)
    ;; message-setup-1 forcibly regenerates References/In-Reply-To from
    ;; `message-reply-headers' (unset here, since we're not going through a
    ;; real Gnus reply buffer), silently dropping whatever `other-headers'
    ;; supplied for them.  Add them back afterward instead, via
    ;; `message-add-header', which only inserts a header if not already
    ;; present -- exactly what's needed since message-setup-1 just cleared it.
    (when in-reply-to
      (message-add-header (format "In-Reply-To: %s" in-reply-to)))
    (when references
      (message-add-header (format "References: %s" references)))
    (message-goto-body)
    (save-excursion
      (insert (format "%s <%s> writes:\n\n"
                       (or (plist-get comment :author) "")
                       (or (plist-get comment :submitter-email) "")))
      (insert (patchwork-mail--quote-content (or (plist-get comment :content) "")))
      (insert "\n"))))

(defun patchwork-mail--header-value (headers key)
  "Return HEADERS's KEY as a single string, or nil if absent.
Patchwork occasionally reports a raw mail header as a list of repeated
values rather than a single string; join those with \", \" so callers
always get a plain string."
  (let ((value (plist-get headers key)))
    (cond ((null value) nil)
          ((stringp value) value)
          ((listp value) (string-join value ", "))
          (t (format "%s" value)))))

(defun patchwork-mail--patch-reply-plist (patch-json)
  "Build a reply-able plist (the shape `patchwork-mail-reply-to-comment'
expects) from a full patch detail PATCH-JSON, as returned by
`patchwork-api-get-patch'.  A patch's *list* view (used for routine
syncing, cached in the `patches' table) omits mail headers entirely;
only the single-patch *detail* endpoint has them, which is why this
works from a live fetch rather than anything cached."
  (let* ((headers (plist-get patch-json :headers))
         (submitter (plist-get patch-json :submitter)))
    (list :author (or (plist-get submitter :name) (plist-get submitter :email))
          :submitter-email (plist-get submitter :email)
          :content (or (plist-get patch-json :content) "")
          :msgid (plist-get patch-json :msgid)
          :subject (or (plist-get patch-json :name)
                       (patchwork-mail--header-value headers :Subject))
          :to (patchwork-mail--header-value headers :To)
          :cc (patchwork-mail--header-value headers :Cc)
          :references (patchwork-mail--header-value headers :References)
          :in-reply-to (patchwork-mail--header-value headers :In-Reply-To))))

(defun patchwork-mail--cached-patch-reply-plist (patch)
  "Build a reply-able plist from PATCH, a plist as returned by
`patchwork-db-get-patch' (or `patchwork-db-get-series-patches')."
  (list :author (or (plist-get patch :submitter) (plist-get patch :submitter-email))
        :submitter-email (plist-get patch :submitter-email)
        :content (or (plist-get patch :content) "")
        :msgid (plist-get patch :msgid)
        :subject (plist-get patch :name)
        :to (plist-get patch :to)
        :cc (plist-get patch :cc)
        :references (plist-get patch :references)
        :in-reply-to (plist-get patch :in-reply-to)))

(defun patchwork-mail-reply-to-patch (server patch-id)
  "Compose a wide-reply mail message to PATCH-ID on SERVER.
Prefers mail data already cached at sync time (a patch's list view has
no mail headers, so the sync layer fetches and caches its full detail
separately); falls back to a live `patchwork-api-get-patch' fetch if
the cache doesn't have it yet (an older cache from before this was
tracked, or that one sync-time detail fetch failed)."
  (let* ((cached (patchwork-db-get-patch (plist-get server :url) patch-id))
         (plist (if (and cached (plist-get cached :msgid))
                    (patchwork-mail--cached-patch-reply-plist cached)
                  (patchwork-mail--patch-reply-plist
                   (patchwork-api-get-patch server patch-id)))))
    (patchwork-mail-reply-to-comment plist)))

(provide 'patchwel-mail)

;;; patchwel-mail.el ends here
