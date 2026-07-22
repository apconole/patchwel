;;; patchwel-mail.el --- Reply to Patchwork comments as mail -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'message)
(require 'patchwel-db)
(require 'patchwel-api)

(defvar patchwork-mail-compose-hook nil
  "Hook run with no arguments once `patchwork-mail-reply-to-comment'/
`-to-patch' have finished composing a reply, in that message-mode
buffer (current buffer), after the quoted body is inserted.

Patchwel composes replies via plain `message-mail' rather than
through Gnus's own summary-reply commands, so Gnus-specific context
such a buffer would normally have (`gnus-newsgroup-name',
`message-reply-headers') is never set -- meaning any sent-mail
archiving that depends on it (a Gcc: header inserted by
`gnus-configure-posting-style' matching on group/reply context, for
instance) may not fire the way it would for a reply you sent directly
from Gnus, even though the very same `message-mode-hook' still runs.
Use this hook to insert whatever your setup actually needs, e.g. a
Gcc header matching a monthly-rotating archive group (a common Gnus
convention, e.g. \"nnfolder+archive:sent.2026-07\" for this month --
adjust the group name to match your own archive method/naming):

  (add-hook \\='patchwork-mail-compose-hook
            (lambda ()
              (message-add-header
               (format-time-string \"Gcc: nnfolder+archive:sent.%Y-%m\"))))

or to set the Gnus context posting-style matching depends on before
asking Gnus to (re-)apply it:

  (add-hook \\='patchwork-mail-compose-hook
            (lambda ()
              (setq gnus-newsgroup-name \"nntp+news.example.com:some.group\")
              (gnus-configure-posting-style)))

Empty by default -- patchwel doesn't guess at Gcc/Fcc conventions it
can't verify.")

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

(defun patchwork-mail--compose (to subject other-headers)
  "Compose a message-mode buffer for TO/SUBJECT/OTHER-HEADERS, preferring
`gnus-msg-mail' over plain `message-mail' when Gnus is already running
in this session.  `gnus-msg-mail' is, per its own docstring, \"Like
`message-mail', but with Gnus paraphernalia, particularly the Gcc:
header for archiving purposes\" -- it wraps the same `message-mail'
call in `gnus-setup-message', which is what actually wires up Gcc
processing at send time; a Gcc header alone (e.g. one added via
`patchwork-mail-compose-hook') does nothing without it, since nothing
would be watching for it.  Falls back to plain `message-mail' when
Gnus isn't running (`gnus-msg-mail' has this same fallback built in,
but the fboundp/featurep checks here avoid ever loading Gnus just to
ask -- no behavior or cost change for anyone not already using it)."
  (if (and (featurep 'gnus) (fboundp 'gnus-msg-mail) (gnus-alive-p))
      (gnus-msg-mail to subject other-headers)
    (message-mail to subject other-headers)))

(defun patchwork-mail-reply-to-comment (comment)
  "Compose a wide-reply `message-mode' buffer replying to COMMENT.
COMMENT is a plist as returned by `patchwork-db-get-comment'.  To/Cc,
In-Reply-To, and References are populated the same way a Gnus wide
reply to the original email would, and COMMENT's content is inserted
quoted one level deeper than it already was.  Composes via
`patchwork-mail--compose', which prefers Gnus's own `gnus-msg-mail'
(sets up Gcc/archiving properly) when Gnus is running.  Runs
`patchwork-mail-compose-hook' once done, in case your setup needs to
add a Gcc/Fcc header or similar for sent-mail archiving."
  (let* ((recipients (patchwork-mail--wide-reply-recipients comment))
         (to (car recipients))
         (cc (cdr recipients))
         (subject (patchwork-mail--reply-subject (plist-get comment :subject)))
         (in-reply-to (plist-get comment :msgid))
         (references (patchwork-mail--reply-references comment))
         (other-headers (delq nil
                               (list (and (not (string-empty-p cc)) (cons "Cc" cc))))))
    (patchwork-mail--compose to subject other-headers)
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
      (insert "\n"))
    (run-hooks 'patchwork-mail-compose-hook)))

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
