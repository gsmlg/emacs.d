;;; mu4e-proc.el -- part of mu4e, the mu mail user agent -*- lexical-binding: t -*-

;; Copyright (C) 2011-2020 Dirk-Jan C. Binnema

;; Author: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
;; Maintainer: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'mu4e-vars)
(require 'mu4e-utils)
(require 'mu4e-meta)

;;; Internal vars

(defvar mu4e~proc-buf nil
  "Buffer (string) for data received from the backend.")
(defconst mu4e~proc-name " *mu4e-proc*"
  "Name of the server process, buffer.")
(defvar mu4e~proc-process nil
  "The mu-server process.")

;; dealing with the length cookie that precedes expressions
(defconst mu4e~cookie-pre "\376"
  "Each expression starts with a length cookie:
<`mu4e~cookie-pre'><length-in-hex><`mu4e~cookie-post'>.")
(defconst mu4e~cookie-post "\377"
  "Each expression starts with a length cookie:
<`mu4e~cookie-pre'><length-in-hex><`mu4e~cookie-post'>.")
(defconst mu4e~cookie-matcher-rx
  (concat mu4e~cookie-pre "\\([[:xdigit:]]+\\)" mu4e~cookie-post)
  "Regular expression matching the length cookie.
Match 1 will be the length (in hex).")

;;; Functions

(defun mu4e~proc-running-p  ()
  "Whether the mu process is running."
  (and mu4e~proc-process
       (memq (process-status mu4e~proc-process)
             '(run open listen connect stop))
       t))

(defsubst mu4e~proc-eat-sexp-from-buf ()
  "'Eat' the next s-expression from `mu4e~proc-buf'.
Note: this is a string, not an emacs-buffer. `mu4e~proc-buf gets
its contents from the mu-servers in the following form:
   <`mu4e~cookie-pre'><length-in-hex><`mu4e~cookie-post'>
Function returns this sexp, or nil if there was none.
`mu4e~proc-buf' is updated as well, with all processed sexp data
removed."
  (ignore-errors ;; the server may die in the middle...
    ;; mu4e~cookie-matcher-rx:
    ;;  (concat mu4e~cookie-pre "\\([[:xdigit:]]+\\)]" mu4e~cookie-post)
    (let ((b (string-match mu4e~cookie-matcher-rx mu4e~proc-buf))
          (sexp-len) (objcons))
      (when b
        (setq sexp-len (string-to-number (match-string 1 mu4e~proc-buf) 16))
        ;; does mu4e~proc-buf contain the full sexp?
        (when (>= (length mu4e~proc-buf) (+ sexp-len (match-end 0)))
          ;; clear-up start
          (setq mu4e~proc-buf (substring mu4e~proc-buf (match-end 0)))
          ;; note: we read the input in binary mode -- here, we take the part
          ;; that is the sexp, and convert that to utf-8, before we interpret
          ;; it.
          (setq objcons (read-from-string
                         (decode-coding-string
                          (substring mu4e~proc-buf 0 sexp-len)
                          'utf-8 t)))
          (when objcons
            (setq mu4e~proc-buf (substring mu4e~proc-buf sexp-len))
            (car objcons)))))))


(defun mu4e~proc-filter (_proc str)
  "Filter string STR from PROC.
This processes the 'mu server' output. It accumulates the
strings into valid sexps by checking of the ';;eox' `end-of-sexp'
marker, and then evaluating them.

The server output is as follows:

   1. an error
      (:error 2 :message \"unknown command\")
      ;; eox
   => passed to `mu4e-error-func'.

   2a. a message sexp looks something like:
 \(
  :docid 1585
  :from ((\"Donald Duck\" . \"donald@example.com\"))
  :to ((\"Mickey Mouse\" . \"mickey@example.com\"))
  :subject \"Wicked stuff\"
  :date (20023 26572 0)
  :size 15165
  :references (\"200208121222.g7CCMdb80690@msg.id\")
  :in-reply-to \"200208121222.g7CCMdb80690@msg.id\"
  :message-id \"foobar32423847ef23@pluto.net\"
  :maildir: \"/archive\"
  :path \"/home/mickey/Maildir/inbox/cur/1312254065_3.32282.pluto,4cd5bd4e9:2,\"
  :priority high
  :flags (new unread)
  :attachments ((2 \"hello.jpg\" \"image/jpeg\") (3 \"laah.mp3\" \"audio/mp3\"))
  :body-txt \" <message body>\"
\)
;; eox
   => this will be passed to `mu4e-header-func'.

  2b. After the list of message sexps has been returned (see 2a.),
  we'll receive a sexp that looks like
  (:found <n>) with n the number of messages found. The <n> will be
  passed to `mu4e-found-func'.

  3. a view looks like:
  (:view <msg-sexp>)
  => the <msg-sexp> (see 2.) will be passed to `mu4e-view-func'.

  4. a database update looks like:
  (:update <msg-sexp> :move <nil-or-t>)

   => the <msg-sexp> (see 2.) will be passed to
   `mu4e-update-func', :move tells us whether this is a move to
   another maildir, or merely a flag change.

  5. a remove looks like:
  (:remove <docid>)
  => the docid will be passed to `mu4e-remove-func'

  6. a compose looks like:
  (:compose <reply|forward|edit|new> [:original<msg-sexp>] [:include <attach>])
  `mu4e-compose-func'."
  (mu4e-log 'misc "* Received %d byte(s)" (length str))
  (setq mu4e~proc-buf (concat mu4e~proc-buf str)) ;; update our buffer
  (let ((sexp (mu4e~proc-eat-sexp-from-buf)))
    (with-local-quit
      (while sexp
        (mu4e-log 'from-server "%S" sexp)
        (cond
         ;; a header plist can be recognized by the existence of a :date field
         ((plist-get sexp :date)
          (funcall mu4e-header-func sexp))

         ;; the found sexp, we receive after getting all the headers
         ((plist-get sexp :found)
          (funcall mu4e-found-func (plist-get sexp :found)))

         ;; viewing a specific message
         ((plist-get sexp :view)
          (funcall mu4e-view-func (plist-get sexp :view)))

         ;; receive an erase message
         ((plist-get sexp :erase)
          (funcall mu4e-erase-func))

         ;; receive a :sent message
         ((plist-get sexp :sent)
          (funcall mu4e-sent-func
                   (plist-get sexp :docid)
                   (plist-get sexp :path)))

         ;; received a pong message
         ((plist-get sexp :pong)
          (funcall mu4e-pong-func sexp))

         ;; received a contacts message
         ;; note: we use 'member', to match (:contacts nil)
         ((plist-member sexp :contacts)
          (funcall mu4e-contacts-func
                   (plist-get sexp :contacts)
                   (plist-get sexp :tstamp)))

         ;; something got moved/flags changed
         ((plist-get sexp :update)
          (funcall mu4e-update-func
                   (plist-get sexp :update)
                   (plist-get sexp :move)
                   (plist-get sexp :maybe-view)))

         ;; a message got removed
         ((plist-get sexp :remove)
          (funcall mu4e-remove-func (plist-get sexp :remove)))

         ;; start composing a new message
         ((plist-get sexp :compose)
          (funcall mu4e-compose-func
                   (plist-get sexp :compose)
                   (plist-get sexp :original)
                   (plist-get sexp :include)))

         ;; do something with a temporary file
         ((plist-get sexp :temp)
          (funcall mu4e-temp-func
                   (plist-get sexp :temp)   ;; name of the temp file
                   (plist-get sexp :what)   ;; what to do with it
                   ;; (pipe|emacs|open-with...)
                   (plist-get sexp :docid)  ;; docid of the message
                   (plist-get sexp :param)));; parameter for the action

         ;; get some info
         ((plist-get sexp :info)
          (funcall mu4e-info-func sexp))

         ;; receive an error
         ((plist-get sexp :error)
          (funcall mu4e-error-func
                   (plist-get sexp :error)
                   (plist-get sexp :message)))

         (t (mu4e-message "Unexpected data from server [%S]" sexp)))

        (setq sexp (mu4e~proc-eat-sexp-from-buf))))))

(defun mu4e~escape (str)
  "Escape string STR for transport.
Put it in quotes, and escape existing quotation. In particular,
backslashes and double-quotes."
  (let ((esc (replace-regexp-in-string "\\\\" "\\\\\\\\" str)))
    (format "\"%s\"" (replace-regexp-in-string "\"" "\\\\\"" esc))))

(defun mu4e~proc-start ()
  "Start the mu server process."
  (unless (and mu4e-mu-binary (file-executable-p mu4e-mu-binary))
    (mu4e-error
     (format
      "`mu4e-mu-binary' (%S) not found; please set to the path to the mu executable"
      mu4e-mu-binary)))
  (let* ((process-connection-type nil) ;; use a pipe
         (args (when mu4e-mu-home `(,(format"--muhome=%s" mu4e-mu-home))))
         (args (cons "server" args)))
    (setq mu4e~proc-buf "")
    (setq mu4e~proc-process (apply 'start-process
                                   mu4e~proc-name mu4e~proc-name
                                   mu4e-mu-binary args))
    ;; register a function for (:info ...) sexps
    (unless mu4e~proc-process
      (mu4e-error "Failed to start the mu4e backend"))
    (set-process-query-on-exit-flag mu4e~proc-process nil)
    (set-process-coding-system mu4e~proc-process 'binary 'utf-8-unix)
    (set-process-filter mu4e~proc-process 'mu4e~proc-filter)
    (set-process-sentinel mu4e~proc-process 'mu4e~proc-sentinel)))

(defun mu4e~proc-kill ()
  "Kill the mu server process."
  (let* ((buf (get-buffer mu4e~proc-name))
         (proc (and (buffer-live-p buf) (get-buffer-process buf))))
    (when proc
      (let ((delete-exited-processes t))
        (mu4e~call-mu '(quit)))
      ;; try sending SIGINT (C-c) to process, so it can exit gracefully
      (ignore-errors
        (signal-process proc 'SIGINT))))
  (setq
   mu4e~proc-process nil
   mu4e~proc-buf nil))

;; error codes are defined in src/mu-util
;;(defconst mu4e-xapian-empty 19 "Error code: xapian is empty/non-existent")

(defun mu4e~proc-sentinel (proc _msg)
  "Function called when the server process PROC terminates with MSG."
  (let ((status (process-status proc)) (code (process-exit-status proc)))
    (setq mu4e~proc-process nil)
    (setq mu4e~proc-buf "") ;; clear any half-received sexps
    (cond
     ((eq status 'signal)
      (cond
       ((or(eq code 9) (eq code 2)) (message nil))
       ;;(message "the mu server process has been stopped"))
       (t (error (format "mu server process received signal %d" code)))))
     ((eq status 'exit)
      (cond
       ((eq code 0)
        (message nil)) ;; don't do anything
       ((eq code 19)
        (error "Database is locked by another process"))
       (t (error "Mu server process ended with exit code %d" code))))
     (t
      (error "Something bad happened to the mu server process")))))

(defun mu4e~call-mu (form)
  "Call 'mu' with some command."
  (unless (mu4e~proc-running-p) (mu4e~proc-start))
  (let* ((print-length nil) (print-level nil)
         (cmd (format "%S" form)))
    (mu4e-log 'to-server "%s" cmd)
    (process-send-string mu4e~proc-process (concat cmd "\n"))))

(defun mu4e~docid-msgid-param (docid-or-msgid)
  "Construct a backend parameter based on DOCID-OR-MSGID."
  (if (stringp docid-or-msgid)
      `(:msgid ,(mu4e~escape docid-or-msgid))
    `(:docid ,docid-or-msgid)))

(defun mu4e~proc-add (path)
  "Add the message at PATH to the database.
On success, we receive `'(:info add :path <path> :docid <docid>)'
as well as `'(:update <msg-sexp>)`'; otherwise, we receive an error."
  (mu4e~call-mu `(add :path ,path)))

(defun mu4e~proc-compose (type decrypt &optional docid)
  "Compose a message of TYPE, DECRYPT it and use DOCID.
TYPE is a symbol, either `forward', `reply', `edit', `resend' or
`new', based on an original message (ie, replying to, forwarding,
editing, resending) with DOCID or nil for type `new'.

The result is delivered to the function registered as
`mu4e-compose-func'."
  (mu4e~call-mu `(compose
                  :type ,type
                  :decrypt ,(and decrypt t)
                  :docid   ,docid)))

(defun mu4e~proc-contacts (personal after tstamp)
  "Ask for contacts with PERSONAL AFTER TSTAMP.
S-expression (:contacts (<list>) :tstamp \"<tstamp>\") is expected in
response. If PERSONAL is non-nil, only get personal contacts, if
AFTER is non-nil, get only contacts seen AFTER (the time_t
value)."
  (mu4e~call-mu `(contacts
                  :personal ,(and personal t)
                  :after    ,(or after nil)
                  :tstamp   ,(or tstamp nil))))

(defun mu4e~proc-extract (action docid index decrypt
                                 &optional path what param)
  "Perform ACTION  on part with DOCID INDEX DECRYPT PATH WHAT PARAM.
Use a message with DOCID and perform ACTION on it (as symbol,
either `save', `open', `temp') which mean: * save: save the part
to PATH (a path) (non-optional for save)$ * open: open the part
with the default application registered for doing so * temp: save
to a temporary file, then respond with
       (:temp <path> :what <what> :param <param>)."
  (mu4e~call-mu `(extract
                  :action ,action
                  :docid ,docid
                  :index ,index
                  :decrypt ,(and decrypt t)
                  :path ,path
                  :what ,what
                  :param ,param)))

(defun mu4e~proc-find (query threads sortfield sortdir maxnum skip-dups
                             include-related)
  "Run QUERY with THREADS SORTFIELD SORTDIR MAXNUM SKIP-DUPS INCLUDE-RELATED.
If THREADS is non-nil, show results in threaded fashion, SORTFIELD
is a symbol describing the field to sort by (or nil); see
`mu4e~headers-sortfield-choices'. If SORT is `descending', sort
Z->A, if it's `ascending', sort A->Z. MAXNUM determines the
maximum number of results to return, or nil for 'unlimited'. If
SKIP-DUPS is non-nil, show only one of duplicate messages (see
`mu4e-headers-skip-duplicates'). If INCLUDE-RELATED is non-nil,
include messages related to the messages matching the search
query (see `mu4e-headers-include-related').

For each result found, a function is called, depending on the
kind of result. The variables `mu4e-error-func' contain the
function that will be called for, resp., a message (header row)
or an error."
  (mu4e~call-mu `(find
                  :query ,query
                  :threads ,threads
                  :sortfield ,sortfield
                  :descending ,(if (eq sortdir 'descending) t nil)
                  :maxnum ,maxnum
                  :skip-dups ,skip-dups
                  :include-related ,include-related)))

(defun mu4e~proc-index (&optional cleanup lazy-check)
  "Index messages with possible CLEANUP and LAZY-CHECK."
  (mu4e~call-mu `(index :cleanup ,cleanup :lazy-check ,lazy-check)))

(defun mu4e~proc-mkdir (path)
  "Create a new maildir-directory at filesystem PATH."
  ;;(mu4e~proc-send-command "cmd:mkdir path:%s"  (mu4e~escape path))
  (mu4e~call-mu `(mkdir :path ,path)))


(defun mu4e~proc-move (docid-or-msgid &optional maildir flags no-view)
  "Move message identified by DOCID-OR-MSGID.
Optionally to MAILDIR and optionally setting FLAGS. If MAILDIR is
nil, message will be moved within the same maildir.

At least one of MAILDIR and FLAGS must be specified. Note that
even when MAILDIR is nil, this is still a filesystem move, since
a change in flags implies a change in message filename.

MAILDIR must be a maildir, that is, the part _without_ cur/ or new/
or the root-maildir-prefix. E.g. \"/archive\". This directory must
already exist.

The FLAGS parameter can have the following forms:
  1. a list of flags such as '(passed replied seen)
  2. a string containing the one-char versions of the flags, e.g. \"PRS\"
  3. a delta-string specifying the changes with +/- and the one-char flags,
     e.g. \"+S-N\" to set Seen and remove New.

The flags are any of `deleted', `flagged', `new', `passed', `replied' `seen' or
`trashed', or the corresponding \"DFNPRST\" as defined in [1]. See
`mu4e-string-to-flags' and `mu4e-flags-to-string'.
The server reports the results for the operation through
`mu4e-update-func'.

If the variable `mu4e-change-filenames-when-moving' is
non-nil, moving to a different maildir generates new names forq
the target files; this helps certain tools (such as mbsync).

If NO-VIEW is non-nil, don't update the view.

Returns either (:update ... ) or (:error ) sexp, which are handled my
`mu4e-update-func' and `mu4e-error-func', respectively."
  (unless (or maildir flags)
    (mu4e-error "At least one of maildir and flags must be specified"))
  (unless (or (not maildir)
              (file-exists-p (concat (mu4e-root-maildir) "/" maildir "/")))
    (mu4e-error "Target dir does not exist"))
  (mu4e~call-mu `(move
                  :docid ,(if (stringp docid-or-msgid) nil docid-or-msgid)
                  :msgid ,(if (stringp docid-or-msgid) docid-or-msgid nil)
                  :flags ,(or flags nil)
                  :maildir ,(or maildir nil)
                  :rename  ,(and maildir mu4e-change-filenames-when-moving t)
                  :no-view ,(and no-view t))))

(defun mu4e~proc-ping (&optional queries)
  "Sends a ping to the mu server, expecting a (:pong ...) in response.
QUERIES is a list of queries for the number of results with read/unread status
are returned in the 'pong' response."
  (mu4e~call-mu `(ping :queries ,queries)))

(defun mu4e~proc-remove (docid)
  "Remove message  with DOCID.
The results are reporter through either (:update ... )
or (:error) sexp, which are handled my `mu4e-error-func',
respectively."
  (mu4e~call-mu `(remove :docid ,docid)))

(defun mu4e~proc-sent (path)
  "Add the message at PATH to the database.

 if this works, we will receive (:info add :path <path> :docid
<docid> :fcc <path>)."
  (mu4e~call-mu `(sent :path ,path)))

(defun mu4e~proc-view (docid-or-msgid &optional images decrypt verify)
  "Get a message DOCID-OR-MSGID.
Optionally, if IMAGES is non-nil, backend will any images
attached to the message, and return them as temp files. DECRYPT and VERIFY
if necessary. The result will be delivered to the function
registered as `mu4e-view-func'."
  (mu4e~call-mu `(view
                  :docid ,(if (stringp docid-or-msgid) nil docid-or-msgid)
                  :msgid ,(if (stringp docid-or-msgid) docid-or-msgid nil)
                  :extract-images ,(if images t nil)
                  :decrypt ,(and decrypt t)
                  :verify  ,(and verify t))))

(defun mu4e~proc-view-path (path &optional images decrypt)
  "View message at PATH..
Optionally, if IMAGES is non-nil, backend will any images
attached to the message, and return them as temp files. The
result will be delivered to the function registered as
`mu4e-view-func'. Optionally DECRYPT and VERIFY."
  (mu4e~call-mu `(view
                  :path ,path
                  :extract-images ,(and images t)
                  :decrypt        ,(and decrypt t)
                  :verify         ,(and verify t))))

;;; _
(provide 'mu4e-proc)
;;; mu4e-proc.el ends here
