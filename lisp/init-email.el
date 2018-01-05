(require 'mu4e)
(require-package 'mu4e-alert)
(require-package 'mu4e-maildirs-extension)

  ;;; Set up some common mu4e variables
(setq mu4e-maildir "~/.Mail"
      mu4e-sent-folder "/zdns/Sent Messages"
      mu4e-drafts-folder "/zdns/Drafts"
      mu4e-get-mail-command "mbsync -a"
      mu4e-update-interval 300
      mu4e-compose-signature-auto-include nil
      mu4e-view-show-images t
      mu4e-view-show-addresses t)

  ;;; Mail directory shortcuts
(setq mu4e-maildir-shortcuts
      '(("/zdns/INBOX" . ?z)
        ("/qq/INBOX" . ?q)
        ("/live/INBOX" . ?l)))

  ;;; Bookmarks
(setq mu4e-bookmarks
      `(("flag:unread AND NOT flag:trashed" "Unread messages" ?u)
        ("date:today..now" "Today's messages" ?t)
        ("date:7d..now" "Last 7 days" ?w)
        ("flag:attach" "Messages with attachment" ?a)
        ("mime:image/*" "Messages with images" ?p)
        ("size:5M..500M" "Big messages" ?b)
        ("flag:flagged" "Flagged messages" ?f)
        (,(mapconcat 'identity
                     (mapcar
                      (lambda (maildir)
                        (concat "maildir:" (car maildir)))
                      mu4e-maildir-shortcuts) " OR ")
         "All inboxes" ?i)))

(setq mu4e-account-alist
      '(("zdns"
         (mu4e-sent-messages-behavior sent)
         (mu4e-sent-folder "/zdns/Sent Messages")
         (mu4e-drafts-folder "/zdns/Drafts")
         (mu4e-get-mail-command "mbsync zdns")
         (user-mail-address "gaoshiming@zdns.cn")
         (user-full-name "Gao Shi Ming"))
        ("qq"
         (mu4e-send-messages-behavior sent)
         (mu4e-sent-folder "/qq/Sent Messages")
         (mu4e-drafts-folder "/qq/Drafts")
         (mu4e-get-mail-command "mbsync qq")
         (user-mail-address "gsmlg@qq.com")
         (user-full-name "GSMLG"))
        ("live"
         (mu4e-send-messages-behavior sent)
         (mu4e-sent-folder "/live/Sent Messages")
         (mu4e-drafts-folder "/live/Drafts")
         (mu4e-get-mail-command "mbsync live")
         (user-mail-address "gaoshiming@live.com")
         (user-full-name "Gao"))))

;; use msmtp
(setq message-send-mail-function 'message-send-mail-with-sendmail)
(setq sendmail-program "/usr/local/bin/msmtp")
;; tell msmtp to choose the SMTP server according to the from field in the outgoing email
(setq message-sendmail-extra-arguments '("--read-envelope-from"))
(setq message-sendmail-f-is-evil 't)
