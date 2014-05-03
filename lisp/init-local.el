;; init-local local settings

;; set indent level for all mode
(setq preferred-indent-level 4)

(setq js2-basic-offset preferred-indent-level
      js-indent-level preferred-indent-level
      coffee-tab-width preferred-indent-level
      sgml-basic-offset preferred-indent-level
      sh-basic-offset preferred-indent-level
      c-basic-offset preferred-indent-level
      ruby-indent-level preferred-indent-level
      css-indent-offset preferred-indent-level
      )

;; add w3m support
(require-package 'w3m)

;; set evernote-mode not work now
;;TODO make it work
;;(require-package 'evernote-mode)
;; (setq evernote-developer-token "")

(provide 'init-local)
