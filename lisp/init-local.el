;; init-local local settings

;; key bindings
;; remap help , help is at <F1>
(global-set-key (kbd "C-h") 'delete-backward-char)

;; add editorconfig plugin for projects
(require-package 'editorconfig)

;; set global-mode-string
;(add-to-list 'global-mode-string  current-time-string)

;; set gnus news group
;(setq gnus-select-method '(nntp "news.supernews.com"))



;; set indent level for all mode
(setq preferred-indent-level 4)

(defun gsmlg/set-indent (&optional width)
  "set the indent of each language mode
now in js js2 coffeescript sgml(html,xml) sh(shell) c ruby css
should be set as samewidth"
  (interactive)
  (let ((indent-width (or width preferred-indent-level)))
  (setq js2-basic-offset indent-width
        js-indent-level indent-width
        coffee-tab-width indent-width
        sgml-basic-offset indent-width
        sh-basic-offset indent-width
        c-basic-offset indent-width
        ruby-indent-level indent-width
        css-indent-offset indent-width
        )))
;; run indent settings first time
(gsmlg/set-indent)

;; add w3m support
(require-package 'w3m)

;; set evernote-mode not work now
;;TODO make it work
;;(require-package 'evernote-mode)
;; (setq evernote-developer-token "")


;; set toggle-viper-mode key
(global-set-key (kbd "<f12>") 'toggle-viper-mode)


(provide 'init-local)
