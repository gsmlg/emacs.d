;; init-local local settings

;; key bindings
;; remap help , help is at <F1>
(global-set-key (kbd "C-h") 'delete-backward-char)

;; set global-mode-string
;;(add-to-list 'global-mode-string  current-time-string)

;; set gnus news group
;;(setq gnus-select-method '(nntp "news.supernews.com"))


;; set evernote-mode not work now
;;TODO make it work
;;(require-package 'evernote-mode)
;; (setq evernote-developer-token "")


(provide 'init-local)
