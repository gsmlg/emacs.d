;; init-local local settings
;;------------------------------------------------------------------------------
;; Set Org Mode Variables
;;------------------------------------------------------------------------------
(setq org-directory "~/Documents/org/"
      org-agenda-files "~/Documents/org/.agenda_files")

;;------------------------------------------------------------------------------
;; Global key binding
;;------------------------------------------------------------------------------
;; remap help , help is at <F1>
(global-set-key (kbd "C-h") 'delete-backward-char)

;;------------------------------------------------------------------------------
;; Helm key bindings in global
;;------------------------------------------------------------------------------
(global-set-key [remap execute-extended-command] 'helm-M-x)
(global-set-key (kbd "C-x C-m") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x C-g") 'helm-do-grep-ag)

(provide 'init-local)
