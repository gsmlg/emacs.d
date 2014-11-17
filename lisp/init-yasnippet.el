;; yasnippet install and settings
(require-package 'yasnippet)
;; work with helm
(require-package 'helm-c-yasnippet)

(defvar yas-snippet-dirs (list ()))

(add-to-list 'yas-snippet-dirs (expand-file-name "snippets" user-emacs-directory))

(yas-global-mode 1)

(provide 'init-yasnippet)
