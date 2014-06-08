;; yasnippet install and settings
(require-package 'yasnippet)

(add-to-list 'yas-snippet-dirs (expand-file-name "yasnippet-snippets" user-emacs-directory))
(add-to-list 'yas-snippet-dirs (expand-file-name "yasmate/snippets" user-emacs-directory))

(yas-global-mode 1)

(provide 'init-yasnippet)
