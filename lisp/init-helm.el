;; setup helm incremental completion and selection narrowing framework
(require-package 'helm)
(require-package 'helm-ls-git)
(require-package 'pcsv)
(require-package 'esqlite)
(require-package 'esqlite-helm)
(require-package 'helm-dash)
(require-package 'helm-proc)
(require-package 'helm-flycheck)
(eval-after-load 'flycheck
  '(define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck))
(eval-after-load 'projectile
  '(require-package 'helm-projectile))

(defvar helm-dash-docsets-path "~/.docsets")
(setq helm-dash-min-length 2)
(require 'dash)
(setq helm-dash-common-docsets
      (let ((docs (-reduce
                   (lambda (memo item)
                     "filter list and remove suffix '.docset'"
                     (let ((docsets (if (stringp memo)
                                        (if (string-match "\.docset$" memo)
                                            (list (replace-regexp-in-string "\.docset$" "" memo))
                                          '())
                                      memo)))
                       (if (string-match "\.docset$" item)
                           (cons (replace-regexp-in-string "\.docset$" "" item) docsets)
                         docsets)))
                   (directory-files helm-dash-docsets-path))))
        docs))

(require 'helm-config)

;; replace eshell pcomplete
(add-hook 'eshell-mode-hook
          #'(lambda ()
               (progn
                 (define-key eshell-mode-map
                   [remap eshell-pcomplete]
                   'helm-esh-pcomplete)
                 (define-key eshell-mode-map
                   (kbd "M-p")
                   'helm-eshell-history)
                 )))

(helm-mode 1)

(provide 'init-helm)

