;; setup helm incremental completion and selection narrowing framework
(require-package 'helm)
(require-package 'helm-cmd-t)
(require-package 'helm-ls-git)
(require-package 'pcsv)
(require-package 'esqlite)
(require-package 'esqlite-helm)
(require-package 'helm-dash)
(require-package 'helm-proc)
(require-package 'helm-flycheck)
(eval-after-load 'flycheck
  '(define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck))

(defvar helm-dash-docsets-path "~/.docsets")
(when (not (file-exists-p helm-dash-docsets-path))
  (make-directory helm-dash-docsets-path))
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
(global-set-key (kbd "C-`") 'helm-dash-at-point)

(require 'helm-config)

(require-package 'helm-swoop)

;; Change the keybinds to whatever you like :)
(global-set-key (kbd "M-i") 'helm-swoop)
(global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
(global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
(global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)

;; When doing isearch, hand the word over to helm-swoop
(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
;; From helm-swoop to helm-multi-swoop-all
;; (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
;; When doing evil-search, hand the word over to helm-swoop
;; (define-key evil-motion-state-map (kbd "M-i") 'helm-swoop-from-evil-search)

;; Save buffer when helm-multi-swoop-edit complete
(setq helm-multi-swoop-edit-save t)

;; If this value is t, split window inside the current window
(setq helm-swoop-split-with-multiple-windows nil)

;; Split direcion. 'split-window-vertically or 'split-window-horizontally
(setq helm-swoop-split-direction 'split-window-vertically)

;; If nil, you can slightly boost invoke speed in exchange for text color
(setq helm-swoop-speed-or-color nil)

;; set heml-ag
(when (executable-find "ag")
  (require-package 'helm-ag)
  (setq helm-ag-base-command "ag --nocolor --nogroup --ignore-case")
  (setq helm-ag-command-option "--all-text")
  (setq helm-ag-thing-at-point 'symbol)
  )

;; replace eshell pcomplete
(add-hook 'eshell-mode-hook
          #'(lambda ()
              (progn
                (sanityinc/no-trailing-whitespace)
                (define-key eshell-mode-map
                  [remap eshell-pcomplete]
                  'helm-esh-pcomplete)
                (define-key eshell-mode-map
                  (kbd "M-p")
                  'helm-eshell-history)
                )))

(helm-mode 1)

(provide 'init-helm)

