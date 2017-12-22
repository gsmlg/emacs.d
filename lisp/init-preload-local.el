;;; define some function

;; set indent level for all mode
(setq gsmlg/preferred-indent-level 4)

(defun gsmlg/set-indent (&optional width)
  "set the indent of each language mode,
now in js js2 coffeescript sgml(html,xml) sh(shell) c ruby css
should be set as same width"
  (interactive)
  (let ((indent-width (or width gsmlg/preferred-indent-level)))
    (setq js2-basic-offset indent-width
          js-switch-indent-offset indent-width
          js-indent-level indent-width
          coffee-tab-width indent-width
          sgml-basic-offset indent-width
          sh-basic-offset indent-width
          c-basic-offset indent-width
          ruby-indent-level indent-width
          css-indent-offset indent-width
          yaml-indent-offset indent-width
          )))

;; remap Command key binding when use macOS keyboard
(defun gsmlg/mac-osx-remap-command ()
  (interactive)
  (progn
    (setq mac-command-modifier 'meta)
    (setq mac-option-modifier 'none)
    (setq-default default-input-method "MacOSX")
    ;; Make mouse wheel / trackpad scrolling less jerky
    (setq mouse-wheel-scroll-amount '(1
                                      ((shift) . 5)
                                      ((control))))
    (dolist (multiple '("" "double-" "triple-"))
      (dolist (direction '("right" "left"))
        (global-set-key (read-kbd-macro (concat "<" multiple "wheel-" direction ">")) 'ignore)))
    (global-set-key (kbd "M-`") 'ns-next-frame)
    (global-set-key (kbd "M-h") 'ns-do-hide-emacs)
    (global-set-key (kbd "M-˙") 'ns-do-hide-others)
    (after-load 'nxml-mode
      (define-key nxml-mode-map (kbd "M-h") nil))
    (global-set-key (kbd "M-ˍ") 'ns-do-hide-others) ;; what describe-key reports for cmd-option-h
    ))

;; remap Command key binding when use normal keyboard in macOS
(defun gsmlg/mac-osx-unremap-command ()
  (interactive)
  (progn
    (setq mac-command-modifier 'super)
    (setq mac-option-modifier 'meta)
    (setq-default default-input-method "MacOSX")
    ;; Make mouse wheel / trackpad scrolling less jerky
    (setq mouse-wheel-scroll-amount '(1
                                      ((shift) . 5)
                                      ((control))))
    (dolist (multiple '("" "double-" "triple-"))
      (dolist (direction '("right" "left"))
        (global-set-key (read-kbd-macro (concat "<" multiple "wheel-" direction ">")) 'ignore)))
    (global-set-key (kbd "s-`") 'ns-next-frame)
    (global-set-key (kbd "s-h") 'ns-do-hide-emacs)
    (global-set-key (kbd "s-˙") 'ns-do-hide-others)
    (after-load 'nxml-mode
      (define-key nxml-mode-map (kbd "s-h") nil))
    (global-set-key (kbd "s-ˍ") 'ns-do-hide-others) ;; what describe-key reports for cmd-option-h
    ))



(provide 'init-preload-local)
