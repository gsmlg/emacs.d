;; use spacemacs-theme `https://github.com/nashamri/spacemacs-theme'
(require-package 'spacemacs-theme)

;; if you don't customize it, this is the theme you get.
(setq-default custom-enabled-themes '(spacemacs-dark))

;; Ensure that themes will be applied even if they have not been customized
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

(add-hook 'after-init-hook 'reapply-themes)


;;------------------------------------------------------------------------------
;; Toggle between light and dark
;;------------------------------------------------------------------------------
(defun light ()
  "Activate a light color theme."
  (interactive)
  (setq custom-enabled-themes '(spacemacs-light))
  (reapply-themes))

(defun dark ()
  "Activate a dark color theme."
  (interactive)
  (setq custom-enabled-themes '(spacemacs-dark))
  (reapply-themes))

;;------------------------------------------------------------------------------
;; Config modeline
;;------------------------------------------------------------------------------
(require-package 'spaceline)
(require-package 'spaceline-all-the-icons)

(require 'spaceline-config)
(spaceline-emacs-theme)
(spaceline-helm-mode)
(spaceline-info-mode)

(spaceline-all-the-icons-theme)
(spaceline-all-the-icons--setup-anzu)            ;; Enable anzu searching
(spaceline-all-the-icons--setup-package-updates) ;; Enable package update indicator
(spaceline-all-the-icons--setup-git-ahead)       ;; Enable # of commits ahead of upstream in git
(spaceline-all-the-icons--setup-paradox)         ;; Enable Paradox mode line
(spaceline-all-the-icons--setup-neotree)         ;; Enable Neotree mode line

(setq spaceline-show-default-input-method t)

;;------------------------------------------------------------------------------
;; add all the icons packages for file icon
;;------------------------------------------------------------------------------
(require-package 'all-the-icons)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
(maybe-require-package 'dimmer)


(provide 'init-themes)
