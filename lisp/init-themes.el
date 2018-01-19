;;------------------------------------------------------------------------------
;; Use spacemacs-theme `https://github.com/nashamri/spacemacs-theme'
;;------------------------------------------------------------------------------
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
;; add all the icons packages for file icon
;;------------------------------------------------------------------------------
(require-package 'all-the-icons)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
;; highlight window
(require-package 'dimmer)
(dimmer-mode)


;;------------------------------------------------------------------------------
;; Config modeline
;;------------------------------------------------------------------------------
(require-package 'spaceline)
(require-package 'spaceline-all-the-icons)

(require 'spaceline-config)
(setq spaceline-all-the-icons-slim-render t
      spaceline-all-the-icons-separator-type 'arrow
      spaceline-all-the-icons-icon-set-sun-time 'sun/moon)

(spaceline-helm-mode)
(spaceline-info-mode)

(spaceline-all-the-icons-theme 'input-method 'buffer-encoding-abbrev 'org-pomodoro 'mu4e-alert-segment)
(spaceline-all-the-icons--setup-anzu)            ;; Enable anzu searching
(spaceline-all-the-icons--setup-package-updates) ;; Enable package update indicator
(spaceline-all-the-icons--setup-git-ahead)       ;; Enable # of commits ahead of upstream in git
(spaceline-all-the-icons--setup-paradox)         ;; Enable Paradox mode line
(spaceline-all-the-icons--setup-neotree)         ;; Enable Neotree mode line


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


(provide 'init-themes)
