;;; package --- setup cygwin
;;; Commentary:
;;; Code:
(defconst *has-a-cygwin* (not (eq (getenv "CYGWIN_ROOT") nil)))

(when *has-a-cygwin*
  (let ((root (getenv "CYGWIN_ROOT")))
    (setq cygwin-root-directory root)
    (setq cygwin-mount-cygwin-bin-directory (expand-file-name "bin" root))
    )
  ;; install cygwin-mount
  (require-package 'cygwin-mount)
  (require-package 'setup-cygwin)
)

(provide 'init-windows-nt)
;;; init-windows-nt.el ends here
