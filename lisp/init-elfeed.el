(require-package 'elfeed)
(require-package 'elfeed-goodies)
(require-package 'elfeed-org)

(elfeed-goodies/setup)
(elfeed-org)

(setq elfeed-db-directory "~/.emacs.d/elfeed")
(setq rmh-elfeed-org-files '("~/.emacs.d/elfeed.org"))

(provide 'init-elfeed)
