;;--------------------------------------------------------------------
;; set projectile for manage projects
;; projectile.el at https://github.com/bbatsov/projectile
;;--------------------------------------------------------------------

(when (maybe-require-package 'projectile)
  (add-hook 'after-init-hook 'projectile-global-mode))

;; perspective for projectile quickly switch project
;;(require-package 'perspective)
;;(require-package 'persp-projectile)
;;(persp-mode)

(after-load 'projectile
  '(progn
     ;;(global-set-key (kbd "C-<f9>") 'projectile-mode)
     ;; set the default keymap prefix from <C-c p> to this:
     (setq projectile-keymap-prefix (kbd "C-c p"))
     (setq projectile-enable-idle-timer t)
     ;; enable caching file index
     (setq projectile-enable-caching t)
     (define-key projectile-mode-map (kbd "C-c p f") 'helm-projectile)
     ;; -- Switching projects --
     ;; use <C-c p s> `projectile-switch-project' to switch in known projects
     (define-key projectile-mode-map (kbd "C-c p s") 'projectile-persp-switch-project)
     (when (maybe-require-package 'helm-do-ag)
       (define-key projectile-mode-map (kbd "C-c p g") '(lambda ()
                                                          (interactive)
                                                          (helm-do-ag (projectile-project-root)))))
     (when (maybe-require-package 'editorconfig)
       (editorconfig-mode 1)
       (editorconfig-apply))
     ;; Shorter modeline
     (setq-default
      projectile-mode-line
      '(:eval
        (if (file-remote-p default-directory)
            " Pr"
          (format " Pr[%s]" (projectile-project-name)))))
     ;; force native indexing on windows
     ;; alien require unix command like find, git, etc.
     (when *is-a-win*
       ;; specifies the indexing method used by Projectile
       ;; two methods - native and alien
       (setq projectile-indexing-method 'native))
     ;; The following code means you get a menu if you hit "C-c p" and wait
     (after-load 'guide-key
       (add-to-list 'guide-key/guide-key-sequence "C-c p"))
     ))

;; use <C-c p f> which is `projectile-find-file' find a file in project
;; use <C-u> <C-c p f> will rebuild the indexed of Projectile and find file
;; use <C-c p z> `projectile-cache-current-file' add currently visited file to the cache for the current project
;; with `projectile-purge-file-from-cache' is purge an individual file from the cache
;; with `projectile-purge-dir-from-cache' is purge an entire directory

;; change the remote file exists cache expire to 10 minutes - if wanna disable it, just set nil
(setq projectile-file-exists-remote-cache-expire (* 10 60))

;; You can also enable the cache for local file systems, that is normally not needed but possible:
;; (setq projectile-file-exists-local-cache-expire (* 5 60))

;; If you want Projectile to be usable in every directory (even without the presence of project file):
;; (setq projectile-require-project-root nil)

;; Projectile invokes the command specified in `projectile-switch-project-action' (default is `project-find-file')
;; When `projectile-remember-window-configs' is t (default is nil),
;; the most recent window configuration of the target project is restored instead of calling `projectile-switch-project-action'.
;; If the target project has no window configuration in the current editing session,
;; `projectile-switch-project-action' is otherwise invoked as described above.

;; Depending on your personal workflow and habits,
;; you may prefer to alter the value of projectile-switch-project-action:

;; `projectile-find-file'
;; This is the default. With this setting,
;; once you have selected your project via Projectile's completion system (see below),
;; you will remain in the completion system to select a file to visit.

;; `projectile-dired'
;; (setq projectile-switch-project-action 'projectile-dired)
;; With this setting, once you have selected your project,
;; the top-level directory of the project is immediately opened for you in a dired buffer.

;; `projectile-find-dir'
;; (setq projectile-switch-project-action 'projectile-find-dir)
;; With this setting, once you have selected your project,
;; you will remain in Projectile's completion system to select a sub-directory of your project,
;; and then that sub-directory is opened for you in a dired buffer.
;; If you use this setting, then you will probably also want to set

;; (setq projectile-find-dir-includes-top-level t)
;; in order to allow for the occasions where you want to select the top-level directory.


;; --- Completion Options ---
;; use the variable `projectile-completion-system' to set the completion system
;; known are `ido', `grizzl', `default' or customize it as a function
(setq projectile-completion-system 'ido)

;; use `projectile-tags-command' is the command projectile generate tags
;; use `projectile-regenerate-tags' is used to regenerate tags

;; genrate tags without prop
;; (setq tags-revert-without-query 1)


;; You can set the values of
;; `projectile-project-root-files',
;; `projectile-project-root-files-top-down-recurring',
;; `projectile-project-root-files-bottom-up'
;; `projectile-project-root-files-functions'
;; to customize how project roots are identified.
;; use `M-x' `customize-group' `RET' `projectile' `RET'

(provide 'init-projectile)
