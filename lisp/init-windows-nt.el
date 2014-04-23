;; setup cygwin

(setq cygwin-mount-cygwin-bin-directory "c:\cygwin64\bin")

;; install cygwin-mount
(require-package 'cygwin-mount)


;; from setup-cygwin.el
(require 'cygwin-mount)
(when (< emacs-major-version 21)
  (defun add-to-list (list-var element &optional append)
    "Add to the value of LIST-VAR the element ELEMENT if it isn't there yet.
The test for presence of ELEMENT is done with `equal'.
If ELEMENT is added, it is added at the beginning of the list,
unless the optional argument APPEND is non-nil, in which case
ELEMENT is added at the end.

If you want to use `add-to-list' on a variable that is not defined
until a certain package is loaded, you should put the call to `add-to-list'
into a hook function that will be run only after loading the package.
`eval-after-load' provides one way to do this.  In some cases
other hooks, such as major mode hooks, can do the job."
    (if (member element (symbol-value list-var))
        (symbol-value list-var)
      (set list-var
           (if append
               (append (symbol-value list-var) (list element))
             (cons element (symbol-value list-var)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup setup-cygwin nil
  "Set up Emacs to use Cygwin.")

(defun setcyg-dir-p (directory)
  "Return DIRECTORY if DIRECTORY is a readable directory, nil otherwise."
  (and (file-directory-p directory)  (file-readable-p directory)  directory))

(defcustom cygwin-root-directory (or (setcyg-dir-p "C:/cygwin64/")  (setcyg-dir-p "C:/cygwin/"))
  "Root directory of Cygwin installation.
It should have subdirectories `bin' and `usr/info'.
Subdirectory `bin' should have file `bin/bash.exe'."
  :group 'setup-cygwin :type 'directory)

(unless (setcyg-dir-p cygwin-root-directory)
  (error "Cannot find Cygwin.  Please customize option `cygwin-root-directory'"))


;;; Make Cygwin paths accessible
(cygwin-mount-activate)

;;; Follow Cygwin symlinks.
;;; Handles old-style (text file) symlinks and new-style (.lnk file) symlinks.
;;; (Non-Cygwin-symlink .lnk files, such as desktop shortcuts, are still loaded as such.)
(defun follow-cygwin-symlink ()
  "Follow Cygwin symlinks.
Handles old-style (text file) and new-style (.lnk file) symlinks.
\(Non-Cygwin-symlink .lnk files, such as desktop shortcuts, are still
loaded as such.)"
  (save-excursion
    (goto-char 0)
    (if (looking-at
         "L\x000\x000\x000\x001\x014\x002\x000\x000\x000\x000\x000\x0C0\x000\x000\x000\x000\x000\x000\x046\x00C")
        (progn
          (re-search-forward
           "\x000\\([-A-Za-z0-9_\\.\\\\\\$%@(){}~!#^'`][-A-Za-z0-9_\\.\\\\\\$%@(){}~!#^'`]+\\)")
          (find-alternate-file (match-string 1)))
      (when (looking-at "!<symlink>")
        (re-search-forward "!<symlink>\\(.*\\)\0")
        (find-alternate-file (match-string 1))))))
(add-hook 'find-file-hooks 'follow-cygwin-symlink)

;;; Use Unix-style line endings.
(setq-default buffer-file-coding-system 'undecided-unix)

;;; Use /dev/null, not NUL.
(setq null-device  "/dev/null")

;;; Without this env var setting, Cygwin causes `ediff-buffers', at least, to raise an error.
;;; Making this setting here might have no effect, as the env var is checked only by the first Cygwin process
;;; invoked during your Windows session.  For best results, set this env var globally, in Windows itself.
;;; An alternative might be to use `cygpath' to change from MS Windows file names to POSIX.
(setenv "CYGWIN" "nodosfilewarning")

;;; Add Cygwin Info pages
(add-to-list 'Info-default-directory-list (expand-file-name "usr/info" cygwin-root-directory) 'APPEND)

;;; Use `bash' as the default shell in Emacs.
(add-to-list 'exec-path (expand-file-name "bin" cygwin-root-directory))
(setq shell-file-name  (expand-file-name "bin/bash.exe" cygwin-root-directory)) ; Subprocesses invoked by shell.
(setenv "SHELL" shell-file-name)
;; (setenv "PATH" (concat (getenv "PATH") ";" (expand-file-name "bin" cygwin-root-directory)))
(setenv "PATH" (concat (expand-file-name "bin" cygwin-root-directory) ";" (getenv "PATH")))
(setq explicit-shell-file-name  shell-file-name) ; Interactive shell
(setq ediff-shell               shell-file-name)    ; Ediff shell
(setq explicit-shell-args       '("--login" "-i"))

;;;;; (setq shell-command-switch "-ic") ; SHOULD THIS BE "-c" or "-ic"?
(setq w32-quote-process-args ?\") ;; " @@@ IS THIS BETTER? ;@@@ WAS THIS BEFORE: (setq w32-quote-process-args t)

;; These don't seem to be needed.
;; They were recommended by http://www.khngai.com/emacs/cygwin.php
;;;;; (add-hook 'comint-output-filter-functions
;;;;;     'shell-strip-ctrl-m nil t)
;;;;; ;; Removes unsightly ^M characters that would otherwise appear in output of java applications.
;;;;; (add-hook 'comint-output-filter-functions
;;;;;     'comint-watch-for-password-prompt nil t)
;;;;; (setq explicit-shell-file-name "bash.exe")
;;;;; ;; For subprocesses invoked via the shell
;;;;; ;; (e.g., "shell -c command")
;;;;; (setq shell-file-name explicit-shell-file-name)


;;;###autoload
(defun bash ()
  "Start `bash' shell."
  (interactive)
  (let ((binary-process-input t)
        (binary-process-output nil))
    (shell)))

(setq process-coding-system-alist
      (cons '("bash" . (raw-text-dos . raw-text-unix)) process-coding-system-alist))


;; From: http://www.dotfiles.com/files/6/235_.emacs
;;;###autoload
(defun set-shell-bash()
  "Enable on-the-fly switching between the bash shell and DOS."
  (interactive)
  ;; (setq binary-process-input t)
  (setq shell-file-name "bash")
  (setq shell-command-switch "-c")      ; SHOULD IT BE (setq shell-command-switch "-ic")?
  (setq explicit-shell-file-name "bash")
  (setenv "SHELL" explicit-shell-file-name)
  ;;;;;(setq explicit-sh-args '("-login" "-i")) ; Undefined?
  (setq w32-quote-process-args ?\") ;; "
  ;;;;;(setq mswindows-quote-process-args t)) ; Undefined?
  )

;;;###autoload
(defun set-shell-cmdproxy()
  "Set shell to `cmdproxy'."
  (interactive)
  (setq shell-file-name "cmdproxy")
  (setq explicit-shell-file-name "cmdproxy")
  (setenv "SHELL" explicit-shell-file-name)
  ;;;;;(setq explicit-sh-args nil)           ; Undefined?
  (setq w32-quote-process-args nil))

;;;;;;;;;;;;;;;;;;;;;;;


(provide 'init-windows-nt)
