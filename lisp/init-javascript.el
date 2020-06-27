;;; init-javascript.el --- Support for Javascript and derivatives -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(maybe-require-package 'json-mode)
(maybe-require-package 'js2-mode)
(maybe-require-package 'rjsx-mode)
(maybe-require-package 'coffee-mode)
(maybe-require-package 'tern)
(maybe-require-package 'company-tern)
(maybe-require-package 'typescript-mode)
(maybe-require-package 'prettier-js)

(defcustom preferred-javascript-mode
  (first (remove-if-not #'fboundp '(rjsx-mode js2-jsx-mode js-mode)))
  "Javascript mode to use for .js files."
  :type 'symbol
  :group 'programming
  :options '(rjsx-mode js2-jsx-mode js-mode))

(defconst preferred-javascript-indent-level 2)

;; Need to first remove from list if present, since elpa adds entries too, which
;; may be in an arbitrary order
(eval-when-compile (require 'cl))
(setq auto-mode-alist (cons `("\\.\\(js\\|es6\\|jsx\\)\\(\\.erb\\)?\\'" . ,preferred-javascript-mode)
                            (loop for entry in auto-mode-alist
                                  unless (eq preferred-javascript-mode (cdr entry))
                                  collect entry)))
;; Need to first remove from list if present, since elpa adds entries too, which
;; may be in an arbitrary order

(add-to-list 'auto-mode-alist '("\\.\\(js\\|es6\\)\\(\\.erb\\)?\\'" . js2-mode))

;; js2-mode

;; Change some defaults: customize them to override
(setq-default js2-bounce-indent-p nil)
(with-eval-after-load 'js2-mode
  ;; Disable js2 mode's syntax error highlighting by default...
  (setq-default js2-mode-show-parse-errors nil
                js2-mode-show-strict-warnings nil)
  ;; ... but enable it if flycheck can't handle javascript
  (autoload 'flycheck-get-checker-for-buffer "flycheck")
  (defun sanityinc/enable-js2-checks-if-flycheck-inactive ()
    (unless (flycheck-get-checker-for-buffer)
      (setq-local js2-mode-show-parse-errors t)
      (setq-local js2-mode-show-strict-warnings t)))
  (add-hook 'js2-mode-hook 'sanityinc/enable-js2-checks-if-flycheck-inactive)

  (add-hook 'js2-mode-hook (lambda () (setq mode-name "JS2")))

  (when (executable-find "tern")
    (add-hook 'js2-mode-hook (lambda () (tern-mode t)))
    (after-load 'company
      (add-hook 'js2-mode-hook (lambda () (add-to-list 'company-backends 'company-tern)))))

  (setq-default
   js2-basic-offset preferred-javascript-indent-level
   js2-bounce-indent-p nil)

  (after-load 'js2-mode
    (js2-imenu-extras-setup)))

(setq-default js-indent-level 2)
;; In Emacs >= 25, the following is an alias for js-indent-level anyway
(setq-default js2-basic-offset 2)

;; rjx-mode
;; fix rjsx-mode indents closed html tag with extra spaces
(defadvice js-jsx-indent-line (after js-jsx-indent-line-after-hack activate)
  "Workaround `sgml-mode' and follow airbnb component style."
  (let* ((cur-line (buffer-substring-no-properties
                    (line-beginning-position)
                    (line-end-position))))
    (if (string-match "^\\( +\\)\/?> *$" cur-line)
        (let* ((empty-spaces (match-string 1 cur-line)))
          (replace-regexp empty-spaces
                          (make-string (- (length empty-spaces) sgml-basic-offset) 32)
                          nil
                          (line-beginning-position) (line-end-position))))))

(add-to-list 'interpreter-mode-alist (cons "node" 'js2-mode))



(when (and (executable-find "ag")
           (maybe-require-package 'xref-js2))
  (with-eval-after-load 'js2-mode
    (define-key js2-mode-map (kbd "M-.") nil)
    (add-hook 'js2-mode-hook
              (lambda () (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))))



;;; Coffeescript

(with-eval-after-load 'coffee-mode
  (setq-default coffee-js-mode 'js2-mode
                coffee-tab-width js-indent-level))

(when (fboundp 'coffee-mode)
  (add-to-list 'auto-mode-alist '("\\.coffee\\.erb\\'" . coffee-mode)))

;; ---------------------------------------------------------------------------
;; Run and interact with an inferior JS via js-comint.el
;; ---------------------------------------------------------------------------

(when (maybe-require-package 'js-comint)
  (setq js-comint-program-command "node")

  (defvar inferior-js-minor-mode-map (make-sparse-keymap))
  (define-key inferior-js-minor-mode-map "\C-x\C-e" 'js-send-last-sexp)
  (define-key inferior-js-minor-mode-map "\C-cb" 'js-send-buffer)

  (define-minor-mode inferior-js-keys-mode
    "Bindings for communicating with an inferior js interpreter."
    nil " InfJS" inferior-js-minor-mode-map)

  (dolist (hook '(js2-mode-hook js-mode-hook))
    (add-hook hook 'inferior-js-keys-mode)))

;; ---------------------------------------------------------------------------
;; Alternatively, use skewer-mode
;; ---------------------------------------------------------------------------

(when (maybe-require-package 'skewer-mode)
  (with-eval-after-load 'skewer-mode
    (add-hook 'skewer-mode-hook
              (lambda () (inferior-js-keys-mode -1)))))

(require-package 'js-doc)
(setq js-doc-mail-address (shell-command-to-string "git config --get user.email")
      js-doc-author (format (concat (shell-command-to-string "git config --get user.name") " <%s>") js-doc-mail-address)
      js-doc-url (shell-command-to-string "git config --get remote.origin.url")
      js-doc-license "GPLv3")

(add-hook 'js2-mode-hook
          #'(lambda ()
              (setq js-switch-indent-offset js2-basic-offset)
              (define-key js2-mode-map "\C-ci" 'js-doc-insert-function-doc)
              (define-key js2-mode-map "@" 'js-doc-insert-tag)))




(when (maybe-require-package 'add-node-modules-path)
  (with-eval-after-load 'typescript-mode
    (add-hook 'typescript-mode-hook 'add-node-modules-path))
  (with-eval-after-load 'js2-mode
    (add-hook 'js2-mode-hook 'add-node-modules-path)))


(provide 'init-javascript)
;;; init-javascript.el ends here
