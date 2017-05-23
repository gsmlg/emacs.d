(require-package 'web-mode)


(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
;(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
;(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(add-to-list 'auto-mode-alist '("\\.api\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))

;;(setq web-mode-content-types-alist
;;      '(("json" . "/some/path/.*\\.api\\'")
;;        ("xml"  . "/other/path/.*\\.api\\'")
;;        ("jsx"  . "/some/react/path/.*\\.js[x]?\\'")))



(defun my-web-mode-hook ()
  "Hooks for Web mode."

  ;; HTML element offset indentation
  (setq web-mode-markup-indent-offset preferred-indent-level)
  (setq web-mode-markup-indent-offset preferred-indent-level)
  ;; CSS offset indentation
  (setq web-mode-css-indent-offset preferred-indent-level)
  ;; Script/code offset indentation (for JavaScript, Java, PHP, Ruby, VBScript, Python, etc.)
  (setq web-mode-code-indent-offset preferred-indent-level)

  ;; By default, tag attributes are indented like this:
  ;; <img src="pix.png"
  ;;      class="noborder"/>
  ;; You can force a fixed indentation with web-mode-attr-indent-offset
  ;; <img src="pix.png"
  ;;   class="noborder"/>

  ;; Left padding
  ;; For <style> parts
  (setq web-mode-style-padding preferred-indent-level)
  ;; For <script> parts
  (setq web-mode-script-padding preferred-indent-level)
  ;; For multi-line blocks
  (setq web-mode-block-padding 0)

  ;; Enable / disable features
  ;; Auto-pairing
  (setq web-mode-enable-auto-pairing t)
  ;; CSS colorization
  (setq web-mode-enable-css-colorization t)
  ;; Block face: can be used to set blocks background and default foreground (see web-mode-block-face)
  (setq web-mode-enable-block-face t)
  ;; Part face: can be used to set parts background and default foreground (see web-mode-script-face and web-mode-style-face which inheritate from web-mode-part-face)
  (setq web-mode-enable-part-face t)
  ;; Comment keywords (see web-mode-comment-keyword-face)
  (setq web-mode-enable-comment-keywords t)
  ;; Heredoc (cf. PHP strings) fontification (when the identifier is <<<EOTHTML or <<<EOTJAVASCRIPT)
  (setq web-mode-enable-heredoc-fontification t)

  )

(add-hook 'web-mode-hook  'my-web-mode-hook)


(provide 'init-web-mode)
;;; init-web-mode ends here
