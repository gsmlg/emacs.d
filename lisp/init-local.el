;; init-local local settings

;; set indent level for all mode
(setq preferred-indent-level 4)

(setq js2-basic-offset preferred-indent-level
      js-indent-level preferred-indent-level
      coffee-tab-width preferred-indent-level
      sgml-basic-offset preferred-indent-level
      sh-basic-offset preferred-indent-level
      c-basic-offset preferred-indent-level
      ruby-indent-level preferred-indent-level
      css-indent-offset preferred-indent-level
      )

;; add w3m support
(require-package 'w3m)

;; set evernote-mode not work now
;;TODO make it work
;;(require-package 'evernote-mode)
;; (setq evernote-developer-token "")


;; screencast

(setq screencast-vertical-padding 0)

(defun screencast-get-size-string ()
  (let ((width (frame-pixel-width))
        (height (frame-pixel-height)))
    (concat (number-to-string (if (oddp width) (+ 1 width) width))
            "x"
            (number-to-string (+ screencast-vertical-padding (if (oddp height) (+ 1 height) height))))))

(defun screencast-get-position-string ()
  (let ((left (frame-parameter nil 'left))
        (top (frame-parameter nil 'top)))
    (concat ":0.0+"
            (number-to-string left)
            ","
            (number-to-string top))))

; TODO: defcustom
(setq screencast-process-name "screencast-process")
(setq screencast-buffer-name "*screencast*")
(setq screencast-mode-line-string " <<Recording>>")
(setq screencast-recording-directory (expand-file-name "~/Videos"))

(defun screencast-make-date ()
  (let* ((date-list (split-string (current-time-string)))
         (year (nth 4 date-list))
         (month (nth 1 date-list))
         (day (nth 2 date-list))
         (time (nth 3 date-list)))
    (concat year "-" month "-" day "-" time)))

(defun screencast-create-recording-name ()
  (let ((base-name "emacs")
        (base-extension ".webm"))
    (concat base-name "-" (screencast-make-date) base-extension)))

(defun screencast-start ()
  (interactive)
  (if (equalp (process-status screencast-process-name) nil)
      (progn
        (start-process screencast-process-name
                       screencast-buffer-name
                       "/usr/bin/avconv"
                       "-f" "alsa" "-ac" "2" "-i" "pulse" "-f" "x11grab" "-r" "30" "-s"
                       (screencast-get-size-string)
                       "-i"
                       (screencast-get-position-string)
                       "-acodec" "libvorbis" "-vcodec" "libvpx" "-threads" "0"
                       (concat screencast-recording-directory "/" (screencast-create-recording-name)))
        (add-to-list 'global-mode-string screencast-mode-line-string 1))))

(defun screencast-stop ()
  (interactive)
  (let* ((p (get-process screencast-process-name)))
    (defun cleanup-out-buffer (process event)
      (let ((out-buffer (get-buffer screencast-buffer-name)))
        (kill-buffer out-buffer)))
    (set-process-sentinel p 'cleanup-out-buffer)
    (interrupt-process p)
    (delq screencast-mode-line-string global-mode-string)
    (force-mode-line-update)))

; Set up the key bindings
(defun screencast-default-keybindings ()
  (interactive)
  (define-prefix-command 'screencast-map)
  (global-set-key (kbd "C-c s") screencast-map)
  (global-set-key (kbd "C-c s a") 'screencast-start)
  (global-set-key (kbd "C-c s e") 'screencast-stop))


(provide 'init-local)
