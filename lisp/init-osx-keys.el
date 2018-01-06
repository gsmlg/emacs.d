(when *is-a-mac*
  (if (eq (getenv "KEYBOARD_TYPE") "windows")
      (gsmlg/mac-osx-unremap-command)
    (gsmlg/mac-osx-remap-command)
    ))


(provide 'init-osx-keys)
