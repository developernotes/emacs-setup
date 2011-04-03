
(if (not (eq window-system nil))
    (require 'mine-color-theme))

;; adjust pathing
(when (equal system-type 'darwin)
  (append-to-environment-variable "PATH" "/opt/local/bin")
  (append-to-environment-variable "PATH" "/usr/local/bin")
  (append-to-environment-variable "PATH" "/usr/local/git/bin")
  (append-to-environment-variable "PATH" "/opt/local/bin")
  (append-to-environment-variable "PATH" "/usr/texbin"))

(add-to-list 'exec-path "/usr/local/git/bin")
(add-to-list 'exec-path "/usr/local/mysql/bin")
(add-to-list 'exec-path "/usr/local/bin")

(setq doc-view-ghostscript-program "/usr/local/bin/gs"
      inferior-lisp-program "/usr/local/bin/sbcl"
      ns-command-modifier 'meta)

(ansi-color-for-comint-mode-on)

(global-set-key (kbd "M-RET") 'ns-toggle-fullscreen)

(provide 'mine-osx)