
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

(setq doc-view-ghostscript-program "/usr/local/bin/gs")

(setq inferior-lisp-program "/usr/local/bin/sbcl")

(ansi-color-for-comint-mode-on)

(menu-bar-mode t)

(setq ns-command-modifier 'meta)

(global-set-key (kbd "M-RET") 'ns-toggle-fullscreen)

(provide 'mine-osx)