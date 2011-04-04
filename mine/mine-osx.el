
(add-to-list 'exec-path "/usr/local/git/bin")
(add-to-list 'exec-path "/usr/local/mysql/bin")
(add-to-list 'exec-path "/usr/local/bin")

(setq doc-view-ghostscript-program "/usr/local/bin/gs"
      inferior-lisp-program "/usr/local/bin/sbcl"
      ns-command-modifier 'meta)

(ansi-color-for-comint-mode-on)

(global-set-key (kbd "M-RET") 'ns-toggle-fullscreen)

(provide 'mine-osx)