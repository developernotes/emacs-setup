
(add-to-list 'exec-path "/usr/local/git/bin")
(add-to-list 'exec-path "/usr/local/mysql/bin")
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "/Applications/Racket/bin")

(setq doc-view-ghostscript-program "/usr/local/bin/gs"
      inferior-lisp-program "/usr/local/bin/sbcl"
      ns-command-modifier 'meta)

(ansi-color-for-comint-mode-on)

(defun toggle-fullscreen ()
  "Toggle full screen"
  (interactive)
  (set-frame-parameter nil 'fullscreen
                       (when (not (frame-parameter nil 'fullscreen)) 'fullscreen))
  (if (frame-parameter nil 'fullscreen)
      (display-time-mode 1))
  (if (not (frame-parameter nil 'fullscreen))
      (display-time-mode 0)))

(global-set-key (kbd "M-RET") 'toggle-fullscreen)

(provide 'mine-osx)
