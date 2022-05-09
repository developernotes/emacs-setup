(require 'subr-x)
(add-to-list 'exec-path "/usr/local/git/bin")
(add-to-list 'exec-path "/usr/local/bin")

(setq doc-view-ghostscript-program "/usr/local/bin/gs"
      inferior-lisp-program "/usr/local/bin/sbcl"
      ns-command-modifier 'meta)

(ansi-color-for-comint-mode-on)

(add-to-list 'auto-mode-alist
             '("\\.ios\\'" . (lambda ()
                               (makefile-bsdmake-mode))))

(add-to-list 'auto-mode-alist
             '("\\.osx\\'" . (lambda ()
                               (makefile-bsdmake-mode))))

(defun toggle-fullscreen ()
  "Toggle full screen"
  (interactive)
  (set-frame-parameter nil 'fullscreen
                       (when (not (frame-parameter nil 'fullscreen)) 'fullscreen))
  (if (frame-parameter nil 'fullscreen)
      (display-time-mode 1))
  (if (not (frame-parameter nil 'fullscreen))
      (display-time-mode 0)))

(defun is-dark-mode ()
  "Invoke applescript using Emacs using external shell command; this is less efficient, but works for non-GUI emacs"
  (string-equal "true"
                (string-trim
                 (shell-command-to-string
                  "osascript -e 'tell application \"System Events\" to tell appearance preferences to return dark mode'"))))

(global-set-key (kbd "M-RET") 'toggle-fullscreen)

(provide 'init-osx)
