
(require 'cygwin-mount)

(setq doc-view-ghostscript-program "C:/Program Files/gs/gs8.70/bin/gswin32c.exe")
(setq haskell-program-name "C:/Program Files/Haskell Platform/2009.2.0.2/bin/ghci.exe")

(setenv "PATH" (concat "c:/cygwin/bin;" (getenv "PATH")))
(setq exec-path (cons "c:/tools/bin" exec-path))
(setq exec-path (cons "c:/cygwin/bin/" exec-path))

(cygwin-mount-activate)

(add-hook 'comint-output-filter-functions
					'shell-strip-ctrl-m nil t)
(add-hook 'comint-output-filter-functions 
					'comint-watch-for-password-prompt nil t)

(setq explicit-shell-file-name "bash.exe")

;; for subprocesses invoked via the shell (e.g., "shell -c command")
(setq shell-file-name explicit-shell-file-name)

;; display ansi color escape sequences
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(set-face-font 'default "-*-Anonymous Pro-bold-normal-normal-*-*-*-*-*-m-0-iso10646-1")

(require 'w32-fullscreen)
(global-set-key (kbd "M-RET") 'w32-fullscreen)

 ;; suppress 'directory is unsafe error
(require 'server)
(when (and (= emacs-major-version 23) (equal window-system 'w32))
  (defun server-ensure-safe-dir (dir) "Noop" t))

(provide 'mine-windows)
