
(require 'server)
(require 'cygwin-mount)
(require 'w32-fullscreen)
(require 'powershell)

(autoload 'powershell-mode "powershell-mode" "A editing mode for Microsoft PowerShell." t)

(setenv "PATH" (concat "c:/cygwin/bin;" (getenv "PATH")))

(add-to-list 'exec-path "c:/tools/bin")
(add-to-list 'exec-path "c:/cygwin/bin")
(add-to-list 'exec-path "c:/tools/scala/bin")
(add-to-list 'exec-path "c:/tools/clojure-clr/bin")

(add-to-list 'auto-mode-alist '("\\.ps1$" . powershell-mode))

(cygwin-mount-activate)

(add-hook 'comint-output-filter-functions
          'shell-strip-ctrl-m nil t)
(add-hook 'comint-output-filter-functions
          'comint-watch-for-password-prompt nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(setq explicit-shell-file-name "zsh.exe"
      shell-file-name explicit-shell-file-name
      doc-view-ghostscript-program "C:/Program Files/gs/gs8.70/bin/gswin32c.exe"
      haskell-program-name "C:/Program Files/Haskell Platform/2010.2.0.0/bin/ghci.exe")

(global-set-key (kbd "M-RET") 'w32-fullscreen)

;; suppress 'directory is unsafe error
(when (and (= emacs-major-version 23) (equal window-system 'w32))
  (defun server-ensure-safe-dir (dir) "Noop" t))

(provide 'mine-windows)
