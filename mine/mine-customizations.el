
(require 'uniquify) 

(setq custom-file (concat emacs-root "/mine/mine-customizations.el"))

(setenv "EDITOR" "emacsclient -a=vim")

;; disable the startup screen
(setq inhibit-startup-screen t)

;; type only y or n for confirmation
(fset 'yes-or-no-p 'y-or-n-p)

(setq indent-tabs-mode nil) 
(delete-selection-mode t)

;; disable user prompt
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; enable line numbers
(global-linum-mode 1)

;; disable audible bell
(setq visible-bell t)

;; remove menubar/toolbar
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
	(tool-bar-mode -1))

(setq-default tab-width 2)

;; highlight current line
(global-hl-line-mode t)

;; identify buffers with same name, postfix folder name
(setq 
  uniquify-buffer-name-style 'post-forward
  uniquify-separator ":")

;; all backup files in one place
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; ELPA
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))

(provide 'mine-customizations)
