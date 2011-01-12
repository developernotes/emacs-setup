
(require 'uniquify)
(require 'undo-tree)

(global-undo-tree-mode)

(autoload 'scratch "scratch" nil t)

(setq visible-bell t
      display-time-24hr-format t
      inhibit-startup-screen t
      uniquify-buffer-name-style 'post-forward
      uniquify-separator ":"
      custom-file (concat emacs-root "/mine/mine-customizations.el")
      backup-directory-alist         `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      kill-buffer-query-functions    (remq 'process-kill-buffer-query-function
                                           kill-buffer-query-functions))

(global-linum-mode 1)
(blink-cursor-mode t)
(display-battery-mode)
(display-time-mode t)
(global-hl-line-mode t)
(delete-selection-mode t)

(setq-default tab-width 2
              indent-tabs-mode nil)

;; type only y or n for confirmation
(fset 'yes-or-no-p 'y-or-n-p)

;; disable user prompt
(put 'upcase-region    'disabled nil)
(put 'downcase-region  'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; remove menubar/toolbar
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(provide 'mine-customizations)
