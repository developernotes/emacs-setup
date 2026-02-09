
(require 'uniquify)
(require 'backup-dir)

(autoload 'scratch "scratch" nil t)

(setq visible-bell t
      c-basic-offset 2
      menu-bar-mode -1
      debug-on-error nil
      linum-format " %d "
      column-number-mode t
      redisplay-dont-pause t
      ring-bell-function 'ignore
      display-time-24hr-format t
      inhibit-startup-screen t
      uniquify-buffer-name-style 'post-forward
      uniquify-separator ":"
      ag-highlight-search t
      initial-scratch-message nil
      ispell-program-name "aspell"
      ediff-window-setup-function 'ediff-setup-windows-plain
      mouse-wheel-progressive-speed nil
      global-display-fill-column-indicator-mode t
      mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil))
      bkup-backup-directory-info '((t "~/.backups" ok-create full-path))
      custom-file (concat emacs-root "/mine/mine-customizations.el")
      kill-buffer-query-functions    (remq 'process-kill-buffer-query-function
                                           kill-buffer-query-functions))

(global-display-line-numbers-mode)
(blink-cursor-mode t)
(when (string-match-p "^Battery" (battery))
  (display-battery-mode))
(display-time-mode t)
(delete-selection-mode t)

(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
(add-hook 'text-mode-hook #'display-fill-column-indicator-mode)
(add-hook 'fundamental-mode-hook #'display-fill-column-indicator-mode)

;; (if (not (eq window-system nil))
;;     (require 'init-color-theme))

(setq-default tab-width 2
              indent-tabs-mode nil)

(fset 'yes-or-no-p 'y-or-n-p)

(put 'upcase-region    'disabled nil)
(put 'downcase-region  'disabled nil)
(put 'narrow-to-region 'disabled nil)

(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when window-system
  (let* ((window-width 120)
        (window-height 50)
        (pos-x (/ (display-pixel-width) 2))
        (pos-y (/ (display-pixel-height) 2)))
    (set-frame-size (selected-frame) window-width window-height)
    ;;(set-frame-position (selected-frame) pos-x pos-y)
  ))

(eval-after-load "dabbrev" '(defalias 'dabbrev-expand 'hippie-expand))
(setq hippie-expand-try-functions-list
      '(try-expand-all-abbrevs
        try-expand-list
        try-expand-dabbrev-visible
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol
        try-complete-file-name-partially
        try-complete-file-name))

(provide 'init-customizations)
