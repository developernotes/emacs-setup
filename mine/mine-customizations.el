
(require 'uniquify)
(require 'undo-tree)
(require 'backup-dir)
(require 'whitespace)

(global-undo-tree-mode)

(autoload 'scratch "scratch" nil t)

(setq visible-bell t
      redisplay-dont-pause t
      ring-bell-function 'ignore
      display-time-24hr-format t
      inhibit-startup-screen t
      uniquify-buffer-name-style 'post-forward
      uniquify-separator ":"
      ispell-program-name "aspell"
      ediff-window-setup-function 'ediff-setup-windows-plain
      bkup-backup-directory-info '((t "~/.backups" ok-create full-path))
      custom-file (concat emacs-root "/mine/mine-customizations.el")
      kill-buffer-query-functions    (remq 'process-kill-buffer-query-function
                                           kill-buffer-query-functions))

(global-linum-mode 1)
(blink-cursor-mode t)
(when (string-match-p "^Battery" (battery))
  (display-battery-mode))
(display-time-mode t)
(delete-selection-mode t)

(if (not (eq window-system nil))
    (require 'mine-color-theme))

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

(set-face-attribute 'whitespace-line nil
                    :background "#888a85"
                    :foreground "yellow"
                    :weight 'bold)

;; face for Tabs
(set-face-attribute 'whitespace-tab nil
                    :background "#888a85"
                    :foreground "yellow"
                    :weight 'bold)

(setq whitespace-line-column 80
      whitespace-style '(face tabs trailing lines-tail))

(setq whitespace-mode-inhibit-modes-list '(fundamental-mode
                                           eshell-mode
                                           shell-mode
                                           dired-mode
                                           erc-mode))

(defadvice linum-on (around linum-on-inhibit-for-modes)
  "Inhibit load of linum-mode for specified major modes."
  (unless (member major-mode whitespace-mode-inhibit-modes-list)
    ad-do-it))

(defun whitespace-turn-on-if-enabled ()
  (when (cond
         ((eq whitespace-global-modes t))
         ((listp whitespace-global-modes)
          (if (eq (car-safe whitespace-global-modes) 'not)
              (not (memq major-mode (cdr whitespace-global-modes)))
            (memq major-mode whitespace-global-modes)))
         (t nil))
    (let (inhibit-quit)
      ;; Don't turn on whitespace mode if...
      (or
       ;; ...we don't have a display (we're running a batch job)
       noninteractive
       ;; ...or if the buffer is invisible (name starts with a space)
       (eq (aref (buffer-name) 0) ?\ )
       ;; ...or if the buffer is temporary (name starts with *)
       (and (eq (aref (buffer-name) 0) ?*)
            ;; except the scratch buffer.
            (not (string= (buffer-name) "*scratch*")))
       (member major-mode whitespace-mode-inhibit-modes-list)
       ;; Otherwise, turn on whitespace mode.
       (whitespace-turn-on)))))

(global-whitespace-mode 1)

(provide 'mine-customizations)
