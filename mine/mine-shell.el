
(defvar mine-x-cut-program)
(defvar mine-x-paste-program)

(add-to-list 'auto-mode-alist '("\\.zsh$" . shell-script-mode))

(when (eq window-system nil)
  (case system-type
    ('cygwin
     (setq mine-x-cut-program "putclip"
           mine-x-paste-program "getclip"))
    ('darwin
     (setq mine-x-cut-program "pbcopy"
           mine-x-paste-program "pbpaste")))
  (setq x-select-enable-clipboard t
        interprogram-cut-function 'xsel-cut-function
        interprogram-paste-function 'xsel-paste-function))

(setq multi-shell-command "zsh"
      multi-shell-revert-window-after-complete nil
      eshell-aliases-file (concat emacs-root "mine/mine-eshell-alias"))

(global-set-key (kbd "C-c t") '(lambda () (interactive) (eshell)))
(global-set-key (kbd "C-c T") '(lambda () (interactive) (eshell "a")))

(add-hook 'shell-mode-hook 'n-shell-mode-hook)

(defun xsel-cut-function (text &optional push)
  (with-temp-buffer
    (insert text)
    (call-process-region (point-min) (point-max) mine-x-cut-program nil 0 nil "" "")))

(defun xsel-paste-function ()
  (let ((output (shell-command-to-string mine-x-paste-program)))
    (unless (string= (car kill-ring) output)
      output)))

(defun n-shell-mode-hook ()
  "shell mode customizations."
  (local-set-key '[up]          'comint-previous-input)
  (local-set-key '[down]        'comint-next-input)
  (local-set-key '[(shift tab)] 'comint-next-matching-input-from-input)
  (setq comint-input-sender     'n-shell-simple-send))

(defun n-shell-simple-send (proc command)
  "Various commands pre-processing before sending to shell."
  (cond
   ;; checking for clear command and execute it.
   ((string-match "^[ \t]*clear[ \t]*$" command)
    (comint-send-string proc "\n")
    (erase-buffer))

   ;; checking for man command and execute it.
   ((string-match "^[ \t]*man[ \t]*" command)
    (comint-send-string proc "\n")
    (setq command (replace-regexp-in-string "^[ \t]*man[ \t]*" "" command))
    (setq command (replace-regexp-in-string "[ \t]+$" "" command))
    ;;(message (format "command %s command" command))
    (funcall 'man command))

   ;; send other commands to the default handler.
   (t (comint-simple-send proc command))))

(defun eshell/clear ()
  "Clears the shell buffer"
  (interactive)
  (let ((inhibit-read-only t))
	(delete-region (point-min) (point-max))))

(provide 'mine-shell)
