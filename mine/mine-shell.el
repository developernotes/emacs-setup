
(require 'multi-eshell)

(defvar mine-x-cut-program)
(defvar mine-x-paste-program)

(add-to-list 'auto-mode-alist '("\\.zsh$" . shell-script-mode))
(when (eq system-type 'windows-nt)
  (require 'bat-mode)
  (setq auto-mode-alist
        (append
         (list (cons "\\.[bB][aA][tT]$" 'bat-mode))
         (list (cons "CONFIG\\."   'bat-mode))
         (list (cons "AUTOEXEC\\." 'bat-mode))
         auto-mode-alist)))

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

(setq multi-eshell-shell-function '(eshell)
      eshell-aliases-file (concat emacs-root "mine/mine-eshell-alias"))

(global-set-key (kbd "C-c t") 'multi-eshell-switch)
(global-set-key (kbd "C-c b") 'multi-eshell-go-back)

(add-hook 'shell-mode-hook 'n-shell-mode-hook)

(defun xsel-cut-function (text &optional push)
  (with-temp-buffer
    (insert text)
    (call-process-region (point-min) (point-max)
                         mine-x-cut-program nil 0 nil "" "")))

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
    (funcall 'man command))

   ;; send other commands to the default handler.
   (t (comint-simple-send proc command))))

(defun eshell/clear ()
  "Clears the shell buffer"
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(defun eshell/load-environment-path ()
  "Sets `eshell-path-env' to the value of the PATH environment variable"
  (interactive)
  (let* ((shell-command "$SHELL -i -c 'echo $PATH'")
        (path (string-replace "\n" ""(shell-command-to-string shell-command))))
    (setq eshell-path-env path)))

(eval-after-load "eshell" '(eshell/load-environment-path))

(defun eshell/branch ()
  "Return the current git branch, if applicable."
  (let ((branch (shell-command-to-string "git branch 2> /dev/null")))
    (string-match "^\\* \\(.*\\)" branch)
    (if (equal branch "")
        ""
      (match-string 1 branch))))

(defun eshell/extract (file)
  (let ((command (some (lambda (x)
                         (if (string-match-p (car x) file)
                             (cadr x)))
                       '((".*\.tar.bz2" "tar xjf")
                         (".*\.tar.gz" "tar xzf")
                         (".*\.bz2" "bunzip2")
                         (".*\.rar" "unrar x")
                         (".*\.gz" "gunzip")
                         (".*\.tar" "tar xf")
                         (".*\.tbz2" "tar xjf")
                         (".*\.tgz" "tar xzf")
                         (".*\.zip" "unzip")
                         (".*\.Z" "uncompress")
                         (".*" "echo 'Could not extract the file:'")))))
    (eshell-command-result (concat command " " file))))

(setq
 eshell-hist-ignoredups t
 eshell-history-size 10
 eshell-prompt-function
 (lambda ()
   (let ((prompt (eshell/pwd))
         (home-dir (expand-file-name "~"))
         (branch (eshell/branch)))
     (setq prompt (string-replace (expand-file-name "~") "~" prompt))
     (concat prompt
             (if (or (equal branch nil)
                     (equal branch ""))
                 ""
               (format " (%s)" branch))
             (if (= (user-uid) 0) " # " " $ ")))))

(provide 'mine-shell)
