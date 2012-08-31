
(require 'multi-eshell)

(add-to-list 'auto-mode-alist '("\\.zsh$" . shell-script-mode))

(when (eq system-type 'windows-nt)
  (require 'bat-mode)
  (setq auto-mode-alist
        (append
         (list (cons "\\.[bB][aA][tT]$" 'bat-mode))
         (list (cons "CONFIG\\."   'bat-mode))
         (list (cons "AUTOEXEC\\." 'bat-mode))
         auto-mode-alist)))

(global-set-key (kbd "C-c t") 'multi-eshell-switch)
(global-set-key (kbd "C-c b") 'multi-eshell-go-back)

(defconst pcmpl-git-commands
  '( "ad" "add" "bisect" "branch" "checkout" "clone"
     "co" "commit" "diff" "fetch" "grep"
     "init" "log" "merge" "mv" "pull" "push" "rebase"
     "reset" "rm" "show" "status" "tag" "to-push")
  "List of `git' commands")

(defvar pcmpl-git-ref-list-cmd "git for-each-ref refs/ --format='%(refname)'"
  "The `git' command to run to get a list of refs")

(defun pcmpl-git-get-refs (type)
  "Return a list of `git' refs filtered by TYPE"
  (with-temp-buffer
    (insert (shell-command-to-string pcmpl-git-ref-list-cmd))
    (goto-char (point-min))
    (let ((ref-list))
      (while (re-search-forward (concat "^refs/" type "/\\(.+\\)$") nil t)
        (add-to-list 'ref-list (match-string 1)))
      ref-list)))

(defun pcomplete/git ()
  "Completion for `git'"
  (pcomplete-here* pcmpl-git-commands)
  (cond
   ((pcomplete-match (regexp-opt '("add" "rm")) 1)
    (while (pcomplete-here (pcomplete-entries))))
   ((pcomplete-match (regexp-opt '("checkout" "co"))  1)
    (pcomplete-here* (pcmpl-git-get-refs "heads")))))

(add-hook 'eshell-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-r") 'eshell-previous-matching-input)))

(defun eshell/clear ()
  "Clears the shell buffer"
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(defun eshell/expand-paths (paths)
  (mapconcat (lambda (path)
               (expand-file-name path)) paths ":"))

(defun eshell/branch ()
  "Return the current git branch, if applicable."
  (let ((branch (shell-command-to-string "git branch --no-color 2> /dev/null")))
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
 eshell-last-dir-ring-size 10
 eshell-last-dir-unique t
 multi-eshell-shell-function '(eshell)
 eshell-aliases-file (concat emacs-root "mine/mine-eshell-alias")
 eshell-rc-script (concat emacs-root "mine/mine-eshell-profile")
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
