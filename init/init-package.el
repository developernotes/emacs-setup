(when (string-match "^24\." emacs-version)
  (progn
    (require 'package)
    (package-initialize)
    (add-to-list 'package-archives
                 '("melpa" . "http://melpa.milkbox.net/packages/") t)))

(setq required-packages
      '(ag 
        browse-kill-ring 
        geiser 
        git-commit-mode 
        git-rebase-mode
        helm
        js2-mode
        log4j-mode
        magit 
        markdown-mode
        org 
        paredit 
        scratch 
        smex 
        switch-window
        undo-tree 
        yasnippet))

(defun install-required-packages ()
  "Install packages defined within `required-packages'."
  (interactive)
  (package-refresh-contents)
  (mapc (lambda (package)
          (unless (package-installed-p package)
            (message (format "Installing %s" package))
            (package-install package)))
        required-packages))

(provide 'init-package)
