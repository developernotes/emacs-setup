(when (string-match "^24\." emacs-version)
  (progn
    (require 'package)
    (package-initialize)
    (add-to-list 'package-archives
                 '("melpa" . "http://melpa.milkbox.net/packages/") t)))

(setq required-packages
      '(ag
        browse-kill-ring
        flx
        flx-ido
        geiser
        git-commit-mode
        git-rebase-mode
        grizzl
        helm
        haskell-mode
        ibuffer-git
	ido-vertical-mode
        js2-mode
        log4j-mode
        magit
        markdown-mode
        merlin
        monokai-theme
        org
        paredit
        projectile
        racket-mode
        scratch
        smyx
        solarized-theme
        smex
        sublime-themes
        switch-window
        undo-tree
        yasnippet))

(defun install-required-packages ()
  "Install packages defined within the variable `required-packages'."
  (interactive)
  (package-refresh-contents)
  (mapc (lambda (package)
          (unless (package-installed-p package)
            (message (format "Installing %s" package))
            (package-install package)))
        required-packages))

(provide 'init-package)
