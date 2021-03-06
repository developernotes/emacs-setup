(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://www.melpa.org/packages/") t)
(package-initialize)

(setq required-packages
      '(ag
        atom-dark-theme
        browse-kill-ring
        csharp-mode
        fill-column-indicator
        flx
        flx-ido
        grizzl
        groovy-mode
        haskell-mode
        ibuffer-git
        ido-vertical-mode
        js2-mode
        log4j-mode
        magit
        markdown-mode
        monokai-theme
        org
        paredit
        racket-mode
        scratch
        solarized-theme
        smex
        slime
        switch-window
        undo-tree
        yasnippet))

(defun package-install-required-packages ()
  "Install packages defined within the variable `required-packages'."
  (interactive)
  (package-refresh-contents)
  (mapc (lambda (package)
          (unless (package-installed-p package)
            (message (format "Installing %s" package))
            (package-install package)))
        required-packages))

(provide 'init-package)
