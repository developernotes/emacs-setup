
(defun mine-light-color-theme ()
  (interactive)
    (load-theme 'tomorrow t))

(defun mine-dark-color-theme ()
  (interactive)
  (load-theme 'monokai t))

(when (string-match "^24\." emacs-version)
  (add-to-list 'custom-theme-load-path (concat emacs-root "site-lisp/themes")))

(provide 'mine-color-theme)
