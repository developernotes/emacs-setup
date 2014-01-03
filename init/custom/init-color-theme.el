(defvar current-theme nil)

(defun mine-light-color-theme ()
  (interactive)
  (set-theme 'standard-light))

(defun mine-dark-color-theme ()
  (interactive)
  (set-theme 'tango-2))

(defun set-theme (theme)
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
			     (mapcar 'symbol-name
				     (custom-available-themes))))
    nil nil))
  (if current-theme
      (disable-theme current-theme))
  (load-theme theme t)
  (setq current-theme theme))

(when (string-match "^24\." emacs-version)
  (add-to-list 'custom-theme-load-path (concat emacs-root "site-lisp/themes")))

(mine-light-color-theme)

(provide 'init-color-theme)
