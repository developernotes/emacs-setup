(defvar current-theme nil)
(defvar next-theme nil)

(setq dark-themes
      '(brin
        dorsey
        fogus
        graham
        granger
        hickey
        ir-black
        junio
        monokai
        odersky
        solarized-dark
        spolsky
        tango-2
        tomorrow-night
        underwater
        wilson))

(setq light-themes
      '(mccarthy
        standard-light))

(defun mine-light-color-theme ()
  (interactive)
  (set-theme 'standard-light))

(defun mine-dark-color-theme ()
  (interactive)
  (set-theme 'tango-2))

(defun set-random-theme (themes)
   (setq next-theme current-theme)
   (while (string= current-theme next-theme)
     (setq next-theme (nth (random (length themes)) themes)))
   (set-theme next-theme))

(defun set-random-dark-theme ()
  (interactive)
  (set-random-theme dark-themes))

(defun set-random-light-theme ()
  (interactive)
  (set-random-theme light-themes))

(defun set-theme (theme)
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
                             (mapcar 'symbol-name
                                     (custom-available-themes))))))
  (if current-theme
      (disable-theme current-theme))
  (load-theme theme t)
  (setq current-theme theme)
  (message (format "Set theme to %s" theme)))

(when (string-match "^24\." emacs-version)
  (add-to-list 'custom-theme-load-path (concat emacs-root "site-lisp/themes")))

(mine-light-color-theme)

(provide 'init-color-theme)
