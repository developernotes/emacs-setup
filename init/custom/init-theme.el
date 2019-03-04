(defvar current-theme nil)
(defvar next-theme nil)
(defvar theme-path (concat emacs-root "site-lisp/themes"))

(setq dark-themes
      '(atom-dark
        badwolf
        flatland
        gruvbox
        ir-black
        monokai
        noctilux
        smyx
        solarized-dark
        spacegray
        tango-2
        tomorrow-night
        underwater))

(setq light-themes
      '(soft-stone
        solarized-light
        standard-light
        standard-reeder
        standard-stone))

(defun set-light-theme ()
  (interactive)
  (set-theme 'standard-light))

(defun set-dark-theme ()
  (interactive)
  (set-theme 'badwolf))

(defun set-random-theme ()
  (interactive)
  (set-random-theme-from (append dark-themes light-themes)))

(defun set-random-theme-from (themes)
   (setq next-theme current-theme)
   (while (string= current-theme next-theme)
     (setq next-theme (nth (random (length themes)) themes)))
   (set-theme next-theme))

(defun set-random-dark-theme ()
  (interactive)
  (set-random-theme-from dark-themes))

(defun set-random-light-theme ()
  (interactive)
  (set-random-theme-from light-themes))

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


(when (not (member theme-path custom-theme-load-path))
  (add-to-list 'custom-theme-load-path theme-path))

(set-light-theme)

(provide 'init-theme)
