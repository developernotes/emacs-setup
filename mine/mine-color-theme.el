
(add-path "site-lisp/themes")
(add-path "site-lisp/color-theme-6.6.0")

(require 'color-theme)
(require 'color-theme-zen-and-art)

(color-theme-initialize)

(global-hl-line-mode t)

(defun mine-light-color-theme ()
  (interactive)
  (color-theme-standard)
  (set-face-background 'region "#999966")
  (set-face-background 'hl-line "#DBDBDB"))

(defun mine-dark-color-theme ()
  (interactive)
  (color-theme-zen-and-art)
  (set-face-background 'hl-line "#333638")
  (set-face-background 'region "#999966")
  (set-face-foreground 'font-lock-comment-face "#d8e5c5"))

(mine-dark-color-theme)

(provide 'mine-color-theme)
