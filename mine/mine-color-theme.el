
(add-path "site-lisp/themes")
(add-path "site-lisp/color-theme-6.6.0")

(require 'color-theme)
(require 'color-theme-merbivore)

(color-theme-initialize)

(global-hl-line-mode t)

(defun mine-light-color-theme ()
  (interactive)
  (color-theme-standard)
  (set-face-background 'region "#ffec8b")
  (set-face-background 'hl-line "#b4eeb4")
  (set-face-foreground 'font-lock-doc-face "#d2691e")
  (custom-set-faces    '(outline-2 ((t (:foreground "#006400" :bold t))))))

(defun mine-dark-color-theme ()
  (interactive)
  (color-theme-merbivore)
  (set-face-background 'hl-line "#3b3b3b")
  (set-face-background 'region "#999966")
  (set-face-foreground 'font-lock-comment-face "#d8e5c5")
  (custom-set-faces    '(outline-2 ((t (:foreground "#a5f26e" :bold t))))))

(mine-dark-color-theme)

(provide 'mine-color-theme)
