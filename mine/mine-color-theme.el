
(add-path "site-lisp/themes")
(add-path "site-lisp/color-theme-6.6.0")

(require 'color-theme)
(require 'color-theme-zen-and-art)

(color-theme-initialize)
(color-theme-zen-and-art)

;; highlight current line
(global-hl-line-mode t)

;; dark color current line
(set-face-background 'hl-line "#333638")
(set-face-background 'region "#999966")

;; light color current line
;;(set-face-background 'hl-line "#DBDBDB")

(provide 'mine-color-theme)
