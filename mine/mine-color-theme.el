
(add-path "site-lisp/themes")
(add-path "site-lisp/color-theme-6.6.0")

(require 'color-theme)
(color-theme-initialize)

(require 'color-theme-zen-and-art)
(color-theme-zen-and-art)

(provide 'mine-color-theme)
