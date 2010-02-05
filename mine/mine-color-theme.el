
(add-path "site-lisp/themes")
(add-path "site-lisp/color-theme-6.6.0")

(require 'color-theme)
(color-theme-initialize)

(require 'color-theme-twilight)
(color-theme-twilight)

(provide 'mine-color-theme)
