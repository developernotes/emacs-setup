(provide 'mine-dependencies)

;; smex
(add-path "site-lisp/smex")
(require 'smex)
(eval-after-load "init.el" '(smex-initialize))
(smex-auto-update)

