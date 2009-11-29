
;; smex
(add-path "site-lisp/smex")
(require 'smex)
(eval-after-load "init.el" '(smex-initialize))
(smex-auto-update)

(provide 'mine-dependencies)