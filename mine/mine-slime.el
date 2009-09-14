
(provide 'mine-slime)

;; Lisp system
(setq inferior-lisp-program "sbcl") 

;; Slime path
(add-path "vendor/slime-2009-09-06")
    (require 'slime-autoloads)
    (slime-setup)



