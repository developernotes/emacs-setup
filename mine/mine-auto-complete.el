
(add-path "/site-lisp/auto-complete")
(require 'auto-complete-config)

(add-to-list 'ac-dictionary-directories "~/.emacs.d/site-lisp/auto-complete/ac-dict")
(ac-config-default)

(provide 'mine-auto-complete)