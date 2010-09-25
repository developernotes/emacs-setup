
(add-path "site-lisp/showoff-mode")

(require 'showoff-mode)
(add-to-list 'auto-mode-alist '("\\.md$" . showoff-mode))

(provide 'mine-showoff)