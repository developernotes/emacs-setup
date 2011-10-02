
(add-path "site-lisp/showoff-mode")

;;(require 'showoff-mode)
(autoload 'showoff-mode "showoff-mode" "Mode for editing showoff documents" t)
(add-to-list 'auto-mode-alist '("\\.md$" . showoff-mode))

(provide 'mine-showoff)
