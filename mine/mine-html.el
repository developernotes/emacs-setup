(require 'textile-mode)

(add-to-list 'auto-mode-alist '("\\.less$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.textile$" . textile-mode))

(provide 'mine-html)