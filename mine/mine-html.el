
(autoload 'textile-mode "textile-mode" "Mode for editing textile documents" nil)
(add-to-list 'auto-mode-alist '("\\.less$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.textile$" . textile-mode))
(add-to-list 'auto-mode-alist '("\\.xhtml$" . html-mode))

(provide 'mine-html)
