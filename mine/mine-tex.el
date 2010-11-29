
(add-path "site-lisp/auctex")
(add-path "site-lisp/auctex/site-start.d")

(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)
(add-hook 'TeX-mode-hook '(lambda ()
                            (TeX-PDF-mode 1)
                            ))

(provide 'mine-tex)
