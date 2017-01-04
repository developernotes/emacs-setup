(add-hook 'c-mode-common-hook
          (lambda ()
            (local-set-key (kbd "C-c f") 'ff-find-other-file)))

(add-hook 'objc-mode-hook
          (lambda ()
            (set (make-local-variable 'cc-other-file-alist)
                 '(("\\.m\\'" (".h")) ("\\.h\\'" (".m" ".c" ".cpp"))))))

(add-hook 'c-mode-hook
          (lambda ()
            (set (make-local-variable 'compile-command)
                 (format "make -f %sMakefile " (locate-dominating-file default-directory "Makefile")))))

(add-hook 'c-mode-common-hook
          (lambda ()
            (define-key c-mode-base-map (kbd "C-x c") 'compile)))

(provide 'init-c)
