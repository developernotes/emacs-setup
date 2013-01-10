
(add-hook 'c-mode-common-hook
          (lambda ()
            (local-set-key (kbd "C-c f") 'ff-find-other-file)))

(add-hook 'objc-mode-hook
          (lambda ()
            (set (make-local-variable 'cc-other-file-alist)
                 '(("\\.m\\'" (".h")) ("\\.h\\'" (".m" ".c" ".cpp"))))))

(provide 'mine-c)
