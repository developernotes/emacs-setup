
(add-path "site-lisp/scala")
(add-path "site-lisp/ensime/src/main/elisp")

(require 'scala-mode-auto)
(require 'ensime)

(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
(add-hook 'scala-mode-hook
          '(lambda ()
             (local-set-key (kbd "RET") 'newline-and-indent)
             (c-subword-mode t)
             (local-set-key (kbd "C-c C-b") 'scala-eval-buffer)
             (local-set-key (kbd "C-c C-r") 'scala-eval-region)
             (local-set-key (kbd "C-M-x") 'scala-eval-definition)))

(provide 'mine-scala)