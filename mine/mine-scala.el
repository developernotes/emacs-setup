
(add-path "site-lisp/scala")
(add-path "site-lisp/ensime/src/main/elisp")

(require 'scala-mode-auto)
(require 'ensime)

(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

(provide 'mine-scala)