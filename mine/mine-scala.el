
(add-path "site-lisp/scala")
(require 'scala-mode-auto)

(add-path "site-lisp/ensime/src/main/elisp")
(require 'ensime)

(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

(provide 'mine-scala)