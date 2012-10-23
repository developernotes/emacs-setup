
(add-path "site-lisp/smex")
(add-path "site-lisp/yasnippet-0.6.1c")

(require 'yasnippet)

(setq yas/prompt-functions '(yas/dropdown-prompt
                             yas/ido-prompt
                             yas/no-prompt))

(defun yas/advise-indent-function (function-symbol)
  (eval `(defadvice ,function-symbol (around yas/try-expand-first activate)
           ,(format
             "Try to expand a snippet before point, then call `%s' as usual"
             function-symbol)
           (let ((yas/fallback-behavior nil))
             (unless (and (interactive-p)
                          (yas/expand))
               ad-do-it)))))

(yas/advise-indent-function 'ruby-indent-line)

(yas/load-directory (concat emacs-root "mysnippets/"))

(provide 'mine-dependencies)
