
(add-path "site-lisp/smex")
(add-path "site-lisp/yasnippet-0.6.1c")

(require 'smex)
(require 'yasnippet)

(smex-initialize)

;; yasnippet
(yas/initialize)
(setq yas/root-directories
      (list (concat emacs-root "site-lisp/yasnippet-0.6.1c/snippets/")
            (concat emacs-root "mysnippets/")))
(mapc 'yas/load-directory yas/root-directories)

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
				
(provide 'mine-dependencies)
