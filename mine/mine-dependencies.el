
(add-path "site-lisp/smex")
(add-path "site-lisp/yasnippet-0.6.1c")

(require 'yasnippet)

(autoload 'smex "smex.el" "smex" t)
(autoload 'smex-initialize "smex.el" "smex" t)

(defun load-smex-jit ()
  "Rebind M-x after loading smex"
  (interactive)
  (or (boundp 'smex-cache)
      (smex-initialize))
  (global-set-key (kbd "M-x") 'smex)
  (smex))

(global-set-key (kbd "M-x") 'load-smex-jit)

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

(add-hook 'org-mode-hook
          '(lambda ()
             (yas/reload-all)
             (yas/minor-mode)))

(add-hook 'ruby-mode-hook
          '(lambda ()
             (yas/reload-all)
             (yas/minor-mode)))

(provide 'mine-dependencies)
