
(add-path "site-lisp/smex")

(require 'smex)
(require 'rect-mark)

(eval-after-load "init.el" '(smex-initialize))
(smex-auto-update)

;; rect-mark
(global-set-key (kbd "C-x r C-SPC") 'rm-set-mark)
(global-set-key (kbd "C-x r C-x") 'rm-exchange-point-and-mark)
(global-set-key (kbd "C-x r C-w") 'rm-kill-region)
(global-set-key (kbd "C-x r M-w") 'rm-kill-ring-save)
(autoload 'rm-set-mark "rect-mark"
	"Set mark for rectangle." t)
(autoload 'rm-exchange-point-and-mark "rect-mark"
	"Exchange point and mark for rectangle." t)
(autoload 'rm-kill-region "rect-mark"
	"Kill a rectangular region and save it in the kill ring." t)
(autoload 'rm-kill-ring-save "rect-mark"
	"Copy a rectangular region to the kill ring." t)

;; yasnippet
(add-path "site-lisp/yasnippet-0.6.1c")
(require 'yasnippet)
(yas/initialize)
(setq yas/root-directories
      (list (concat emacs-root "site-lisp/yasnippet-0.6.1c/snippets/")
            (concat emacs-root "mysnippets/")))
(mapc 'yas/load-directory yas/root-directories)

(setq yas/prompt-functions '(yas/dropdown-prompt
                             yas/ido-prompt
                             yas/no-prompt))
				
(provide 'mine-dependencies)