;; (setq geiser-active-implementations '(guile racket))

;; (autoload 'enable-paredit-mode "paredit"
;;   "Turn on pseudo-structural editing of Lisp code." t)

;; (add-hook 'scheme-mode-hook 'enable-paredit-mode)

;; (defun geiser-eval-buffer (&optional and-go raw nomsg)
;;   "Eval the current buffer in the Geiser REPL.
;; With prefix, goes to the REPL buffer afterwards (as
;; `geiser-eval-buffer-and-go')"
;;   (interactive "P")
;;   (let ((start (point-min))
;;         (end (point-max)))
;;     (save-restriction
;;       (narrow-to-region start end)
;;       (check-parens))
;;     (geiser-debug--send-region nil
;;                                start
;;                                end
;;                                (and and-go 'geiser--go-to-repl)
;;                                (not raw)
;;                                nomsg)))

;; (defun geiser-eval-buffer-and-go ()
;;   "Eval the current buffer in the Geiser REPL and visit it afterwads."
;;   (interactive)
;;   (geiser-eval-buffer t))

;; (eval-after-load 'geiser-mode
;;   '(progn
;;      (define-key geiser-mode-map (kbd "C-c C-b") 'geiser-eval-buffer)
;;      (define-key geiser-mode-map (kbd "C-c M-b") 'geiser-eval-buffer-and-go)))
