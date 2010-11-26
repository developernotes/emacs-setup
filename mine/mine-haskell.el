
(add-path "site-lisp/haskell-mode-2.8.0")
(load "haskell-site-file")

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'font-lock-mode)
(add-hook 'haskell-mode-hook '(lambda ()
                                (setq haskell-font-lock-symbols 'unicode)
                                (define-key haskell-mode-map (kbd "C-c r") 'haskell-run-buffer-in-runghc)))

(defun haskell-run-buffer-in-runghc()
  (interactive)
  (when (get-buffer "*runghc*")
    (kill-buffer "*runghc*"))
  (let ((args (list (buffer-file-name (current-buffer)))))
    (apply 'make-comint "runghc" "runghc" nil args)
    (display-buffer "*runghc*")))

(provide 'mine-haskell)