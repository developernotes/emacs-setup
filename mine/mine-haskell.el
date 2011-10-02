
(add-path "site-lisp/haskell-mode-2.8.0")
;;(load "haskell-site-file")

(autoload 'haskell-mode "haskell-mode" "Mode for editing haskell files" t)
(setq auto-mode-alist
      (append '(("\\.hs" . haskell-mode)
                ("\\.lhs" . haskell-mode))
                auto-mode-alist))


(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'font-lock-mode)
(add-hook 'haskell-mode-hook '(lambda ()
                                (setq haskell-font-lock-symbols 'unicode)
                                (define-key haskell-mode-map (kbd "C-c i") 'switch-to-haskell)
                                (define-key haskell-mode-map (kbd "C-c b") 'haskell-run-buffer-in-runghc)))

(defun haskell-run-buffer-in-runghc()
  (interactive)
  (when (get-buffer "*runghc*")
    (kill-buffer "*runghc*"))
  (let ((args (list (buffer-file-name (current-buffer)))))
    (apply 'make-comint "runghc" "runghc" nil args)
    (display-buffer "*runghc*")))

(provide 'mine-haskell)
