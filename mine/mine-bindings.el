
(global-set-key "\C-cv" 'mine-toggle-transparency)
(global-set-key "\C-cw" 'toggle-truncate-lines)

;; extend search to show occurances
(define-key isearch-mode-map (kbd "C-o")
  (lambda ()
    (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string
               (regexp-quote isearch-string))))))

;; smex
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c M-x") 'smex-update-and-run)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; dired
(global-set-key "\C-x\C-d" 'dired)

(provide 'mine-bindings)