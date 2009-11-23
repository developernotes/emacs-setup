(provide 'mine-bindings)

(global-set-key "\C-cv" 'mine-toggle-transparency)
(global-set-key "\C-cw" 'toggle-truncate-lines)

;; extend search to show occurances
(define-key isearch-mode-map (kbd "C-o")
  (lambda ()
    (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string
               (regexp-quote isearch-string))))))
