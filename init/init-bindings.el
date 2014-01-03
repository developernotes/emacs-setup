
;; misc
(global-set-key "\r"            'newline-and-indent)
(global-set-key "\C-a"          'beginning-of-line-or-back-to-indention)
(global-set-key "\C-cv"         'mine-toggle-transparency)
(global-set-key "\C-cw"         'toggle-truncate-lines)
(global-set-key "\C-cs"         'helm-imenu)
(global-set-key "\C-cp"         'mine-goto-symbol-at-point)
(global-set-key "\M-g"          'goto-line)
(global-set-key "\C-c\o"        'cleanup-buffer-or-region)
(global-set-key "\C-x\C-p"      'find-file-at-point)
(global-set-key "\C-x\C-d"      'dired)
(global-set-key "\C-c2"         'swap-windows)
(global-set-key "\C-c3"         'rotate-window-split)
(global-set-key (kbd "C-c r")   'replace-string)
(global-set-key (kbd "C-c e b") 'eval-buffer)
(global-set-key (kbd "C-c e r") 'eval-region)
(global-set-key (kbd "C-c f l") 'find-library)
(global-set-key (kbd "C-c f f") 'find-function)
(global-set-key (kbd "M-j")     (lambda () (interactive) (join-line -1)))
(global-set-key (kbd "C-c C-f") 'ag)
(global-set-key (kbd "C-x 0") 'delete-other-window)



;; extend search to show occurances
(define-key isearch-mode-map (kbd "C-o")
  (lambda ()
    (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string
               (regexp-quote isearch-string))))))

(provide 'init-bindings)
