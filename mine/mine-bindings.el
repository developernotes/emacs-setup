
;; misc
(global-set-key "\C-cv" 'mine-toggle-transparency)
(global-set-key "\C-cw" 'toggle-truncate-lines)
(global-set-key "\C-cs" 'ido-goto-symbol)
(global-set-key "\C-cp" 'mine-goto-symbol-at-point)
(global-set-key "\M-g"  'goto-line)
(global-set-key "\C-cn" 'clean-up-buffer-or-region)

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
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; dired
(global-set-key "\C-x\C-d" 'dired)

;; window resizing
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; file navigation
(global-set-key "\C-x\C-p" 'find-file-at-point)

;; diff the current buffer with the file contents
(global-set-key (kbd "C-c d")
   (lambda () (interactive) (diff-buffer-with-file (current-buffer))))

(provide 'mine-bindings)
