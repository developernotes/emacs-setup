
(put 'dired-find-alternate-file 'disabled nil)

(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map (kbd "<return>") 'dired-find-alternate-file)
            (define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))))

(setq dired-auto-revert-buffer t)

(defun dired-open-marked-files ()
  "Opens all marked files"
  (interactive)
  (dolist (file (dired-get-marked-files))
    (find-file file)))

(provide 'mine-dired)
