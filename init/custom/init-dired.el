(put 'dired-find-alternate-file 'disabled nil)

(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map (kbd "M->") 'end-of-dired-buffer)
            (define-key dired-mode-map (kbd "M-<") 'beginning-of-dired-buffer)
            (define-key dired-mode-map (kbd "<return>")
              'dired-find-alternate-file)
            (define-key dired-mode-map (kbd "^")
              (lambda () (interactive) (find-alternate-file "..")))))

(autoload (quote dired-jump) "dired" "" t nil)
(define-key global-map "\C-x\C-j" 'dired-jump)

(setq dired-auto-revert-buffer t)

(defun dired-open-marked-files ()
  "Opens all marked files"
  (interactive)
  (dolist (file (dired-get-marked-files))
    (find-file file)))

(defun beginning-of-dired-buffer ()
  "Move point to first line containing a filename"
  (interactive)
  (goto-char (point-min))
  (next-line 4)
  (dired-move-to-filename))

(defun end-of-dired-buffer ()
  "Move point to last line of buffer, place cursor on filename"
  (interactive)
  (goto-char (point-max))
  (previous-line)
  (dired-move-to-filename))

(provide 'init-dired)
