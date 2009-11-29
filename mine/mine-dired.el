
(provide 'mine-dired)

(defun dired-open-files ()
  "Opens all marked files"
  (interactive)
  (dolist (file (dired-get-marked-files))
    (find-file file)))

(global-set-key "\C-x\C-d" 'dired)