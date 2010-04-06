
(defun kill-all-buffers ()
  "kill all buffers, leaving *scratch* only"
  (interactive)
  (mapcar (lambda (x) (kill-buffer x))
					(buffer-list))
  (delete-other-windows))

(defun kill-all-buffers-but-current ()
	"kills all buffers except your current buffer"
	(interactive)
	(mapcar (lambda (x) (kill-buffer x))
					(remove (current-buffer) (buffer-list)))
	(delete-other-windows))

(defun tail ()
	"tail the file loaded within the current buffer"
	(interactive)
	(auto-revert-tail-mode))

(defun string-replace (old-value new-value source)
  "replace old-value with new-value from source"
  (with-temp-buffer
    (insert source)
    (goto-char (point-min))
    (while (search-forward old-value nil t)
      (replace-match new-value nil t))
    (buffer-substring (point-min) (point-max))))

(defun rename-file-and-buffer ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (cond ((get-buffer new-name)
               (message "A buffer named '%s' already exists!" new-name))
              (t
               (rename-file name new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)))))))

(provide 'mine-defuns)