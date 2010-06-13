
(defun swap-windows ()
  "If you have 2 windows, it swaps them."
  (interactive)
  (cond ((/= (count-windows) 2)
         (message "You need exactly 2 windows to do this."))
        (t
         (let* ((w1 (first (window-list)))
                (w2 (second (window-list)))
                (b1 (window-buffer w1))
                (b2 (window-buffer w2))
                (s1 (window-start w1))
                (s2 (window-start w2)))
           (set-window-buffer w1 b2)
           (set-window-buffer w2 b1)
           (set-window-start w1 s2)
           (set-window-start w2 s1))))
  (other-window 1))

(defun clean-up-buffer-or-region ()
  "Untabifies, indents and deletes trailing whitespace from buffer or region."
  (interactive)
  (save-excursion
    (unless (region-active-p)
      (mark-whole-buffer))
    (untabify (region-beginning) (region-end))
    (indent-region (region-beginning) (region-end))
    (save-restriction
      (narrow-to-region (region-beginning) (region-end))
      (delete-trailing-whitespace))))

(defun append-to-environment-variable (variable path)
	(setenv variable (concat (format "%s:%s" (getenv variable) path)))) 

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