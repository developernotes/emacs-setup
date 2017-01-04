(defun center-selected-frame ()
  "Centers the current selected frame within the desktop."
  (interactive)
  (let* ((center-x (/ (nth 3 (assq 'geometry (car (display-monitor-attributes-list)))) 2))
         (center-y (/ (nth 4 (assq 'geometry (car (display-monitor-attributes-list)))) 2))
         (x (- center-x
               (/ (frame-pixel-width (selected-frame)) 2)))
         (y (- center-y
               (/ (frame-pixel-height (selected-frame)) 2))))
    (set-frame-position (selected-frame) x y)))

(defun beginning-of-line-or-back-to-indention ()
  (interactive)
  "This goes to back to indention or if already there beginning of line"
  (let ((previous-point (point)))
    (back-to-indentation)
    (if (equal previous-point (point))
        (beginning-of-line))))

(defun mine-server-start ()
  "Starts the emacs server, navigating to either a directory or file
   if set via environment variables"
  (server-force-delete)
  (server-start)
  (let ((dir  (getenv "EOPEN_DIR"))
        (file (getenv "EOPEN_FILE")))
    (if dir
        (cd dir))
    (if file
        (find-file file))))

(defun dictionary-lookup (word)
  (interactive "sLookup word? ")
  (shell-command (format "open dict://%s" word)))

(defun create-tags (directory)
  "Create tags file."
  (interactive "DDirectory: ")
  (async-shell-command
   (format "%s -f %sTAGS -e -R %s" "ctags" directory
           (directory-file-name directory))))

(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer-or-region ()
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

(provide 'init-defuns)
