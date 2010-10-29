
(defun toggle-line-numbers ()
  "Enable or disable line numbers globally"
  (interactive)
  (if global-linum-mode
      (global-linum-mode -1)
    (global-linum-mode 1)))

(defun toggle-highlight-line ()
  "Enable or disable highlighting of current line"
  (interactive)
  (if global-hl-line-mode
      (global-hl-line-mode -1)
    (global-hl-line-mode 1)))

(defun beginning-of-line-or-back-to-indention ()
  (interactive)
  "This goes to back to indention or if already there beginning of line"
  (let ((previous-point (point)))
    (back-to-indentation)
    (if (equal previous-point (point))
        (beginning-of-line))))

(defun mine-server-start ()
  "Starts the emacs server, navigating to either a directory or file if set via environment variables"
  (server-start)
  (let ((dir  (getenv "EOPEN_DIR"))
        (file (getenv "EOPEN_FILE")))
    (if dir
        (cd dir))
    (if file
        (find-file file))))

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