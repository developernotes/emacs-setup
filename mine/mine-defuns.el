
(defun kill-all-buffers ()
  "kill all buffers, leaving *scratch* only"
  (interactive)
  (mapcar (lambda (x) (kill-buffer x))
	  (buffer-list))
  (delete-other-windows))

(defun tail ()
	"tail the file loaded within the current buffer"
	(interactive)
	(auto-revert-tail-mode))

(provide 'mine-defuns)