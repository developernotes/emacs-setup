
(add-path "site-lisp/epresent")
(require 'epresent)

(add-to-list 'after-make-frame-functions 'prepare-presentation-frame)
(add-to-list 'delete-frame-functions 'close-presentation-frame)

(defun prepare-presentation-frame (frame)
  (when (equalp (frame-parameter frame 'title) "EPresent")
    (global-linum-mode -1)
    (global-hl-line-mode -1)))

(defun close-presentation-frame (frame)
  (when (equalp (frame-parameter frame 'title) "EPresent")
    (global-linum-mode 1)
    (global-hl-line-mode 1)))

(provide 'mine-epresent)