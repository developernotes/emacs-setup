(provide 'mine-pretty)

(defun mine-use-transparency ()
  (interactive)
  (set-frame-parameter (selected-frame) 'alpha '(90 80))
  (add-to-list 'default-frame-alist '(alpha 90 80)))
 
(defun mine-use-no-transparency ()
  (interactive)
  (set-frame-parameter (selected-frame) 'alpha '(100 100))
  (add-to-list 'default-frame-alist '(alpha 100 100)))
 
(defun mine-toggle-transparency ()
  (interactive)
  (if (/=
       (cadr (find 'alpha (frame-parameters nil) :key #'car))
       100)
      (mine-use-transparency)
    (mine-use-no-transparency)))
 
(if (functionp 'scroll-bar-mode)
    (scroll-bar-mode -1))
 
(defun mine-normal-display ()
  (interactive)
  (mine-use-transparency))
 
(defun mine-pair-display ()
  (interactive)
  (mine-use-no-transparency))
 
(mine-normal-display)