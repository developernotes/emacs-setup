(provide 'mine-pretty)

(defun mine-use-normal-font()
  (interactive)
  (set-frame-parameter (selected-frame) 'font "Envy Code R-13")
  (add-to-list 'default-frame-alist '(font . "Envy Code R-13")))

(defun mine-use-fullscreen ()
  (interactive)
  (set-frame-parameter (selected-frame) 'fullscreen 'fullboth)
  (add-to-list 'default-frame-alist '(fullscreen . 'fullboth)))

(defun mine-use-big-font()
  (interactive)
  (set-frame-parameter (selected-frame) 'font "Envy Code R-20")
  (add-to-list 'default-frame-alist '(font . "Envy Code R-20")))

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
      (mine-use-no-transparency)
    (mine-use-transparency)))
 
(if (functionp 'scroll-bar-mode)
    (scroll-bar-mode -1))
 
(defun mine-normal-display ()
  (interactive)
  (mine-use-normal-font)
  (mine-use-transparency))
 
(defun mine-pair-display ()
  (interactive)
  (mine-use-big-font)
  (mine-use-no-transparency))
 
(mine-normal-display)