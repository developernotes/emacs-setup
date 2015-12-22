(show-paren-mode t)

(defvar font-name "Hack")
(defvar font-normal-size 20)
(defvar font-large-size 22)

(defun get-font (type)
  (case type
    ('normal (format "%s-%s" font-name font-normal-size))
    ('large  (format "%s-%s:bold" font-name font-large-size))))

(defmacro use-font (type)
  `(let ((font (get-font ,type)))
     (set-frame-parameter (selected-frame) 'font font)
     (add-to-list 'default-frame-alist '(\'font "." font))))

(defun mine-use-normal-font()
  (interactive)
  (use-font 'normal))

(defun mine-use-large-font()
  (interactive)
  (use-font 'large))

(defun mine-use-fullscreen ()
  (interactive)
  (set-frame-parameter (selected-frame) 'fullscreen 'fullboth)
  (add-to-list 'default-frame-alist '(fullscreen . 'fullboth)))

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
  (mine-use-no-transparency))

(defun mine-presenter-display ()
  (interactive)
  (mine-use-large-font)
  (mine-use-no-transparency))

(mine-normal-display)

(provide 'init-display)
