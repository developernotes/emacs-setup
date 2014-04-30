(defvar mine-font-name-normal)
(defvar mine-font-name-large)

(show-paren-mode t)

(case system-type
  ('windows-nt (setq mine-font-name-normal "Ubuntu Mono-12"
                     mine-font-name-large "DejaVu Sans Mono-18:bold"))
  ('gnu/linux  (setq mine-font-name-normal "DejaVu Sans Mono-12:bold"
                     mine-font-name-large "DejaVu Sans Mono-18:bold"))
  ('darwin     (setq mine-font-name-normal "DejaVu Sans Mono-16"
                     mine-font-name-large "DejaVu Sans Mono-22:bold"))
  ('cygwin     (setq mine-font-name-normal ""
                     mine-font-name-large "")))

(defun mine-use-normal-font()
  (interactive)
  (set-frame-parameter (selected-frame) 'font mine-font-name-normal)
  (add-to-list 'default-frame-alist '(\'font "." mine-font-name-normal)))

(defun mine-use-fullscreen ()
  (interactive)
  (set-frame-parameter (selected-frame) 'fullscreen 'fullboth)
  (add-to-list 'default-frame-alist '(fullscreen . 'fullboth)))

(defun mine-use-big-font()
  (interactive)
  (set-frame-parameter (selected-frame) 'font mine-font-name-large)
  (add-to-list 'default-frame-alist '(\'font "." mine-font-name-large)))

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
  (mine-use-big-font)
  (mine-use-no-transparency))

(mine-normal-display)

(provide 'init-pretty)
