(show-paren-mode t)

(defvar next-font nil)
(defvar fonts
  '("Hack" "Triplicate T4c" "Monaco" "DejaVu Sans Mono" "Iosevka"
    "Source Code Pro" "Ubuntu Mono" "Menlo" "PragmataPro Mono" "Fira Mono"
    "Bitstream Vera Sans Mono"))

;;(defvar current-font "PragmataPro Mono")
;;(defvar current-font "Fira Code")
;;(defvar current-font "Source Code Pro")

(defvar current-font "Bitstream Vera Sans Mono")
(defvar font-weight "medium")
(defvar font-normal-size 16)
(defvar font-large-size 22)

(defun get-font-size ()
  (interactive)
  (let ((font-description (frame-parameter (selected-frame) 'font)))
    (string-match "[0-9]+" font-description)
    (let ((font-size (match-string 0 font-description)))
      (message font-size)
      (string-to-number font-size))))

(defun set-font-size (size)
  (interactive
   (list
    (intern (completing-read "Set font size: "
                             nil nil nil (number-to-string (get-font-size))))))
  (let ((font-description (format "%s-%s" current-font size)))
    (set-frame-parameter (selected-frame) 'font font-description)))

(defun font-size-increase ()
  (interactive
   (font-size-modify 1)))

(defun font-size-decrease ()
  (interactive
   (font-size-modify -1)))

(defun font-size-modify (value)
  (let ((new-size (+ value (get-font-size))))
    (set-font-size new-size)))
  
(defun set-random-font ()
  (interactive)
  (setq next-font current-font)
  (while (string= current-font next-font)
    (setq next-font (nth (random (length fonts)) fonts)))
  (set-font next-font))

(defun set-font (font)
  (interactive
   (list
    (intern (completing-read "Set font: " fonts))))
  (setq current-font font)
  (let ((font-description (format "%s-%s:weight=%s" font (get-font-size) font-weight)))
  (set-frame-parameter (selected-frame) 'font font-description))
  (message (format "Set font to %s" font)))

(defun get-font (type)
  (case type
    ('normal (format "%s-%s" current-font font-normal-size))
    ('large  (format "%s-%s:bold" current-font font-large-size))))

(defun mine-use-normal-font()
  (interactive)
  (set-font-size font-normal-size)
  (set-font current-font))

(defun mine-use-large-font()
  (interactive)
  (set-font-size font-large-size)
  (set-font current-font))

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
  ;;(set-frame-font (format "%s-%s" current-font font-normal-size))
  (set-font-size font-normal-size)
  (set-font current-font))

(defun mine-presenter-display ()
  (interactive)
  (mine-use-large-font)
  (mine-use-no-transparency))

(mine-normal-display)

(global-set-key (kbd "M-=") 'font-size-increase)
(global-set-key (kbd "M--") 'font-size-decrease)

(provide 'init-display)
