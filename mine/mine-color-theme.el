
(add-path "site-lisp/themes")
(add-path "site-lisp/color-theme-6.6.0")

(require 'color-theme)
(require 'color-theme-zen-and-art)

(color-theme-initialize)

(setq custom-theme-settings
      (progn
        (let ((theme-settings (make-hash-table :test 'equal)))
          (puthash 'color-theme-standard
                   '((set-face-background 'region "#ffec8b")
                     (set-face-background 'hl-line "#b4eeb4")
                     (set-face-foreground 'font-lock-doc-face "#d2691e")
                     (custom-set-faces    '(outline-2 ((t (:foreground "#006400" :bold t))))))
                   theme-settings)
          (puthash 'color-theme-merbivore
                   '((set-face-background 'hl-line "#3b3b3b")
                     (set-face-background 'region "#999966")
                     (set-face-foreground 'font-lock-comment-face "#d8e5c5")
                     (custom-set-faces    '(outline-2 ((t (:foreground "#a5f26e" :bold t))))))
                   theme-settings)
          (puthash 'color-theme-zen-and-art
                   '((set-face-background 'hl-line "#3b3b3b")
                     (set-face-background 'region "#999966")
                     (set-face-foreground 'font-lock-comment-face "#d8e5c5")
                     (custom-set-faces    '(outline-2 ((t (:foreground "#a5f26e" :bold t))))))
                   theme-settings)
          theme-settings)))

(defun apply-custom-theme-with-settings (theme)
  (funcall theme)
  (mapcar 'eval (gethash theme custom-theme-settings)))

(defun mine-light-color-theme ()
  (interactive)
  (apply-custom-theme-with-settings 'color-theme-standard))

(defun mine-dark-color-theme ()
  (interactive)
  (apply-custom-theme-with-settings 'color-theme-zen-and-art))

(mine-light-color-theme)

(provide 'mine-color-theme)