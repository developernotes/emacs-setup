
(add-path "site-lisp/themes")
(add-path "site-lisp/color-theme-6.6.0")

(require 'color-theme)

(color-theme-initialize)

(setq custom-theme-settings
      (progn
        (let ((theme-settings (make-hash-table :test 'equal)))
          (puthash 'color-theme-standard
                   '((set-face-background 'region "#ffec8b")
                     (set-face-foreground 'font-lock-doc-face "#d2691e")
                     (custom-set-faces    '(outline-2 ((t (:foreground "#006400" :bold t))))))
                   theme-settings)
          (puthash 'color-theme-tango-2
                   '((set-face-background 'region "#999966"))
                   theme-settings)
          (puthash 'color-theme-merbivore
                   '((set-face-background 'region "#999966")
                     (set-face-foreground 'font-lock-comment-face "#d8e5c5")
                     (custom-set-faces    '(outline-2 ((t (:foreground "#a5f26e" :bold t)))))
                     (custom-set-faces    '(modeline ((t (:background "#3F3B3B" :foreground "white")))))
                     (custom-set-faces    '(modeline-buffer-id ((t (:background "#3F3B3B" :foreground "white")))))
                     (custom-set-faces    '(modeline-mousable ((t (:background "#a5baf1" :foreground "black")))))
                     (custom-set-faces    '(modeline-mousable-minor-mode ((t (:background "#a5baf1" :foreground "black")))))
                     (custom-set-faces    '(isearch ((t (:background "#555555")))))
                     (custom-set-faces    '(minibuffer-prompt ((t (:bold t :foreground "#ff6600"))))))
                   theme-settings)
          (puthash 'color-theme-zen-and-art
                   '((set-face-background 'region "#999966")
                     (set-face-foreground 'font-lock-comment-face "#d8e5c5")
                     (custom-set-faces    '(outline-2 ((t (:foreground "#a5f26e" :bold t))))))
                   theme-settings)
          theme-settings)))

(defun apply-custom-theme-with-settings (theme)
  (funcall theme)
  (mapcar 'eval (gethash theme custom-theme-settings)))

(defun mine-light-color-theme ()
  (interactive)
  (when (string-match "^24\." emacs-version)
    (load-theme 'tomorrow t))
  (when (string-match "^23\." emacs-version)
    (apply-custom-theme-with-settings 'color-theme-standard)))

(defun mine-dark-color-theme ()
  (interactive)
  (load-theme 'tomorrow-night t))

(when (string-match "^24\." emacs-version)
  (add-to-list 'custom-theme-load-path (concat emacs-root "site-lisp/themes")))

(provide 'mine-color-theme)
