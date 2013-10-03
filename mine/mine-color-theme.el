
(defun mine-light-color-theme ()
  (interactive)
  (load-theme 'dichromacy t))

(defun mine-dark-color-theme ()
  (interactive)
  (load-theme 'tango-2 t))

(when (string-match "^24\." emacs-version)
  (add-to-list 'custom-theme-load-path (concat emacs-root "site-lisp/themes")))

(custom-set-faces
`(mode-line ((t (:box (:line-width 1 :color "#414141")
                         :background "#bdbdbd" :foreground "#000000"))))
 `(mode-line-inactive ((t (:box (:line-width 1 :color "#cccccc")
                                  :background "#f2f2f2" :foreground "#6e6e6e"))))
 '(eshell-prompt ((t (:foreground "Blue" :bold nil))))) 

;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(cursor ((((background light)) (:background "Blue")) (((background dark)) (:background "Blue")) (t (:background "Blue"))))
;;  '(diff-added ((t (:inherit diff-changed :foreground "green4"))))
;;  '(diff-removed ((t (:inherit diff-changed :foreground "red4"))))
;;  '(enh-ruby-heredoc-delimiter-face-xxx ((t (:foreground "Brown"))))
;;  '(erc-input-face ((t (:foreground "dark green"))))
;;  '(erc-my-nick-face ((t (:foreground "dark green" :weight bold))))
;;  '(eshell-prompt ((t (:foreground "Blue" :bold t))))
;;  '(flyspell-incorrect ((t (:underline "red"))))
;;  '(font-lock-comment-face ((((class color) (min-colors 88) (background light)) (:foreground "Dark Blue"))))
;;  '(font-lock-constant-face ((((class color) (min-colors 88) (background light)) (:foreground "SlateBlue4"))))
;;  '(font-lock-string-face ((((class color) (min-colors 88) (background light)) (:foreground "Forest Green"))))
;;  '(whitespace-line ((((background light)) (:background "gray80")) (((type tty)) (:background "gray20")) (t (:background "red")))))

(provide 'mine-color-theme)
