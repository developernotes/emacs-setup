(deftheme standard-light
  "Standard light color theme")

(custom-theme-set-faces
 'standard-light
 `(mode-line ((t (:box (:line-width 1 :color "#414141")
                       :background "#bdbdbd" :foreground "#000000"))))
 `(mode-line-inactive ((t (:box (:line-width 1 :color "#cccccc")
                                :background "#f2f2f2" :foreground "#6e6e6e"))))
 `(eshell-prompt ((t (:foreground "#457BB1" :bold nil))))
 `(eshell-directory ((t (:foreground "#457BB1" :bold nil)))))


;; eshell-ls-directory ((t (:foreground ,gruvbox-neutral_yellow))))
;;    `(eshell-ls-symlink ((t (:foreground ,gruvbox-neutral_blue))))
;;    `(eshell-ls-executable ((t (:foreground ,gruvbox-bright_green)

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'standard-light)
