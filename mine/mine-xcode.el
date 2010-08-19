(require 'xcode)

(add-hook 'objc-mode-hook '(lambda ()
                             (define-key objc-mode-map (kbd "C-c f") 'xcode/toggle-header-and-source)
                             (define-key objc-mode-map (kbd "C-c b") 'xcode/build-compile)))

(provide 'mine-xcode)