(run-with-idle-timer 1 nil
                     (lambda ()
                       (progn
                         (autoload 'bm-toggle   "bm" "Toggle bookmark in current buffer." t)
                         (autoload 'bm-next     "bm" "Goto bookmark."                     t)
                         (autoload 'bm-previous "bm" "Goto previous bookmark."            t)
                         (autoload 'bm-toggle-mouse "bm" "Toggle bookmark in fringe with mouse" t)
                         (global-set-key (kbd "<left-fringe> <mouse-1>") 'bm-toggle-mouse)
                         (global-set-key (kbd "C-x r m") 'bm-toggle)
                         (global-set-key (kbd "C-x r l") 'bm-show-all)
                         (global-set-key (kbd "C-x r n") 'bm-next)
                         (global-set-key (kbd "C-x r p") 'bm-previous))))

(provide 'init-bookmark)
