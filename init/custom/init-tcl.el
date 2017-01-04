(add-hook 'tcl-mode-hook
          (lambda ()
            (setq tcl-indent-level 2
                  tcl-continued-indent-level 2)))

(provide 'init-tcl)
