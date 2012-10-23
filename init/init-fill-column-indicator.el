(setq-default fci-rule-column 80)

(setq fci-mode-inhibit-modes-list '(eshell-mode
                                    fundamental-mode
                                    shell-mode
                                    dired-mode
                                    erc-mode))

(define-globalized-minor-mode global-fci-mode fci-mode
  (lambda ()
    (unless (member major-mode fci-mode-inhibit-modes-list)
      (fci-mode))))

(global-fci-mode)
