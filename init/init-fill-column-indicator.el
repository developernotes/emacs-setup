(setq-default fci-rule-column 80)

(setq fci-mode-inhibit-buffer-list '("*Org Agenda*"
                                     "*Ibuffer*"
                                     "*scratch*"))

(setq fci-mode-inhibit-modes-list '(compilation-mode
                                    dired-mode
                                    erc-mode
                                    eshell-mode
                                    fundamental-mode
                                    recentf-dialog-mode
                                    shell-mode))

(define-globalized-minor-mode global-fci-mode fci-mode
  (lambda ()
    (unless
        (or
         (member (buffer-name (current-buffer)) fci-mode-inhibit-buffer-list)
         (member major-mode fci-mode-inhibit-modes-list))
      (fci-mode))))

(global-fci-mode)
