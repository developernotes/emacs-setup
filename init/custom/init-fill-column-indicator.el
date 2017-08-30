(setq fci-rule-column 80)
(define-globalized-minor-mode global-fci-mode fci-mode
  (lambda ()
    (when (not (or
                (eq major-mode 'eshell-mode)
                (eq major-mode 'minibuffer-inactive-mode)))
      (fci-mode 1))))

(global-fci-mode 1)
