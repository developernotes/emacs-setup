;; (global-undo-tree-mode t)

(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
(run-with-idle-timer 1 nil (lambda () (global-undo-tree-mode t)))

(provide 'init-undo-tree)

