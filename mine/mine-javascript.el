
(setq auto-mode-alist
      (remove (rassoc 'js2-mode auto-mode-alist) auto-mode-alist))
(message "removed js2-mode")
;;(add-to-list 'auto-mode-alist '("\\.js\\'" . js-mode))
(setq js-indent-level 2)

(provide 'mine-javascript)
