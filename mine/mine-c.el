
(add-hook 'c-mode-common-hook
  (lambda ()
    (local-set-key (kbd "C-c f") 'ff-find-other-file)))

(provide 'mine-c)