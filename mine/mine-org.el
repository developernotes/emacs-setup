
(provide 'mine-org)

;; Enable Org Mode
(require 'org-install)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

(defun gtd()
  (interactive)
  (find-file "~/.emacs.d/org/gtd-items.org"))
