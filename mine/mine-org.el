
(provide 'mine-org)

;; Enable Org Mode
(require 'org-install)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cr" 'org-remember)


(setq remember-annotation-functions '(org-remember-annotation))
(setq remember-handler-functions '(org-remember-handler))
(add-hook 'remember-mode-hook 'org-remember-apply-template)

(setq org-directory "~/.emacs.d/org/")
(setq org-agenda-files '("~/.emacs.d/org/gtd-items.org"))
(setq org-log-done t)

(defun gtd()
  (interactive)
  (find-file "~/.emacs.d/org/gtd-items.org"))


(setq org-remember-templates
      '(("Todo" ?t "* TODO %?\n %i\n %a" "~/.emacs.d/org/gtd-items.org" "Todo")
        ("Inbox" ?i "* %?" "~/.emacs.d/org/gtd-items.org" "Inbox")
        ("Misc Task" ?m "* TODO %? %^g\n" "~/.emacs.d/org/gtd-items.org" "Misc Tasks")))
