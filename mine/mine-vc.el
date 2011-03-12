
(add-path "site-lisp/magit/")
(autoload 'magit-status "magit" "use magit git magic" t)

(defun mine-toggle-vc ()
  "Enable or disable emacs version control interface"
  (interactive)
  (if (eq vc-handled-backends nil)
      (progn
        (setq vc-handled-backends '(CVS SVN Bzr Git Hg))
        (message "vc interface enabled"))
    (progn
      (setq vc-handled-backends nil)
      (message "vc interface disabled"))))

(mine-toggle-vc)

(provide 'mine-vc)
