
(add-path "site-lisp/magit/")
(autoload 'magit-status "magit" "use magit git magic" t)

(defun mine-toggle-vc ()
  (interactive)
  "Enable or disable emacs version control interface"
  (let ((enabled (not (eq vc-handled-backends nil))))
    (setf vc-handled-backends (if enabled nil '(CVS SVN Bzr Git Hg)))
    (message "vc interface %s" (if enabled "disabled" "enabled"))))

(mine-toggle-vc)

(provide 'mine-vc)
