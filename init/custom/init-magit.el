(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "green3")
     (set-face-foreground 'magit-diff-del "red3")
     (define-key magit-mode-map (kbd "q") 'magit-quit-session)))

(defun mine-toggle-vc ()
  (interactive)
  "Enable or disable emacs version control interface"
  (let ((enabled (not (eq vc-handled-backends nil))))
    (setf vc-handled-backends (if enabled nil '(CVS SVN Bzr Git Hg)))
    (message "Version control interface %s" (if enabled "disabled" "enabled"))))

(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(mine-toggle-vc)
(global-set-key "\C-c\m" 'magit-status)

(provide 'init-magit)
