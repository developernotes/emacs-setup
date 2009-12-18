
(require 'uniquify) 

(setq custom-file (concat emacs-root "/mine/mine-customizations.el"))

;; Disable the startup screen
(setq inhibit-startup-screen t)

;; Type only y or n for confirmation
(fset 'yes-or-no-p 'y-or-n-p)

(setq indent-tabs-mode nil) 
(delete-selection-mode t)

;; Disable user prompt
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Enable line numbers
(global-linum-mode 1)

;; Disable audible bell
(setq visible-bell t)

;; Remove menubar/toolbar
(menu-bar-mode -1)
(tool-bar-mode -1)

(setq-default tab-width 2)

;; Highlight current line
(global-hl-line-mode t)

;; Identify buffers with same name, postfix folder name
(setq 
  uniquify-buffer-name-style 'post-forward
  uniquify-separator ":")

;; All backup files in one place
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Copy or kill entire line if mark not set
(defadvice kill-ring-save (before slick-copy activate compile) "When called
  interactively with no active region, copy a single line instead."
  (interactive (if mark-active (list (region-beginning) (region-end)) (message
  "Copied line") (list (line-beginning-position) (line-beginning-position
  2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
    (if mark-active (list (region-beginning) (region-end))
      (list (line-beginning-position)
        (line-beginning-position 2)))))

;; ELPA
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))

(provide 'mine-customizations)
