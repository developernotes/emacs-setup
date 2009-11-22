(setq custom-file (concat emacs-root "/mine/mine-customizations.el"))
(provide 'mine-customizations)

;; Disable the startup screen
(setq inhibit-startup-screen t)

;; Type only y or n for confirmation
(fset 'yes-or-no-p 'y-or-n-p)

(setq indent-tabs-mode nil) 
(delete-selection-mode t)

;; Disable user prompt
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Enable ido mode
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)
(setq confirm-nonexistent-file-or-buffer -1)

;; Enable line numbers
(global-linum-mode 1)

;; Disable audible bell
(setq visible-bell t)

;; Remove menubar/toolbar
(menu-bar-mode -1)
(tool-bar-mode -1)

;; Highlight current line
(global-hl-line-mode t)

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



;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))

