(setq custom-file (concat emacs-root "/mine/mine-customizations.el"))
(provide 'mine-customizations)

;; Disable the startup screen
(setq inhibit-startup-screen t)

;; Type only y or n for confirmation
(fset 'yes-or-no-p 'y-or-n-p)

;; Disable user prompt
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Enable ido mode
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

;; Enable line numbers
(global-linum-mode 1)

;; Disable audible bell
(setq visible-bell t)

;; Remove menubar/toolbar
(menu-bar-mode nil)
(tool-bar-mode nil)

;; Highlight current line
(global-hl-line-mode t)

;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))

