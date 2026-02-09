;;(eval-when-compile
;;  (require 'use-package))

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(use-package benchmark-init
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

;; init-navigation.el
(use-package bind-key
  :ensure t
  :config
  (add-to-list 'same-window-buffer-names "*Personal Keybindings*"))

(use-package ido
  :ensure t
  :config
  (setq ido-use-virtual-buffers t
        ido-enable-flex-matching t
        ido-create-new-buffer 'always
        confirm-nonexistent-file-or-buffer nil
        ido-vertical-define-keys 'C-n-C-p-up-and-down)
  (ido-mode t))

(use-package flx-ido
  :ensure t
  :requires ido
  :config (flx-ido-mode))

(use-package ido-vertical-mode
  :ensure t
  :requires ido
  :config (ido-vertical-mode))

(use-package imenu
  :ensure t)

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package fzf
  :ensure t
  :bind
  ;; Don't forget to set keybinds!
  :config
  (setq fzf/args "-x --color bw --print-query --margin=1,0 --no-hscroll"
        fzf/executable "/usr/local/bin/fzf"
        fzf/git-grep-args "-i --line-number %s"
        ;; command used for `fzf-grep-*` functions
        ;; example usage for ripgrep:
        ;; fzf/grep-command "rg --no-heading -nH"
        fzf/grep-command "grep -nrH"
        ;; If nil, the fzf buffer will appear at the top of the window
        fzf/position-bottom t
        fzf/window-height 15))

;; new start
;; Enable Vertico.
;; (use-package vertico
;;   :ensure t
;;   ;; :custom
;;   ;; (vertico-scroll-margin 0) ;; Different scroll margin
;;   ;; (vertico-count 20) ;; Show more candidates
;;   ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
;;   ;; (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
;;   :init
;;   (vertico-mode))

;; ;; Persist history over Emacs restarts. Vertico sorts by history position.
;; (use-package savehist
;;   :ensure t
;;   :init
;;   (savehist-mode))

;; ;; Emacs minibuffer configurations.
;; (use-package emacs
;;   :ensure t
;;   :custom
;;   ;; Support opening new minibuffers from inside existing minibuffers.
;;   (enable-recursive-minibuffers t)
;;   ;; Hide commands in M-x which do not work in the current mode.  Vertico
;;   ;; commands are hidden in normal buffers. This setting is useful beyond
;;   ;; Vertico.
;;   (read-extended-command-predicate #'command-completion-default-include-p)
;;   ;; Do not allow the cursor in the minibuffer prompt
;;   (minibuffer-prompt-properties
;;    '(read-only t cursor-intangible t face minibuffer-prompt)))

;; ;; Optionally use the `orderless' completion style.
;; (use-package orderless
;;   :ensure t
;;   :custom
;;   ;; Configure a custom style dispatcher (see the Consult wiki)
;;   ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
;;   ;; (orderless-component-separator #'orderless-escapable-split-on-space)
;;   (completion-styles '(orderless basic))
;;   (completion-category-defaults nil)
;;   (completion-category-overrides '((file (styles partial-completion)))))
;; new end

(use-package thingatpt)

(use-package smex
  :ensure t
  :config
  (global-set-key (kbd "M-x") 'smex))

(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :config
  (add-hook 'go-mode-hook
            (lambda ()
              (add-hook 'before-save-hook
                        #'gofmt-before-save
                        nil t))))

(add-hook 'js-json-mode-hook
          (lambda ()
            (setq tab-width 2)
            (setq js-indent-level 2)))

;; init-customizations.el
(setq
 emacs-root (concat (getenv "HOME") "/.emacs.d/")
 backup-directory-alist '(("." . "/tmp/emacs-backups/"))
 load-prefer-newer t
 init-file-before-load-time (current-time)
 visible-bell t
 c-basic-offset 2
 mac-command-modifier 'meta
 menu-bar-mode -1
 debug-on-error nil
 linum-format " %d "
 column-number-mode t
 redisplay-dont-pause t
 ring-bell-function 'ignore
 display-time-24hr-format t
 inhibit-startup-screen t
 uniquify-buffer-name-style 'post-forward
 uniquify-separator ":"
 ag-highlight-search t
 initial-scratch-message nil
 ;;use-package-compute-statistics t
 ediff-window-setup-function 'ediff-setup-windows-plain
 mouse-wheel-progressive-speed nil
 global-display-fill-column-indicator-mode t
 mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil))
 bkup-backup-directory-info '((t "~/.backups" ok-create full-path))
 custom-file (concat emacs-root "/mine/mine-customizations.el")
 kill-buffer-query-functions (remq 'process-kill-buffer-query-function
                                   kill-buffer-query-functions))
(setq-default
 tab-width 2
 indent-tabs-mode nil)

(show-paren-mode t)
(global-display-line-numbers-mode)
(blink-cursor-mode t)
(when (string-match-p "^Battery" (battery))
  (display-battery-mode))
(display-time-mode t)
(delete-selection-mode t)
(fset 'yes-or-no-p 'y-or-n-p)
(put 'upcase-region    'disabled nil)
(put 'downcase-region  'disabled nil)
(put 'narrow-to-region 'disabled nil)
(menu-bar-mode -1)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(when window-system
  (let* ((window-width 120)
         (window-height 50)
         (pos-x (/ (display-pixel-width) 2))
         (pos-y (/ (display-pixel-height) 2)))
    (set-frame-size (selected-frame) window-width window-height)))

;; Rebind C-x C-x to (delete-frame) when running a daemon
(when (daemonp)
  (global-set-key (kbd "C-x C-c") 'delete-frame))

;; init-theme.el
(defvar current-theme nil)
(defvar next-theme nil)
(defvar theme-path
  (concat emacs-root "site-lisp/themes"))

(setq dark-themes
      '(atom-dark
        doom-ayu-mirage
        badwolf
        flatland
        gruvbox
        ir-black
        monokai
        noctilux
        smyx
        solarized-dark
        spacegray
        tango-2
        tomorrow-night
        underwater))

(setq light-themes
      '(soft-stone
        solarized-light
        standard-light
        standard-reeder
        standard-stone))

(defun set-light-theme ()
  (interactive)
  ;; (set-theme 'standard-light)
  (setq catppuccin-flavor 'latte)
  (set-theme 'catppuccin))

(defun set-dark-theme ()
  (interactive)
  ;; (set-theme 'doom-tokyo-night)
  (setq catppuccin-flavor 'mocha)
  (set-theme 'catppuccin)
  ;; (set-theme 'doom-ayu-mirage)
  )

(defun set-random-theme ()
  (interactive)
  (set-random-theme-from (append dark-themes light-themes)))

(defun set-random-theme-from (themes)
  (setq next-theme current-theme)
  (while (string= current-theme next-theme)
    (setq next-theme (nth (random (length themes)) themes)))
  (set-theme next-theme))

(defun set-random-dark-theme ()
  (interactive)
  (set-random-theme-from dark-themes))

(defun set-random-light-theme ()
  (interactive)
  (set-random-theme-from light-themes))

(defun set-theme (theme)
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
                             (mapcar 'symbol-name
                                     (custom-available-themes))))))
  (if current-theme
      (disable-theme current-theme))
  (load-theme theme t)
  (setq current-theme theme)
  (message (format "Set theme to %s" theme)))


(when (not (member theme-path custom-theme-load-path))
  (add-to-list 'custom-theme-load-path theme-path))


;; init-defuns.el
(defun center-selected-frame ()
  "Centers the current selected frame within the desktop."
  (interactive)
  (let* ((center-x (/ (nth 3 (assq 'geometry (car (display-monitor-attributes-list)))) 2))
         (center-y (/ (nth 4 (assq 'geometry (car (display-monitor-attributes-list)))) 2))
         (x (- center-x
               (/ (frame-pixel-width (selected-frame)) 2)))
         (y (- center-y
               (/ (frame-pixel-height (selected-frame)) 2))))
    (set-frame-position (selected-frame) x y)))

(defun beginning-of-line-or-back-to-indention ()
  (interactive)
  "This goes to back to indention or if already there beginning of line"
  (let ((previous-point (point)))
    (back-to-indentation)
    (if (equal previous-point (point))
        (beginning-of-line))))

(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer-or-region ()
  "Untabifies, indents and deletes trailing whitespace from buffer or region."
  (interactive)
  (save-excursion
    (unless (region-active-p)
      (mark-whole-buffer))
    (untabify (region-beginning) (region-end))
    (indent-region (region-beginning) (region-end))
    (save-restriction
      (narrow-to-region (region-beginning) (region-end))
      (delete-trailing-whitespace))))

(defun append-to-environment-variable (variable path)
  (setenv variable (concat (format "%s:%s" (getenv variable) path))))

(defun kill-all-buffers ()
  "kill all buffers, leaving *scratch* only"
  (interactive)
  (mapcar (lambda (x) (kill-buffer x))
          (buffer-list))
  (delete-other-windows))

(defun kill-all-buffers-but-current ()
  "kills all buffers except your current buffer"
  (interactive)
  (mapcar (lambda (x) (kill-buffer x))
          (remove (current-buffer) (buffer-list)))
  (delete-other-windows))

(defun tail ()
  "tail the file loaded within the current buffer"
  (interactive)
  (auto-revert-tail-mode))

(defun string-replace (old-value new-value source)
  "replace old-value with new-value from source"
  (with-temp-buffer
    (insert source)
    (goto-char (point-min))
    (while (search-forward old-value nil t)
      (replace-match new-value nil t))
    (buffer-substring (point-min) (point-max))))

(defun rename-file-and-buffer ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (cond ((get-buffer new-name)
               (message "A buffer named '%s' already exists!" new-name))
              (t
               (rename-file name new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)))))))

(defun is-macos-dark-mode ()
  "Reports whether macOS is displaying dark mode or not."
  (interactive)
  (string=
   (shell-command-to-string
    "printf %s \"$( osascript -e \'tell application \"System Events\" to tell appearance preferences to return dark mode\' )\"") "true"))

;; init-display.el
(defvar next-font nil)
(defvar fonts
  '("Berkeley Mono" "Hack" "Triplicate T4c" "Monaco" "DejaVu Sans Mono" "Iosevka"
    "Source Code Pro" "Ubuntu Mono" "Menlo" "PragmataPro Mono" "Fira Mono"
    "Bitstream Vera Sans Mono"))

(defvar current-font "Berkeley Mono")
(defvar font-weight "medium")
(defvar font-normal-size 17)
(defvar font-large-size 22)

(defun get-font-size ()
  (interactive)
  (let ((font-description (frame-parameter (selected-frame) 'font)))
    (string-match "[0-9]+" font-description)
    (let ((font-size (match-string 0 font-description)))
      (message font-size)
      (string-to-number font-size))))

(defun set-font-size (size)
  (interactive
   (list
    (intern (completing-read "Set font size: "
                             nil nil nil (number-to-string (get-font-size))))))
  (let ((font-description (format "%s-%s" current-font size)))
    (set-frame-parameter (selected-frame) 'font font-description)))

(defun font-size-increase ()
  (interactive
   (font-size-modify 1)))

(defun font-size-decrease ()
  (interactive
   (font-size-modify -1)))

(defun font-size-modify (value)
  (let ((new-size (+ value (get-font-size))))
    (set-font-size new-size)))

(defun set-random-font ()
  (interactive)
  (setq next-font current-font)
  (while (string= current-font next-font)
    (setq next-font (nth (random (length fonts)) fonts)))
  (set-font next-font))

(defun set-font (font)
  (interactive
   (list
    (intern (completing-read "Set font: " fonts))))
  (setq current-font font)
  (let ((font-description (format "%s-%s:weight=%s" font (get-font-size) font-weight)))
    (set-frame-parameter (selected-frame) 'font font-description))
  (message (format "Set font to %s" font)))

(defun get-font (type)
  (case type
        ('normal (format "%s-%s" current-font font-normal-size))
        ('large  (format "%s-%s:bold" current-font font-large-size))))

(if (functionp 'scroll-bar-mode)
    (scroll-bar-mode -1))

(require 'server)

(defun mine-normal-display ()
  (interactive)
  (set-frame-font (format "%s-%s" current-font font-normal-size))
  (set-font-size font-normal-size)
  (set-font current-font))

(defun mine-presenter-display ()
  (interactive)
  (mine-use-large-font)
  (mine-use-no-transparency))

(global-set-key (kbd "M-=") 'font-size-increase)
(global-set-key (kbd "M--") 'font-size-decrease)

;; enable winmove
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
(windmove-default-keybindings 'meta)

;; recent files
(use-package recentf
  :ensure t
  :bind ("C-x C-r" . recentf-open-files)
  :config
  (setq recentf-max-menu-items 15
        recentf-max-saved-items 100))

;; region/line movement
(global-set-key [M-S-up] 'move-text-up)
(global-set-key [M-S-down] 'move-text-down)

(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line)
        (when (or (< arg 0) (not (eobp)))
          (transpose-lines arg))
        (forward-line -1))
      (move-to-column column t)))))

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

(defun rotate-window-split ()
  "Rotates two windows from vertical to horizontal or horizontal to vertical orientation"
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defun swap-windows ()
  "If you have 2 windows, it swaps them."
  (interactive)
  (cond ((/= (count-windows) 2)
         (message "You need exactly 2 windows to do this."))
        (t
         (let* ((w1 (first (window-list)))
                (w2 (second (window-list)))
                (b1 (window-buffer w1))
                (b2 (window-buffer w2))
                (s1 (window-start w1))
                (s2 (window-start w2)))
           (set-window-buffer w1 b2)
           (set-window-buffer w2 b1)
           (set-window-start w1 s2)
           (set-window-start w2 s1))))
  (other-window 1))

;; mark commands
(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region
Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))
;;(global-set-key (kbd "C-`") 'push-mark-no-activate)

(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))
;;(global-set-key (kbd "M-`") 'jump-to-mark)

(defun ido-goto-symbol (&optional symbol-list)
  "Refresh imenu and jump to a place in the buffer using Ido."
  (interactive)
  (cond
   ((not symbol-list)
    (let ((ido-mode ido-mode)
          (ido-enable-flex-matching
           (if (boundp 'ido-enable-flex-matching)
               ido-enable-flex-matching t))
          name-and-pos symbol-names position)
      (unless ido-mode
        (ido-mode 1)
        (setq ido-enable-flex-matching t))
      (while (progn
               (imenu--cleanup)
               (setq imenu--index-alist nil)
               (ido-goto-symbol (imenu--make-index-alist))
               (setq selected-symbol
                     (ido-completing-read "Symbol? " symbol-names))
               (string= (car imenu--rescan-item) selected-symbol)))
      (unless (and (boundp 'mark-active) mark-active)
        (push-mark nil t nil))
      (setq position (cdr (assoc selected-symbol name-and-pos)))
      (cond
       ((overlayp position)
        (goto-char (overlay-start position)))
       (t
        (goto-char position)))))
   ((listp symbol-list)
    (dolist (symbol symbol-list)
      (let (name position)
        (cond
         ((and (listp symbol) (imenu--subalist-p symbol))
          (ido-goto-symbol symbol))
         ((listp symbol)
          (setq name (car symbol))
          (setq position (cdr symbol)))
         ((stringp symbol)
          (setq name symbol)
          (setq position
                (get-text-property 1 'org-imenu-marker symbol))))
        (unless (or (null position) (null name)
                    (string= (car imenu--rescan-item) name))
          (add-to-list 'symbol-names name)
          (add-to-list 'name-and-pos (cons name position))))))))


;; init-bindings.el
(global-set-key "\r"            'newline-and-indent)
(global-set-key "\C-a"          'beginning-of-line-or-back-to-indention)
(global-set-key "\C-cv"         'mine-toggle-transparency)
(global-set-key "\C-cw"         'toggle-truncate-lines)
(global-set-key "\C-cs"         'ido-goto-symbol)
(global-set-key "\C-cp"         'mine-goto-symbol-at-point)
(global-set-key "\M-g"          'goto-line)
(global-set-key "\C-c\o"        'cleanup-buffer-or-region)
(global-set-key "\C-x\C-p"      'find-file-at-point)
(global-set-key "\C-x\C-d"      'dired)
(global-set-key "\C-c2"         'swap-windows)
(global-set-key "\C-c3"         'rotate-window-split)
(global-set-key (kbd "C-c r")   'replace-string)
(global-set-key (kbd "C-c e b") 'eval-buffer)
(global-set-key (kbd "C-c e r") 'eval-region)
(global-set-key (kbd "C-c f l") 'find-library)
(global-set-key (kbd "C-c f f") 'find-function)
(global-set-key (kbd "M-j")     (lambda () (interactive) (join-line -1)))
(global-set-key (kbd "C-c C-f") 'ag)
(global-set-key (kbd "C-x 0")   'delete-other-window)
(global-set-key (kbd "C-x m")   (lambda () (interactive) (push-mark)))
(global-set-key (kbd "<f5>")    (lambda () (interactive) (dired "~/org")))
(global-set-key (kbd "C-x p")   'mine-goto-project)
(global-set-key (kbd "C-c i")   'cleanup-buffer-or-region)
;;(global-set-key (kbd "M-S-SPC") 'mark-word)

;; extend search to show occurances
(define-key isearch-mode-map (kbd "C-o")
            (lambda ()
              (interactive)
              (let ((case-fold-search isearch-case-fold-search))
                (occur (if isearch-regexp isearch-string
                         (regexp-quote isearch-string))))))


;; init-advice.el
;; copy or kill entire line if mark not set
(defadvice kill-ring-save (before slick-copy activate compile) "When called
  interactively with no active region, copy a single line instead."
           (interactive
            (if mark-active
                (list (region-beginning) (region-end))
              (message "Copied line")
              (list (line-beginning-position) (line-beginning-position 2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

;; go to line number if available
(defadvice find-file-at-point (around forward-line compile activate)
  (let ((line (and (looking-at ".*:\\([0-9]+\\)")
                   (string-to-number (match-string 1)))))
    ad-do-it
    (and line (forward-line line))))

;; setup
(defun main-setup()
  (interactive)
  (mine-normal-display)
  (center-selected-frame))

(main-setup)

(when (is-macos-dark-mode)
  (set-theme 'spacegray))

(message (format "Emacs loaded in %.2f seconds"
                 (float-time
                  (time-subtract (current-time) before-init-time))))
