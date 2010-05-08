
(require 'ido)
(require 'thingatpt)
(require 'ibuffer-git)
(require 'bm)
(require 'browse-kill-ring)
(require 'recentf)

;; killring
(browse-kill-ring-default-keybindings)

;; recent files
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; region/line movement
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

(global-set-key [M-S-up] 'move-text-up)
(global-set-key [M-S-down] 'move-text-down)

;; Ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)

(setq ibuffer-formats 
			'((mark modified read-only git-status-mini 
			  " "
        (name 18 18 :left :elide)
        " "
        (size 9 -1 :right)
        " "
        (mode 16 16 :left :elide)
        " "
        (git-status 8 8 :left)
        " " filename-and-process)))

(global-set-key (kbd "C-c m")      'bm-toggle)
(global-set-key (kbd "C-c k")      'bm-remove-all-current-buffer)
(global-set-key (kbd "C-<right>")  'bm-next)
(global-set-key (kbd "C-<left>")   'bm-previous)

;; Ido
(ido-mode t)
(setq ido-enable-flex-matching t)
(setq confirm-nonexistent-file-or-buffer -1)
(ido-everywhere 1)

(defun mine-goto-symbol-at-point ()
	"Will navigate to the symbol at the current point of the cursor"
	(interactive)
	(ido-goto-symbol (thing-at-point 'symbol)))

(defun ido-goto-symbol (&optional a-symbol)
    "Will update the imenu index and then use ido to select a symbol to navigate to"
    (interactive)
    (imenu--make-index-alist)
    (let ((name-and-pos '())
          (symbol-names '()))
      (flet ((addsymbols (symbol-list)
                         (when (listp symbol-list)
                           (dolist (symbol symbol-list)
                             (let ((name nil) (position nil))
                               (cond
                                ((and (listp symbol) (imenu--subalist-p symbol))
                                 (addsymbols symbol))
   
                                ((listp symbol)
                                 (setq name (car symbol))
                                 (setq position (cdr symbol)))
   
                                ((stringp symbol)
                                 (setq name symbol)
                                 (setq position (get-text-property 1 'org-imenu-marker symbol))))
   
                               (unless (or (null position) (null name))
                                 (add-to-list 'symbol-names name)
                                 (add-to-list 'name-and-pos (cons name position))))))))
        (addsymbols imenu--index-alist))
      (let* ((selected-symbol 
							(if (null a-symbol)
								(ido-completing-read "Symbol? " symbol-names)
								a-symbol))
             (position (cdr (assoc selected-symbol name-and-pos))))
        (cond
         ((overlayp position)
          (goto-char (overlay-start position)))
         (t
          (goto-char position))))))

(provide 'mine-navigation)
