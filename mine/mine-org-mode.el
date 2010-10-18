(add-path "site-lisp/org-mode/lisp")
(add-path "site-lisp/org-mode/contrib/lisp")
(add-path "site-lisp/gnuplot-mode.0.6.0")

(require 'org)
(autoload 'gnuplot-mode "gnuplot" "gnuplot major mode" t)
(autoload 'gnuplot-make-buffer "gnuplot" "open a buffer in gnuplot mode" t)

(setq org-directory "~/org/"
      org-agenda-files '("~/org/gtd-items.org")
      org-log-done t
      org-use-fast-todo-selection t
      org-enforce-todo-dependencies t
      org-hide-leading-stars t
      org-odd-levels-only t
      org-src-fontify-natively nil
      org-agenda-prefix-format "           %t %s"
      safe-local-variable-values (quote ((eval org-display-inline-images)))
      org-tag-alist '(("work" . ?w) ("home" . ?h) ("read" . ?r) ("meeting" . ?m))
      auto-mode-alist (append '(("\\.gp$" . gnuplot-mode)) auto-mode-alist)
      org-refile-targets (quote ((org-agenda-files :maxlevel . 10) (nil :maxlevel . 10)))
      org-todo-keywords
      '((sequence "TODO(t)" "IN-PROGRESS(i)"  "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))

(run-at-time t 3600 'org-save-all-org-buffers)
(run-at-time t 7200 'mine-org-mobile-sync)

(defun my-org-file (file)
  (concat org-directory "/" file))

(defun my-org-mobile-file ()
  (interactive)
  (find-file "~/Dropbox/MobileOrg/mobileorg.org"))

(defun gtd()
  (interactive)
  (find-file "~/org/gtd-items.org"))

(defun gtd-agenda ()
  (interactive)
  (if (equal (buffer-name (current-buffer))
             "*Org Agenda*")
      (switch-to-buffer (other-buffer))
    (if (get-buffer "*Org Agenda*")
        (switch-to-buffer "*Org Agenda*")
      (progn
        (org-agenda nil "A")
        (delete-other-windows)))))

(defun gtd-switch-to-agenda ()
  (interactive)
  (if (get-buffer "*Org Agenda*")
      (progn
        (split-window-vertically)
        (other-window 1)
        (switch-to-buffer "*Org Agenda*")
        (org-fit-agenda-window))
    (org-agenda nil "A")))

(defun mine-org-mobile-sync ()
  (interactive)
  (message (format "Syncing org-mobile at %s" (current-time-string)))
  (org-mobile-pull)
  (org-mobile-push))

(global-set-key (kbd "C-c l")   'org-store-link)
(global-set-key (kbd "C-c a")   'org-agenda)
(global-set-key (kbd "C-c c")   'org-capture)
(global-set-key (kbd "C-c g g") 'gtd)
(global-set-key (kbd "C-c g a") 'gtd-switch-to-agenda)
(global-set-key (kbd "C-c g m") 'my-org-mobile-file)
(global-set-key (kbd "C-c g w") '(lambda () (interactive)(find-file (concat org-directory "work.org"))))

;; org-babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (gnuplot . t)
   (ruby . t)))

;; org capture setup
(setq org-default-notes-file (concat org-directory "/gtd-items.org"))
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "gtd-items.org" "Todo") "* TODO %?")
        ("i" "In Progress" entry (file+headline "gtd-items.org" "In Progress") "* IN-PROGRESS %?")
        ("w" "Work" entry (file+headline "work.org" "Work Tasks") "* TODO %?")))

;; org-mobile setup
(setq org-mobile-directory "~/Dropbox/MobileOrg"
      org-mobile-inbox-for-pull (my-org-file "mobile-updates.org"))

;; agenda configuraion
(setq org-agenda-search-headline-for-time nil
      org-agenda-dim-blocked-tasks 'invisible
      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-start-on-weekday nil
      org-deadline-warning-days 2
      org-agenda-ndays 7
      org-agenda-compact-blocks t
      org-agenda-tags-column -92
      org-habit-preceding-days 20
      org-habit-following-days 3
      org-habit-graph-column 55)

(setq org-agenda-custom-commands
      '(("A" "Action List"
         ((agenda "")
          (alltodo))
         ((org-agenda-todo-ignore-deadlines nil)
          (org-agenda-todo-ignore-scheduled nil)
          (org-agenda-todo-ignore-with-date nil)
          (org-agenda-files '("~/org/gtd-items.org"))
          (org-agenda-sorting-strategy '(priority-down time-up tag-up))))))

(custom-set-faces
 '(outline-1 ((t (:foreground "#D6B163" :bold t))))
 '(outline-2 ((t (:foreground "#A5F26E" :bold t))))
 '(outline-3 ((t (:foreground "#B150E7" :bold nil))))
 '(outline-4 ((t (:foreground "#529DB0" :bold nil))))
 '(outline-5 ((t (:foreground "#CC7832" :bold nil))))
 '(org-level-1 ((t (:inherit outline-1))))
 '(org-level-2 ((t (:inherit outline-2))))
 '(org-level-3 ((t (:inherit outline-3))))
 '(org-level-4 ((t (:inherit outline-4))))
 '(org-level-5 ((t (:inherit outline-5))))
 '(org-agenda-date ((t (:inherit font-lock-type-face))))
 '(org-agenda-date-weekend ((t (:inherit org-agenda-date))))
 '(org-scheduled-today ((t (:foreground "#ff6ab9" :italic t))))
 '(org-scheduled-previously ((t (:foreground "#d74b4b"))))
 '(org-upcoming-deadline ((t (:foreground "#d6ff9c"))))
 '(org-warning ((t (:foreground "#8f6aff" :italic t))))
 '(org-date ((t (:inherit font-lock-constant-face))))
 '(org-tag ((t (:inherit font-lock-comment-delimiter-face))))
 '(org-hide ((t (:foreground "#191919"))))
 '(org-todo ((t (:background "#E41B17" :foreground "white" :box (:line-width 1 :style released-button)))))
 '(org-done ((t (:background "DarkGreen" :foreground "white" :box (:line-width 1 :style released-button)))))
 '(org-column ((t (:background "#222222"))))
 '(org-column-title ((t (:background "DarkGreen" :foreground "white" :bold t :box (:line-width 1 :style released-button))))))

(setq org-todo-keyword-faces
      '(
        ("IN-PROGRESS" . (:foreground "white" :background "#E9AB17"))
        ))

(provide 'mine-org-mode)