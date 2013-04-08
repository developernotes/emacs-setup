
(setq org-directory "~/org/"
      org-agenda-files (directory-files "~/org" t "\\.org$")
      org-log-done t
      org-use-fast-todo-selection t
      org-enforce-todo-dependencies t
      org-hide-leading-stars t
      org-odd-levels-only t
      org-use-speed-commands t
      org-src-fontify-natively nil
      org-agenda-prefix-format "           %t %s"
      safe-local-variable-values (quote ((eval org-display-inline-images)))
      org-tag-alist '(("work" . ?w) ("home" . ?h) ("read" . ?r) ("meeting" . ?m))
      auto-mode-alist (append '(("\\.gp$" . gnuplot-mode)) auto-mode-alist)
      org-refile-targets (quote (("~/org/someday-maybe.org" :maxlevel . 10) (nil :maxlevel . 10)))
      org-todo-keywords
      '((sequence "TODO(t)" "IN-PROGRESS(i)"  "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))

(defun yas/org-very-safe-expand ()
  (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))

(add-hook 'org-mode-hook
          '(lambda ()
             (yas/reload-all)
             (yas/minor-mode)))

(add-hook 'ruby-mode-hook
          '(lambda ()
             (yas/reload-all)
             (yas/minor-mode)))

(defun my-org-mobile-file ()
  (interactive)
  (find-file "~/Dropbox/MobileOrg/mobileorg.org"))

(defun org-file (file)
  (concat org-directory "/" file))

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
(global-set-key (kbd "C-c g w") '(lambda () (interactive)(find-file (concat org-directory "work-notes.org"))))

;; org capture setup
(setq org-default-notes-file (concat org-directory "/gtd-items.org"))
(setq org-capture-templates
      '(("t" "Todo" entry
         (file+headline "gtd-items.org" "Todo")
         "* TODO %?")
        ("i" "In Progress" entry
         (file+headline "gtd-items.org" "In Progress")
         "* IN-PROGRESS %?")
        ("l" "Links to Read" entry
         (file+headline "gtd-items.org" "Links to Read")
         "* %a\n %?\n %i" :immediate-finish t)
        ("w" "Work (general)" entry
         (file+headline (org-file "work-notes.org") "General Todo")
         "* TODO %?")
        ("c" "SQLCipher core" entry
         (file+olp (org-file "work-notes.org") "SQLCipher" "Todo")
         "* TODO %?")
        ("s" "SQLCipher for Android" entry
         (file+olp (org-file "work-notes.org") "SQLCipher for Android" "Todo"))
        ("a" "STRIP for Android" entry
         (file+olp (org-file "work-notes.org") "STRIP for Android" "Todo")
         "* TODO %?")
        ("n" "STRIP for Windows" entry
         (file+olp (org-file "work-notes.org") "STRIP for Windows" "Todo")
         "* TODO %?")))

;; org-mobile setup
(setq org-mobile-directory "~/Dropbox/MobileOrg"
      org-mobile-inbox-for-pull (org-file "mobile-updates.org"))

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
          (org-agenda-todo-ignore-scheduled t)
          (org-agenda-todo-ignore-with-date nil)
          (org-agenda-files '("~/org/gtd-items.org" "~/org/work-notes.org"))
          (org-agenda-sorting-strategy '(priority-down time-up tag-up))))))

 (custom-set-faces
  '(outline-2 ((t (:foreground "#cc7832"))))
  '(org-level-2 ((t (:inherit outline-2))))
  '(org-scheduled-today ((t (:foreground "#ff6ab9" :italic t))))
  '(org-done ((t (:background "DarkGreen" :foreground "white"
                              :box (:line-width 1 :style released-button))))))

(setq org-todo-keyword-faces
      '(("IN-PROGRESS" . (:foreground "white" :background "#E9AB17"))
        ("TODO" . (:foreground "white" :background "#E41B17"
                               :box (:line-width 1 :style released-button)))))
