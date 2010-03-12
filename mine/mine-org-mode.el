(add-path "site-lisp/org-mode/lisp")
(add-path "site-lisp/org-mode/contrib/lisp")
(add-path "site-lisp/org-jekyll")

;; enable org-mode
(require 'org)
(require 'org-publish)
(require 'org-jekyll)

;; configuration
(setq org-directory "~/org/")
(setq org-agenda-files '("~/org/gtd-items.org"))
(setq org-log-done t)

;; org-jekyll
(add-to-list 'org-publish-project-alist
						 `("blog"
							 :base-directory "~/blog"
							 :recursive t
							 :base-extension "org"
							 :blog-publishing-directory "~/blog"
							 :jekyll-sanitize-permalinks t
							 :site-root "http://developernotes.com"))

;; keybindings
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cr" 'org-remember)
(global-set-key (kbd "C-c g g") 'gtd)
(global-set-key (kbd "C-c g a") 'gtd-switch-to-agenda)

(run-at-time t 3600 'org-save-all-org-buffers)

(defun my-org-file (file)
  (concat org-directory "/" file))

(setq org-tag-alist '(("work" . ?w) ("home" . ?h) ("read" . ?r) ("meeting" . ?m)))
(setq remember-annotation-functions '(org-remember-annotation))
(setq remember-handler-functions '(org-remember-handler))
(add-hook 'remember-mode-hook 'org-remember-apply-template)

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

(setq org-agenda-prefix-format "           %t %s")

(defun generate-timestamp ()
	(format-time-string "%Y-%m-%d" (current-time)))

(defun sanitize-title (title)
	(string-replace " " "-" (downcase title)))

(defun generate-filename (title)
	(format "%s-%s.textile" (generate-timestamp) (sanitize-title title)))

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

(setq org-enforce-todo-dependencies t
      org-todo-keywords
      '((sequence "TODO(t)" "IN-PROGRESS(i)"  "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)"))
      org-use-fast-todo-selection t)

(setq org-remember-templates
      '(("Todo" ?t "* TODO %?\n %i\n %a" "gtd-items.org" "Todo")
        ("In Progress" ?i "* IN-PROGRESS %?" "gtd-items.org" "Todo")
				("Blog" ?b "---\nlayout: post\ntitle: \"%^{Title of post? }\"\n---\n" "%(generate-filename (read-from-minibuffer \"File name? \"))")))

(defun gtd-switch-to-agenda ()
  (interactive)
  (if (get-buffer "*Org Agenda*")
      (progn
        (split-window-vertically)
        (other-window 1)
        (switch-to-buffer "*Org Agenda*")
        (org-fit-agenda-window))
      (org-agenda nil "A")))

;; for a popup window for remember mode
(defadvice remember-finalize (after delete-remember-frame activate)
  "Advise remember-finalize to close the frame if it is the remember frame"
  (if (equal "remember" (frame-parameter nil 'name))
      (delete-frame)))

(defadvice remember-destroy (after delete-remember-frame activate)
  "Advise remember-destroy to close the frame if it is the rememeber frame"
  (if (equal "remember" (frame-parameter nil 'name))
      (delete-frame)))

;; make the frame contain a single window. by default org-remember
;; splits the window.
(add-hook 'remember-mode-hook
          'delete-other-windows)

(defun make-remember-frame ()
  "Create a new frame and run org-remember."
  (interactive)
  (make-frame '((name . "remember") (width . 80) (height . 10)))
  (select-frame-by-name "remember")
  (org-remember))

;; org-mobile setup
(setq org-mobile-directory (my-org-file "stage/"))
(setq org-mobile-inbox-for-pull (my-org-file "from-mobile.org"))

(case system-type
  ('windows-nt (setq 
								mine-shell-copy-command                 "cp -r"
								mine-org-mobile-local-staging-glob      "~/org/stage/*"
								mine-org-mobile-local-staging-directory "~/org/stage/"
								mine-org-mobile-local-staging-file      "~/org/stage/mobileorg.org"
								mine-org-mobile-remote-staging-file     "m:/org/mobileorg.org"
								mine-org-mobile-remote-directory        "m:/org/"
								))) 

(autoload 'org-mobile-push "org-mobile" "Push the state of the org files to org-mobile-directory" t)
(autoload 'org-mobile-pull "org-mobile" "Pull the contents of org-mobile-capture-file" t)

(defun copy-staged-files-to-remote ()
	(shell-command (format "%s %s %s"
												 mine-shell-copy-command 
												 mine-org-mobile-local-staging-glob 
												 mine-org-mobile-remote-directory)))

(defun copy-remote-changes-to-local-staging ()
	(shell-command (format "%s %s %s"
												 mine-shell-copy-command
												 mine-org-mobile-remote-staging-file
												 mine-org-mobile-local-staging-directory)))

(defun update-remote-with-applied-changes ()
	(shell-command (format "%s %s %s"
												 mine-shell-copy-command
												 mine-org-mobile-local-staging-file
												 mine-org-mobile-remote-directory)))

(defun mine-org-mobile-sync ()
  (interactive)
  (message (format "Syncing org-mobile at %s" (current-time-string)))
  (org-mobile-pull)
  (org-mobile-push))

(add-hook 'org-mobile-post-push-hook
       (lambda () (copy-staged-files-to-remote)))
(add-hook 'org-mobile-pre-pull-hook
       (lambda () (copy-remote-changes-to-local-staging)))
(add-hook 'org-mobile-post-pull-hook
       (lambda () (update-remote-with-applied-changes)))

(run-at-time t 7200 'mine-org-mobile-sync)

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