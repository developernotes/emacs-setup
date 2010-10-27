
(add-path "site-lisp/rinari")

(require 'rinari)
(require 'ruby-electric)

(case system-type
  ('darwin (require 'rvm)
           (rvm-use-default)))

;;File type associations
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\Gemfile\\'" . ruby-mode))

(add-hook 'ruby-mode-hook '(lambda () (ruby-electric-mode t)))

(define-key ruby-mode-map "\r" 'reindent-then-newline-and-indent)

;; autotest setup
(autoload 'autotest "autotest" "Run autotest" t)
(setq autotest-use-ui t)

(defun autotest-rspec ()
  "Runs autotest as rspec enabled"
  (interactive)
  (setq autotest-command "RSPEC=true autotest")
  (autotest)
  (setq autotest-command "autotest"))

(defun autotest-rspec-with-features ()
  "Runs autotest as rspec and cucumber features enabled"
  (interactive)
  (setq autotest-command "AUTOFEATURE=true RSPEC=true autotest")
  (autotest)
  (setq autotest-command "autotest"))

(defun rinari-web-server-restart ()
  "If rinari-web-server is running, kill it and start a new server, otherwise just launch the server"
  (interactive)
  (let ((rinari-web-server-buffer "*server*"))
  (if (get-buffer rinari-web-server-buffer)
      (progn
        (set-process-query-on-exit-flag (get-buffer-process rinari-web-server-buffer) nil)
        (kill-buffer rinari-web-server-buffer))
    nil)
    (rinari-web-server)))



(autoload 'feature-mode "feature-mode" "Major mode for editing plain text stories" t)
(add-to-list 'auto-mode-alist '("\\.feature\\'" . feature-mode))
(define-key ruby-mode-map (kbd "C-c C-a") 'autotest-switch)

(provide 'mine-ruby)
