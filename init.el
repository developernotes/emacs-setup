(defvar emacs-root (concat (getenv "HOME") "/.emacs.d/"))

(defun add-path (path)
  (add-to-list 'load-path (concat emacs-root path)))

(defun multi-require (packages)
  (mapcar (lambda (package)
            (require package)) packages))

(defvar *emacs-load-start* (current-time))

(add-path "mine")
(add-path "site-lisp")

(multi-require '(cl mine-vc mine-navigation mine-dependencies mine-customizations
                 mine-defuns mine-advice mine-bindings mine-package mine-pretty
                 mine-erc mine-org-mode mine-tex mine-auto-complete mine-dired
                 mine-shell mine-html mine-csharp mine-ruby mine-javascript
                 mine-haskell mine-lisp mine-showoff))

(case system-type
  ('windows-nt (require 'mine-windows))
  ('darwin (require 'mine-osx)))

(setq debug-on-error nil)

(message "My .emacs loaded in %ds."
         (destructuring-bind (hi lo ms)
             (current-time) (- (+ hi lo) (+ (first *emacs-load-start*) (second *emacs-load-start*)))))

(cd "~/")
