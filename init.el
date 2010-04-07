(defvar emacs-root (concat (getenv "HOME") "/.emacs.d/"))

(defun add-path (path)
  (add-to-list 'load-path (concat emacs-root path)))

(defvar *emacs-load-start* (current-time))

(add-path "mine")
(add-path "site-lisp")

(require 'cl)
(require 'mine-vc)
(require 'mine-navigation)
(require 'mine-dependencies)
(require 'mine-customizations)
(require 'mine-defuns)
(require 'mine-advice)
(require 'mine-bindings)
(require 'mine-pretty)
(require 'mine-erc)
(require 'mine-org-mode)
(require 'mine-info-mode)
(require 'mine-bookmark)
(require 'mine-color-theme)
(require 'mine-auto-complete)
(require 'mine-dired)
(require 'mine-shell)
(require 'mine-html)
(require 'mine-csharp)
(require 'mine-ruby)
(require 'mine-javascript)
(require 'mine-haskell)

(case system-type
  ('windows-nt (require 'mine-windows))
  ('darwin (require 'mine-osx)))

(setq debug-on-error nil)

(server-start)

(message "My .emacs loaded in %ds." (destructuring-bind (hi lo ms) (current-time) (- (+ hi lo) (+ (first *emacs-load-start*) (second *emacs-load-start*)))))

(cd "~/")
(gtd-agenda)
