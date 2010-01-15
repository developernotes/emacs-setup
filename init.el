(defvar emacs-root (concat (getenv "HOME") "/.emacs.d/"))

(defun add-path (path)
  (add-to-list 'load-path (concat emacs-root path)))

(defvar *emacs-load-start* (current-time))

(add-path "mine")
(add-path "vendor")
(add-path "themes")
(add-path "vendor/color-theme-6.6.0")

(require 'cl)
(require 'mine-ido)
(require 'mine-dependencies)
(require 'mine-advice)
(require 'mine-bindings)
(require 'mine-customizations)
(require 'mine-pretty)
(require 'mine-erc)
(require 'mine-org-mode)
(require 'mine-info-mode)
(require 'mine-bookmark)
(require 'mine-color-theme)
(require 'mine-maxframe)
(require 'mine-dired)
(require 'mine-jira)
(require 'mine-minimap)
(require 'mine-eshell)
(require 'mine-textmate)
(require 'feature-mode)
(require 'cucumber-mode)
(require 'mine-ruby)
(require 'mine-rinari)

(case system-type
  ('windows-nt (require 'mine-windows))
  ('darwin (require 'mine-osx)))

(setq debug-on-error nil)

(message "My .emacs loaded in %ds." (destructuring-bind (hi lo ms) (current-time) (- (+ hi lo) (+ (first *emacs-load-start*) (second *emacs-load-start*)))))

(cd "~/")
(gtd-agenda)
