(defvar emacs-root (concat (getenv "HOME") "/.emacs.d/"))

(defun add-path (path)
  (add-to-list 'load-path (concat emacs-root path)))

(add-path "mine")
(add-path "site-lisp")

(require 'cl)
(require 'cl-lib)
(require 'mine-navigation)
(require 'mine-customizations)
(require 'mine-defuns)
(require 'mine-advice)
(require 'mine-package)
(require 'mine-el-get)
(require 'mine-android)
(require 'mine-javascript)
(require 'mine-pretty)
(require 'mine-erc)
(require 'mine-dired)
(require 'mine-magit)
(require 'mine-shell)
(require 'mine-c)
(require 'mine-bookmark)
(require 'mine-ag)
(require 'mine-spotify)
(require 'mine-bindings)

(case system-type
  ('windows-nt (require 'mine-windows))
  ('darwin (require 'mine-osx)))

(setq debug-on-error nil)

(cd "~/")
(mine-server-start)

(message (format "emacs loaded in %.1f seconds"
                 (float-time
                  (time-subtract (current-time) before-init-time))))
