(let ((file-name-handler-alist nil))
  (defvar emacs-root (concat (getenv "HOME") "/.emacs.d/"))

  (defun add-path (path)
    (add-to-list 'load-path (concat emacs-root path)))

  (add-path "init")
  (add-path "init/custom")
  (add-path "site-lisp")
  (add-path "site-lisp/themes")

  (require 'init-package)
  (package-initialize)
  (mapc (lambda (init-file)
          (setq init-file-before-load-time (current-time))
          (load init-file nil t)
          (message (format "Load time %.3f seconds for %s"
                           (float-time
                            (time-subtract (current-time)
                                           init-file-before-load-time))
                           (file-name-nondirectory init-file))))
        (file-expand-wildcards (concat emacs-root "init/custom/*.el")))
  (require 'init-navigation)
  (require 'init-defuns)
  (require 'init-advice)
  (require 'init-customizations)
  (require 'init-bindings)

  (cd "~/")
  (mine-server-start)
  (message (format "Emacs loaded in %.2f seconds"
                   (float-time
                    (time-subtract (current-time) before-init-time)))))

(defvar theme-name-dark 'underwater)
(defvar theme-name-light 'standard-light)

(if (string-equal system-type "darwin")
    (if (is-dark-mode)
        (set-theme theme-name-dark)
      (set-theme theme-name-light))
  (set-theme theme-name-light))

(center-selected-frame)
