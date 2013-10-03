(add-path "el-get/el-get")

(unless (require 'el-get nil t)
  (url-retrieve
   "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
   (lambda (s)
     (goto-char (point-max))
     (eval-print-last-sexp))))

(setq el-get-user-package-directory "~/.emacs.d/init")

(setq mine-pkgs-to-install
      (append
       '(
         ;;auto-complete
         browse-kill-ring
         csharp-mode
         geiser
         magit
         markdown-mode
         multiple-cursors
         org-mode
         paredit
         rust-mode
         smex
         switch-window
         undo-tree
         scratch
         yasnippet)
       (mapcar 'el-get-source-name el-get-sources)))

(el-get 'sync mine-pkgs-to-install)

(provide 'mine-el-get)
