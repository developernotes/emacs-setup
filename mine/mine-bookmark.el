
(require 'bookmark)

(setq bookmark-default-file "~/bookmarks/emacs.bmk")

(defadvice bookmark-save (before adjust-file-path activate compile)
	"Adjusts the bookmarks to always use the tilde path operator over a fully qualified path"
	(rebind-path "sicp" "~/info/sicp.info")
	(rebind-path "clj"  "~/pdf/ClojureInAction.pdf")
	(rebind-path "zsh"  "~/info/zsh.info"))

(defun rebind-path(key path)
	(if (bookmark-get-bookmark key)
			(progn
				(bookmark-prop-set
				 (bookmark-get-bookmark key)
				 'filename path))))

(provide 'mine-bookmark)