
(require 'bookmark)

(setq bookmark-default-file "~/bookmarks/emacs.bmk")

(defadvice bookmark-save (before adjust-file-path activate compile)
	"Adjusts the sicp bookmark to always use the tilde path operator over a fully qualified path"
	(if (bookmark-get-bookmark "sicp")
			(progn 
				(bookmark-prop-set 
				 (bookmark-get-bookmark "sicp") 
				 'filename "~/info/sicp.info")))
	(if (bookmark-get-book-mark "clj")
			(progn
				(bookmark-prop-set
				 (bookmark-get-bookmark "clj")
				 'filename "~/pdf/ClojureInAction.pdf")))
	)




(provide 'mine-bookmark)