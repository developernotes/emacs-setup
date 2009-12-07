
(require 'bookmark)

(setq bookmark-default-file "~/bookmarks/emacs.bmk")

(defadvice bookmark-save (before adjust-file-path activate compile)
	"Adjusts the sicp bookmark to always use the tilde path operator over a fully qualified path"
	(bookmark-prop-set 
	 (bookmark-get-bookmark "sicp") 
	 'filename "~/info/sicp.info"))

(provide 'mine-bookmark)