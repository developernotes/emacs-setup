
(add-hook 'Info-mode-hook
					'(lambda ()
						 (define-key Info-mode-map [?j] 
							 'next-line)))

(add-hook 'Info-mode-hook
					'(lambda ()
						 (define-key Info-mode-map [?k]
							 'previous-line)))

(provide 'mine-info-mode)