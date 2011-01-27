


(add-hook 'lusty-setup-hook
	  '(lambda ()
	     (define-key lusty-mode-map (kbd "RET") 'lusty-select-current-name)

	     ))


(provide '029-lusty-explorer)
;; 029-lusty-explorer.el ends here.
