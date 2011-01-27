
;;;;;; Auto-mode alist
(add-to-list 'auto-mode-alist '("\\.nl$" . newlisp-mode))

;;;;;; Hook
(add-hook 'newlisp-mode-hook
	  '(lambda ()
	     ;; Highlight the parentheses
	     (highlight-parentheses-mode)
	     ;; Enable pretty-lambda
	     (turn-on-pretty-lambda-mode)
	     ;; programming common hook
	     (programming-common-hook)
	     ))


(provide 'lang-newlisp)
;; lang-newlisp.el ends here.
