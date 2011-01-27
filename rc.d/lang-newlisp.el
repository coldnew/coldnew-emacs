
;;;;;; Auto-mode alist
(add-to-list 'auto-mode-alist '("\\.nl$" . newlisp-mode))
(add-to-list 'auto-mode-alist '("\\.lsp$" . newlisp-mode))


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
;;;;;; Keybindings

;;;;;; Functions
(defun run-newlisp ()
  "Starts newlisp interperter/or shows if already running.  Requires
newlisp-mode to be loaded."
  (interactive)
  (newlisp-show-interpreter)
  (other-window 1)
  )



(provide 'lang-newlisp)
;; lang-newlisp.el ends here.
