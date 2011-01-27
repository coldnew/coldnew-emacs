
;;;;;; Auto-mode alist
(add-to-list 'auto-mode-alist '("\\.nl$"  . newlisp-mode))
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
(add-hook 'newlisp-mode-hook
	  '(lambda ()
	     ;; Normal map
	     (vim:local-nmap (kbd "M-j") 'newlisp-next-function)
	     (vim:local-nmap (kbd "M-k") 'newlisp-previous-function)
	     (vim:local-nmap (kbd "z;") 'newlisp-context-qualify)
	     (vim:local-nmap (kbd "zE") 'newlisp-sexp-end)
	     (vim:local-nmap (kbd "zI") 'newlisp-indent-line)
	     (vim:local-nmap (kbd "zS") 'newlisp-sexp-start)
	     (vim:local-nmap (kbd "zd") 'newlisp-delete-sexp)
	     (vim:local-nmap (kbd "ze") 'newlisp-evaluate-buffer)
	     (vim:local-nmap (kbd "zf") 'newlisp-evaluate-function)
	     (vim:local-nmap (kbd "zi") 'run-newlisp)
	     (vim:local-nmap (kbd "zl") 'newlisp-clear-comint-buffer)
	     (vim:local-nmap (kbd "zn") 'newlisp-nudge-region)
	     (vim:local-nmap (kbd "zr") 'newlisp-evaluate-region)
	     (vim:local-nmap (kbd "zs") 'newlisp-evaluate-prev-sexp)
	     (vim:local-nmap (kbd "zt") 'newlisp-tab-region)
	     (vim:local-nmap (kbd "zv") 'newlisp-visit-interpreter)
	     (vim:local-nmap (kbd "zx") 'newlisp-indent-sexp)
	     (vim:local-nmap (kbd "zF") 'forward-sexp)
	     (vim:local-nmap (kbd "zB") 'backward-sexp)
	     ;; Insert Map
	     (vim:local-imap (kbd "M-k") 'newlisp-previous-function)
	     (vim:local-imap (kbd "M-j") 'newlisp-next-function)
	     (vim:local-imap (kbd "M-l") 'forward-sexp)
	     (vim:local-imap (kbd "M-h") 'backward-sexp)
	     ))

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
