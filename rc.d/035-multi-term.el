
(setq term-default-bg-color nil)
(setq term-default-fg-color nil)

(setq multi-term-program "/bin/bash")

;;;; Keybindings
(add-hook 'term-mode-hook
	  '(lambda ()
	     (vim:local-imap (kbd "<f3>") 'shell-pop)
	     (vim:local-imap (kbd "RET") 'term-send-raw)
	     ))


(provide '035-multi-term)
;; 035-multi-term.el ends here.
