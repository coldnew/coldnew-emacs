;;
(eval-when-compile (require 'cl))

;; Shell-pop
(shell-pop-set-internal-mode "ansi-term")
(shell-pop-set-internal-mode-shell "/bin/bash")
(shell-pop-set-window-height 20)
(shell-pop-set-window-position "bottom")

;; Multi-term

(setq multi-term-program "/bin/bash")

;; Term
(setq term-default-bg-color nil)
(setq term-default-fg-color nil)

;;;; Keybindings
(add-hook 'term-mode-hook
	  '(lambda ()
	     (vim:local-nmap (kbd "C-p") 'term-send-up)
	     (vim:local-nmap (kbd "C-n") 'term-send-down)
	     (vim:local-imap (kbd "<f4>") 'shell-pop)
	     (vim:local-imap (kbd "RET") 'term-send-raw)
	     ))


(provide '034-terminal)
;; 034-terminal.el ends here.
