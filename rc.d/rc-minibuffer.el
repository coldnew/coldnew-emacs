;;
(provide 'rc-minibuffer)

;;;; Minibuffer
(add-hook 'minibuffer-setup-hook
	  '(lambda ()
	     (local-set-key (kbd "M-g")   'minibuffer-keyboard-quit)
	     (local-set-key (kbd "M-h")   'backward-char)
	     (local-set-key (kbd "M-l")   'forward-char)
	     (local-set-key (kbd "M-j")   'next-history-element)
	     (local-set-key (kbd "M-k")   'previous-history-element)
	     (local-set-key (kbd "M-J")   'next-matching-history-element)
	     (local-set-key (kbd "M-K")   'previous-matching-history-element)
	     (local-set-key (kbd "M-b")   'backward-word)
	     (local-set-key (kbd "M-w")   'forward-word)
	     (local-set-key (kbd "C-w")   'kill-word)
	     (local-set-key (kbd "C-b")   'backward-kill-word)))

;;;; switch-buffer
