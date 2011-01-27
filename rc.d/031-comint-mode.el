

;;;;;; Keybinding
(add-hook 'comint-mode-hook
	  '(lambda ()
	     (vim:local-nmap (kbd "M-k") 'comint-previous-input)
	     (vim:local-nmap (kbd "M-j") 'comint-previous-input)
	     (vim:local-imap (kbd "RET") 'comint-send-input)
	     ))


;;;;;; Functions
(defun comint-mode:clear-region ()
  (interactive)
  (delete-region (point-min) (point-max))
  (comint-send-input))


(provide '031-comint-mode)
;;  031-comint-mode.el ends here.
