;; init for matlab

(provide 'rc-matlab-mode)
(add-to-list 'auto-mode-alist '("\\.m$" . matlab-mode))

(when (require 'matlab nil 'noerror)
  (setq matlab-shell-command "/opt/matlab/bin/matlab")
  (setq matlab-shell-command-switches '("-nosplash" "-nodesktop"))

  ;; customize matlab-shell-mode
  (add-hook 'matlab-shell-mode-hook
	    '(lambda ()
	       (when (featurep 'vim)
		 (vim:imap (kbd "RET") 'comint-send-input))
	       ))

  (add-hook 'matlab-mode-hook
	    '(lambda ()
	       ))


  )
