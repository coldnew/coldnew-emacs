;; init for matlab

(provide 'rc-matlab-mode)
(add-to-list 'auto-mode-alist '("\\.m$" . matlab-mode))

(when (require 'matlab nil 'noerror)
  (setq matlab-shell-command "/opt/matlab/bin/matlab")
  (setq matlab-shell-command-switches '("-nosplash" "-nodesktop"))

  ;; Matlab Shell Mode
  (add-hook 'matlab-shell-mode-hook
	    '(lambda ()
	       (when (require 'rc-vim-mode nil 'noerror)
		 (vim:imap (kbd "RET") 'comint-send-input))
	       ))
  ;; Matlab Mode
  (add-hook 'matlab-mode-hook
	    '(lambda ()
	       ))


  )
