;;
(eval-when-compile (require 'cl))



;; auto-update egg-status on file save
(setq egg-auto-update t)

;; do not switch to the status buffer in the same window
(setq egg-switch-to-buffer nil)

;; kyebindings
(add-hook 'egg-status-buffer-mode-hook
	  '(lambda ()
	     (vim:local-nmap (kbd "s") 'egg-stage-untracked-files)
	     (vim:local-nmap (kbd "S") 'egg-stage-all-files)
	     (vim:local-nmap (kbd "c") 'egg-commit-log-edit)
	     (vim:local-nmap (kbd "p") 'egg-buffer-cmd-navigate-prev)
	     (vim:local-nmap (kbd "n") 'egg-buffer-cmd-navigate-next)
	     (vim:local-nmap (kbd "l") 'egg-log)
	     ))

;;(define-key egg-buffer-mode-map)

(provide '054-egg)
;; 054-egg.el ends here.
