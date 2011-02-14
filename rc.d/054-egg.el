;;
(eval-when-compile (require 'cl))



;; auto-update egg-status on file save
(setq egg-auto-update t)

;; do not switch to the status buffer in the same window
(setq egg-switch-to-buffer t)

;; kyebindings
(add-hook 'egg-status-buffer-mode-hook
	  '(lambda ()
	     (vim:local-nmap (kbd "s") 'egg-stage-untracked-files)
	     (vim:local-nmap (kbd "S") 'egg-stage-all-files)
	     (vim:local-nmap (kbd "c") 'egg-commit-log-edit)
	     (vim:local-nmap (kbd "p") 'egg-buffer-cmd-navigate-prev)
	     (vim:local-nmap (kbd "n") 'egg-buffer-cmd-navigate-next)
	     (vim:local-nmap (kbd "l") 'egg-log)
	     (vim:local-nmap (kbd "G") 'egg-status-buffer-redisplay)
	     (vim:local-nmap (kbd "h") 'egg-section-cmd-toggle-hide-show)
	     (vim:local-nmap (kbd "H") 'egg-section-cmd-toggle-hide-show-children)
	     (vim:local-nmap (kbd "h") 'egg-quit-buffer)
	     (vim:local-nmap (kbd "u") 'egg-diff-section-cmd-undo)
	     ))

;;;;;;;; Advice

;; visiting a file in ibuffer makes it "fullscreen"
(defadvice egg-status (around goto-egg-status-buffer activate)
  "Delete other windows after visiting egg-status"
  ad-do-it
  (delete-other-windows))


(provide '054-egg)
;; 054-egg.el ends here.
