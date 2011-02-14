;;
(eval-when-compile (require 'cl))



;; auto-update egg-status on file save
(setq egg-auto-update t)

;; do not switch to the status buffer in the same window
(setq egg-switch-to-buffer t)

;;;;;;;; kyebindings
;; all local-nmap use prefix M- ,this make me don't need to switch imap to press command
(add-hook 'egg-status-buffer-mode-hook
	  '(lambda ()
	     (vim:local-nmap (kbd "M-s") 'egg-stage-untracked-files)
	     (vim:local-nmap (kbd "M-S") 'egg-stage-all-files)
	     (vim:local-nmap (kbd "M-c") 'egg-commit-log-edit)
	     (vim:local-nmap (kbd "M-p") 'egg-buffer-cmd-navigate-prev)
	     (vim:local-nmap (kbd "M-n") 'egg-buffer-cmd-navigate-next)
	     (vim:local-nmap (kbd "M-l") 'egg-log)
	     (vim:local-nmap (kbd "M-g") 'egg-status-buffer-redisplay)
	     (vim:local-nmap (kbd "M-h") 'egg-section-cmd-toggle-hide-show)
	     (vim:local-nmap (kbd "M-H") 'egg-section-cmd-toggle-hide-show-children)
	     (vim:local-nmap (kbd "M-q") 'egg-quit-buffer)
	     (vim:local-nmap (kbd "M-u") 'egg-diff-section-cmd-undo)
	     ))
(add-hook 'egg-log-msg-mode-hook
	  '(lambda ()
	     (vim:local-nmap (kbd "M-p") 'egg-buffer-cmd-navigate-prev)
	     (vim:local-nmap (kbd "M-n") 'egg-buffer-cmd-navigate-next)
	     (vim:local-nmap (kbd "M-/") 'egg-search-changes)
	     (vim:local-nmap (kbd "M-s") 'egg-status)
	     (vim:local-nmap (kbd "M-G") 'egg-buffer-cmd-refresh)
	     (vim:local-nmap (kbd "M-q") 'egg-quit-buffer)
	     (vim:local-nmap (kbd "M-h") 'egg-section-cmd-toggle-hide-show)
	     (vim:local-nmap (kbd "M-H") 'egg-section-cmd-toggle-hide-show-children)
	     (vim:local-nmap (kbd "M-o") 'egg-log-buffer-checkout-commit)
	     (vim:local-nmap (kbd "M-b") 'egg-log-buffer-start-new-branch)
	     (vim:local-nmap (kbd "M-a") 'egg-log-buffer-attach-head)
	     (vim:local-nmap (kbd "M-t") 'egg-log-buffer-tag-commit)
	     (vim:local-nmap (kbd "M-T") 'egg-log-buffer-atag-commit)
	     (vim:local-nmap (kbd "M-B") 'egg-log-buffer-create-new-branch)
	     (vim:local-nmap (kbd "M-m") 'egg-log-buffer-merge)
	     (vim:local-nmap (kbd "M-r") 'egg-log-buffer-rebase)
	     (vim:local-nmap (kbd "M-R") 'egg-log-buffer-rebase-interactive)
	     ;;
	     ;; (vim:local-nmap (kbd "S") 'egg-stage-all-files)
	     ;; (vim:local-nmap (kbd "c") 'egg-commit-log-edit)
	     ;; (vim:local-nmap (kbd "l") 'egg-log)
	     ;; (vim:local-nmap (kbd "u") 'egg-diff-section-cmd-undo)
	     ))

;;;;;;;; Advice

;; visiting a file in ibuffer makes it "fullscreen"
(defadvice egg-status (around goto-egg-status-buffer activate)
  "Delete other windows after visiting egg-status"
  ad-do-it
  (delete-other-windows))


(provide '054-egg)
;; 054-egg.el ends here.
