;;
(eval-when-compile (require 'cl))



;; do not auto-update egg-status on file save
(setq egg-auto-update nil)

;; do not switch to the status buffer in the same window
(setq egg-switch-to-buffer t)

;;;;;;;; kyebindings
;; all local-nmap use prefix M- ,this make me don't need to switch imap to press command
(add-hook 'egg-status-buffer-mode-hook
	  '(lambda ()
	     ;; I want to use j and k to move
	     (vim:local-nmap (kbd "j") 'egg-buffer-cmd-navigate-next)
	     (vim:local-nmap (kbd "k") 'egg-buffer-cmd-navigate-prev)
	     (vim:local-imap (kbd "j") 'vim:motion-down)
	     (vim:local-imap (kbd "k") 'vim:motion-up)
	     ;; Following key missing in nmap
	     (vim:local-nmap (kbd "c") 'egg-commit-log-edit)
	     ;;	     (vim:local-nmap (kbd "b") 'vim:motion-bwd-word)
	     ;;	     (vim:local-nmap (kbd "h") 'vim:motion-left)
	     ;;	     (vim:local-nmap (kbd "M-s") 'egg-stage-untracked-files)
	     ;;	     (vim:local-nmap (kbd "M-S") 'egg-stage-all-files)
	     ;;	     (vim:local-nmap (kbd "M-c") 'egg-commit-log-edit)
	     ;;	     (vim:local-nmap (kbd "M-p") 'egg-buffer-cmd-navigate-prev)
	     ;;	     (vim:local-nmap (kbd "M-n") 'egg-buffer-cmd-navigate-next)
	     ;;	     (vim:local-nmap (kbd "M-l") 'egg-log)
	     ;;	     (vim:local-nmap (kbd "M-g") 'egg-status-buffer-redisplay)
	     ;;	     (vim:local-nmap (kbd "M-h") 'egg-section-cmd-toggle-hide-show)
	     ;;	     (vim:local-nmap (kbd "M-H") 'egg-section-cmd-toggle-hide-show-children)
	     ;;	     (vim:local-nmap (kbd "M-q") 'egg-quit-buffer)
	     ;;	     (vim:local-nmap (kbd "M-u") 'egg-diff-section-cmd-undo)
	     ))
(add-hook 'egg-log-msg-mode-hook
	  '(lambda ()
	     ;;	     (vim:local-nmap (kbd "b") 'vim:motion-bwd-word)
	     ;;	     (vim:local-nmap (kbd "h") 'vim:motion-left)
	     ;;	     (vim:local-nmap (kbd "p") 'egg-buffer-cmd-navigate-prev)
	     ;;	     (vim:local-nmap (kbd "n") 'egg-buffer-cmd-navigate-next)
	     ;;	     (vim:local-nmap (kbd "/") 'egg-search-changes)
	     ;;	     (vim:local-nmap (kbd "s") 'egg-status)
	     ;;	     (vim:local-nmap (kbd "G") 'egg-buffer-cmd-refresh)
	     ;;	     (vim:local-nmap (kbd "q") 'egg-quit-buffer)
	     ;;	     (vim:local-nmap (kbd "M-h") 'egg-section-cmd-toggle-hide-show)
	     ;;	     (vim:local-nmap (kbd "H") 'egg-section-cmd-toggle-hide-show-children)
	     ;;	     (vim:local-nmap (kbd "o") 'egg-log-buffer-checkout-commit)
	     ;;	     (vim:local-nmap (kbd "b") 'egg-log-buffer-start-new-branch)
	     ;;	     (vim:local-nmap (kbd "a") 'egg-log-buffer-attach-head)
	     ;;	     (vim:local-nmap (kbd "t") 'egg-log-buffer-tag-commit)
	     ;;	     (vim:local-nmap (kbd "T") 'egg-log-buffer-atag-commit)
	     ;;	     (vim:local-nmap (kbd "B") 'egg-log-buffer-create-new-branch)
	     ;;	     (vim:local-nmap (kbd "m") 'egg-log-buffer-merge)
	     ;;	     (vim:local-nmap (kbd "r") 'egg-log-buffer-rebase)
	     ;;	     (vim:local-nmap (kbd "R") 'egg-log-buffer-rebase-interactive)
	     ;;	     ;;
	     ;;	     ;; (vim:local-nmap (kbd "S") 'egg-stage-all-files)
	     ;;	     ;; (vim:local-nmap (kbd "c") 'egg-commit-log-edit)
	     ;;	     ;; (vim:local-nmap (kbd "l") 'egg-log)
	     ;;	     ;; (vim:local-nmap (kbd "u") 'egg-diff-section-cmd-undo)
	     ))

;;;;;;;; Advice

(defadvice egg-status (around goto-egg-status-buffer activate)
  "Delete other windows after visiting egg-status."
  ad-do-it
  (delete-other-windows))

(defadvice egg-commit-log-edit (around goto-egg-commit-buffer activate)
  "Delete other windows after visiting egg-commit-buffer."
  ad-do-it
  (delete-other-windows))

(defadvice egg-commit-log-edit (around kill-egg-status-buffer activate)
  "Delete other windows after visiting egg-commit-buffer."
  (kill-buffer)
  ad-do-it)

(defadvice egg-log-msg-done (before egg-log-msg-done activate)
  "Delete other windows after visiting egg-commit-buffer."
  ;; (kill-buffer)
  (message (buffer-name))
  )

(provide '054-egg)
;; 054-egg.el ends here.
