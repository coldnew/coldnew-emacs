;;
(eval-when-compile (require 'cl))



;; do not auto-update egg-status on file save
(setq egg-auto-update nil)

;; do not switch to the status buffer in the same window
(setq egg-switch-to-buffer t)

;;;;;;;; kyebindings
(add-hook 'egg-status-buffer-mode-hook
	  '(lambda ()
	     ;; I want to use j and k to move
	     (vim:local-nmap (kbd "j") 'egg-buffer-cmd-navigate-next)
	     (vim:local-nmap (kbd "k") 'egg-buffer-cmd-navigate-prev)
	     (vim:local-imap (kbd "j") 'vim:motion-down)
	     (vim:local-imap (kbd "k") 'vim:motion-up)
	     ;; Following key missing in nmap
	     (vim:local-nmap (kbd "c") 'egg-commit-log-edit)
	     (vim:local-nmap (kbd "l") 'egg-log)
	     ))

(add-hook 'egg-log-msg-mode-hook
	  '(lambda ()
	     ;; Use c in nmap to commit
	     (vim:local-nmap (kbd "c") 'egg-log-msg-done)
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
  "Delete egg-status buffer after visiting egg-commit-buffer."
  (let ((egg-status-buffer (current-buffer)))
    (kill-buffer egg-status-buffer)
    ad-do-it))

(defadvice egg-log-msg-done (around kill-egg-commit-buffer activate)
  "Delete egg-commit-buffer after commit."
  (let ((egg-commit-buffer (current-buffer)))
    ad-do-it
    (kill-buffer egg-commit-buffer)))

(provide '054-egg)
;; 054-egg.el ends here.
