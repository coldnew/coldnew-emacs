;;
(eval-when-compile (require 'cl))


;;;;;;;; Packages Import
(require 'coldnew-macro)
(require 'coldnew-functions)
(require 'coldnew-commands)
(require 'coldnew-variables)
(require 'coldnew-vim)



(when (require* 'egg)
  ;; do not auto-update egg-status on file save
  (setq egg-auto-update nil)

  ;; do not switch to the status buffer in the same window
  (setq egg-switch-to-buffer t)

;;;;;;;; kyebindings
  (add-hook 'egg-status-buffer-mode-hook
	    '(lambda ()
	       (vim:local-nmap (kbd "j") 'egg-buffer-cmd-navigate-next)
	       (vim:local-nmap (kbd "k") 'egg-buffer-cmd-navigate-prev)
	       (vim:local-imap (kbd "j") 'vim:motion-down)
	       (vim:local-imap (kbd "k") 'vim:motion-up)
	       (vim:local-nmap (kbd "c") 'egg-commit-log-edit)
	       (vim:local-nmap (kbd "l") 'egg-log)
	       ))
  (add-hook 'egg-commit-buffer-mode-hook
	    '(lambda ()
	       (vim:local-nmap (kbd "c") 'egg-log-msg-done)
	       ))

  (add-hook 'egg-log-buffer-mode-hook
	    '(lambda ()
	       ;;;; Normal-map
	       (vim:local-nmap (kbd "c") 'egg-log-msg-done)
	       (vim:local-nmap (kbd "j") 'egg-log-buffer-next-ref)
	       (vim:local-nmap (kbd "k") 'egg-log-buffer-prev-ref)
	       (vim:local-nmap (kbd "h") 'vim:motion-left)
	       (vim:local-nmap (kbd "l") 'vim:motion-right)
	       (vim:local-nmap (kbd "U") 'egg-log-buffer-push-to-remote)
	       (vim:local-nmap (kbd "u") 'egg-log-buffer-push-to-local)
	       (vim:local-nmap (kbd "s") 'egg-status)
	       ;;;; Insert-map
	       (vim:local-imap (kbd "j") 'vim:motion-down)
	       (vim:local-imap (kbd "k") 'vim:motion-up)
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

  ;; FIXME:bug?
  ;; (defadvice egg-log-msg-done (around kill-egg-commit-buffer activate)
  ;;   "Delete egg-commit-buffer after commit."
  ;;   (let ((egg-commit-buffer (current-buffer)))
  ;;     ad-do-it
  ;;     (kill-buffer egg-commit-buffer)))
  )


(provide 'coldnew-git)
;; coldnew-git.el ends here.
