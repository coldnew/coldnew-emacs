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
	       (define-key evil-normal-state-local-map(kbd "j") 'egg-buffer-cmd-navigate-next)
	       (define-key evil-normal-state-local-map (kbd "k") 'egg-buffer-cmd-navigate-prev)
	       ;; (define-key evil-insert-state-local-map (kbd "j") 'vim:motion-down)
	       ;; (define-key evil-insert-state-local-map (kbd "k") 'vim:motion-up)
	       (define-key evil-normal-state-local-map (kbd "c") 'egg-commit-log-edit)
	       (define-key evil-normal-state-local-map (kbd "l") 'egg-log)
	       ))

  (add-hook 'egg-commit-buffer-mode-hook
	    '(lambda ()
	       (define-key evil-normal-state-local-map (kbd "c") 'egg-log-msg-done)
	       ))

  (add-hook 'egg-log-buffer-mode-hook
	    '(lambda ()
	       ;;;; Normal-map
	       (define-key evil-normal-state-local-map (kbd "c") 'egg-log-msg-done)
	       (define-key evil-normal-state-local-map (kbd "j") 'egg-log-buffer-next-ref)
	       (define-key evil-normal-state-local-map (kbd "k") 'egg-log-buffer-prev-ref)
	       ;; (define-key evil-normal-state-local-map (kbd "h") 'vim:motion-left)
	       ;; (define-key evil-normal-state-local-map (kbd "l") 'vim:motion-right)
	       (define-key evil-normal-state-local-map (kbd "U") 'egg-log-buffer-push-to-remote)
	       (define-key evil-normal-state-local-map (kbd "u") 'egg-log-buffer-push-to-local)
	       (define-key evil-normal-state-local-map (kbd "s") 'egg-status)
	       ;;;; Insert-map
	       ;; (define-key evil-insert-state-local-map (kbd "j") 'vim:motion-down)
	       ;; (define-key evil-insert-state-local-map (kbd "k") 'vim:motion-up)
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
