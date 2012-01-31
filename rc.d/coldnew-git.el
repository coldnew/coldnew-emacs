;;
(eval-when-compile (require 'cl))


;;;;;;;; Packages Import
(require 'coldnew-macro)
(require 'coldnew-functions)
(require 'coldnew-commands)
(require 'coldnew-variables)
(require 'coldnew-evil)



(when (require* 'egg)

  ;; do not auto-update egg-status on file save
  (setq egg-auto-update nil)

  ;; do not switch to the status buffer in the same window
  (setq egg-switch-to-buffer t)

;;;;;;;; kyebindings
  ;; egg status buffer
  (evil-define-key 'normal egg-status-buffer-mode-map (kbd "j") 'egg-buffer-cmd-navigate-next)
  (evil-define-key 'normal egg-status-buffer-mode-map (kbd "k") 'egg-buffer-cmd-navigate-prev)
  (evil-define-key 'normal egg-status-buffer-mode-map (kbd "c") 'egg-commit-log-edit)
  (evil-define-key 'normal egg-status-buffer-mode-map (kbd "l") 'egg-log)
  (evil-define-key 'insert egg-status-buffer-mode-map (kbd "j") 'evil-next-line)
  (evil-define-key 'insert egg-status-buffer-mode-map (kbd "k") 'evil-previous-line)

  ;; egg commit buffer
  (add-hook 'egg-commit-buffer-mode-hook
	    '(lambda ()
	       (define-key evil-normal-state-local-map (kbd "c") 'egg-log-msg-done)))
  ;; egg log buffer mode
  (evil-define-key 'normal egg-log-buffer-mode-map (kbd "c") 'egg-log-msg-done)
  (evil-define-key 'normal egg-log-buffer-mode-map (kbd "j") 'egg-log-buffer-next-ref)
  (evil-define-key 'normal egg-log-buffer-mode-map (kbd "k") 'egg-log-buffer-prev-ref)
  (evil-define-key 'normal egg-log-buffer-mode-map (kbd "h") 'evil-backward-char)
  (evil-define-key 'normal egg-log-buffer-mode-map (kbd "l") 'evil-forward-char)
  (evil-define-key 'normal egg-log-buffer-mode-map (kbd "U") 'egg-log-buffer-push-to-remote)
  (evil-define-key 'normal egg-log-buffer-mode-map (kbd "u") 'egg-log-buffer-push-to-local)
  (evil-define-key 'normal egg-log-buffer-mode-map (kbd "s") 'egg-status)
  (evil-define-key 'insert egg-log-buffer-mode-map (kbd "j") 'evil-next-line)
  (evil-define-key 'insert egg-log-buffer-mode-map (kbd "k") 'evil-previous-line)

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
