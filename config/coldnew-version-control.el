;;; coldnew-version-control.el ---
(eval-when-compile (require 'cl))

;;;; ---------------------------------------------------------------------------
;;;; Egg
;;;; ---------------------------------------------------------------------------
(require 'egg)

;; do not auto-update egg-status on file save
(setq egg-auto-update nil)

;; do not switch to the status buffer in the same window
(setq egg-switch-to-buffer t)

;; make egg auto guess next action
(setq egg-confirm-next-action nil)

;; remodify next-action priority
(defsubst egg-guess-next-action (desc)
  (cond
   ((memq :file-has-merged-conflict desc) :merge-file)
   ((memq :file-is-modified desc)         :stage-file)
   ((memq :has-staged-changes desc)       :commit)
   ((memq :file-is-unmerged desc)         :stage-file)
   ((memq :wdir-has-merged-conflict desc) :status)
   ((memq :wdir-has-unmerged-files  desc) :stage-all)
   ((memq :wdir-is-modified desc)         :stage-all)
   ((memq :rebase-in-progress desc)       :rebase-continue)
   (t                                     :quit)))

;;;;;;;; Advice
(defadvice egg-status (around goto-egg-status-buffer activate)
  "Delete other windows after visiting egg-status."
  ad-do-it
  (delete-other-windows))

(defadvice egg-commit-log-edit (around goto-egg-commit-buffer activate)
  "Delete other windows after visiting egg-commit-buffer."
  ad-do-it
  (delete-other-windows))


;;;;;;;; Magit
(require 'magit)
;; if use magit, do not use vc-git to handle Git interface.
;; (when (featurep 'magit)
;;   (setq vc-handled-backends (remq 'Git vc-handled-backends)))
(require 'git-emacs)
;;   ;; disable git-emacs's advice
;;   (ad-disable-advice 'vc-next-action 'around 'git--vc-git-next-action)
;;   (ad-activate 'vc-next-action)
(define-key git--branch-mode-map (kbd "C-g") 'git--quit-buffer)





(provide 'coldnew-version-control)
;; coldnew-version-control.el ends here.
