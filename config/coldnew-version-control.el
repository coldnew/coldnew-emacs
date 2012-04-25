;;; coldnew-version-control.el ---
(eval-when-compile (require 'cl))

;;;; ---------------------------------------------------------------------------
;;;; Git
;;;; ---------------------------------------------------------------------------

;;;;;;;; Magit
(require 'magit)

;; if use magit, do not use vc-git to handle Git interface.
;; (when (featurep 'magit)
;;   (setq vc-handled-backends (remq 'Git vc-handled-backends)))

;;;;;;;; Egg
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
   (t                                     :sync)))



;; (require 'git-emacs)


;;   ;; disable git-emacs's advice
;;   (ad-disable-advice 'vc-next-action 'around 'git--vc-git-next-action)
;;   (ad-activate 'vc-next-action)






(provide 'coldnew-version-control)
;; coldnew-version-control.el ends here.
