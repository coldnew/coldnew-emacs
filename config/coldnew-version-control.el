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
;; (require 'egg)
;; ;; do not auto-update egg-status on file save
;; (setq egg-auto-update nil)
;; ;; do not switch to the status buffer in the same window
;; (setq egg-switch-to-buffer t)

(require 'git-emacs)

(provide 'coldnew-version-control)
;; coldnew-version-control.el ends here.
