;;; coldnew-project.el --- project-management
(eval-when-compile (require 'cl))

;;;; ---------------------------------------------------------------------------
;;;; jrockway/eproject
;;;; ---------------------------------------------------------------------------
;;(require 'eproject)
;;(require 'eproject-extras)

;;;; ---------------------------------------------------------------------------
;;;; projectile
;;;; ---------------------------------------------------------------------------
(require* 'projectile)
(require 'helm-projectile)

;;;; ---------------------------------------------------------------------------
;;;; ede
;;;; ---------------------------------------------------------------------------
(global-ede-mode t)
(setq ede-project-placeholder-cache-file (concat emacs-cache-dir "ede-project.el"))

;;; FIXME: why follwoing unuse ?
(ede-emacs-project "coldnew-emacs"
		   :file "/home/coldnew/.emacs.d/README"
		   )



(provide 'coldnew-project)
;; coldnew-project.el ends here.
