;;
(eval-when-compile (require 'cl))


;;;;;;;; color-theme
;; Color theme is an Emacs-Lisp package with more than
;; 50 color themes for your use
;;
(when (require* 'color-theme)
  ;; Basic color-theme settings
  (color-theme-initialize)
  (setq color-theme-is-global nil)
  (setq color-theme-is-cumulative t)
  (setq color-theme-load-all-themes nil)
  )


;;;;;; Functions

;; Switch between color-theme-day and color-theme-night
(defvar *color-mode* 'day
  "Color mode for ``switch-day-and-night''.Can be 'day or 'night.")
(defun switch-day-and-night ()
  "Switch between day and nigh color-themes."
  (interactive)
  (case *color-mode*
    ('day (color-theme-coldnew-day)
	  (setq *color-mode* 'night))
    ('night (color-theme-coldnew-night)
	    (setq *color-mode* 'day))
    ))

(provide 'coldnew-theme)
;; coldnew-theme.el ends here.
