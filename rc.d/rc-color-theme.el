;;;; initial color theme
(provide 'rc-color-theme)


(when (require 'color-theme nil 'noerror)

  (color-theme-initialize)
  (setq color-theme-is-global t)
  (setq color-theme-is-cumulative t)
  (setq color-theme-load-all-themes nil))

(require 'color-theme-coldnew-day nil 'noerror)
(require 'color-theme-coldnew-night nil 'noerror)

;; Default theme use night.
(cond
 (mac-p   (progn
	    (color-theme-coldnew-day)
	    (setq *color-mode* 'day)))
 (linux-p (progn
	    (color-theme-coldnew-night)
	    (setq *color-mode* 'night)))
 )


;;;; Specified special color-theme according different modes
;;(add-hook 'after-change-major-mode-hook;;
;;(if (and (not mac-p) linux-p)
(if linux-p
    (add-hook 'post-command-hook
	      '(lambda ()
		 (if (derived-mode-p 'w3m-mode)
		     (color-theme-coldnew-day)
		   (if (not (minibufferp))
		       (case *color-mode*
			 ('night (color-theme-coldnew-night))
			 ('day   (color-theme-coldnew-day))))
		   ))))

;;;;;; Functions

;; Switch between color-theme-day and color-theme-night
(defvar *color-mode* 'night
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
