;;;; initial color theme
(provide 'rc-color-theme)


(when (require 'color-theme nil 'noerror)

  (color-theme-initialize)
  (setq color-theme-is-global nil)
  (setq color-theme-is-cumulative t)
  (setq color-theme-load-all-themes nil))

(require 'color-theme-coldnew-night nil 'noerror)
(require 'color-theme-coldnew-day nil 'noerror)

;; Default theme use night.
(cond
 (mac-p   (progn
	    (setq *color-mode* 'day)
	    (color-theme-coldnew-day)))
 (linux-p (progn
	    (setq *color-mode* 'night)
	    (color-theme-coldnew-night)))
 )


;;;; Specified special color-theme according different modes
(if emacs24-p
    (add-hook 'post-command-hook
	      '(lambda ()
		 (cond
		  ;; When enter w3m, chang color to color-thme-day.
		  ((derived-mode-p 'w3m-mode) (color-theme-coldnew-day))
		  ;; We don't want to change color iwhen enter minibuffer.
		  ;; This is the default option and might set in the end.
		  ((not (minibufferp))
		   (case *color-mode*
		     ('night (color-theme-coldnew-night))
		     ('day   (color-theme-coldnew-day))))))))


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
