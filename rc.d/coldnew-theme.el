;;
(eval-when-compile (require 'cl))

;;;;;;;; Packages Import
(require 'coldnew-macro)
(require 'coldnew-functions)
(require 'coldnew-commands)
(require 'coldnew-variables)

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

  ;; TODO: I must rewrite coldnew's color-theme
  (require* 'color-theme-coldnew-day-old)
  (require* 'color-theme-coldnew-night)

  ;; Default theme use night.
  (cond
   (mac?   (progn
	    (setq *color-mode* 'day)
	    (color-theme-coldnew-day)))
   (linux? (progn
	    (setq *color-mode* 'night)
	    (color-theme-coldnew-night))))


  ;; ;; Specified special color-theme according different modes
  (if linux?
      (add-hook 'post-command-hook
		'(lambda ()
		   (cond
		    ;; When enter w3m, change color to color-thme-day.
		    ((derived-mode-p 'w3m-mode) (color-theme-coldnew-day))
		    ;; We don't want to change color when enter minibuffer.
		    ;; This is the default option and might set in the end.
		    ((not (minibufferp))
		     (case *color-mode*
		       ('night (color-theme-coldnew-night))
		       ('day   (color-theme-coldnew-day)))))
		   ;; FIXME: I wish to change modeline if current buffer is read-only
		   ;; derived-mode-p can't use here, why?
		   (if (derived-mode-p 'view-mode)
		       (set-face-attribute 'modeline nil :box '(:color "red")))
		   )))

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
