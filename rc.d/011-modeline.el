;;
(eval-when-compile (require 'cl))

(setq default-mode-line-format
      '((" "
	 mode-line-mule-info
	 mode-line-modified
	 mode-line-frame-identification
	 mode-line-buffer-identification
	 "  "
	 vim:mode-string
	 "   "
	 mode-line-position
	 (which-func-mode ("" which-func-format ""))
	 (vc-mode vc-mode)
	 " "
	 mode-line-modes
	 "  "
	 display-time-string
	 )))

;; Make all mode-line use default-mode-line-format
(setq mode-line-format default-mode-line-format)



(provide '011-modeline)
;; 011-modeline.el ends here.
