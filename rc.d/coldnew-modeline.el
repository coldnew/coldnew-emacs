;;
(eval-when-compile (require 'cl))



;;;;;;;; Settings
;;;; Mode-Line Settings
(setq default-mode-line-format
      '((" "
	 mode-line-mule-info
	 mode-line-modified
	 mode-line-frame-identification
	 mode-line-buffer-identification
	 "   "
	 (when (require* 'vim)
	   vim:mode-string)
	 "   "
	 (when (require* 'pomodoro)
	   pomodoro-display-string)
	 "   "
	 (which-func-mode ("" which-func-format ""))
	 (vc-mode vc-mode)
	 "   "
	 mode-line-position
	 " "
	 ;; "<< " major-mode-string " >>"
	 mode-line-modes
	 ;; " ( " minor-mode-alist " )"
	 "  "
	 display-time-string
	 " "
	 )))

;; Make all mode-line use default-mode-line-format
(setq mode-line-format default-mode-line-format)

;;;; Remove or shrink some mode-line strings
(when (require* 'diminish)
  (when (require* 'yasnippet)
    (diminish 'yas/minor-mode ""))
  (when (require* 'egg)
    (diminish 'egg-minor-mode ""))
  (when (require* 'eldoc)
    (diminish 'eldoc-mode ""))
  (when (require* 'undo-tree)
    (diminish 'undo-tree-mode ""))
  (when (require* 'highlight-parentheses)
    (diminish 'highlight-parentheses-mode ""))
  (when (require* 'auto-complete)
    (diminish 'auto-complete-mode ""))
  ;;(when (require* 'textmate)
  ;;  (diminish 'textmate-mode ""))
  )


(provide 'coldnew-modeline)
;; coldnew-modeline.el ends here.
