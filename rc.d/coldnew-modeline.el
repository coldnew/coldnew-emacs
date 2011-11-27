;;
(eval-when-compile (require 'cl))

;;;;;;;; Packages Import
(require 'coldnew-macro)
(require 'coldnew-functions)
(require 'coldnew-commands)
(require 'coldnew-variables)

;;;;see
;; https://github.com/ZaneA/Dotfiles/blob/master/.emacs#L206


(defun vim-mode-string ()
  (let ((vim-string (substring vim:mode-string 1 2)))
    (setq vim-string-face
	  (cond
	   ((string= "N" vim-string) 'mode-line-vim-string-N)
	   ((string= "I" vim-string) 'mode-line-vim-string-I)
	   ((string= "V" vim-string) 'mode-line-vim-string-V)
	   ((string= "E" vim-string) 'mode-line-vim-string-E)
	   ))
    (concat "<" (propertize vim-string 'face vim-string-face) ">")
    ))


(defun mode-line-major-mode ()
  "Get major-mode name with << >>."
  (concat "<< " (propertize mode-name 'face 'mode-line-mode-name-face) " >>")
  )

;;TODO: add mode line face
;; (setq mode-line-in-non-selected-windows nil)
;;;;;;;; Settings
;; Make all mode-line use mode-line-format as default
(setq-default mode-line-format
	      '((" "
		 mode-line-mule-info
		 mode-line-modified
		 mode-line-frame-identification
		 mode-line-buffer-identification
		 "   "
		 (when (featurep 'vim)
		   (:eval (vim-mode-string)))

		 ;; "   "
		 ;; (when (require* 'pomodoro)
		 ;;   pomodoro-display-string)
		 "   "
		 (which-func-mode ("" which-func-format ""))
		 (vc-mode vc-mode)
		 "   "
		 ;;; major-mode name
		 (:eval (mode-line-major-mode))

		 ;; (:propertize mode-name
		 ;;           help-echo (format-mode-line minor-mode-alist)
		 ;;           face 'mode-line-mode-name-face)
		 " [" minor-mode-alist " ]"
		 "   "
		 mode-line-position
		 "  "
		 display-time-string
		 " "
		 )))

;;;; Remove or shrink some mode-line strings
(when (require* 'diminish)
  (when (featurep 'yasnippet)
    (diminish 'yas/minor-mode ""))
  (when (featurep 'egg)
    (diminish 'egg-minor-mode ""))
  (when (featurep 'eldoc)
    (diminish 'eldoc-mode ""))
  (when (featurep 'undo-tree)
    (diminish 'undo-tree-mode ""))
  (when (featurep 'highlight-parentheses)
    (diminish 'highlight-parentheses-mode ""))
  (when (featurep 'auto-complete)
    (diminish 'auto-complete-mode ""))

  )


(provide 'coldnew-modeline)
;; coldnew-modeline.el ends here.
