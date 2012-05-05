;;; coldnew-modeline.el --- modeline default setting

(eval-when-compile (require 'cl))

;;;; ---------------------------------------------------------------------------
;;;; Initial modeline setting
;;;; ---------------------------------------------------------------------------
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;;;; ---------------------------------------------------------------------------
;;;; Functions
;;;; ---------------------------------------------------------------------------
(defun mode-line-major-mode ()
  "Get major-mode name with << >>."
  (concat "<< " (propertize mode-name 'face 'mode-line-mode-name-face) " >>"))

;;;; ---------------------------------------------------------------------------
;;;; modeline User-Interfaced setting
;;;; ---------------------------------------------------------------------------
(setq-default mode-line-format
	      '((" "
		 mode-line-mule-info
		 ;; read-only or modified status
		 (:eval
		  (cond (buffer-read-only
			 (propertize "RO" 'face 'mode-line-read-only-face))
			((buffer-modified-p)
			 (propertize "**" 'face 'mode-line-modified-face))
			(t "--")))
		 "   "
		 mode-line-buffer-identification
		 "   "
		 ;; major-mode name
		 (:eval (mode-line-major-mode))
		 "   "
		 ;; line and column
		 "("
		 (:eval (propertize "%02l" 'face 'font-lock-type-face))
		 ","
		 (:eval (propertize "%02c" 'face 'font-lock-type-face))
		 ")"

		 "   "
		 (vc-mode vc-mode)
		 "   "
		 ;; relative position, size of file
		 "["
		 (:eval (propertize "%p" 'face 'font-lock-constant-face)) ;; % above top
		 "/"
		 (:eval (propertize "%I" 'face 'font-lock-constant-face)) ;; size
		 "] "
		 )))



(provide 'coldnew-modeline)
;; coldnew-modeline.el ends here.
