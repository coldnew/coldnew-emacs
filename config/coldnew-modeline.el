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

;; (defun mode-line-state ()
;;   (let ((mode-line-state-string "Emacs"))
;;     (cond
;;      (view-mode (setq mode-line-state-string "View"))
;;      (overwrite-mode (setq mode-line-state-string "Overwrite"))
;;      (t 'mode-line-state-string-E))
;;     (concat "< " mode-line-state-string " >")))

(defun mode-line-state ()
  (let ((mode-line-state-string)
	(propertize-string))
    (setq mode-line-state-string
	  (cond
	   (view-mode "View")
	   (overwrite-mode "Overwrite")
	   (t "Emacs")))
    (setq propertize-string
	  (cond
	   (view-mode 'mode-line-read-only-face)
	   (t 'mode-line-normal-state-face)
	   ))
    (concat "< " (propertize mode-line-state-string 'face propertize-string) " >")))


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
		 (:eval (mode-line-state))
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
