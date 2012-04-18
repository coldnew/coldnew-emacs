;;; coldnew-buffer.el ---                                                     
(eval-when-compile (require 'cl))                                        



;;;; ---------------------------------------------------------------------------
;;;; *scratch*
;;;; ---------------------------------------------------------------------------

;; if *scratch* buffer does not exist, creat it automatically
(run-with-idle-timer 1 t
		     '(lambda () (get-buffer-create "*scratch*")))

;;;; ---------------------------------------------------------------------------
;;;; Uniquify
;;;; ---------------------------------------------------------------------------
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "/")
;; rename after killing uniquified
(setq uniquify-after-kill-buffer-p t)
;; don't muck with special buffers
(setq uniquify-ignore-buffers-re "^\\*")



(provide 'coldnew-buffer)                                             
;; coldnew-buffer.el ends here.                                               
