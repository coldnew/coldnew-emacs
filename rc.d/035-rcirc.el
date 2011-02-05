;;
(eval-when-compile (require 'cl))

;; Persional Info
(setq rcirc-default-nick "coldnew")
(setq rcirc-default-user-name "coldnew")
(setq rcirc-default-full-name "coldnew")

;; Hooks
(add-hook 'rcirc-mode-hook
	  '(lambda ()
	     ;; Keep input line at bottom.
	     (set (make-local-variable 'scroll-conservatively) 8192)
	     ;; for debugging.
	     (setq rcirc-debug-flag t)
	     ;; Include date in time stamp.
	     (setq rcirc-time-format "%Y-%m-%d %H:%M ")
	     ;; Join these channels at startup.
	     (setq rcirc-server-alist
		   '(("irc.freenode.net" :channels ("#emacs" "##linux"))))

	     ))


;; Don't print /away messages.
(defun rcirc-handler-301 (process cmd sender args)
  "/away message handler.")


(provide '035-rcirc)
;; 035-rcirc.el ends here.
