;;; coldnew-eshell.el ---
(eval-when-compile (require 'cl))

(require 'eshell)

;; ;;; Use ANSI color in eshell
;; (add-to-list 'eshell-output-filter-functions 'eshell-handle-ansi-color)


;;; Setup prompt function
(setq eshell-prompt-function
      '(lambda ()
	 (concat
	  user-login-name "@" system-name " "
	  (if (string= (eshell/pwd) (directory-file-name (expand-file-name (getenv "HOME"))))
	      "~"
	    (eshell/pwd))
	  (if (= (user-uid) 0) " # " " $ ")
	  )))



(provide 'coldnew-eshell)
;; coldnew-eshell.el ends here.
