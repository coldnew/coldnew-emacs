;;
(eval-when-compile (require 'cl))

(add-to-list 'gnus-secondary-select-methods '(nnml ""))

(setq gnus-permanently-visible-groups "mail")

(setq gnus-posting-styles
      '((".*" (name "Yen-Chin,Lee"))))

(setq mail-sources
      '((file :path "var/mail")
	(pop :server "pop.gmail.com"
	     :port 995
	     :user "coldnew.tw"
	     :connection ssl
	     :leave t)))

;; Configure outbound mail (SMTP)
(setq smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-default-smtp-server "smtp.gmail.com"
      send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      smtpmail-smtp-service 587
      smtpmail-auth-credentials '(("smtp.gmail.com"
				   587
				   "coldnew.tw@gmail.com"
				   nil)))


(provide '040-gnus)
;; 040-gnus.el ends here.
