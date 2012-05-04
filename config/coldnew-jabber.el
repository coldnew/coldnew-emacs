;;; coldnew-jabber.el ---
(eval-when-compile (require 'cl))


(require* 'jabber)

(setq jabber-account-list
      '(("coldnew.tw@gmail.com"
	 (:network-server . "talk.google.com")
	 (:connection-type . ssl))))



(provide 'coldnew-jabber)
;; coldnew-jabber.el ends here.
