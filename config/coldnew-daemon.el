;;; coldnew-daemon.el ---
(eval-when-compile (require 'cl))


;; start server for emacsclient
(when (require* 'server)
  (unless (server-running-p) (server-start)))


(provide 'coldnew-daemon)
;; coldnew-daemon.el ends here.
