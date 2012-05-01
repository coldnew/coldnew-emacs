;;; coldnew-daemon.el ---
(eval-when-compile (require 'cl))

;; Only start server mode if I'm not root
(unless (string-equal "root" (getenv "USER"))
  (require 'server)
  (unless (server-running-p) (server-start)))


(provide 'coldnew-daemon)
;; coldnew-daemon.el ends here.
