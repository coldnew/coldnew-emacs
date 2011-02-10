;;
(eval-when-compile (require 'cl))

;; Setting for gmail
(setq ssl-certificate-verification-policy 1)

(setq elmo-imap4-default-server "imap.gmail.com")
(setq elmo-imap4-default-user "email@gmail.com")
(setq elmo-imap4-default-authenticate-type 'clear)
(setq elmo-imap4-default-port 993)
(setq elmo-imap4-default-stream-type 'ssl)
(setq elmo-imap4-use-modified-utf7 t)

(setq wl-smtp-connection-type 'ssl)
(setq wl-smtp-posting-port 587)
(setq wl-smtp-authenticate-type "plain")

(setq wl-smtp-posting-user "email@gmail.com")
(setq wl-smtp-posting-server "smtp.gmail.com")

(setq wl-from "Me <mail@gmail.com>")



(provide '052-wanderlust)
;; 052-wanderlust.el ends here.
