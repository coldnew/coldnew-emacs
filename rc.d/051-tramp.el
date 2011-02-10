;;
(eval-when-compile (require 'cl))

;; Deafult use ssh
(setq tramp-default-method "ssh")

;;
(setq tramp-persistency-file-name "~/.emacs.d/var/cache/tramp.cache")

;; Make password-cache do not expire, that I only need type password once
(setq password-cache-expiry nil)




(provide '051-tramp)
;; 051-tramp.el ends here.
