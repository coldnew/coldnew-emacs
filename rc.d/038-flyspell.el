;;
(eval-when-compile (require 'cl))

;; use hunspell instead ispell
(setq-default ispell-program-name "hunspell")
;;(setq-default ispell-program-name "ispell")
;;(setq ispell-dictionary "english")
;;(setq ispell-dictionary "default")
;;(setq rw-hunspell-default-dictionary "en_US")
;;(setq ispell-personal-dictionary "~/.ispell-dico-perso")

;; Always use spell check
;;(flyspell-mode t)


(provide '038-flyspell)
;; 038-flyspell.el ends here.
