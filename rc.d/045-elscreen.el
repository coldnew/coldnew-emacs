;;
(eval-when-compile (require 'cl))

;; Display elscreen tabar
(setq elscreen-display-tab t)

;; I use vim-elscreen to combine elscreen with vimlike commands
;; so I don't need this prefix
(elscreen-set-prefix-key "")

;; Make elscreen automatically create new screen when only one tab there
(defmacro elscreen-create-automatically (ad-do-it)
  `(if (not (elscreen-one-screen-p))
       ,ad-do-it
     (elscreen-create)
     (elscreen-notify-screen-modification 'force-immediately)
     (elscreen-message "New screen is automatically created")))

(defadvice elscreen-next (around elscreen-create-automatically activate)
  (elscreen-create-automatically ad-do-it))

(defadvice elscreen-previous (around elscreen-create-automatically activate)
  (elscreen-create-automatically ad-do-it))

(defadvice elscreen-toggle (around elscreen-create-automatically activate)
  (elscreen-create-automatically ad-do-it))




(provide '045-elscreen)
;; 045-elscreen.el ends here.
