;;; coldnew-elscreen.el ---
(eval-when-compile (require 'cl))

(require* 'elscreen)

;;; start elscreen
(elscreen-start)

;;; Make TAB show control ico
(setq elscreen-tab-display-control t)

;;; Make TAB do not display kill-screen icon
(setq elscreen-tab-display-kill-screen nil)

;;; Change elscreen prefix to
;;(elscreen-set-prefix-key nil)

;;; Make elscreen create automatically
(defmacro elscreen-create-automatically (ad-do-it)
  (` (if (not (elscreen-one-screen-p))
	 (, ad-do-it)
       (elscreen-create)
       (elscreen-notify-screen-modification 'force-immediately)
       (elscreen-message "New screen is automatically created"))))

(defadvice elscreen-next (around elscreen-create-automatically activate)
  (elscreen-create-automatically ad-do-it))

(defadvice elscreen-previous (around elscreen-create-automatically activate)
  (elscreen-create-automatically ad-do-it))

(defadvice elscreen-toggle (around elscreen-create-automatically activate)
  (elscreen-create-automatically ad-do-it))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elscreen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(evil:nmap (kbd "gt") 'elscreen-next)
(evil:nmap (kbd "gT") 'elscreen-next)


(provide 'coldnew-elscreen)
;; coldnew-elscreen.el ends here.
