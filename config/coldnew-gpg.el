;;; coldnew-gpg.el ---
(eval-when-compile (require 'cl))

;;;; ---------------------------------------------------------------------------
;;;; gpg
;;;; ---------------------------------------------------------------------------
(require 'epa-file)


;; Control whether or not to pop up the key selection dialog.
(setq epa-file-select-keys 0)

;; Cache passphrase for symmetric encryption.
(setq epa-file-cache-passphrase-for-symmetric-encryption t)


(provide 'coldnew-gpg)
;; coldnew-gpg.el ends here.
