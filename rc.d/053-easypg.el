;;
(eval-when-compile (require 'cl))

;; enable use EasyPG when attatch file which extension is .gpg
(epa-file-enable)

;; Select GnuPG Program
(setq epg-gpg-program "/usr/bin/gpg")

;; Always use symmetric encryption
(setq epa-file-encrypt-to 'silent)

;; save the password that only need type password once
(setq epa-file-cache-passphrase-for-symmetric-encryption t)

;; Enable auto-save in EasyPG
(setq epa-file-inhibit-auto-save nil)


(provide '053-easypg)
;; 053-easypg.el ends here.
