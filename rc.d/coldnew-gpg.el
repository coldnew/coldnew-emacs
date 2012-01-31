;;
(eval-when-compile (require 'cl))

;;;;;;;; Packages Import
(require 'coldnew-macro)
(require 'coldnew-functions)
(require 'coldnew-commands)
(require 'coldnew-variables)
(require 'coldnew-evil)
(require 'epa-file)

;; use local gpg program instaed of system one
;; only work under linux
(if linux?
    (setq epg-gpg-program (concat emacs-bin-dir "gpg")))

;; (setenv "GPG_AGENT_INFO" nil)

;; Control whether or not to pop up the key selection dialog.
(setq epa-file-select-keys 0)

;; Cache passphrase for symmetric encryption.
(setq epa-file-cache-passphrase-for-symmetric-encryption t)



(provide 'coldnew-gpg)
;; coldnew-gpg.el ends here.
