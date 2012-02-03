;;
(eval-when-compile (require 'cl))

;;;;;;;; Packages Import
(require 'coldnew-macro)
(require 'coldnew-functions)
(require 'coldnew-commands)
(require 'coldnew-variables)
(require 'coldnew-anything)
(require 'coldnew-buffer)


;; (require 'eproject)
;; (require 'eproject-extras)
;; (require 'eproject-anything)
;; (require 'eproject-tags)


;; (define-project-type emacs-config (generic)
;;   (look-for "init.el")
;;   :project-name "emacs-config"
;;   :relevant-files ("\\.el$" "\\.org$" "\\.gpg$"))

;; (add-hook 'emacs-config-project-file-visit-hook '(lambda ()
;;						   (ignore-errors
;;						    (message "Emacs Configuration")
;;						    )))

;; (define-project-type debian-package
;;   (generic)
;;   (look-for "debian")
;;   :common-compiles ("dpkg-buildpackage -rfakeroot"
;;		    "dpkg-buildpackage -rfakeroot -k834BE2B6"))

;; (define-project-type android-package
;;   (generic)
;;   (look-for "AndroidManifest.xml")
;;   :common-compiles ("ant compile"))

;; (define-project-type cbnl-tree
;;   (generic)
;;   (and
;;    (look-for "Makefile.cleanenv")
;;    (look-for "build-system"))
;;   :common-compiles ("make build-nms PLATFORM=Linux_Desktop"
;;		    "make build-nms PLATFORM=Linux_Desktop EMSDEBUG=1"
;;		    "make build-nms PLATFORM=Linux_Desktop EMSDEBUG=1 VNMS=0"
;;		    "make pkg-nms PLATFORM=Linux_Desktop"
;;		    "make release PLATFORM=Linux_OE_RC"
;;		    "make -C packaging PLATFORM=Linux_OE_RC"))


(provide 'coldnew-project)
;; coldnew-project.el ends here.
