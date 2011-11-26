;; -*-emacs-lisp-*-
;;
;; Emacs startup file for the Debian GNU/Linux dpkg-dev-el package

(cond
 ((not (file-exists-p "/usr/share/emacs/site-lisp/dpkg-dev-el"))
  (message "Package dpkg-dev-el removed but not purged.  Skipping setup."))
 ((not (file-exists-p (concat "/usr/share/"
                              (symbol-name debian-emacs-flavor)
                              "/site-lisp/dpkg-dev-el/readme-debian.elc")))
  (message "Package dpkg-dev-el not fully installed.  Skipping setup."))
 (t
  (debian-pkg-add-load-path-item
   (concat "/usr/share/"
           (symbol-name debian-emacs-flavor)
           "/site-lisp/dpkg-dev-el"))

  (require 'dpkg-dev-el)))
