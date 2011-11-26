;; -*-emacs-lisp-*-
;;
;; Emacs startup file for the Debian GNU/Linux devscripts-el package

(cond
 ((not (file-exists-p "/usr/share/emacs/site-lisp/debian-el"))
  (message "Package debian-el removed but not purged.  Skipping setup."))
 ((not (file-exists-p (concat "/usr/share/"
                              (symbol-name debian-emacs-flavor)
                              "/site-lisp/debian-el/preseed.elc")))
  (message "Package debian-el not fully installed.  Skipping setup."))
 (t
  (debian-pkg-add-load-path-item
   (concat "/usr/share/"
           (symbol-name debian-emacs-flavor)
           "/site-lisp/debian-el"))
  
  (require 'debian-el)))
