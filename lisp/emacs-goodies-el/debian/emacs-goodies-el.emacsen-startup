;; -*-emacs-lisp-*-
;;
;; Emacs startup file for the Debian GNU/Linux emacs-goodies-el package

(cond
 ((not (file-exists-p "/usr/share/emacs/site-lisp/emacs-goodies-el"))
  (message
   "Package emacs-goodies-el removed but not purged.  Skipping setup."))
 ((not (file-exists-p (concat "/usr/share/"
                              (symbol-name debian-emacs-flavor)
                              "/site-lisp/emacs-goodies-el/xrdb-mode.elc")))
  (message "Package emacs-goodies-el not fully installed.  Skipping setup."))
 (t 
  (debian-pkg-add-load-path-item
   (concat "/usr/share/"
           (symbol-name debian-emacs-flavor)
           "/site-lisp/emacs-goodies-el"))
  
  (require 'emacs-goodies-el)))
