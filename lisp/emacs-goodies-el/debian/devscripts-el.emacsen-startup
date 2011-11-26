;; -*-emacs-lisp-*-
;;
;; Emacs startup file for the Debian GNU/Linux devscripts-el package

(cond
 ((not (file-exists-p "/usr/share/emacs/site-lisp/devscripts-el"))
  (message "Package devscripts-el removed but not purged.  Skipping setup."))
 ((not (file-exists-p (concat "/usr/share/"
                              (symbol-name debian-emacs-flavor)
                              "/site-lisp/devscripts-el/pbuilder-mode.elc")))
  (message "Package devscripts-el not fully installed.  Skipping setup."))
 (t

  (debian-pkg-add-load-path-item
   (concat "/usr/share/"
           (symbol-name debian-emacs-flavor)
           "/site-lisp/devscripts-el"))
  
  ;; autoloads for devscripts.el
  (autoload 'debuild "devscripts" "Run debuild in the current directory." t)
  (autoload 'debc "devscripts" "Run debc in the current directory." t)
  (autoload 'debi "devscripts" "Run debi in the current directory." t)
  (autoload 'debit "devscripts" "Run debit in the current directory." t)
  (autoload 'debdiff "devscripts" "Compare contents of CHANGES-FILE-1 and CHANGES-FILE-2." t)
  (autoload 'debdiff-current "devscripts"   "Compare the contents of .changes file of current version with previous version; 
requires access to debian/changelog, and being in debian/ dir." t)
  (autoload 'debclean "devscripts" "Run debclean in the current directory." t)
  (autoload 'pdebuild "pbuilder-mode" "Run pdebuild in the current directory." t)
  (autoload 'pdebuild-user-mode-linux "pbuilder-mode" "Run pdebuild-user-mode-linux in the current directory." t)
  (autoload 'pbuilder-log-view-elserv "pbuilder-log-view-mode" "Run a elserv session with log view.

Running this requires elserv.  Use elserv, and do `elserv-start' before invoking this command." t)
  (autoload 'debuild-pbuilder "pbuilder-mode" "Run debuild-pbuilder in the current directory." t)
  (autoload 'pbuilder-build "pbuilder-mode" "Run pbuilder-build for the given filename." t)
  (autoload 'pbuilder-user-mode-linux-build "pbuilder-mode" "Run pbuilder-user-mode-linux for the given filename." t)))
