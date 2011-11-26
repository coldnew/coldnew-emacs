;; -*-emacs-lisp-*-
;;
;; Emacs startup file for the Debian GNU/Linux gnus-bonus-el package

;; The gnus-bonus-el package follows the Debian/GNU Linux 'emacsen' policy and
;; byte-compiles its elisp files for each 'emacs flavor' (emacs19,
;; xemacs19, emacs20, xemacs20...).  The compiled code is then
;; installed in a subdirectory of the respective site-lisp directory.

(cond
 ((not (file-exists-p "/usr/share/emacs/site-lisp/gnus-bonus-el"))
  (message "Package gnus-bonus-el removed but not purged.  Skipping setup."))
 ((not (file-exists-p (concat "/usr/share/"
                              (symbol-name debian-emacs-flavor)
                              "/site-lisp/gnus-bonus-el/nntodo.elc")))
  (message "Package gnus-bonus-el not fully installed.  Skipping setup."))
 (t
  (debian-pkg-add-load-path-item
   (concat "/usr/share/"
           (symbol-name debian-emacs-flavor)
           "/site-lisp/gnus-bonus-el"))

  ;; autoloads for gnus-junk.el
  (autoload 'gnus-junk-complain "gnus-junk"
    "Mail a complaint about next messages to (hopefully) relevant people."
    t)

  ;; autoloads for gnus-pers.el
  (autoload 'gnus-personality-init "gnus-pers"
    "Install Personality functionality into message mode."
    t)

  ;; autoloads for gnus-eyecandy.el
  (autoload 'gnus-group-line-add-icon "gnus-eyecandy"
    "Highlight the current line according to `gnus-group-icon-list'."
    nil)

  ;; autoloads for gnus-filterhist.el
  (autoload 'gnus-filter-history "gnus-filterhist"
    "Create a buffer *Filter History* with the results of the latest nnmail split."
    t)
  ))
