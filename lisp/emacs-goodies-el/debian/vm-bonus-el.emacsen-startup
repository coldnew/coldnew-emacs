;; -*-emacs-lisp-*-
;;
;; Emacs startup file for the Debian GNU/Linux vm-bonus-el package

(cond
 ((not (file-exists-p "/usr/share/emacs/site-lisp/vm-bonus-el"))
  (message "Package vm-bonus-el removed but not purged.  Skipping setup."))
 ((not (file-exists-p (concat "/usr/share/"
                              (symbol-name debian-emacs-flavor)
                              "/site-lisp/vm-bonus-el/vm-bogofilter.elc")))
  (message "Package vm-bonus-el not fully installed.  Skipping setup."))
 (t
  (debian-pkg-add-load-path-item
   (concat "/usr/share/"
           (symbol-name debian-emacs-flavor)
           "/site-lisp/vm-bonus-el"))

  (defgroup vm-bonus-el nil
    "Customize vm-bonus-el Debian packages."
    :group 'vm)

  ;; vm-bogofilter.el
  (defgroup vm-bogofilter nil
    "VM Spam Filter Options"
    :group 'vm
    :group 'vm-bonus-el
    :load 'vm-bogofilter)

  (autoload 'vm-bogofilter-setup "vm-bogofilter"
    "Initialize vm-bogofilter."
    t)

  (defcustom vm-bogofilter-setup nil
    "Whether to initialize vm-bogofilter on startup.
vm-bogofilter interfaces VM with the bogofilter spam filter."
    :type 'boolean
    :set (lambda (symbol value)
           (set-default symbol value)
           (when value
             (vm-bogofilter-setup)))
    :load 'vm-bogofilter
    :group 'vm
    :group 'vm-bogofilter
    :group 'vm-bonus-el)
  ))
