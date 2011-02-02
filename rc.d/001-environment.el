;;

(cond
 (mac-p		;; If running on Mac OSX
  ;; Add binary PATH for Mac OSX
  (add-to-list 'exec-path "~/Gentoo/bin") ; Gentoo prefix
  (add-to-list 'exec-path "/usr/local/bin/")
  (add-to-list 'exec-path "/opt/local/bin/")
  (add-to-list 'exec-path "/usr/bin/")
  (add-to-list 'exec-path "/usr/X11/bin/"))
 (windows-p	;; If running in Windows
  )
 (t		;; Default is Linux
  (add-to-list 'exec-path "~/.emacs.d/lisp/gccsense/bin/")

  )
 )

;;;; Following setting must run before load the libraries.

;; el-get.el
(setq el-get-dir "~/.emacs.d/lisp/")
(setq el-get-recipe-path "~/.emacs.d/etc/recipes")

;; auto-complete.el
(setq ac-comphist-file "~/.emacs.d/var/cache/auto-complete.cache")


(provide '001-environment)
;; 001-environment.el ends here.
