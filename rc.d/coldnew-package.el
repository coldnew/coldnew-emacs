
(eval-when-compile (require 'cl))

;;;;;;;; el-get
;;
(setq el-get-dir "~/.emacs.d/lisp/")
(when (require* 'el-get)
  (add-to-list 'el-get-recipe-path "~/.emacs.d/etc/recipes")
  )

;;;;;;;; ELPA
;; Simple package system for Emacs
;;
(when (require* 'package)
  (setq package-user-dir "~/.emacs.d/lisp/elpa")
  )

(provide 'coldnew-package)
;; coldnew-package.el ends here.
