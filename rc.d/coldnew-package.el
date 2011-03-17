
(eval-when-compile (require 'cl))

;;;;;;;; Packages Import
(require 'coldnew-macro)
(require 'coldnew-functions)
(require 'coldnew-commands)
(require 'coldnew-variables)

;;;;;;;; el-get
;; Manage the external elisp bits and pieces you depend upon
;;
(setq el-get-dir emacs-lisp-dir)
(when (require* 'el-get)
  (add-to-list 'el-get-recipe-path (concat emacs-etc-dir "recipes"))
  )

;;;;;;;; ELPA
;; Simple package system for Emacs
;;
(when (require* 'package)
  (setq package-user-dir (concat emacs-lisp-dir "elpa"))
  )

(provide 'coldnew-package)
;; coldnew-package.el ends here.
