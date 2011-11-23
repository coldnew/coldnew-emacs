
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
  ;; (add-to-list 'el-get-recipe-path (concat el-get-dir "el-get/recipes"))
  )

;;;;;;;; ELPA
;; Simple package system for Emacs
;;
(when (require* 'package)
  (setq package-user-dir (concat emacs-lisp-dir "elpa"))
  (setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
			   ("gnu" . "http://elpa.gnu.org/packages/")
			   ("marmalade" . "http://marmalade-repo.org/packages/")))
  )

(provide 'coldnew-package)
;; coldnew-package.el ends here.
