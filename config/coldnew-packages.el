;;; coldnew-packages.el --- default package system setting

(eval-when-compile (require 'cl))

;;;;;;;; Packages Import
(require 'coldnew-core)


;;;; ---------------------------------------------------------------------------
;;;; el-get
;;;; ---------------------------------------------------------------------------
(setq-default el-get-dir emacs-lisp-dir)
(require 'el-get)
(add-to-list 'el-get-recipe-path emacs-recipes-dir)

;; Make el-get recipes in emacs-lisp-mode
(add-to-list 'auto-mode-alist '("\\.rcp$" . emacs-lisp-mode))

;;;; ---------------------------------------------------------------------------
;;;; elpa
;;;; ---------------------------------------------------------------------------
(setq-default package-user-dir emacs-elpa-dir)
(require 'package)
(setq package-archives
      '(("ELPA" . "http://tromey.com/elpa/")
	("gnu" . "http://elpa.gnu.org/packages/")
	("marmalade" . "http://marmalade-repo.org/packages/")
	("melpa" . "http://melpa.milkbox.net/packages/")))



(provide 'coldnew-packages)
;; coldnew-packages.el ends here.
