;;
(eval-when-compile (require 'cl))



(global-ede-mode t)


;; Database
(global-semanticdb-minor-mode 1)
(setq semanticdb-default-save-directory
      (expand-file-name "~/.emacs.d/var/semanticdb"))

;; Support for GNU Global
(semanticdb-enable-gnu-global-databases 'c-mode)
(semanticdb-enable-gnu-global-databases 'c++-mode)

;; BUG:?
;; enable ctags for some languages:
;;  Unix Shell, Perl, Pascal, Tcl, Fortran, Asm
					;(semantic-load-enable-primary-exuberent-ctags-support)


(semantic-add-system-include "/usr/include" 'c-mode)
(semantic-add-system-include "/usr/include" 'cpp-mode)
(setq semanticdb-search-system-databases t)

;;
(defconst cedet-user-include-dirs
  (list ".." "../include" "../inc" "../common" "../public" "."
	"../.." "../../include" "../../inc" "../../common" "../../public"))

(setq cedet-sys-include-dirs (list
			      "/usr/include"
			      "/usr/include/bits"
			      "/usr/include/glib-2.0"
			      "/usr/include/gnu"
			      "/usr/include/gtk-2.0"
			      "/usr/include/gtk-2.0/gdk-pixbuf"
			      "/usr/include/gtk-2.0/gtk"
			      "/usr/local/include"
			      "/usr/local/include"))

(let ((include-dirs cedet-user-include-dirs))
  (setq include-dirs (append include-dirs cedet-sys-include-dirs))
  (mapc (lambda (dir)
	  (semantic-add-system-include dir 'c++-mode)
	  (semantic-add-system-include dir 'c-mode))
	include-dirs))

(setq semantic-c-dependency-system-include-path "/usr/include/")




;;



;; Add Support for Qt4
(setq qt4-base-dir "/usr/include/qt4")
(setq qt4-gui-dir (concat qt4-base-dir "/QtGui"))
(semantic-add-system-include qt4-base-dir 'c++-mode)
(semantic-add-system-include qt4-gui-dir 'c++-mode)
(add-to-list 'auto-mode-alist (cons qt4-base-dir 'c++-mode))
(add-to-list 'semantic-lex-c-preprocessor-symbol-map '("Q_GUI_EXPORT" . ""))
(add-to-list 'semantic-lex-c-preprocessor-symbol-map '("Q_CORE_EXPORT" . ""))
(add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "/Qt/qconfig.h"))
(add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "/Qt/qconfig-dist.h"))
(add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "/Qt/qglobal.h"))

(provide '043-semactic)
;; 043-semactic.el ends here.
