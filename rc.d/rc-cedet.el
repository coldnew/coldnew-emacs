;;
(provide 'rc-cedet)


;;;; Shell Popup
(when (require 'shell-pop nil 'noerror)
  (shell-pop-set-internal-mode "ansi-term")
  (shell-pop-set-internal-mode-shell "/bin/bash")
  (shell-pop-set-window-height 20)
  (shell-pop-set-window-position "bottom")
  (when (featurep 'vim)
    (vim:nmap (kbd "<f3>") 'shell-pop)))

(require 'cedet)
(global-ede-mode t)


(when (require 'semantic nil 'noerror)
  (require 'semantic/ia nil 'noerror)
  (require 'semactic/lex nil 'noerror)


  ;; Database
  (require 'semantic/db nil 'noerror)
  (require 'semantic/db-el nil 'noerror)
  (require 'semantic/db-find nil 'noerror)
  (require 'semantic/db-file nil 'noerror)
  (require 'semantic/db-ref nil 'noerror)
  (require 'semantic/db-global nil 'noerror)
  (require 'semantic/db-typecache nil 'noerror)
  (global-semanticdb-minor-mode 1)
  (setq semanticdb-default-save-directory
    (expand-file-name "~/.emacs.d/var/semanticdb"))

  ;; Support for GNU Global
  (cond (emacs24-p
	(semanticdb-enable-gnu-global-databases 'c-mode)
	(semanticdb-enable-gnu-global-databases 'c++-mode)))

  ;; BUG:?
  ;; enable ctags for some languages:
  ;;  Unix Shell, Perl, Pascal, Tcl, Fortran, Asm
                    ;(semantic-load-enable-primary-exuberent-ctags-support)

  (require 'semantic/bovine nil 'noerror)
  (require 'semantic/bovine/c nil 'noerror)
  (require 'semantic/bovine/gcc nil 'noerror)
  ;; Add Support for Qt4
  (setq qt4-base-dir "/usr/include/qt4")
  (semantic-add-system-include qt4-base-dir 'c++-mode)
  (add-to-list 'auto-mode-alist (cons qt4-base-dir 'c++-mode))
  (cond (emacs24-p
  (add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "/Qt/qconfig.h"))
  (add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "/Qt/qconfig-dist.h"))
  (add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "/Qt/qglobal.h"))
  )))
