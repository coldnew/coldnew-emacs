;;;;;;;; cedet
;; CEDET is a Collection of Emacs Development Environment Tools written with the
;; end goal of creating an advanced development environment in Emacs.
(eval-when-compile (require 'cl))

;;;;;;;; Packages Import
(require 'coldnew-macro)
(require 'coldnew-functions)
(require 'coldnew-commands)
(require 'coldnew-variables)
(require 'cedet)

;;
;; loding libraries
(require 'semantic)
(require 'semantic/sb)
(require 'srecode)

;; Enable EDE (Project Management) features
(global-ede-mode 1)

(setq ede-project-placeholder-cache-file (concat emacs-cache-dir "ede-project.cache"))

(ede-enable-generic-projects)

;; Enable Semantic feactures
(semantic-mode 1)

;;;;;;;; Setting up Semantic-mode
;;;; Enable
;; Maintain tag database
(global-semanticdb-minor-mode 1)
;; Reparse buffer when idle
(global-semantic-idle-scheduler-mode 1)
;; Show completions when idle
(global-semantic-idle-completions-mode 1)
;; Provide `switch-to-buffer'-like keybinding for tag names.
(global-semantic-mru-bookmark-mode 1)
;; Show summary of tag at point
(global-semantic-idle-summary-mode 1)
;;;; Disable
;; Highlight the current tag.
(global-semantic-highlight-func-mode -1)
;; Show current fun in header line
(global-semantic-stickyfunc-mode -1)
;; Additional tag decorations
(global-semantic-decoration-mode -1)


;;;; Enable support for GNU Global
(semanticdb-enable-gnu-global-databases 'c-mode)
(semanticdb-enable-gnu-global-databases 'c++-mode)



(setq semanticdb-default-save-directory (concat emacs-cache-dir "semanticdb"))
(set-default 'semantic-case-fold t)

(provide 'coldnew-cedet)
;; coldnew-cedet.el ends here.
