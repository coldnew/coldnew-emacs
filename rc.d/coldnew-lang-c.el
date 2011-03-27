;;
(eval-when-compile (require 'cl))


;;;;;;;; Packages Import
(require 'coldnew-editor)


;;;;;;;; c-mode extensions
(add-to-list 'auto-mode-alist '("\\.h$" . c-mode))
(add-to-list 'auto-mode-alist '("\\.c$" . c-mode))


;;;;;;;; Auto Complete Settings
(when (require* 'auto-complete)
  (defun ac-c-mode-setup ()
    "auto-complete settings for c-mode."
    (setq ac-sources '(ac-source-dictionary
		       ac-source-symbols
		       ac-source-variables
		       ac-source-functions
		       ac-source-features
		       ac-source-filename
		       ac-source-words-in-buffer
		       ac-source-company-clang
		       ac-source-words-in-same-mode-buffers
		       ))))


;;;;;;;; Hooks
(add-hook 'emacs-lisp-mode-hook
	  '(lambda ()

	     ;; Enable Auto Complete
	     (when (require* 'auto-complete)
	       (ac-c-mode-setup))

	     ;; Enable c-eldoc
	     (when (require* 'c-eldoc)
	       (c-turn-on-eldoc-mode))

	     ;; Use global programming mode
	     (programming-mode)


	     ))




(provide 'coldnew-lang-c)
;; coldnew-lang-c.el ends here.
