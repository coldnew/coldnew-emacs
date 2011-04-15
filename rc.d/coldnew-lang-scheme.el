;;
(eval-when-compile (require 'cl))

;;;;;;;; Packages Import
(require 'coldnew-editor)
(require 'geiser-install)
(require 'quack)



;;;;;;;; Scheme-mode extensions
(add-to-list 'auto-mode-alist '("\\.scm$" . scheme-mode))

;;;;;;;; Auto Complete Settings
(when (require* 'auto-complete)
  (defun ac-scheme-mode-setup ()
    "auto-complete settings for scheme-mode"
    (setq ac-sources '(ac-source-dictionary
		       ac-source-symbols
		       ac-source-variables
		       ac-source-functions
		       ac-source-features
		       ac-source-filename
		       ac-source-words-in-buffer
		       ac-source-words-in-same-mode-buffers
		       ))))
;;;;;;;; Settings
(add-hook 'scheme-mode-hook
	  '(lambda ()

	     ;; Use Gambit-C as my Scheme implementation
	     (setq scheme-program-name "gsi")

	     ))

;;;;;;;; Hooks
(add-hook 'scheme-mode-hook
	  '(lambda ()

	     ;; Enable Auto Complete
	     (when (require* 'auto-complete)
	       (ac-guile-mode-setup))

	     ;; Highlight differnet parentheses
	     (when (require* 'highlight-parentheses)
	       (highlight-parentheses-mode))

	     ;; Use Greek character lambda instead of string
	     (when (require* 'pretty-lambdada)
	       (turn-on-pretty-lambda-mode))

	     ;; Use global programming mode
	     (programming-mode)

	     ))



(provide 'coldnew-lang-scheme)
;; coldnew-lang-scheme.el ends here.
