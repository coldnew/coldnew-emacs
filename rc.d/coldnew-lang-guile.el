;;
(eval-when-compile (require 'cl))

;;;;;;;; Packages Import
(require 'coldnew-editor)
(require 'geiser-install)
(require 'quack)



;;;;;;;; Emacs-lisp-mode extensions
(add-to-list 'auto-mode-alist '("\\.scm$" . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.gl$"  . scheme-mode))


;;;;;;;; Auto Complete Settings
(when (require* 'auto-complete)
  (defun ac-guile-mode-setup ()
    "auto-complete settings for emacs-lisp-mode"
    (setq ac-sources '(ac-source-dictionary
		       ac-source-symbols
		       ac-source-variables
		       ac-source-functions
		       ac-source-features
		       ac-source-filename
		       ac-source-words-in-buffer
		       ac-source-words-in-same-mode-buffers
		       ))))


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

;;;;;;;; Geiser
;;
(when (require* 'geiser-install)

  ;; Don't record duplicates history
  (setq geiser-repl-history-no-dups-p nil)

  ;; Setting history cache file
  (setq geiser-repl-history-filename (concat emacs-cache-dir "geiser-history"))

  ;;

  )


(provide 'coldnew-lang-guile)
;; coldnew-lang-guile.el ends here.
