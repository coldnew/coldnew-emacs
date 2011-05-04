;;
(eval-when-compile (require 'cl))

;;;;;;;; Packages Import
(require 'coldnew-editor)


;;;;;;;; Emacs-lisp-mode extensions
(add-to-list 'auto-mode-alist '("\\.el$" . emacs-lisp-mode))

;;;;;;;; Auto Complete Settings
(when (require* 'auto-complete)
  (defun ac-emacs-lisp-mode-setup ()
    "auto-complete settings for emacs-lisp-mode"
    (setq ac-sources '(ac-source-dictionary
		       ac-source-symbols
		       ac-source-variables
		       ac-source-functions
		       ac-source-features
		       ac-source-filename
		       ac-source-company-elisp
		       ac-source-words-in-same-mode-buffers
		       ))))


;;;;;;;; Hooks
(add-hook 'emacs-lisp-mode-hook
	  '(lambda ()

	     ;; Enable Auto Complete
	     (when (require* 'auto-complete)
	       (ac-emacs-lisp-mode-setup))

	     ;; Highlight differnet parentheses
	     (when (require* 'highlight-parentheses)
	       (highlight-parentheses-mode))

	     ;; Enable eldoc
	     (when (require* 'eldoc)
	       ;; Add extension for eldoc
	       (require* 'eldoc-extension)
	       (turn-on-eldoc-mode))

	     ;; Use Greek character lambda instead of string
	     (when (require* 'pretty-lambdada)
	       (turn-on-pretty-lambda-mode))

	     ;; Highlight Common Lisp style functions
	     (when (require* 'highlight-cl)
	       (highlight-cl-add-font-lock-keywords))

	     ;; Use global programming mode
	     (programming-mode)

	     ;; Use paredit in elisp
	     (use-paredit-mode)

	     ;; After visit elisp file, remove .elc extension file.
	     (remove-elc-when-visit)

	     ))

;;;;;;;; Lisp-interaction mode
;; TODO: is there a more elegent way to achive following?
;; make lisp-interaction-hook use the same functions as in
;; emacs-lisp-mode-hook
;;
(add-hook 'lisp-interaction-mode-hook
	  '(lambda ()

	     ;; Enable Auto Complete
	     (when (require* 'auto-complete)
	       (ac-emacs-lisp-mode-setup))

	     ;; Highlight differnet parentheses
	     (when (require* 'highlight-parentheses)
	       (highlight-parentheses-mode))

	     ;; Enable eldoc
	     (when (require* 'eldoc)
	       ;; Add extension for eldoc
	       (require* 'eldoc-extension)
	       (turn-on-eldoc-mode))

	     ;; Use Greek character lambda instead of string
	     (when (require* 'pretty-lambdada)
	       (turn-on-pretty-lambda-mode))

	     ;; Highlight Common Lisp style functions
	     (when (require* 'highlight-cl)
	       (highlight-cl-add-font-lock-keywords))

	     ;; Use global programming mode
	     (programming-mode)

	     ;; Use paredit in elisp
	     (use-paredit-mode)

	     ;; After visit elisp file, remove .elc extension file.
	     (remove-elc-when-visit)

	     ))

;;;;;;;; Keybindings
(add-hook 'emacs-lisp-mode-hook
	  '(lambda ()

	     ;; Normal Map

	     ;; Insert Map

	     ;;;; Insert and expand by short-key
	     ;; (define )
	     (vim:local-imap-insert-expand (kbd "M-d") "defun")
	     ;; (lambda () )
	     (vim:local-imap-insert-expand (kbd "M-l") "lambda")
	     ;; (defmacro ())
	     (vim:local-imap-insert-expand (kbd "M-D") "defmacro")

	     ))
;;;;;;;; ielm
;; A nice little mode that acts like an interactive Lisp interpreter.
;; It doesn't do all the fancy shell stuff such as redirection
;; – use the EmacsShell if you need that
;; IELM is an alternative to Lisp Interactive mode,
;; which is the mode of buffer "*scratch*".
;;
(when (require* 'ielm)
  ;;;; Hooks
  (add-hook 'ielm-mode-hook
	    '(lambda ()

	       ;; Enable Auto Complete
	       (when (require* 'auto-complete)
		 (ac-emacs-lisp-mode-setup))

	       ;; Highlight differnet parentheses
	       (when (require* 'highlight-parentheses)
		 (highlight-parentheses-mode))

	       ;; Enable eldoc
	       (when (require* 'eldoc)
		 ;; Add extension for eldoc
		 (require* 'eldoc-extension)
		 (turn-on-eldoc-mode))

	       ;; Use Greek character lambda instead of string
	       (when (require* 'pretty-lambdada)
		 (turn-on-pretty-lambda-mode))

	       ;; Highlight Common Lisp style functions
	       (when (require* 'highlight-cl)
		 (highlight-cl-add-font-lock-keywords))

	       ;; Use global programming mode
	       (programming-mode)

	       ))
  )


;;;;;;;; Functions

(defun remove-elc-when-visit ()
  "After visit elisp file, remove .elc extension file."
  (make-local-variable 'find-file-hook)
  (add-hook 'find-file-hook
	    (lambda ()
	      (if (and (file-exists-p (concat buffer-file-name "c"))
		       (file-writable-p (concat buffer-file-name "c")))
		  (delete-file (concat buffer-file-name "c"))))))



(provide 'coldnew-lang-elisp)
;; coldnew-lang-elisp.el ends here.
