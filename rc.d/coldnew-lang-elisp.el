;;
(eval-when-compile (require 'cl))

;;;;;;;; Packages Import
(require 'coldnew-editor)

;;;;;;;; Loding libraries


;;;;;;;; Emacs-lisp-mode extensions
(add-to-list 'auto-mode-alist '("\\.el$" . emacs-lisp-mode))

;;;;;;;; Auto Complete Settings
(defun ac-emacs-lisp-mode-setup ()
  "auto-complete settings for emacs-lisp-mode"
  ;; (make-variable-buffer-local 'ac-sources)
  (setq ac-sources
	'(ac-source-dictionary
	  ac-source-symbols
	  ac-source-variables
	  ac-source-functions
	  ac-source-features
	  ;;        ac-source-company-elisp
	  ac-source-abbrev
	  ac-source-semantic
	  ac-source-filename
	  ac-source-files-in-current-dir
	  ac-source-words-in-same-mode-buffers
	  ))
  )


;;;;;;;; Flymake
;;(defun flymake-elisp-init ()
;;  (flymake-generic-init "elisplint"))

;;(add-to-list 'flymake-allowed-file-name-masks
;;       '(".+\\el$"
;;         flymake-elisp-init
;;         flymake-simple-cleanup
;;         flymake-get-real-file-name))

;; ;;;;;;;; Anything Setting
;; (when (featurep 'anything)
;;   (defun elisp-mode:anything-info ()
;;     ""
;;     (interactive)
;;     (anything
;;      :prompt "Info about: "
;;      :candidate-number-limit 5
;;      :sources
;;      '( anything-c-source-emacs-functions
;;      anything-c-source-emacs-variables
;;      anything-c-source-info-elisp
;;      anything-c-source-emacs-source-defun
;;      anything-c-source-emacs-lisp-expectations
;;      anything-c-source-emacs-lisp-toplevels
;;      anything-c-source-emacs-functions-with-abbrevs
;;      anything-c-source-info-emacs)))
;;   )

;;;;;;;; Hooks
(add-hook 'emacs-lisp-mode-hook
	  '(lambda ()
	     ;; Enable Auto Complete
	     (when (require* 'auto-complete)
	       (ac-emacs-lisp-mode-setup))

	     ;; TODO: whate's the defierence between highlight-parentheses and rainbow-delimiters?
	     ;;
	     ;; Highlight differnet parentheses
	     ;; (when (require* 'highlight-parentheses)
	     ;;   (highlight-parentheses-mode))

	     ;; Color nested parentheses, brackets, and braces according to their depth
	     (when (require* 'rainbow-delimiters)
	       (rainbow-delimiters-mode))

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
;;;; Insert and expand by short-key
  ;;;; Insert Map
;; (define )
(evil-define-key-insert 'insert emacs-lisp-mode-map (kbd "M-d") "defun")
;; (require* 'FEATURE) or (require 'FEATURE)
(evil-define-key-insert 'insert emacs-lisp-mode-map (kbd "M-r") "require")
;; (lambda () )
(evil-define-key-insert 'insert emacs-lisp-mode-map (kbd "M-l") "lambda")
;; (defmacro ())
(evil-define-key-insert 'insert emacs-lisp-mode-map (kbd "M-D") "defmacro")

(evil-define-key 'normal emacs-lisp-mode-map (kbd "K") 'elisp-mode:anything-info)



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
