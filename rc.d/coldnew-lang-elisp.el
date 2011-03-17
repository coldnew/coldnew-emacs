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
    (setq ac-sources '(ac-source-symbols ac-source-company-elisp
					 ac-source-words-in-same-mode-buffers)))
  )


;;;;;;;; Hooks
(add-hook 'emacs-lisp-mode-hook
	  '(lambda ()
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
	     (when (require* 'hightlight-cl)
	       (highlight-cl-add-font-lock-keywords))

	     ;; Use global programming mode
	     (programming-mode)

	     ;; After visit elisp file, remove .elc extension file.
	     (remove-elc-when-visit)

	     ))

;;;;;;;; Functions

(defun remove-elc-when-visit ()
  "After visit elisp file, remove .elc extension file."
  (make-local-variable 'find-file-hook)
  (add-hook 'find-file-hook
	    (lambda ()
	      (if (file-exists-p (concat buffer-file-name "c"))
		  (delete-file (concat buffer-file-name "c"))))))



(provide 'coldnew-lang-elisp)
;; coldnew-lang-elisp.el ends here.
