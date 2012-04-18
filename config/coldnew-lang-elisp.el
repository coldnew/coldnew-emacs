;;; coldnew-lang-elisp.el ---                                                     
(eval-when-compile (require 'cl))                                        

(require 'coldnew-editor)

;;;; emacs-lisp-mode extensions
(add-to-list 'auto-mode-alist '("\\.el$" . emacs-lisp-mode))

;;;; ---------------------------------------------------------------------------
;;;; Hooks
;;;; ---------------------------------------------------------------------------

;; when visit file, remove .elc extension file 
(add-hook 'emacs-lisp-mode-hook 'remove-elc-when-visit)

;; use my lisp-common-setting
(add-hook 'emacs-lisp-mode-hook 'coldnew-lisp-common-setting)

;; use eldoc
(require 'eldoc)
(require 'eldoc-extension)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

;; highlight common lisp style functions
(require 'highlight-cl)
(add-hook 'emacs-lisp-mode-hook 'highlight-cl-add-font-lock-keywords)


;;;; ---------------------------------------------------------------------------
;;;; Keybindings
;;;; ---------------------------------------------------------------------------



;;;; ---------------------------------------------------------------------------
;;;; Functions
;;;; ---------------------------------------------------------------------------

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
