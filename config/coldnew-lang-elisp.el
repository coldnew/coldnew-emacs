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

;; my auto-complete for elisp
;;(require 'auto-complete-emacs-lisp)
(add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)

;; use eldoc
(require 'eldoc)
(require 'eldoc-extension)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
;; make eldoc work with paredit
(when (featurep 'paredit)
  (eldoc-add-command 'paredit-backward-delete 'paredit-close-round))

;; highlight common lisp style functions
(require 'highlight-cl)
(add-hook 'emacs-lisp-mode-hook 'highlight-cl-add-font-lock-keywords)

;;;; ---------------------------------------------------------------------------
;;;; Keybindings
;;;; ---------------------------------------------------------------------------
(define-key emacs-lisp-mode-map (kbd "C-c i") 'elisp-mode:anything-info-search)

;;;; ---------------------------------------------------------------------------
;;;; Autocomplete
;;;; ---------------------------------------------------------------------------

(defun ac-emacs-lisp-mode-setup ()
  "auto-complete settings for emacs-lisp-mode"
  (setq ac-sources
	'(ac-source-symbols
	  ac-source-variables
	  ac-source-functions
	  ac-source-features
	  ac-source-filename
	  ac-source-files-in-current-dir
	  ac-source-words-in-same-mode-buffers
	  )))
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

(defun elisp-mode:anything-info-search ()
  "search info for elisp"
  (interactive)
  (anything
   :prompt "Info about: "
   :candidate-number-limit 5
   :source
   '(anything-c-source-emacs-functions
     anything-c-source-emacs-variables
     anything-c-source-info-elisp
     anything-c-source-emacs-commands
     anything-c-source-emacs-source-defun
     anything-c-source-emacs-lisp-expectations
     anything-c-source-emacs-lisp-toplevels
     anything-c-source-emacs-functions-with-abbrevs
     anything-c-source-info-emacs
     )))




(provide 'coldnew-lang-elisp)
;; coldnew-lang-elisp.el ends here.
