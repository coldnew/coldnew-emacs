
(eval-when-compile (require 'cl))

;;;;;; Auto-mode alist
(add-to-list 'auto-mode-alist '("\\.el$" . emacs-lisp-mode))

;;;;; Auto-complete
(defun ac-emacs-lisp-mode-setup ()
  "auto-complete settings for emacs-lisp-mode"
  (setq ac-sources '(ac-source-symbols ac-source-company-elisp
				       ac-source-words-in-same-mode-buffers)))

;;;;;; Hook
(add-hook 'emacs-lisp-mode-hook
	  '(lambda ()
	     ;; Highlight the parentheses
	     (highlight-parentheses-mode)
	     ;; Enable eldoc
	     (turn-on-eldoc-mode)
	     ;; Remodify auto-complete setting in auto-complete-config.el
	     (ac-emacs-lisp-mode-setup)
	     ;; Enable pretty-lambda
	     (turn-on-pretty-lambda-mode)
	     ;; Highlight common-lisp functions
	     (highlight-cl-add-font-lock-keywords)
	     ;; Hooks for emacs-lisp-mode
	     (remove-elc-when-visit)	; when visit elisp file, remove .elc extensioon
	     (programming-common-hook)	; programming common hook
	     ))

;;;;;; Keybindings
(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    ;;;; Normal map
	    (vim:local-nmap (kbd "zv") 'describe-variable)
	    (vim:local-nmap (kbd "zf") 'describe-function)
	    (vim:local-nmap (kbd "zk") 'describe-key)
	    (vim:local-nmap (kbd "zs") 'describe-syntax)
	    (vim:local-nmap (kbd "zm") 'describe-mode)
	    (vim:local-nmap (kbd "zc") 'describe-coding-system)
	    (vim:local-nmap (kbd "<f5>") 'eval-current-buffer)

	    ;;;; Insert map
	    ;; Insert (if )
	    (vim:local-imap (kbd "M-i") (lambda () (interactive) (insert "if") (yas/expand)))
	    ;; Insert (setq )
	    (vim:local-imap (kbd "M-s") (lambda () (interactive) (insert "setq") (yas/expand)))
	    ;; Insert (add-hook 'HOOK 'FUNCTION)
	    (vim:local-imap (kbd "M-a") (lambda () (interactive) (insert "add-hook") (yas/expand)))
	    ;; Insert (require 'FEATURE)
	    (vim:local-imap (kbd "M-r") (lambda () (interactive) (insert "require") (yas/expand)))
	    ;; Insert (lambda () "DOCSTRING" (interactive) BODY)
	    (vim:local-imap (kbd "M-l") (lambda () (interactive) (insert "lambda") (yas/expand)))
	    ;; Insert (format "STRING" &rest OBJECTS)
	    (vim:local-imap (kbd "M-f") (lambda () (interactive) (insert "format") (yas/expand)))
	    ;; Insert(defun NAME (ARGLIST) "DOCSTRING" BODY)
	    (vim:local-imap (kbd "M-d") (lambda () (interactive) (insert "defun") (yas/expand)))
	    (vim:local-imap (kbd "<f5>") 'eval-current-buffer)
	    ))

;;;;;; Misc Settings
;; if *scratch* does not exist, create it.
(run-with-idle-timer 1 t
		     '(lambda () (get-buffer-create "*scratch*")))
(add-hook 'lisp-interaction-mode-hook 'highlight-cl-add-font-lock-keywords)

;;;;;; Functions

(defun remove-elc-when-visit ()
  "When visit, remove <filename>.elc"
  (make-local-variable 'find-file-hook)
  (add-hook 'find-file-hook
	    (lambda ()
	      (if (file-exists-p (concat buffer-file-name "c"))
		  (delete-file (concat buffer-file-name "c"))))))

(provide 'lang-emacs-lisp)
;;; rc-emacs-lisp-mode.el ends here
