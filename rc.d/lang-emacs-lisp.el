
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
	     ;;(byte-compile-when-save)	; bytecompile the elisp file after save;

	     (remove-elc-when-visit)	; when visit elisp file, remove .elc extensioon
	     (programming-common-hook)	; programming common hook
	     ))

;;;;;; Keybindings
(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    ;; Normal map
	    (vim:local-nmap (kbd "zv") 'describe-variable)
	    (vim:local-nmap (kbd "zf") 'describe-function)
	    (vim:local-nmap (kbd "zk") 'describe-key)
	    (vim:local-nmap (kbd "zs") 'describe-syntax)
	    (vim:local-nmap (kbd "zm") 'describe-mode)
	    (vim:local-nmap (kbd "zc") 'describe-coding-system)
	    (vim:local-nmap (kbd "<f5>") 'eval-current-buffer)

	    ;; Insert map
	    (vim:local-imap (kbd "M-i") (lambda () (interactive) (insert "if") (yas/expand)))
	    (vim:local-imap (kbd "M-s") (lambda () (interactive) (insert "setq") (yas/expand)))
	    (vim:local-imap (kbd "M-r") 'emacs-lisp-mode:insert-require)
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

(defun byte-compile-when-save()
  "When save, recompile it"
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
	    (lambda ()
	      (if (buffer-file-name)
		  (byte-compile-file buffer-file-name)))))

(defcmd emacs-lisp-mode:insert-require ()
  "insert require smartly
  I use special function `reauiref' and `reauire*' in emacs config
  when buffer is 001-init, insert requiref instead of require
  when buffer is 002-dependency, insert require* "
  (insert "require")
  (yas/expand))



(provide 'lang-emacs-lisp)
;;; rc-emacs-lisp-mode.el ends here
