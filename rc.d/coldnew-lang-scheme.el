;;
(eval-when-compile (require 'cl))

;;;;;;;; Packages Import
(require 'coldnew-editor)
;;(require 'geiser-install)
(require* 'quack)
(require* 'gambit)

;;;;;;;; Scheme-mode extensions
(add-to-list 'auto-mode-alist '("\\.scm$" . scheme-mode))

;;;;;;;; Auto Complete Settings
(when (require* 'auto-complete)
  (defun ac-scheme-mode-setup ()
    "auto-complete settings for scheme-mode"
    (setq ac-sources '(ac-source-dictionary
		       ac-source-filename
		       ac-source-words-in-same-mode-buffers
		       ))))
;;;;;;;; Settings
(add-hook 'scheme-mode-hook
	  '(lambda ()
	     (when (require* 'gambit)
	       (setq scheme-program-name "gsc -:d-"))
	     ))

;;;;;;;; Hooks
(add-hook 'scheme-mode-hook
	  '(lambda ()

	     ;; Enable Auto Complete
	     (when (require* 'auto-complete)
	       (ac-scheme-mode-setup))

	     ;; Highlight differnet parentheses
	     (when (require* 'highlight-parentheses)
	       (highlight-parentheses-mode))

	     ;; Use Greek character lambda instead of string
	     (when (require* 'pretty-lambdada)
	       (turn-on-pretty-lambda-mode))

	     ;; Use Gambit-C in scheme mode
	     (when (require* 'gambit)
	       (function gambit-mode))

	     ;; Use global programming mode
	     (programming-mode)

	     ;; Use paredit in scheme
	     (use-paredit-mode)

	     ))

(add-hook 'inferior-scheme-mode-hook
	  '(lambda ()
	     ;; Enable Auto Complete
	     (when (require* 'auto-complete)
	       (ac-scheme-mode-setup))

	     ;; Highlight differnet parentheses
	     (when (require* 'highlight-parentheses)
	       (highlight-parentheses-mode))

	     ;; Use Greek character lambda instead of string
	     (when (require* 'pretty-lambdada)
	       (turn-on-pretty-lambda-mode))

	     ;; Use Gambit-C in inferior-scheme mode
	     (when (require* 'gambit)
	       (function gambit-inferior-mode))

	     ;; Use global programming mode
	     (programming-mode)

	     ;; Use paredit in scheme
	     (use-paredit-mode)

	     ))

;;;;;;;; Keybindings
(add-hook 'scheme-mode-hook
	  '(lambda ()

	     (when (require* 'vim)
	       ;;;; Normal map
	       (vim:local-nmap (kbd "C-x C-e") 'scheme-send-last-sexp)
	       (vim:local-nmap (kbd "C-c C-z") 'switch-to-scheme-toggle)

	       ;;;; Insert map
	       (vim:local-imap (kbd "C-x C-e") 'scheme-send-last-sexp)
	       (vim:local-imap (kbd "C-c C-z") 'switch-to-scheme-toggle)

	       ;;;; Insert and expand by short-key
	       ;; (define )
	       (vim:local-imap-insert-expand (kbd "M-d") "define")
	       ;; (lambda () )
	       (vim:local-imap-insert-expand (kbd "M-l") "lambda")
	       ;; (begin )
	       (vim:local-imap-insert-expand (kbd "M-b") "begin")
	       ;; (define-macro ())
	       (vim:local-imap-insert-expand (kbd "M-D") "define-macro")
	       )))

(add-hook 'inferior-scheme-mode-hook
	  '(lambda ()
	     (when (require* 'vim)
	       ;;;; Normal map
	       (vim:local-nmap (kbd "C-c C-z") 'switch-to-scheme-toggle)

	       ;;;; Insert map
	       (vim:local-imap (kbd "C-c C-z") 'switch-to-scheme-toggle)

	       )))

;;;;;;;;  Advice
(defadvice run-scheme (after run-scheme activate)
  "Run scheme-inferior-shell with fixed window-height."
  (let ((current-window (selected-window))
	(scheme-window (get-buffer-window "*scheme*"))
	(scheme-window-size 15))
    (select-window scheme-window)
    (shrink-window (- (window-height scheme-window) scheme-window-size))
    (select-window current-window))
  )

;;;;;;;; Functions
(defun switch-to-scheme-toggle ()
  (interactive)
  "Toggle between scheme-inferior-shell and currentwindows"
  (if (equal (current-buffer) (get-buffer-name "*scheme*"))
      (switch-to-buffer (other-buffer))
      (switch-to-scheme))
  )


(provide 'coldnew-lang-scheme)
;; coldnew-lang-scheme.el ends here.
