;;
(eval-when-compile (require 'cl))

;;;;;;;; Packages Import
(require 'coldnew-editor)
;;(require 'geiser-install)
(require* 'quack)
;;(require* 'gambit-mode)

;;;;;;;; Scheme-mode extensions
(add-to-list 'auto-mode-alist '("\\.scm$" . scheme-mode))

;;;;;;;; Auto Complete Settings
(when (require* 'auto-complete)
  (defun ac-scheme-mode-setup ()
    "auto-complete settings for scheme-mode"
    (setq ac-sources '(ac-source-dictionary
		       ;;                      ac-source-symbols
		       ;;ac-source-variables
		       ;;ac-source-functions
		       ;;                      ac-source-features
		       ac-source-filename
		       ac-source-words-in-same-mode-buffers
		       ))))
;;;;;;;; Settings
(add-hook 'scheme-mode-hook
	  '(lambda ()

	     ;; Use Gambit-C as my Scheme implementation
	     ;; (when (require* 'gambit-mode)
	     ;;   (setq scheme-program-name "gsi -:d-"))

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

	     ;; ;; Use Gambit-C as my Scheme implementation
	     ;; (when (require* 'gambit-mode)
	     ;;   (gambit-mode))

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

	     ;; ;; Use Gambit-C as my Scheme implementation
	     ;; (when (require* 'gambit-mode)
	     ;;   (gambit-inferior-mode))

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
	       (vim:local-nmap (kbd "C-c C-l") 'gambit-load-file)
	       (vim:local-nmap (kbd "C-c C")   'gambit-compile-file)

	       ;;;; Insert map
	       (vim:local-imap (kbd "C-x C-e") 'scheme-send-last-sexp)
	       (vim:local-imap (kbd "C-c C-z") 'switch-to-scheme-toggle)
	       (vim:local-nmap (kbd "C-c C-l") 'gambit-load-file)

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
	       (vim:local-nmap (kbd "C-c C-l") 'gambit-load-file)
	       (vim:local-nmap (kbd "C-c C")   'gambit-compile-file)
	       (vim:local-nmap (kbd "C-c C-q") 'gambit-quit)

	       ;;;; Insert map
	       (vim:local-imap (kbd "C-c C-z") 'switch-to-scheme-toggle)
	       (vim:local-nmap (kbd "C-c C-l") 'gambit-load-file)

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
  "Toggle between scheme-inferior-shell and currentwindows"
  (if (equal (current-buffer) (get-buffer-name "*scheme*"))
      (switch-to-buffer (other-buffer))
    (switch-to-scheme))
  )

(defun gambit-quit ()
  "Quit the Gambit Scheme process by sending ,q to it."
  (interactive)
  (scheme-send-string "#||#,q;"))




;; TODO: This need more to fix function.
;; (defun run-gambit ()
;;   "Run gambit-inferior-shell and resize the window.
;;    If current window is gambit-inferior-shell, switch to other window."
;;   (interactive)
;;   (progn
;;     (call-i


;;nteractively 'run-scheme)
;;     (let* ((current-window (selected-window))
;;         (gambit-window (get-buffer-window "*scheme*"))
;;         (gambit-window-size 15))
;;       (select-window gambit-window)
;;       (shrink-window (- (window-height gambit-window) gambit-window-size))
;;       (select-window current-window)
;;       (rename-buffer "*Gambit-C*" t))
;;     ))
;; Copied from scheme-indent-function, but ignore
;; scheme-indent-function property for local variables.

;;(require 'scheme-complete)
;;(setq lisp-indent-function 'scheme-smart-indent-function)



(put 'c-declare 'scheme-indent-function 'dummy)

(defun dummy (state indent-point normal-indent)
  (let ((containing-sexp-start (elt state 1))
	containing-sexp-point
	containing-sexp-column
	body-indent)
    ;;Move to the start of containing sexp, calculate its
    ;;indentation, store its point and move past the function
    ;;symbol so that we can use 'parse-partial-sexp'.
    ;;
    ;;'lisp-indent-function' guarantees that there is at least
    ;;one word or symbol character following open paren of
    ;;containing sexp.
    (forward-char 1)
    (goto-char containing-sexp-start)
    (setq containing-sexp-point (point))
    (setq containing-sexp-column (current-column))
    (setq body-indent 2)
    (forward-char 1)    ;Move past the open paren.
    (forward-sexp 1)    ;Move to the next sexp.

    ;;Now go back to the beginning of the line holding
    ;;the indentation point.
    (parse-partial-sexp (point) indent-point 1 t)
    (while (and (< (point) indent-point)
		(condition-case ()
		    (progn
		      (forward-sexp 1)
		      (parse-partial-sexp (point) indent-point 1 t))
		  (error nil))))
    ;;Point is sitting on the first char of the line.
    (forward-to-indentation 0)  ;Move to the first non-blank char.
    (forward-char 1)            ;Move past the open paren.

    ;; (list
    (if (looking-at "\\<#<<END\\>")
	(progn
	  (beginning-of-line)
	  (delete-char 1)
	  (insert "a"))
      ;;           body-indent
      ;;         (+ 2 body-indent)) containing-sexp-point)

      )
    ))

(defun my-scheme-indent-compensate (state indent-point normal-indent)
  (let ((containing-sexp-start (elt state 1))
	containing-sexp-point
	containing-sexp-column
	body-indent)
    ;;Move to the start of containing sexp, calculate its
    ;;indentation, store its point and move past the function
    ;;symbol so that we can use 'parse-partial-sexp'.
    ;;
    ;;'lisp-indent-function' guarantees that there is at least
    ;;one word or symbol character following open paren of
    ;;containing sexp.
    (forward-char 1)
    (goto-char containing-sexp-start)
    (setq containing-sexp-point (point))
    (setq containing-sexp-column (current-column))
    (setq body-indent 5)
    (forward-char 1)    ;Move past the open paren.
    (forward-sexp 1)    ;Move to the next sexp.

    ;;Now go back to the beginning of the line holding
    ;;the indentation point.
    (parse-partial-sexp (point) indent-point 1 t)
    (while (and (< (point) indent-point)
		(condition-case ()
		    (progn
		      (forward-sexp 1)
		      (parse-partial-sexp (point) indent-point 1 t))
		  (error nil))))
    ;;Point is sitting on the first char of the line.
    (forward-to-indentation 0)  ;Move to the first non-blank char.
    (forward-char 1)            ;Move past the open paren.
    ;;Point is sitting on first character of sexp.
    ;; (list (if (looking-at "\\<with\\>")
    ;;           body-indent
    ;;         (+ 2 body-indent)) containing-sexp-point)

    ))


(provide 'coldnew-lang-scheme)
;; coldnew-lang-scheme.el ends here.
