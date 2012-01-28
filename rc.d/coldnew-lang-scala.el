;;
(eval-when-compile (require 'cl))

;;;;;;;; Packages Import
(require 'coldnew-editor)
(require 'scala-mode)

;;;;;;;; Emacs-lisp-mode extensions
(add-to-list 'auto-mode-alist '("\\.scala$" . scala-mode))

;;;;;;;; Coding-Style Setting
;;;BUG: no use ? why?
;; (add-hook 'scala-mode-hook
;;        (lambda ()
;;          ;; Setting indentation lvel
;;          (setq scala-mode-indent:step 8)
;;          ))

;; Setting indentation lvel
(setq scala-mode-indent:step 8)

;;;;;;;; Hooks
(add-hook 'scala-mode-hook
	  '(lambda ()

	     ;; ENSIME is the ENhanced Scala Interaction Mode for Emacs.
	     (when (require* 'ensime)
	       ensime-scala-mode-hook)

	     ;; Enable Auto Complete
	     (when (require* 'auto-complete)
	       (ac-emacs-lisp-mode-setup))

	     ;; Color nested parentheses, brackets, and braces according to their depth
	     (when (require* 'rainbow-delimiters)
	       (rainbow-delimiters-mode))

	     ;; Use global programming mode
	     (programming-mode)

	     ))

;;;;;;;; Keybinding
(add-hook 'scala-mode-hook
	  '(lambda ()

	     ;; Normal Map

	     ;; Insert Map
	     ;; (vim:local-imap (kbd "RET") 'scala-mode:newline-and-indent)

	     ;;;; Insert and expand by short-key
	     ;; (vim:local-imap-insert (kbd "M-i") "import ")

	     ))


;;;;;;;; Functions
(defun scala-mode:newline-and-indent ()
  (interactive)
  (delete-horizontal-space)
  (let ((last-command nil))
    (newline-and-indent))
  (when (scala-in-multi-line-comment-p)
    (insert "* ")))

;;;;;;;;  Advice
;; (defadvice scala-run-scala (after run-scala activate)
;;   "Run scala-inferior-shell with fixed window-height."
;;   (let ((current-window (selected-window))
;;	(scala-window (get-buffer-window "*inferior-scala*"))
;;	(scala-window-size 25))
;;     (select-window scala-window)
;;     (shrink-window (- (window-height scala-window) scala-window-size))
;;     (select-window current-window))
;;   )
;; (defalias 'run-scala 'scala-run-scala)
;; (defun run-scala ()
;;   (interactive)
;;   (ensime-sbt-action "run")
;;   (ensime-sbt-action "~compile")
;;   (let ((c (current-buffer)))
;;     (switch-to-buffer-other-window
;;      (get-buffer-create (ensime-sbt-build-buffer-name)))
;;     (switch-to-buffer-other-window c)))



(provide 'coldnew-lang-scala)
;; coldnew-lang-scala.el ends here.
