;;
(eval-when-compile (require 'cl))



;; (defmacro vim:local-imap-insert (key name)
;;   "insert string in vim:local-imap."
;;   `(vim:local-imap ,key
;;		   '(lambda () (interactive) (insert ,name))))

;; (defmacro vim:local-imap-insert-expand (key name)
;;   "insert and expand by yasnippet in vim:local-imap."
;;   `(vim:local-imap ,key
;;		   '(lambda () (interactive) (insert ,name) (yas/expand))))


;;;;;;;; Macros
(defmacro when-require (feature &optional file &rest body)
  "Try to require FEATURE, but dont' signal an error if `require' failed."
  `(when (require ,feature ,file 'noerror)
     ,@body
     ))

(font-lock-add-keywords 'emacs-lisp-mode
			'(("(\\(when-require\\*\\)\\s [ \t']*\\(\\sw+\\)?"
			   (1 font-lock-keyword-face)
			   (2 font-lock-constant-face nil t))))

(defmacro require* (feature &optional file)
  "*Try to require FEATURE, but don't signal an error if `require' fails."
  `(let ((require-result (require ,feature ,file 'noerror)))
     (with-current-buffer (get-buffer-create "*Loading Library Log*")
			  (let* ((startup-log-format-string-prefix "\t%-30s\t\t\t[")
				 (startup-log-format-string-postfix "%s")
				 (startup-status (if require-result "LOADED" "FAILED"))
				 (startup-status-face `(face (:foreground
							      ,(if require-result "green" "red")))))
			    (insert (format startup-log-format-string-prefix ,feature))
			    (let ((start-pos (point)))
			      (insert (format startup-log-format-string-postfix startup-status))
			      (add-text-properties start-pos (point) startup-status-face)
			      (insert "]\n"))))
     require-result))

(font-lock-add-keywords 'emacs-lisp-mode
			'(("(\\(require\*\\*\\)\\s [ \t']*\\(\\sw+\\)?"
			   (1 font-lock-keyword-face)
			   (2 font-lock-constant-face nil t))))
(font-lock-add-keywords 'lisp-interaction-mode
			'(("(\\(require\*\\*\\)\\s [ \t']*\\(\\sw+\\)?"
			   (1 font-lock-keyword-face)
			   (2 font-lock-constant-face nil t))))

;;;;;;;; Functions


(defun try-require (&rest args)
  "Attempt to load a library or module. Return true if all of the libraries
given as arguments are successfully loaded"
  (if (member nil
	      (mapcar (lambda (thing)
			(condition-case e
					(if (stringp thing)
					    (load-library thing)
					    (require thing))
					(file-error () nil)))
		      args))
      nil
      t))

(provide 'coldnew-macro)
;; coldnew-macro.el ends here.
