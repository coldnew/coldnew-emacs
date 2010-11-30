;;;
(provide 'rc-macro)
(eval-when-compile (require 'cl))

;;;;;; Macro

(defmacro* defcmd (name &rest body)
  "Define a new command in macro."
  (if (and (consp body)
	   (cdr body)
	   (stringp (car body)))
      (setq doc (car body)
	    body (cdr body))
    (setq doc (format "Command (%s)" name)))
  `(progn
     (put ',name 'function
	  (function* (lambda  ,@body)))
     (defun* ,name (&rest args)
       ,doc
       (interactive)
       (apply (get ',name 'function) args))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;; (defmacro require-maybe (feature &optional file)
;;   "*Try to require FEATURE, but don't signal an error if `require' fails."
;;   `(require ,feature ,file 'noerror))

(defmacro when-available (func foo)
  "*Do something if FUNCTION is available."
  `(when (fboundp ,func) ,foo))

(defmacro require-maybe (feature &optional file)
  "*Try to require FEATURE, but don't signal an error if `require' fails."
  `(let ((require-result (require ,feature ,file 'noerror)))
     (with-current-buffer (get-buffer-create "*Startup Log*")
       (let* ((startup-log-format-string-prefix "%-20s--------[")
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

;; (defvar tes-font-lock
;;   (eval-when-compile
;;     `(
;;       ,(concat "(require-maybe)\\>"
;; 	       "[ \t']*\\(\\sw+\\)?")
;;       (1 font-lock-keyword-face)
;;       (2 font-lock-constant-face nil t)
;;       )
;;     )
;;   )
