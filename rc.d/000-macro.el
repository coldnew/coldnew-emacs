;;;
(eval-when-compile (require 'cl))

;;;;; Extra font-lock face for userdefine macro.

;;;; Extra font-lock
(font-lock-add-keywords 'emacs-lisp-mode
			'(("(\\(\\defcmd\\)\\s \\(\\(?:\\s_\\|\\sw\\)+\\)"
			   (1 font-lock-keyword-face)
			   (2 font-lock-function-name-face))
			  ("(\\(require-maybe\\)\\s [ \t']*\\(\\sw+\\)?"
			   (1 font-lock-keyword-face)
			   (2 font-lock-constant-face nil t))))

(defmacro comment (&rest body)
  "Ignores body, yields nil"
  nil)

(defmacro* defcmd (name &rest body)
  "Define a interactive functions without arguments."
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



;; (defmacro require-maybe (feature &optional file)
;;   "*Try to require FEATURE, but don't signal an error if `require' fails."
;;   `(require ,feature ,file 'noerror))

(defmacro when-available (func foo)
  "*Do something if FUNCTION is available."
  `(when (fboundp ,func) ,foo))

(defmacro require-maybe (feature &optional file)
  "*Try to require FEATURE, but don't signal an error if `require' fails."
  `(let ((require-result (require ,feature ,file 'noerror)))
     (with-current-buffer (get-buffer-create "*Loading Log*")
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

;; unsort macro
;; (defmacro ->> (&rest body)
;;   (let ((result (pop body)))
;;     (dolist (form body result)
;;       (setq result (append form (list result))))))

;; (defmacro -> (&rest body)
;;   (let ((result (pop body)))
;;     (dolist (form body result)
;;       (setq result (append (list (car form) result)
;;			   (cdr form))))))

;; Clojure's Trush operators
(defmacro -> (x &optional form &rest more)
  (cond ((not (null more))
	 `(-> (-> ,x ,form) ,@more))
	((not (null form))
	 (if (sequencep form)
	     `(,(first form) ,x ,@(rest form))
	   (list form x)))
	(t x)))

(defmacro ->> (x form &rest more)
  (cond ((not (null more)) `(->> (->> ,x ,form) ,@more))
	(t (if (sequencep form)
	       `(,(first form) ,@(rest form) ,x)
	     (list form x)))))

(defmacro -?> (x form &rest more)
  (cond ((not (null more)) `(-?> (-?> ,x ,form) ,@more))
	(t (if (sequencep form)
	       `(if (null ,x) nil
		  (,(first form) ,x ,@(rest form)))
	     `(if (null ,x) nil
		,(list form x))))))

(defmacro -?>> (x form &rest more)
  (cond ((not (null more)) `(-?>> (-?>> ,x ,form) ,@more))
	(t (if (sequencep form)
	       `(if (null ,x) nil
		  (,(first form) ,@(rest form) ,x))
	     `(if (null ,x) nil
		,(list form x))))))

;; Functional tools

(defmacro partial (f &rest args)
  `(lambda (&rest more)
     (apply ',f ,@args more)))

(defmacro lexdef (name args &rest body)
  "Defun with lexically-scoped parameters. Could also be called lexical-defun."
  `(defun ,name ,args
     (lexical-let ,(->> args
			(remove-if (partial equal '&rest))
			(mapcar (lambda (arg) (list arg arg))))
       ,@body)))






(provide '000-macro)
;; 000-macro.el ends here.
