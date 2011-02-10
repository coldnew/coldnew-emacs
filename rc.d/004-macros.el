;;
(eval-when-compile (require 'cl))

;;;;;;;; Macros
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

(defmacro when-available (func foo)
  "*Do something if FUNCTION is available."
  `(when (fboundp ,func) ,foo))

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


;;;;;;;; Not Interactive functions
;; ------------------------------------------------------------------------------
;; show buffer major mode
;; ------------------------------------------------------------------------------
(defun show-buffer-major-mode(buffer-or-string)
  "Returns the major mode associated with a buffer."
  (with-current-buffer buffer-or-string major-mode)
  )

;; ------------------------------------------------------------------------------
;; Predicate if process ID is an emacs process
;; ------------------------------------------------------------------------------
(defun emacs-process-p (pid)
  "If pid is the process ID of an emacs process, return t, else nil.
Also returns nil if pid is nil."
  (when pid
    (let ((attributes (process-attributes pid)) (cmd))
      (dolist (attr attributes)
	(if (string= "comm" (car attr))
	    (setq cmd (cdr attr))))
      (if (and cmd (or (string= "emacs" cmd) (string= "emacs.exe" cmd))) t))))


;;;;;;;; Extra font-lock for macros
(font-lock-add-keywords 'emacs-lisp-mode
			'(
			  ("(\\(\\defcmd\\)\\s \\(\\(?:\\s_\\|\\sw\\)+\\)"
			   (1 font-lock-keyword-face)
			   (2 font-lock-function-name-face))
			  ("(\\(when-available\\*\\)\\s [ \t']*\\(\\sw+\\)?"
			   (1 font-lock-keyword-face)
			   (2 font-lock-constant-face nil t))
			  ("(\\(\\comment\\)\\s \\(\\(?:\\s_\\|\\sw\\)+\\)"
			   (1 font-lock-comment-delimiter-face)
			   (2 font-lock-comment-face))
			  ))




(provide '004-macros)
;; 004-macros.el ends here.
