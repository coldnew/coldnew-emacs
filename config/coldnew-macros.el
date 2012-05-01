;;; coldnew-macros.el --- some useful macros
(eval-when-compile (require 'cl))

(defmacro require* (feature &optional file)
  "Try to require FEATURE, but don't signal an error if `reauire' fails.
If this package does not exist, use el-get reinstall it."
  `(let* ((require-result (require ,feature ,file 'noerror)))
     ;; if package does not exist, reinstall it
     (if-not require-result (el-get-reinstall ,feature)
	     ;; TODO: add require-result buffer
	     )))

;; add font-lock
(font-lock-add-keywords 'emacs-lisp-mode
			'(("(\\(require\*\\*\\)\\s [ \t']*\\(\\sw+\\)?"
			   (1 font-lock-keyword-face)
			   (2 font-lock-constant-face nil t))))
(font-lock-add-keywords 'lisp-interaction-mode
			'(("(\\(require\*\\*\\)\\s [ \t']*\\(\\sw+\\)?"
			   (1 font-lock-keyword-face)
			   (2 font-lock-constant-face nil t))))

;;;; ---------------------------------------------------------------------------
;;;; Clojure
;;;; ---------------------------------------------------------------------------
(defmacro comment (&rest body)
  "Comment out one or more s-expressions."
  nil)

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

(defmacro if-not (test then &optional else)
  "Evaluates test. If logical false, evaluates and returns then expr,
  otherwise else expr, if supplied, else nil."
  `(if (not ,test) ,then ,else))



(provide 'coldnew-macros)
;; coldnew-macros.el ends here.
