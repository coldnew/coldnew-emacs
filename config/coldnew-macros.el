;;; coldnew-macros.el --- some useful macros
(eval-when-compile (require 'cl))

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
