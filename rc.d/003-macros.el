;;
(eval-when-compile (require 'cl))


(defmacro comment (&rest body)
  "Ignores body, yields nil"
  nil)

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


;;;; Not Interactive functions
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


(provide '003-macros)
;; 003-macros.el ends here.
