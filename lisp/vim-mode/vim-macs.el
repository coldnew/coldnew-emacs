;;; vim-macs.el - Basic macros for vim-mode.

;; Copyright (C) 2009, 2010, 2011 Frank Fischer

;; Author: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;;
;; This file is not part of GNU Emacs.

;;; Code:

(eval-when-compile (require 'cl))

(defmacro vim:deflocalvar (name &rest args)
  "Defines a buffer-local variable."
  (declare (indent defun))
  `(progn
     (defvar ,name ,@args)
     (make-variable-buffer-local ',name)))

(defmacro* vim:defcmd (name (&rest args) &rest body)
  "Defines a new VIM-command."
  (declare (indent defun))
  (let ((count nil)
        (register nil)
        (motion nil)
        (argument nil)
        (keep-visual nil)
        (repeatable t)
	(force nil)
        (params nil)
        (named-params nil)
        (doc nil))

    ;; extract documentation string
    (if (and (consp body)
               (cdr body)
               (stringp (car body)))
        (setq doc (car body)
              body (cdr body))
      (setq doc (format "VIM - command (%s %s)" name args)))
    
    ;; collect parameters
    (dolist (arg args)
      (case (if (consp arg) (car arg) arg)
        ('count
         (setq count t)
         (push '(count nil) params)
         (when (and (consp arg)
                    (not (eq (cadr arg) 'count)))
           (push `(,(cadr arg) count) named-params)))

        ('register
         (setq register t)
         (push '(register nil) params)
         (when (and (consp arg)
                    (not (eq (cadr arg) 'register)))
           (push `(,(cadr arg) register) named-params)))

        ('motion
         (when motion
           (error "%s: only one motion argument may be specified: %s" 'vim:defcmd arg))
         (setq motion t)
         (push 'motion params)
         (when (and (consp arg)
                    (not (eq (cadr arg) 'motion)))
           (push `(,(cadr arg) motion) named-params)))

        ('motion:optional
         (when motion
           (error "%s: only one motion argument may be specified: %s" 'vim:defcmd arg))
         (setq motion ''optional)
         (push 'motion params)
         (when (and (consp arg)
                    (not (eq (cadr arg) 'motion)))
           (push `(,(cadr arg) motion) named-params)))
        
        ('keep-visual (setq keep-visual t))
        ('do-not-keep-visual (setq keep-visual nil))
        ('repeatable (setq repeatable t))
        ('nonrepeatable (setq repeatable nil))
	('force 
	 (when force 
	   (error "%s: only one force argument may be specified: %s" 'vim:defcmd arg))
	 (setq force t)
	 (push 'force params)
         (when (and (consp arg)
                    (not (eq (cadr arg) 'force)))
           (push `(,(cadr arg) force) named-params)))
        
	(t
	 (let ((arg-name (symbol-name (if (consp arg) (car arg) arg))))
	   (unless (string-match "^argument\\(?::\\([[:word:]]+\\)\\)?$" arg-name)
	     (error "%s: Unexpected argument: %s" 'vim:defcmd arg))
	   (when argument
	     (error "%s: only one argument may be specified: %s" 'vim:defcmd arg))
	   (let ((arg-type (if (match-beginning 1)
			       `',(intern (match-string 1 arg-name))
			     ''text)))
	     (setq argument arg-type)
	     (push 'argument params)
	     (when (and (consp arg)
			(not (eq (cadr arg) 'argument)))
	       (push `(,(cadr arg) argument) named-params)))))))
      
    `(progn
       (put ',name 'type ',(if motion 'complex 'simple))
       (put ',name 'count ,count)
       (put ',name 'motion ,motion)
       (put ',name 'argument ,argument)
       (put ',name 'register ,register)
       (put ',name 'keep-visual ,keep-visual)
       (put ',name 'repeatable ,repeatable)
       (put ',name 'force ,force)
       (put ',name 'function
            (function* (lambda (,@(when params `(&key ,@params))
                                ,@(when named-params `(&aux ,@named-params)))
                         ,@body)))
       (defun* ,name (&rest args)
         ,doc
         (interactive)
         (if (vim:called-interactively-p)
             (funcall vim:active-command-function ',name)
           (apply (get ',name 'function) args))))))

(defmacro* vim:defmotion (name (&rest args) &rest body)
  (declare (indent defun))
  (let ((type nil)
        (count nil)
        (argument nil)
        (params nil)
        (named-params nil)
        (doc nil))

    ;; extract documentation string
    (if (and (consp body)
               (cdr body)
               (stringp (car body)))
        (setq doc (car body)
              body (cdr body))
      (setq doc (format "VIM - motion (%s %s)" name args)))
    
    ;; collect parameters
    (dolist (arg args)
      (case (if (consp arg) (car arg) arg)
        ((inclusive exclusive linewise block)
         (setq type arg))
        
        ('count
         (setq count t)
         (push '(count nil) params)
         (when (and (consp arg)
                    (not (eq (cadr arg) 'count)))
           (push `(,(cadr arg) count) named-params)))
        
        ((argument argument:char)
         (when argument
           (error "%s: only one argument may be specified: %s" 'vim:defcmd arg))
         (setq argument ''char)
         (push 'argument params)
         (when (and (consp arg)
                    (not (eq (cadr arg) 'argument)))
           (push `(,(cadr arg) argument) named-params)))
        
        (t (error "%s: Unexpected argument: %s" 'vim:defmotion arg))))

    (unless type
      (error "%s: Motion type must be specified" 'vim:defmotion))

    `(progn
       (put ',name 'type ',type)
       (put ',name 'count ,count)
       (put ',name 'argument ,argument)
       (put ',name 'function
            (function* (lambda (,@(when params `(&key ,@params))
                                ,@(when named-params `(&aux ,@named-params)))
                         (vim:do-motion ',type (progn ,@body)))))
       (defun* ,name (&rest args)
         ,doc
         (interactive)
         (if (vim:called-interactively-p)
             (vim:execute-command ',name)
           (apply (get ',name 'function) args))))))

(font-lock-add-keywords
 'emacs-lisp-mode 
 '("vim:deflocalvar" "vim:defcmd" "vim:defmotion"))

(provide 'vim-macs)

;;; vim-macs.el ends here
