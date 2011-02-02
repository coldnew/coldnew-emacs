;;; vim-vim.el - Basic functions and macros for vim-mode.

;; Copyright (C) 2009, 2010 Frank Fischer

;; Author: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;;
;; This file is not part of GNU Emacs.

;;; Code:

(defvar vim:repeat-events nil
  "The sequence of events for the repeat command.")

(vim:deflocalvar vim:current-register nil
  "The register of the current command.")

(vim:deflocalvar vim:current-cmd-count nil
  "The count of the current command.")

(vim:deflocalvar vim:current-cmd nil
  "The node of the current command.")

(vim:deflocalvar vim:current-cmd-arg nil
  "The argument of the current command.")

(vim:deflocalvar vim:current-motion-count nil
  "The count of the current motion.")

(vim:deflocalvar vim:current-motion nil
  "The node of the current motion.")

(vim:deflocalvar vim:current-motion-arg nil
  "The argument of the current motion.")

(vim:deflocalvar vim:current-motion-type nil
  "The type of the current motion (inclusive, exclusive, linewise).")

(defun vim:toplevel-execution ()
  "Returns t iff this is a toplevel execution, not a mapping or repeat."
  (not executing-kbd-macro))


(defadvice vim:reset-key-state (before vim:vim-reset-key-state)
  "Resets the current state of the keymap."
  (setq vim:current-register nil
        vim:current-cmd-count nil
        vim:current-cmd nil
        vim:current-cmd-arg nil
        vim:current-motion-count nil
        vim:current-motion nil
        vim:current-motion-arg nil
        vim:current-motion-type nil))
(ad-activate 'vim:reset-key-state)


(defmacro* vim:defcmd (name (&rest args) &rest body)
  "Defines a new VIM-command."
  (declare (indent defun))
  (let ((count nil)
        (register nil)
        (motion nil)
        (argument nil)
        (keep-visual nil)
        (repeatable t)
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
        
        ((argument argument:char argument:file argument:buffer)
         (when argument
           (error "%s: only one argument may be specified: %s" 'vim:defcmd arg))
         (let* ((arg-name (symbol-name (if (consp arg) (car arg) arg)))
                (pos (position ?: arg-name))
                (arg-type (if pos
                              `',(intern (substring arg-name (1+ pos)))
                            t)))
           (setq argument arg-type)
           (push 'argument params)
           (when (and (consp arg)
                      (not (eq (cadr arg) 'argument)))
             (push `(,(cadr arg) argument) named-params))))

        ('keep-visual (setq keep-visual t))
        ('do-not-keep-visual (setq keep-visual nil))
        ('repeatable (setq repeatable t))
        ('nonrepeatable (setq repeatable nil))
        
        (t (error "%s: Unexpected argument: %s" 'vim:defcmd arg))))

    `(progn
       (put ',name 'type ',(if motion 'complex 'simple))
       (put ',name 'count ,count)
       (put ',name 'motion ,motion)
       (put ',name 'argument ,argument)
       (put ',name 'register ,register)
       (put ',name 'keep-visual ,keep-visual)
       (put ',name 'repeatable ,repeatable)
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

(font-lock-add-keywords 'emacs-lisp-mode '("vim:defcmd" "vim:defmotion"))

(defun vim:cmd-count-p (cmd)
  "Returns non-nil iff command cmd takes a count."
  (get cmd 'count))

(defun vim:cmd-register-p (cmd)
  "Returns non-nil iff command may take a register."
  (get cmd 'register))

(defun vim:cmd-motion-p (cmd)
  "Returns non-nil iff command `cmd' takes a motion parameter."
  (get cmd 'motion))

(defun vim:cmd-arg (cmd)
  "Returns the type of command's argument."
  (get cmd 'argument))

(defun vim:cmd-arg-p (cmd)
  "Returns non-nil iff command cmd takes an argument of arbitrary type."
  (not (null (get cmd 'argument))))
  
(defun vim:cmd-text-arg-p (cmd)
  "Returns non-nil iff command cmd takes a text argument."
  (eq (vim:cmd-arg cmd) t))
  
(defun vim:cmd-char-arg-p (cmd)
  "Returns non-nil iff command cmd takes a char argument."
  (eq (vim:cmd-arg cmd) 'char))
  
(defun vim:cmd-file-arg-p (cmd)
  "Returns non-nil iff command cmd takes a file argument."
  (eq (vim:cmd-arg cmd) 'file))
  
(defun vim:cmd-buffer-arg-p (cmd)
  "Returns non-nil iff command cmd takes a buffer argument."
  (eq (vim:cmd-arg cmd) 'buffer))
  
(defun vim:cmd-repeatable-p (cmd)
  "Returns non-nil iff command cmd is repeatable."
  (get cmd 'repeatable))

(defun vim:cmd-keep-visual-p (cmd)
  "Returns non-nil iff command cmd should stay in visual mode."
  (get cmd 'keep-visual))
  
(defun vim:cmd-type (cmd)
  "Returns the type of command cmd."
  (get cmd 'type))

(defun vim:cmd-function (cmd)
  "Returns the function of command `cmd'."
  (get cmd 'function))


(defmacro vim:apply-save-buffer (&rest args)
  "Like `apply' but stores the current buffer."
  (let ((ret (gensym)))
  `(progn
     (save-current-buffer
       (let ((,ret (apply ,@args)))
         (setq vim:new-buffer (current-buffer))
         ,ret)))))


(defmacro vim:funcall-save-buffer (&rest args)
  "Like `funcall' but stores the current buffer."
  (let ((ret (gensym)))
  `(progn
     (save-current-buffer
       (let ((,ret (funcall ,@args)))
         (setq vim:new-buffer (current-buffer))
         ,ret)))))

(defun vim:select-register ()
  "Sets the register for the next command."
  (interactive)
  (setq vim:current-register (read-char-exclusive)))

(defun vim:get-register (register)
  "Returns the content of `register', signals error on fail."
  (let ((txt (get-register register)))
    (unless txt
      (error "Register '%c' empty." register))
    txt))


(defun vim:execute-command (cmd)
  "Executes the vim-command `cmd'.
If an error occures, this function switches back to normal-mode.
Since all vim-mode commands go through this function, this is
the perfect point to do some house-keeping."
  (condition-case err
      (funcall vim:active-command-function cmd)
    (error
     (vim:reset-key-state)
     (vim:clear-key-sequence)
     (vim:adjust-point)
     (vim:activate-normal-mode)
     (signal (car err) (cdr err)))))


(defun vim:execute-current-motion ()
  "Executes the current motion and returns the representing
vim:motion object."
  (if (null vim:current-motion)
      nil
    (let ((cmd vim:current-motion)
          (count (if (or vim:current-cmd-count
                         vim:current-motion-count)
                     (* (or vim:current-cmd-count 1)
                        (or vim:current-motion-count 1))
                   nil))
          (parameters nil))

      ;; build the parameter-list
      (when (vim:cmd-char-arg-p cmd)
        (push vim:current-motion-arg parameters)
        (push :argument parameters))
      (when (vim:cmd-count-p cmd)
        (push count parameters)
        (push :count parameters))

      (vim:apply-save-buffer cmd parameters))))


(defun vim:get-current-cmd-motion ()
  "Returns the motion range for the current command w.r.t.
command-specific transformations."
  (let ((motion (save-excursion (vim:execute-current-motion))))
    (when (and (eq (vim:motion-type motion) 'exclusive)
               (save-excursion
                 (goto-char (vim:motion-end-pos motion))
                 (bolp)))

      ;; exclusive motions may be modified
      (let ((end (vim:adjust-end-of-line-position (1- (vim:motion-end-pos motion)))))
        (if (< (vim:motion-begin motion)
               (vim:motion-end motion))
            (setf (vim:motion-end motion) end)
          (setf (vim:motion-begin motion) end)))
      
      (if (save-excursion
            (goto-char (vim:motion-begin-pos motion))
            (vim:looking-back "^\\s-*"))
          ;; motion becomes linewise(-exclusive)
          (setf (vim:motion-type motion) 'linewise)
        
        ;; motion becomes inclusive
        (setf (vim:motion-type motion) 'inclusive)))
    motion))


(defconst vim:emacs-keymap (vim:make-keymap)
  "Keymap for EMACS mode.")

(vim:define-mode emacs "VIM emacs-mode"
                 :ident "E"
                 :message "-- EMACS --"
                 :keymaps '(vim:emacs-keymap)
                 :command-function 'vim:normal-mode-command)

;; from viper
(defsubst vim:ESC-event-p (event)
  (let ((ESC-keys '(?\e (control \[) escape))
        (key (event-basic-type event)))
    (member key ESC-keys)))

;; from viper
(defun vim:escape-to-emacs (events)
  "Executes some `events' in emacs."

  (let* ((vim-key-mode nil)
         (unread-command-events events)
         (keys (read-key-sequence nil))
         (event (elt (listify-key-sequence keys) 0)))

    (when (vim:ESC-event-p event)
      (let ((unread-command-events keys))
        (setq keys (read-key-sequence nil))))

    (let ((command (key-binding keys)))
      (setq this-command command)
      (setq last-command-event (elt keys (1- (length keys))))
      (setq last-command-char last-command-event)
      (command-execute command)
      (when (memq command '(digit-argument
                            universal-argument))
        (vim:escape-to-emacs nil)))))


(provide 'vim-vim)

;;; vim-vim.el ends here
