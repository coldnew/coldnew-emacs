;;; vim-modes.el - Implementation of VIM submodes.

;; Copyright (C) 2009, 2010 Frank Fischer

;; Author: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;;
;; This file is not part of GNU Emacs.

;;; Code:

(eval-when-compile (require 'cl))
(require 'vim-macs)

(defgroup vim-cursors nil
  "Cursor types for modes."
  :group 'vim-mode)

(vim:deflocalvar vim:mode-string)

(vim:deflocalvar vim:active-mode nil
  "The currently active vim-mode.") 

(vim:deflocalvar vim:active-command-function nil
  "The command function of the currently active vim-mode.")

(defvar vim:mode-alist nil
  "Assocative list of all registered submodes, (mode-symbol . mode-text).")

(defvar vim:emulation-mode-alist nil
  "List of all keymaps used by some modes.")


(defun vim:update-mode-line (ident)
  "Updates the mode-line to show the specified identifier `ident'."
  (setq vim:mode-string (concat "<" (or ident "?") ">"))
  (force-mode-line-update))


(defun vim:mode-name (mode)
  "Converts a mode-name to vim-mode naming conventions, e.g.
'normal is converted to 'vim:normal-mode."
  (intern (concat "vim:" (symbol-name mode) "-mode")))


(defun vim:activate-mode (mode)
  "Activates a certain vim-mode, disabling the currently active one."
  (when vim:active-mode
    (funcall vim:active-mode -1))
  (when mode
    (funcall (vim:mode-name mode) 1)))


;; This function sets up the keymaps for the current mode.
(defmacro vim:set-keymaps (mode-name keymaps)
  (when (eq (car-safe mode-name) 'quote)
    (setq mode-name (cadr mode-name)))
  (when (eq (car-safe keymaps) 'quote)
    (setq keymaps (cadr keymaps)))
  `(setq vim:emulation-mode-alist
         (list
	  ,@(apply #'append '((cons 'vim:intercept-ESC-mode vim:intercept-ESC-keymap))
		    (mapcar #'(lambda (keym)
				(let ((localname (intern (replace-regexp-in-string
							  "mode-keymap" "mode-local-keymap"
							  (symbol-name keym)))))
				  (if (eq localname keym)
				      (list `(cons ',mode-name ,keym))
				    (list `(cons ',mode-name ,localname)
					  `(cons ',mode-name ,keym)))))
			    keymaps)))))


(defmacro* vim:define-mode (name doc
                                 &rest body
                                 &key
                                 ident
                                 message
                                 command-function
                                 (cursor ''box)
                                 keymaps)
  "Defines a new VIM-mode with certain `name', mode-line-identifier `ident',
activation `message', a `command-function' to be called when a
vim-command should be executed, a `cursor' shape and a list of `keymaps'."
  (let* ((mode-name (vim:mode-name name))
         (pred-name (intern (concat (symbol-name mode-name) "-p")))
         (on-name (intern (concat "vim:activate-" (symbol-name name) "-mode")))
         (cursor-name (intern (concat (symbol-name mode-name)
                                      "-cursor"))))
    `(progn
       (defcustom ,cursor-name ,cursor
         ,(concat "The cursor-type for vim-mode " (symbol-name name) ".")
         :group 'vim-cursors)
       
       (push (cons ',mode-name ,(symbol-name name)) vim:mode-alist)
       (define-minor-mode ,mode-name ,doc nil nil nil
         (when ,mode-name
           ,@(when ident `((vim:update-mode-line ,ident)))
           ,@(when message `((let (message-log-max) (message ,message))))
           (setq vim:active-mode ',mode-name)
           (setq vim:active-command-function
                 ,(if command-function
                      command-function
                    'vim:default-command-function))
           (vim:set-cursor ,cursor-name)
           (vim:set-keymaps ',mode-name ,keymaps))
         ,@(progn
             (while (keywordp (car body)) (pop body) (pop body))
             body))

       (defun ,pred-name ()
         ,(concat "Returns t iff vim-mode is in " (symbol-name name) " mode.")
         (and ,mode-name t))
       
       (defun ,on-name ()
         ,(concat "Activates " (symbol-name name) " mode.")
         (interactive)
         (vim:activate-mode ',name)))))

(font-lock-add-keywords 'emacs-lisp-mode '("vim:define-mode"))

(provide 'vim-modes)

;;; vim-modes.el ends here

