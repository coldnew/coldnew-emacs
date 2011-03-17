;;; vim-insert-mode.el - VIM insert-mode.

;; Copyright (C) 2009, 2010, 2011 Frank Fischer

;; Author: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;;
;; This file is not part of GNU Emacs.

;;; TODO :
;; - for some reason GNU Emacs does not show '-- REPLACE --'

;;; Code:

(eval-when-compile (require 'cl))
(require 'vim-macs)
(require 'vim-core)
(require 'vim-undo)
(require 'vim-normal-mode)
(require 'vim-motions)

(vim:deflocalvar vim:last-insert-undo nil)

(vim:deflocalvar vim:insert-count nil
  "The number of times the insertion should be repeated.")

(vim:deflocalvar vim:insert-marker nil
  "A marker which is placed at the point where insertion started.")

(defcustom vim:insert-mode-replace-cursor 'hbar
  "Cursor for replace-mode."
  :group 'vim-cursors)

(vim:define-keymap insert-mode "insert-mode" :map-command imap)

(vim:define-mode insert "VIM insert-mode"
                 :ident "I"
                 :message "-- INSERT --"
                 :keymaps '(vim:insert-mode-keymap)
                 :command-function 'vim:insert-mode-command
                 :cursor 'bar)
(add-hook 'vim:insert-mode-on-hook 'vim:insert-mode-activated)
(add-hook 'vim:insert-mode-off-hook 'vim:insert-mode-deactivated)

(vim:defcmd vim:insert-mode-toggle-replace ()
  "Toggles overwrite-mode in insert-mode."
  (unless (vim:insert-mode-p)
    (error "Toggling overwrite-mode only allowed in insert-mode."))
  (overwrite-mode nil)
  (if overwrite-mode
      (progn
        (let (message-log-max) (message "-- REPLACE --"))
        (setq cursor-type vim:insert-mode-replace-cursor)
        (vim:update-mode-line "R"))
    (progn
      (let (message-log-max) (message "-- INSERT --"))
      (setq cursor-type vim:insert-mode-cursor)
      (vim:update-mode-line "I"))))

(defun vim:insert-mode-insert-newline ()
  "Inserts a newline according to current insert-mode direction."
  (case vim:insert-newline
    (above
     (vim:motion-beginning-of-line)
     (newline)
     (forward-line -1)
     (indent-according-to-mode))
    (below
     (vim:motion-end-of-line)
     (newline)
     (indent-according-to-mode))))

(defun vim:start-insert-mode (&optional count newline)
  "Activates insert-mode with a certain repeat `count'.
`newline' should be 'above or 'below or nil which determines
where to insert a newline."
  (setq vim:insert-count count
	vim:insert-newline newline)
  (vim:insert-mode-insert-newline)
  (when (eq vim:insert-newline 'above)
    (setq vim:insert-newline 'below))
  (if (eobp)
      (setq vim:insert-marker 'eob)
    (setq vim:insert-marker (make-marker))
    (move-marker vim:insert-marker (1+ (point))))
  (vim:activate-insert-mode))

(defun vim:insert-mode-command (command)
  "Executes a simple command in insert mode."
  (case (vim:cmd-type command)
    ('simple (vim:normal-execute-simple-command command))
    ('complex (error "No complex command allowed in insert-mode."))
    (t (vim:normal-execute-motion command))))

(defun vim:insert-mode-activated ()
  "Called when insert-mode is activated."
  (overwrite-mode -1)
  (setq vim:last-insert-undo vim:last-undo)
  (add-hook 'pre-command-hook 'vim:insert-save-key-sequence))
  
(defun vim:insert-mode-deactivated ()
  "Called when insert-mode is deactivated."
  (overwrite-mode -1)
  (vim:set-mark ?^)
  (remove-hook 'pre-command-hook 'vim:insert-save-key-sequence)
  ;; the command that has just ended insert-mode should NOT be repeatable
  ;; and will therefore NOT override repeat-sequence.
  (setq vim:repeat-events (vconcat vim:repeat-events
                                   vim:current-key-sequence))
  (setq vim:last-undo vim:last-insert-undo)

  ;; repeat insertion
  (dotimes (i (1- (or vim:insert-count 1)))
    (goto-char (if (eq vim:insert-marker 'eob)
		   (point-max)
		 (1- vim:insert-marker)))
    (vim:insert-mode-insert-newline)
    (execute-kbd-macro vim:current-key-sequence))
  (when (and vim:insert-marker
	     (not (eq vim:insert-marker 'eob)))
    (move-marker vim:insert-marker nil))
  (setq vim:insert-marker nil
	vim:insert-count nil))


(defun vim:insert-save-key-sequence ()
  "Called in insert-mode to save key-events."
  (when (and (not (eq this-command 'vim:intercept-ESC))
	     (functionp this-command))
    (setq vim:current-key-sequence (vconcat vim:current-key-sequence
                                            (vim:this-command-keys)))))

(provide 'vim-insert-mode)

;;; vim-insert-mode.el ends here
