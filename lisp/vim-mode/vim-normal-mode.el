;;; vim-insert-mode.el - VIM normal mode.

;; Copyright (C) 2009, 2010 Frank Fischer

;; Author: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;;
;; This file is not part of GNU Emacs.


;;; TODO:
;; - bindings in local-omap keymap will not be seen as motions in
;;   normal-mode since the parent-keymap of the normal-mode keymap
;;   is operator-pending-keymap and not its local counterpart. The
;;   reason is that the binding of the local counterpart will be
;;   changed to a buffer-local binding.

;;; Code:

(defconst vim:operator-repeat-keymap (vim:make-keymap)
  "Keymap to bind the repeat-operator-event.")

(vim:define-keymap operator-pending-mode
                   "operator pending mode"
                   :map-command omap)

(vim:define-mode operator-pending "VIM operator-pending mode"
                 :ident "O"
                 :keymaps '(vim:operator-pending-mode-keymap
                            vim:motion-mode-keymap
                            vim:operator-repeat-keymap
                            vim:override-keymap)
                 :command-function 'vim:operator-pending-mode-command)

(add-hook 'vim:operator-pending-mode-hook 'vim:operator-pending-activate)
(add-hook 'vim:operator-pending-mode-off-hook 'vim:operator-pending-deactivate)

(defun vim:operator-pending-activate ()
  (cond
   (vim:operator-pending-mode
    (setq vim:operator-repeat-last-event (vector last-command-event))
    (vim:map vim:operator-repeat-last-event 'vim:motion-lines
	     :keymap vim:operator-repeat-keymap)
    (add-hook 'post-command-hook 'vim:operator-pending-mode-exit))

   (vim:operator-repeat-last-event
    (vim:map vim:operator-repeat-last-event nil :keymap vim:operator-repeat-keymap))))


(defun vim:operator-pending-deactivate ()
  (remove-hook 'post-command-hook 'vim:operator-pending-mode-exit))

(defun vim:operator-pending-mode-exit ()
  "Exits operator-pending-mode and returns to normal-mode."
  (interactive)
  (unless (or (vim:cmd-function this-command)
              (eq this-command 'digit-argument)
              (eq this-command 'universal-argument-other-key))
    (vim:activate-normal-mode)))


(defun vim:operator-pending-mode-command (command)
  "Executes a complex command in operator-pending mode."
  (case (vim:cmd-type command)
    ('simple (error "No simple-commands allowed in operator-pending mode."))
    ('complex (error "No complex-commands allowed in operator-pending mode."))
    (t (vim:normal-execute-complex-command command)))
    
  (when (vim:operator-pending-mode-p)
      (vim:activate-normal-mode)))


(vim:define-keymap normal-mode "normal mode" &map-command nmap)

(vim:define-mode normal "VIM normal mode"
                 :ident "N"
                 :message "-- NORMAL --"
                 :keymaps '(vim:normal-mode-keymap
                            vim:operator-pending-mode-keymap
                            vim:motion-mode-keymap
                            vim:window-mode-keymap
                            vim:override-keymap)
                 :command-function 'vim:normal-mode-command)

(defun vim:normal-mode-command (command)
  "Executes a motion or simple-command or prepares a complex command."
  (case (vim:cmd-type command)
    ('simple (vim:normal-execute-simple-command command))
    ('complex (vim:normal-prepare-complex-command command))
    ('special (error "no special so far"))
    (t (vim:normal-execute-motion command))))


(defun vim:normal-execute-motion (command)
  "Executes a motion."
  (setq vim:current-motion command)

  (when current-prefix-arg
    (setq vim:current-motion-count (prefix-numeric-value current-prefix-arg)))

  (when (vim:cmd-char-arg-p command)
    (setq vim:current-motion-arg (read-char-exclusive)))

  (vim:execute-current-motion)
    
  (vim:reset-key-state)
  (vim:clear-key-sequence)
  (vim:adjust-point))


(defun vim:normal-execute-simple-command (command)
  "Executes a simple command."
  (when current-prefix-arg
    (setq vim:current-cmd-count (prefix-numeric-value current-prefix-arg)))
  
  (when (vim:cmd-char-arg-p command)
    (setq vim:current-cmd-arg (read-char-exclusive)))

  (let ((parameters nil)
        (vim:last-undo buffer-undo-list))
    (when (vim:cmd-count-p command)
      (push vim:current-cmd-count parameters)
      (push :count parameters))
    (when (vim:cmd-char-arg-p command)
      (push vim:current-cmd-arg parameters)
      (push :argument parameters))
    (when (and (vim:cmd-register-p command)
               vim:current-register)
      (push vim:current-register parameters)
      (push :register parameters))
    (vim:apply-save-buffer (vim:cmd-function command) parameters)
    (when (vim:cmd-repeatable-p command)
      (setq vim:repeat-events (vconcat vim:current-key-sequence
                                       (vim:this-command-keys))))
    (vim:connect-undos vim:last-undo))

  (vim:reset-key-state)
  (vim:clear-key-sequence)
  (vim:adjust-point))
    

(defun vim:normal-prepare-complex-command (command)
  "Prepares a complex command, switching to operator-pending mode."
  (when current-prefix-arg
    (setq vim:current-cmd-count (prefix-numeric-value current-prefix-arg)))
  
  (setq vim:current-cmd command)
  (setq vim:current-key-sequence (vconcat vim:current-key-sequence (vim:this-command-keys)))
  (vim:activate-operator-pending-mode))

(defun vim:normal-execute-complex-command (motion-command)
  "Executes a complex command with a certain motion command."
  (setq vim:current-motion motion-command)

  (when current-prefix-arg
    (setq vim:current-motion-count (prefix-numeric-value current-prefix-arg)))

  (when (or vim:current-motion-count vim:current-cmd-count)
    (setq vim:current-motion-count (* (or vim:current-cmd-count 1)
                                      (or vim:current-motion-count 1)))
    (setq vim:current-cmd-count nil))

  (when (vim:cmd-char-arg-p motion-command)
    (setq vim:current-motion-arg (read-char-exclusive)))

  (let ((vim:last-undo buffer-undo-list))
    (if (and (vim:cmd-register-p vim:current-cmd) vim:current-register)
        (vim:funcall-save-buffer (vim:cmd-function vim:current-cmd)
                                 :motion (vim:get-current-cmd-motion)
                                 :register vim:current-register)
      (vim:funcall-save-buffer (vim:cmd-function vim:current-cmd)
                               :motion (vim:get-current-cmd-motion)))
    (when (vim:cmd-repeatable-p vim:current-cmd)
      (setq vim:repeat-events (vconcat vim:current-key-sequence
                                       (vim:this-command-keys))))
    (vim:connect-undos vim:last-undo))
    
  (vim:reset-key-state)
  (vim:clear-key-sequence)
  (vim:adjust-point))


(vim:define-mode operator-pending "VIM operator-pending mode"
                 :ident "O"
                 :keymaps '(vim:operator-pending-mode-keymap
                            vim:motion-mode-keymap
                            vim:operator-repeat-keymap
                            vim:override-keymap)
                 :command-function 'vim:operator-pending-mode-command)

(provide 'vim-normal-mode)

;;; vim-normal-mode.el ends here
