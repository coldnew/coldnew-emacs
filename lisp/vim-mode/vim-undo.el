;;; vim-undo.el - Undo/Redo for VIM.

;; Copyright (C) 2009, 2010 Frank Fischer

;; Author: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;;
;; This file is not part of GNU Emacs.

;;; Commentary:

;; Before the execution of an editing command, the calling function
;; should save the current head of buffer-undo-list.  When the
;; editing-command has finished, the calling function should use
;; vim:connect-undos to connect the changes made during editing to one
;; single undo-block.
;;
;; Insert-mode has a special handling: when activated, it stores the
;; current head of buffer-undo-list in vim:last-insert-undo and used
;; this pointer to connect all editing actions during insert-mode to
;; one undo-block when insert-mode is deactivated.  If a function
;; activates insert-mode it may modify vim:last-insert-undo to an
;; apropriate value (see vim:execute-mapping for an example).

;;; Code:

(eval-when-compile (require 'cl))
(require 'vim-macs)
(require 'vim-defs)
(require 'vim-compat)
(require 'vim-modes)

;; try loading redo+, then redo
(or (condition-case nil (require 'redo+ nil t) (error nil))
    (condition-case nil (require 'redo nil t) (error nil)
    (message "vim-mode: Could not load 'redo+' or 'redo', redo-command not available.")))

(defvar vim:last-undo nil
  "The last item in the undo list.")

;; undo stuff
(defun vim:connect-undos (last-undo)
  (labels
      ((find-mark (lst)
                  (while (not (or (null lst)
                                  (eq lst last-undo)))
                    (setq lst (cdr lst)))
                  (not (null lst))))
                   
    ;; ensure last-undo is still in the undo list
    (when (and last-undo
               (not (eq last-undo buffer-undo-list))
               (find-mark buffer-undo-list))
      
      ;; add the end-of-command mark if not already there
      (unless (null (car buffer-undo-list))
        (push nil buffer-undo-list))

      ;; remove all nils until the mark
      (let ((lst buffer-undo-list))
        (while (and lst
                    (not (eq (cdr lst) last-undo)))
          (if (null (cadr lst))
              (setcdr lst (cddr lst))
            (setq lst (cdr lst))))))))


(vim:defcmd vim:cmd-undo (count nonrepeatable)
  (setq vim:last-undo nil)
  (dotimes (i (or count 1))
    (undo)))
    
(vim:defcmd vim:cmd-redo (count nonrepeatable)
  (setq vim:last-undo nil)
  (redo (or count 1)))

(provide 'vim-undo)

;;; vim-undo.el ends here
