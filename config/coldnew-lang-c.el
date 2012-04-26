;;; coldnew-lang-c.el ---
(eval-when-compile (require 'cl))

;;;; c-mode extensions
(add-to-list 'auto-mode-alist '("\\.c$" . c-mode))
(add-to-list 'auto-mode-alist '("\\.h$" . c-mode))

;;;; ---------------------------------------------------------------------------
;;;; Hooks
;;;; ---------------------------------------------------------------------------


;;;; ---------------------------------------------------------------------------
;;;; Keybindings
;;;; ---------------------------------------------------------------------------


;;;; ---------------------------------------------------------------------------
;;;; Auto Complete
;;;; ---------------------------------------------------------------------------

;;;; ---------------------------------------------------------------------------
;;;; Flymake
;;;; ---------------------------------------------------------------------------

;;;; ---------------------------------------------------------------------------
;;;; Functions
;;;; ---------------------------------------------------------------------------


;;;; ---------------------------------------------------------------------------
;;;; Commands
;;;; ---------------------------------------------------------------------------

(defun c-mode:insert-inc-or-if ()
  "If at the start of line. add `inc' and expand it,
else add `if' and expand it."
  (interactive)
  (let* ((current (point))
	 (begin (line-beginning-position)))
    (if (eq current begin)
	(progn
	  (c-mode:insert-include)
	  (newline-and-indent))
      (progn
	(insert "if")
	(yas/expand)))))

(defun c-mode:insert-main-function ()
  "insert main()."
  (interactive)
  (let* ((current (point))
	 (begin (line-beginning-position)))
    (if (equal current begin)
	(insert "main"))
    (yas/expand)))


(provide 'coldnew-lang-c)
;; coldnew-lang-c.el ends here.
