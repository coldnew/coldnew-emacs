;;; coldnew-lang-c.el ---
(eval-when-compile (require 'cl))

;;;; c-mode extensions
(add-to-list 'auto-mode-alist '("\\.c$" . c-mode))
(add-to-list 'auto-mode-alist '("\\.h$" . c-mode))

;;;; ---------------------------------------------------------------------------
;;;; Hooks
;;;; ---------------------------------------------------------------------------

;; use my cc-mode-common-setting
(add-hook 'c++-mode-hook 'coldnew-cc-mode-common-setting)

;; Enable c-eldoc
(require 'c-eldoc)
(setq c-eldoc-includes "`pkg-config gtk+-3.0 opencv --cflags --libs` -I./ -I../")
(c-turn-on-eldoc-mode)

;; use ctypes
(require 'ctypes)
(setq-default ctypes-file-name (concat emacs-cache-dir "ctypes_std_c.dat"))
(add-hook 'ctypes-load-hook 'my-ctypes-load-hook)

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
(defun my-ctypes-load-hook ()
  (ctypes-read-file ctypes-file-name nil t t))


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
