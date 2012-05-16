;;; coldnew-cursor.el --- cursor-setting
(eval-when-compile (require 'cl))

(defvar emacs-normal-cursor-color "white")
(defvar emacs-normal-cursor-type 'bar)

(defvar emacs-read-only-cursor-color "gray")
(defvar emacs-read-only-cursor-type 'box)

(defvar emacs-overwrite-cursor-color "yellow")
(defvar emacs-overwrite-cursor-type 'hbar)

;;;; ---------------------------------------------------------------------------
;;;; Hooks
;;;; ---------------------------------------------------------------------------
(add-hook 'post-command-hook 'coldnew/set-cursor-according-mode)


;;;; ---------------------------------------------------------------------------
;;;; Functions
;;;; ---------------------------------------------------------------------------
(defun coldnew/set-cursor-according-mode ()
  "change cursor shap and color according mode"
  (cond
   ((string= "Command" coldnew-editor-state)
    (setq cursor-type emacs-read-only-cursor-type)
    (setq cursor-color emacs-read-only-cursor-color))
   (buffer-read-only
    (setq cursor-type emacs-read-only-cursor-type)
    (setq cursor-color emacs-read-only-cursor-color))
   ;; (overwrite-mode
   ;;   (set-cursor-color djcb-overwrite-color)
   ;;   (setq cursor-type djcb-overwrite-cursor-type))
   (t
    (setq cursor-type emacs-normal-cursor-type)
    (setq cursor-color emacs-normal-cursor-color))))







(provide 'coldnew-cursor)
;; coldnew-cursor.el ends here.
