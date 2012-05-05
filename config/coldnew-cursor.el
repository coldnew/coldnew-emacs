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
;;(add-hook 'post-command-hook 'coldnew/set-cursor-according-mode)

;;;; ---------------------------------------------------------------------------
;;;; cursor-chg
;;;; ---------------------------------------------------------------------------

(require* 'cursor-chg)
;; Turn on cursor change when Emacs is idle
(toggle-cursor-type-when-idle 1)
;; Turn on change for overwrite, read-only, and input mode
;;(change-cursor-mode 1)
(setq curchg-default-cursor-color emacs-normal-cursor-color)

;;;; ---------------------------------------------------------------------------
;;;; Functions
;;;; ---------------------------------------------------------------------------


(provide 'coldnew-cursor)
;; coldnew-cursor.el ends here.
