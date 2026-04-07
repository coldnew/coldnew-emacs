;;; coldnew-refactor.el --- Refactor helpers for built-in first config -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Yen-Chin Lee

;;; Commentary:

;; Small helpers used while migrating init.el toward a modular,
;; built-in-first layout inspired by ref/emacs-solo.

;;; Code:

(defgroup coldnew-refactor nil
  "Refactor options for coldnew init."
  :group 'convenience)

(defcustom coldnew-refactor-enable-modules t
  "When non-nil, load optional files from `user-lisp-directory'."
  :type 'boolean
  :group 'coldnew-refactor)

(defun coldnew/refactor-load (feature)
  "Require FEATURE if module loading is enabled.
FEATURE should be a symbol provided by a file in `user-lisp-directory'."
  (when coldnew-refactor-enable-modules
    (require feature nil t)))

(provide 'coldnew-refactor)

;;; coldnew-refactor.el ends here
