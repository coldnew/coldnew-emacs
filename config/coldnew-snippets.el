;;; coldnew-snippets.el --- default snippets setting
(eval-when-compile (require 'cl))

;;;; ---------------------------------------------------------------------------
;;;; yasnippet
;;;; ---------------------------------------------------------------------------
(setq-default yas/snippet-dirs emacs-snippets-dir)
(require 'yasnippet)
(require 'dropdown-list)
;; initial yasnippet
(yas/initialize)
(yas/load-directory emacs-snippets-dir)

(setq yas/prompt-functions '(yas/dropdown-prompt yas/ido-prompt yas/completing-prompt))

;;;;;;;; Functions
(defun yas/dir ()
  (file-name-directory (buffer-file-name)))
(defun yas/file ()
  (file-name-nondirectory (buffer-file-name)))
(defun yas/file-sans ()
  (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))
(defun yas/file-ext ()
  (file-name-extension (file-name-nondirectory (buffer-file-name))))
(defun yas/file-sans-upcase ()
  (upcase (yas/file-sans)))
(defun yas/year ()
  (format-time-string "%Y"))
(defun yas/user-name ()
  (insert user-full-name))
(defun yas/login-name ()
  (insert user-login-name))
(defun yas/user-email ()
  (insert user-mail-address))
(defun yas/user-nickname ()
  (insert user-nickname))



(provide 'coldnew-snippets)
;; coldnew-snippets.el ends here.
