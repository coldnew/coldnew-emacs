
(eval-when-compile (require 'cl))

(add-to-list 'auto-mode-alist '("\\.yasnippet\\'" . snippet-mode))
(add-to-list 'auto-mode-alist '("\\.yas\\'" . snippet-mode))

(yas/initialize)
(yas/load-directory "~/.emacs.d/etc/snippets")
(setq yas/prompt-functions '(yas/dropdown-prompt
			     yas/ido-prompt
			     yas/completing-prompt))

;; Auto add TEMPLATE in new file
(add-hook 'find-file-hook
	  '(lambda ()
	     (when (and (buffer-file-name)
			(not (file-exists-p (buffer-file-name)))
			(= (point-max) 1))
	       (insert "HEADER")
	       (yas/expand))))

;;;;;; Functions
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


(provide '025-yasnippet)
;; 025-yasnippet.el ends here.
