
;;

(eval-when-compile (require 'cl))

;;;;;;;; Packages Import
(require 'coldnew-macro)
(require 'coldnew-functions)
(require 'coldnew-commands)
(require 'coldnew-variables)

(defvar emacs-snippet-file-dir "~/.emacs.d/etc/snippets"
  "")


;;;;;;;; yasnippet
(when (require* 'yasnippet)
  (yas/initialize)
  (yas/load-directory emacs-snippet-file-dir)
  (setq yas/prompt-functions
	'(yas/dropdown-prompt
	  yas/ido-prompt
	  yas/completing-prompt))

  ;; TODO: After finish, move this two line to lang-snippet
  (add-to-list 'auto-mode-alist '("\\.yasnippet\\'" . snippet-mode))
  (add-to-list 'auto-mode-alist '("\\.yas\\'" . snippet-mode))

  ;; Auto add HEADER in new file
  (add-hook 'find-file-hook
	    '(lambda ()
	       (when (and (buffer-file-name)
			  (not (file-exists-p (buffer-file-name)))
			  (= (point-max) 1))
		 (let ((header-snippet "HEADER"))
		   (insert header-snippet)
		   ;; if can't expand snippet, delete insert string
		   (if (not (yas/expand))
		       (backward-delete-char (1+ (length header-snippet)))))))))


;;;; Functions
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
