;;
(eval-when-compile (require 'cl))

;;;;;;;; Packages Import
(require 'coldnew-macro)
(require 'coldnew-functions)
(require 'coldnew-commands)
(require 'coldnew-variables)


(setenv "PATH"
	(concat
	 emacs-bin-dir
	 ":"
	 "~/.lein/bin:"
	 (getenv "PATH")
	 ))

(setq exec-path (cons emacs-bin-dir exec-path))

;; (defun set-exec-path-from-shell-PATH ()
;;   (let ((path-from-shell
;;	 (replace-regexp-in-string "[[:space:]\n]*$" ""
;;				   (shell-command-to-string "$SHELL -l -c 'echo $PATH'"))))
;;     (setenv "PATH" path-from-shell)
;;     (setq exec-path (split-string path-from-shell path-separator))))
;; (when (equal system-type 'darwin) (set-exec-path-from-shell-PATH))
;; (when (equal system-type 'linux-p) (set-exec-path-from-shell-PATH))


(provide 'coldnew-env)
;; coldnew-env.el ends here.
