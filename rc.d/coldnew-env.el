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
	 ";"
	 (getenv "PATH")
	 ))

(setq exec-path (cons emacs-bin-dir exec-path))




(provide 'coldnew-env)
;; coldnew-env.el ends here.
