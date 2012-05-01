;;; coldnew-tramp.el ---
(eval-when-compile (require 'cl))


(require 'tramp)

(setq tramp-default-method "ssh")

(add-to-list 'tramp-default-method-alist
	     '("\\`localhost\\'" "\\`root\\'" "su"))

(add-to-list 'tramp-default-method-alist '("" "root" "ssh"))


(tramp-set-completion-function "ssh"
			       '((tramp-parse-sconfig "/etc/ssh_config")
				 (tramp-parse-sconfig "~/.ssh/config")))


(provide 'coldnew-tramp)
;; coldnew-tramp.el ends here.
