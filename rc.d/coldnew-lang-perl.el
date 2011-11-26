;;
(eval-when-compile (require 'cl))

;;;;;;; Packages Import
(require 'coldnew-editor)
(require 'cc-mode)
(require 'cperl-mode)


;;;;;;;; perl-mode extensions
(add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))


(add-hook 'cperl-mode-hook
	  '(lambda ()
	     (setq cperl-indent-level 8)
	     (setq cperl-continued-statement-offset 0)
	     (setq cperl-extra-newline-before-brace t)

	     ;; Use global programming mode
	     (programming-mode)
	     ))



(provide 'coldnew-lang-perl)
;; coldnew-lang-perl.el ends here.
