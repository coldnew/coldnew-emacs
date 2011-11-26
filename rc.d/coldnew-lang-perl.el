;;
(eval-when-compile (require 'cl))

;;;;;;; Packages Import
(require 'coldnew-editor)
(require 'cc-mode)
(require* 'cperl-mode)



;;;;;;;; perl-mode extensions
(add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))

;;;;;;;; Keybindings
(add-hook 'cperl-mode-hook
	  '(lambda ()
	     (when (require 'perl-completion)
	       ;;
	       (vim:local-imap (kbd "M-a") 'plcmp-cmd-complete-arrays)
	       (vim:local-imap (kbd "M-A") 'plcmp-cmd-complete-all)
	       (vim:local-imap (kbd "M-v") 'plcmp-cmd-complete-variables)
	       (vim:local-imap (kbd "M-h") 'plcmp-cmd-complete-hashes)
	       (vim:local-imap (kbd "M-f") 'plcmp-cmd-complete-functions)
	       (vim:local-imap (kbd "M-m") 'plcmp-cmd-complete-methods)
	       (vim:local-imap (kbd "M-i") 'perl-mode:insert-modules)

	       )
	     ))

;;;;;;;; Hooks
(add-hook 'cperl-mode-hook
	  '(lambda ()
	     (setq cperl-indent-level 8)
	     (setq cperl-continued-statement-offset 0)
	     (setq cperl-extra-newline-before-brace t)

	     ;; Use global programming mode
	     (programming-mode)

	     (when (require* 'perl-completion)
	       (perl-completion-mode t))
	     ))

;;;;;;;; Functions
(defun perl-mode:insert-modules ()
  "Call perl-completion to complete perl-modules when use it."
  (interactive)
  (insert "use ")
  (when (featurep 'perl-completion)
    (plcmp-cmd-complete-modules)
    (insert ";")
    (newline)))


(provide 'coldnew-lang-perl)
;; coldnew-lang-perl.el ends here.
