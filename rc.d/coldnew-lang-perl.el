;;
(eval-when-compile (require 'cl))

;;;;;;; Packages Import
(require 'coldnew-editor)

(require 'cperl-mode)
(require 'perl-completion)


;;;;;;;; perl-mode extensions
(add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))

;;;;;;;; Coding Style
(add-hook 'cperl-mode-hook
	  '(lambda ()
	     (setq cperl-indent-level 8)
	     (setq cperl-brace-offset 0)
	     ))


;;;;;;;; Flymake
(defvar flymake-perl-err-line-patterns '(("\\(.*\\) at \\([^ \n]+\\) line \\([0-9]+\\)[,.\n]" 2 3 nil 1)))

(defun flymake-perl-init ()
  (flymake-generic-init "perl" '("-wc")))

(add-hook 'cperl-mode-hook
	  '(lambda () (setq flymake-err-line-patterns flymake-perl-err-line-patterns)))

(add-to-list 'flymake-allowed-file-name-masks
	     '(".+\\.pl$"
	       flymake-perl-init
	       flymake-simple-cleanup
	       flymake-get-real-file-name))


;;;;;;;; Keybindings
(evil-define-key 'insert cperl-mode-map (kbd "M-a") 'plcmp-cmd-complete-arrays)
(evil-define-key 'insert cperl-mode-map (kbd "M-A") 'plcmp-cmd-complete-all)
(evil-define-key 'insert cperl-mode-map (kbd "M-v") 'plcmp-cmd-complete-variables)
(evil-define-key 'insert cperl-mode-map (kbd "M-h") 'plcmp-cmd-complete-hashes)
(evil-define-key 'insert cperl-mode-map (kbd "M-f") 'plcmp-cmd-complete-functions)
(evil-define-key 'insert cperl-mode-map (kbd "M-m") 'plcmp-cmd-complete-methods)
(evil-define-key 'insert cperl-mode-map (kbd "M-i") 'perl-mode:insert-modules)

;;;;;;;; Hooks
(add-hook 'cperl-mode-hook
	  '(lambda ()
	     (setq cperl-indent-level 8)
	     (setq cperl-continued-statement-offset 0)
	     (setq cperl-extra-newline-before-brace t)

	     ;; Use global programming mode
	     (programming-mode)

	     ;; Enable perl-completion-mode
	     (when (featurep 'perl-completion)
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
