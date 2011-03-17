;;
(eval-when-compile (require 'cl))

;;;;;;;; Packages Import
(require 'coldnew-editor)

;;;;;;;; Ruby-mode extensions
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))

;;;;;;;; Auto Complete Settings
(when (require* 'auto-complete)
  (defun ac-emacs-ruby-mode-setup ()
    "auto-complete settings for ruby-mode"
    (setq ac-sources '(ac-source-symbols ac-source-words-in-same-mode-buffers))))


;;;;;;;; Hooks
(add-hook 'ruby-mode-hook
	  '(lambda ()
	     ;; Use global programming mode
	     (programming-mode)
	     ))



(provide 'coldnew-lang-ruby)
;; coldnew-lang-ruby.el ends here.
