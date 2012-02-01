;;
(eval-when-compile (require 'cl))

;;;;;;;; Packages Import
(require 'coldnew-macro)
(require 'coldnew-functions)
(require 'coldnew-commands)
(require 'coldnew-variables)

(setq evil-default-cursor '("white" box))

(when (require* 'evil)
  ;; Auto strat evil-mode
  (evil-mode 1)

  ;; Modes that should come up in Emacs state.
  (setq evil-emacs-state-modes
	'(comint-mode
	  term-mode
	  slime-repl-mode))

  (setq evil-insert-state-modes nil)

  (defmacro evil-define-key-insert (state map key name)
    "insert string in evil-mode."
    `(evil-define-key ,state ,map ,key
		      '(lambda ()
			 (interactive) (insert ,name) (if (featurep 'yasnippet) (yas/expand))))
    )

  )


(provide 'coldnew-evil)
;; coldnew-evil.el ends here.
