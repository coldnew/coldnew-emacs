;;; coldnew-lang-bash.el ---
(eval-when-compile (require 'cl))

;;;; sh-mode extensions
(add-to-list 'auto-mode-alist '("\\.sh" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.bash" . sh-mode))

;;;; ---------------------------------------------------------------------------
;;;; Hooks
;;;; ---------------------------------------------------------------------------

;; use my editor-mode
(add-hook 'shell-mode-hook '(lambda() (coldnew-editor-mode 1)))

;; auto-complete
(add-hook 'shell-mode-hook 'ac-shell-script-mode-setup)

;; bash-completion
(require 'bash-completion)
(bash-completion-setup)

;;;; ---------------------------------------------------------------------------
;;;; Keybindings
;;;; ---------------------------------------------------------------------------


;;;; ---------------------------------------------------------------------------
;;;; Auto Complete
;;;; ---------------------------------------------------------------------------
(defun ac-shell-script-mode-setup ()
  "auto-complete settings for shell-script-mode"
  (setq ac-sources
	'(ac-source-dictionary
	  ac-source-filename
	  ac-source-files-in-current-dir
	  ac-source-words-in-same-mode-buffers
	  )))

;;;; ---------------------------------------------------------------------------
;;;; Flymake
;;;; ---------------------------------------------------------------------------

;;;; ---------------------------------------------------------------------------
;;;; Functions
;;;; ---------------------------------------------------------------------------


;;;; ---------------------------------------------------------------------------
;;;; Commands
;;;; ---------------------------------------------------------------------------


(provide 'coldnew-lang-bash)
;; coldnew-lang-bash.el ends here.
