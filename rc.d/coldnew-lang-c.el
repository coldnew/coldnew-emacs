;;
(eval-when-compile (require 'cl))


;;;;;;;; Packages Import
(require 'coldnew-editor)
(require 'cc-mode)

;;;;;;;; c-mode extensions
(add-to-list 'auto-mode-alist '("\\.h$" . c-mode))
(add-to-list 'auto-mode-alist '("\\.c$" . c-mode))


;;;;;;;; Auto Complete Settings
(when (require* 'auto-complete)
  (defun ac-c-mode-setup ()
    "auto-complete settings for c-mode."
    (setq ac-sources '(ac-source-dictionary
		       ac-source-symbols
		       ac-source-variables
		       ac-source-functions
		       ac-source-features
		       ac-source-filename
		       ac-source-words-in-buffer
		       ac-source-company-clang
		       ac-source-words-in-same-mode-buffers
		       ))))


;;;;;;;; Coding-Style Setting
(add-hook 'c-mode-hook
	  '(lambda ()

	     ;; Use linux-kernel style
	     (c-set-style "linux")

	     ;; Setting indentation lvel
	     (setq c-basic-offset 8)

	     ;; Make TAB equivilent to 8 spaces
	     (setq tab-width 8)

	     ;; Use spaces to indent instead of tabs.
	     (setq indent-tabs-mode nil)


	     ;; TODO:test this function
	     ;; Handle longname argument in functions
	     (c-set-offset 'arglist-intro '+)

	     ))

;;;;;;;; Hooks
(add-hook 'c-mode-hook
	  '(lambda ()

	     ;; Enable Auto Complete
	     (when (require* 'auto-complete)
	       (ac-c-mode-setup))

	     ;; Enable c-eldoc
	     (setq c-eldoc-includes "`pkg-config gtk+-2.0 --cflags` -I./ -I../")
	     (when (require* 'c-eldoc)
	       (c-turn-on-eldoc-mode))

	     ;; Automatically determine c-basic-offset
	     (when (require* 'guess-offset))

	     ;; Use global programming mode
	     (programming-mode)


	     ))

;;;;;;;; make cedet integrated with c
(when (require 'cedet)
  (semantic-add-system-include "/usr/include" 'c-mode)
  )


(provide 'coldnew-lang-c)
;; coldnew-lang-c.el ends here.
