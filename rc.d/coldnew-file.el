;;
(eval-when-compile (require 'cl))

;;;;;;;; Packages Import
(require 'coldnew-macro)
(require 'coldnew-functions)
(require 'coldnew-commands)
(require 'coldnew-variables)

;;;;;;;; Hooks

;; If save a newfile to nonexist directory, create the directory before save.
(add-hook 'before-save-hook
	  '(lambda ()
	     (or (file-exists-p  (file-name-directory buffer-file-name))
		 (make-directory (file-name-directory buffer-file-name) t))))


;;;;;;;; Lusty Explorer
;; LustyExplorer is a fast and responsive way to manage files and buffers.
;; It includes both a filesystem explorer and a buffer switcher through
;; a common interface.
;; It's like find-file crossed with iswitchb or InteractivelyDoThings,
;; but with a larger and more easily readable dedicated window
;; for matches instead of the minibuffer.
;;
(when (require* 'lusty-explorer)
  ;;;; Keybindings
  (add-hook 'lusty-setup-hook
	    '(lambda ()
	       (define-key lusty-mode-map (kbd "RET") 'lusty-select-current-name)))
  )



(provide 'coldnew-file)
;; coldnew-file.el ends here.
