;;
(eval-when-compile (require 'cl))

;;;;;;;; Packages Import
(require 'coldnew-macro)
(require 'coldnew-functions)
(require 'coldnew-commands)
(require 'coldnew-variables)

(when (require* 'vim)

  ;; Auto strat vim-mode
  (vim-mode)

  ;; Use this function to fix toggle-comment-bug
  (vim:defcmd vim:visual-toggle-comment (motion)
    "Toggles comments in the region."
    (comment-or-uncomment-region (vim:motion-begin-pos motion)
				 (vim:motion-end-pos motion)))


  )




(provide 'coldnew-vim)
;; coldnew-vim.el ends here.
