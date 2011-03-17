;;
(eval-when-compile (require 'cl))

;;;;;;;; Packages Import
(require 'coldnew-macro)
(require 'coldnew-functions)
(require 'coldnew-commands)
(require 'coldnew-variables)


(when (require* 'woman)
  (setq woman-cache-filename (concat emacs-cache-dir "woman.cache"))
  (setq woman-use-own-frame nil)
  )




(provide 'coldnew-help)
;; coldnew-help.el ends here.
