;;
(eval-when-compile (require 'cl))

;;;;;;;; Packages Import
(require 'coldnew-macro)
(require 'coldnew-functions)
(require 'coldnew-commands)
(require 'coldnew-variables)


(when (require* 'woman)
  (setq woman-use-own-frame nil)
  (setq woman-cache-filename (concat emacs-cache-dir "woman.cache"))
  (setq woman-use-topic-at-point nil)
  (setq woman-fill-column 100)
  )




(provide 'coldnew-help)
;; coldnew-help.el ends here.
