;;
(eval-when-compile (require 'cl))
;;;;;;;; Packages Import
(require 'coldnew-editor)

;;;;;;;; Loding libraries
(require 'slime)



;; Slime-repl
(when (featurep 'slime-repl)

  ;; Save REPL history to emacs-cache-dir
  (setq slime-repl-history-file (concat emacs-cache-dir "slime-hist.dat"))

  ;; REPL history size set to 300
  (setq slime-repl-history-size 300)


  )


(provide 'coldnew-slime)
;; coldnew-slime.el ends here.
