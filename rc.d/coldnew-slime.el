;;
(eval-when-compile (require 'cl))
;;;;;;;; Packages Import
(require 'coldnew-editor)

;;;;;;;; Loding libraries
(require 'slime)
(require 'ac-slime)
;;(require 'durendal)

(add-hook 'slime-mode-hook 'set-up-slime-ac)


;; Slime-repl

;; Save REPL history to emacs-cache-dir
(setq slime-repl-history-file (concat emacs-cache-dir "slime-hist.dat"))

;; REPL history size set to 300
(setq slime-repl-history-size 300)

;; Use global programming mode
(add-hook 'slime-repl-mode-hook 'programming-mode)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)



(add-hook 'slime-inspector-mode-hook
	  '(lambda ()
	     (font-lock-add-keywords nil '(("\\(\\w+\\)(" 1 font-lock-function-name-face)))))

;; (add-hook 'slime-repl-mode-hook 'durendal-slime-repl-paredit)
;; (add-hook 'sldb-mode-hook 'durendal-dim-sldb-font-lock)
;; (add-hook 'slime-compilation-finished-hook 'durendal-hide-successful-compile)



(provide 'coldnew-slime)
;; coldnew-slime.el ends here.
