;;
(eval-when-compile (require 'cl))

;;;;;;;; Packages Import
(require 'coldnew-editor)
(require 'coldnew-slime)

;;;;;;;; Loding Libraries
(require 'clojure-mode)
;;(require 'durendal)

;;;;;;;; clojure-mode extensions
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs$" . clojure-mode))
;; (add-to-list 'auto-mode-alist '("\\.c$" . c-mode))



;;;;;;;; Hooks
(add-hook 'clojure-mode-hook
          '(lambda ()

             ;; Color nested parentheses, brackets, and braces according to their depth
             (when (require* 'rainbow-delimiters)
               (rainbow-delimiters-mode))

             ;; Use Greek character lambda instead of string
             (when (require* 'pretty-lambdada)
               (turn-on-pretty-lambda-mode))

             ;; Use global programming mode
             (programming-mode)

             ;; Use paredit in elisp
             (use-paredit-mode)

             ))

;;(add-hook 'clojure-mode-hook 'durendal-enable-auto-compile)



(provide 'coldnew-lang-clojure)
;; coldnew-lang-clojure.el ends here.
