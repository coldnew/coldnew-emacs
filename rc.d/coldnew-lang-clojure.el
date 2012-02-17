;;
(eval-when-compile (require 'cl))

;;;;;;;; Packages Import
(require 'coldnew-editor)

;;;;;;;; Loding Libraries
(require 'clojure-mode)
(require 'slime)
;;(require 'swank-clojure)


;;;;;;;; clojure-mode extensions
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
;; (add-to-list 'auto-mode-alist '("\\.c$" . c-mode))








(provide 'coldnew-lang-clojure)
;; coldnew-lang-clojure.el ends here.
