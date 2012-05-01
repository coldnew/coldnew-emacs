;;; coldnew-cedet.el --- setting for cedet
(eval-when-compile (require 'cl))

(require 'semantic)
(global-semanticdb-minor-mode 1)

(require 'semantic/bovine/c)
(require 'semantic/bovine/gcc)
(require 'semantic/bovine/clang)
(require 'semantic/ia)
(require 'semantic/decorate/include)
(require 'semantic/lex-spp)
(require 'eassist)

;; enable support for gnu global
(semanticdb-enable-gnu-global-databases 'c-mode)
(semanticdb-enable-gnu-global-databases 'c++-mode)


(provide 'coldnew-cedet)
;; coldnew-cedet.el ends here.
