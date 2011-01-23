;;;; All extra dependency
(eval-when-compile (require 'cl))

;; Must include in coldnew's emacs
(require 'vim)				; Use Vim-Mode
(require 'undo-tree)			; Use undo-tree instead of redo.el
(require 'woman)			; Use Woman
(require 'color-theme)			; Color-theme
(require 'ibuffer)			; iBuffer
(require 'python)			; Python
;;(require 'pymacs)			; Pymacs
(require 'midnight)			; Use midnight-mode to auto-clean buffers
(require 'session)			; Use Session to save current positions
(require 'shell-pop)			; Pop-up Shells
(require 'cedet)
;;(require 'matlab)			; Matlab
(require 'paredit)			; Use Paredit to balance par
(require 'eldoc)
(require 'hungry-delete)
(require 'highlight-parentheses)
(require 'eldoc-extension)
(require 'auto-complete)
(require 'pretty-lambdada)
;;(require 'ipython)			; iPython
;;(require 'ibuffer-expert)		; iBuffer Expert



;; Programming Language Configure Settings
(require 'lang-c)			; C
(require 'lang-cpp)			; C++
(require 'lang-emacs-lisp)		; Emacs-Lisp
;;(require 'lang-python)			; Python
(require 'lang-matlab)			; Matlab





(provide '005-dependency)
;; 005-dependency.el ends here.
