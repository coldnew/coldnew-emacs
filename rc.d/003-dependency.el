;;;; All extra dependency
(eval-when-compile (require 'cl))

;; Must include in coldnew's emacs
(require 'auto-complete)		; Auto-Complete
(require 'cedet)
(require 'color-theme)			; Color-theme
(require 'eldoc)			; Emacs-Lisp Documents Browser
(require 'eldoc-extension)		; Emacs-Lisp Documents Browser Extension
(require 'highlight-parentheses)	; Highlight pair parentheses
(require 'hungry-delete)
(require 'ibuffer)			; iBuffer
(require 'midnight)			; Use midnight-mode to auto-clean buffers
(require 'paredit)			; Use Paredit to balance parentheses
(require 'pretty-lambdada)		; Show lambda in smbol
(require 'python)			; Python
(require 'session)			; Use Session to save current positions
(require 'shell-pop)			; Pop-up Shells
(require 'undo-tree)			; Use undo-tree instead of redo.el
(require 'uniquify)
(require 'vim)				; Use Vim-Mode
(require 'woman)			; Use Woman
(require 'ac-company)
;;(require 'ibuffer-expert)		; iBuffer Expert
;;(require 'ipython)			; iPython
;;(require 'matlab)			; Matlab
;;(require 'pymacs)			; Pymacs



;; Programming Language Configure Settings
(require 'lang-c)			; C
(require 'lang-cpp)			; C++
(require 'lang-emacs-lisp)		; Emacs-Lisp
;;(require 'lang-python)			; Python
(require 'lang-matlab)			; Matlab
(require 'lang-newlisp)			; Newlisp





(provide '003-dependency)
;; 003-dependency.el ends here.
