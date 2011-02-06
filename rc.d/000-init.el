;;
(eval-when-compile (require 'cl))


(require '001-environment)		; Environment Setting
(require '002-dependency)		; All libraries included in
(require '003-macros)			; Macros or not interactive functions
(require '004-functions)		; Interactive Functions
(require '005-advices)			; Advice
(require '006-deadcode)			; unusecode, maybe use some day...

(require '005-base)			; Basic emacs config
(require '007-backup)			; Configure Backup Process
(require '008-fonts)			; Setting Fonts
(require '009-locale)			; Setting Locales
(require '010-color-theme)		; Color-themes
(require '011-vim-mode)			; Use Vim Keybindings instead of pure emacs
(require '012-display)			; Configure window's size
(require '013-woman)			; Woman-mode Settings
(require '014-session)			; Store current positions
(require '015-desktop)
;; 016~022
(require '022-ibuffer)			; Call buffer-list
(require '025-yasnippet)		; Yasnippet config
(require '026-auto-complete)		; Auto COmplete config
(require '027-minibuffer)		; Add some keybindinng fot minibuffer
(require '028-uniquify)			; COnfigure uniquify
(require '029-lusty-explorer)		; Another good files explorer
(require '030-xrefactory)		;
(require '031-comint-mode)		;
(require '032-anything)			; Complete anything
(require '033-w3m)			; W3m config
(require '034-terminal)			; Terminal Settings
(require '035-rcirc)			; IRC Client Setting
(require '036-midnight)			;
(require '037-ipa)			; In-place annotations
(require '038-flyspell)			; on-the-fly spell checker
(require '039-speck)			; on-the-fly spell checker
(require '040-gnus)			; GNUS Setting


(require 'rc-cedet)
(require 'rc-common-hook)

;;(cond (emacs23-p
;;       (require 'rc-ecb)))
(require 'rc-org-mode)
(require 'rc-smartchr)
(require 'rc-ielm-mode)
(require 'rc-find-file)
(require 'rc-ccmode-common)

(require '997-el-get)			; emacs lisp manager
(require '998-elpa)			; emacs lisp manager
(require '999-keybinding)		; Global Keybindings, must in the last line.

;; Programming Language Configure Settings
(require 'lang-c)			; C
(require 'lang-cpp)			; C++
(require 'lang-emacs-lisp)		; Emacs-Lisp
;;(require 'lang-python)			; Python
;;(require 'lang-matlab)			; Matlab
(require 'lang-newlisp)			; Newlisp
(require 'lang-sgml)			; SGML

(provide '000-init)
;; 000-init.el ends here.
