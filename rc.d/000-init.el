;;
(eval-when-compile (require 'cl))

;; Libraries configure
(requiref '001-environment)		; Environment Setting
(requiref '002-dependency)		; All libraries included in
(requiref '003-macros)			; Macros or not interactive functions
(requiref '004-functions)		; Interactive Functions
(requiref '005-advices)			; Advice
(requiref '006-deadcode)		; unusecode, maybe use some day...
(requiref '007-base)			; Basic emacs config
(requiref '008-fonts)			; Setting Fonts
(requiref '009-locale)			; Setting Locales
(requiref '010-color-theme)		; Color-themes
(requiref '011-backup)			; Configure Backup Process
(requiref '012-display)			; Configure window's size
(requiref '013-woman)			; Woman-mode Settings
(requiref '014-session)			; Store current positions
(requiref '015-desktop)			;
(requiref '016-vim-mode)		; Use Vim Keybindings instead of pure emacs
;; 017~022
(requiref '022-ibuffer)			; Call buffer-list
(requiref '025-yasnippet)		; Yasnippet config
(requiref '026-auto-complete)		; Auto COmplete config
(requiref '027-minibuffer)		; Add some keybindinng fot minibuffer
(requiref '028-uniquify)		; COnfigure uniquify
(requiref '029-lusty-explorer)		; Another good files explorer
(requiref '030-xrefactory)		;
(requiref '031-comint-mode)		;
(requiref '032-anything)		; Complete anything
(requiref '033-w3m)			; W3m config
(requiref '034-terminal)		; Terminal Settings
(requiref '035-rcirc)			; IRC Client Setting
(requiref '036-midnight)		; clean unuse buffers in midnight
(requiref '037-ipa)			; In-place annotations
(requiref '038-flyspell)		; on-the-fly spell checker
(requiref '039-speck)			; on-the-fly spell checker
(requiref '040-gnus)			; GNUS Setting
(requiref '041-ielm)			; ielm

(requiref 'rc-cedet)
(requiref 'rc-common-hook)

;;(cond (emacs23-p
;;       (requiref 'rc-ecb)))
(requiref 'rc-org-mode)
(requiref 'rc-smartchr)
(requiref 'rc-find-file)
(requiref 'rc-ccmode-common)

(requiref '997-el-get)			; emacs lisp manager
(requiref '998-elpa)			; emacs lisp manager
(requiref '999-keybinding)		; Global Keybindings, must in the last line.

;; Programming Language Configure Settings
(requiref 'lang-c)			; C
(requiref 'lang-cpp)			; C++
(requiref 'lang-emacs-lisp)		; Emacs-Lisp
;;(requiref 'lang-python)			; Python
;;(requiref 'lang-matlab)			; Matlab
(requiref 'lang-newlisp)			; Newlisp
(requiref 'lang-sgml)			; SGML









(provide '000-init)
;; 000-init.el ends here.
