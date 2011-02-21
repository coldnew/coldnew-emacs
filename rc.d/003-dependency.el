
;;;; All extra dependency
(eval-when-compile (require 'cl))


(require* 'ac-anything)			  ; Auto Complete with Anything
(require* 'ac-company)			  ; Auto-Complete with company-mode
(require* 'advice)			  ;
(require* 'ansi-color)			  ;
(require* 'anything)			  ;
(require* 'anything-c-shell-history)	  ; shell history anything.el interface
(require* 'anything-complete)		  ;
(require* 'anything-config)		  ;
(require* 'anything-dabbrev-expand)	  ; dabbrev using anything.el
(require* 'anything-grep)		  ; search refinement of grep result with anything
(require* 'anything-gtags)		  ;
(require* 'anything-include)		  ; Anything-source made maintenance of history of C/C++
(require* 'anything-ipa)		  ;
(require* 'anything-kyr)		  ; Show context-aware commands
(require* 'anything-match-plugin)	  ;
(require* 'anything-menu)		  ; anything.el candidate selection outside Emacs
(require* 'anything-migemo)		  ;
(require* 'anything-show-completion)	  ; Show selection in buffer for anything completion
(require* 'anything-slime)		  ; anything-sources and some utilities for SLIME.
(require* 'anything-yaetags)		  ; Yet another etags interface with anything
(require* 'ascii)			  ; ASCII code display.
(require* 'async-eval)			  ; execute Emacs lisp in a separate process
(require* 'auto-complete)		  ; Auto-Complete
(require* 'auto-complete-clang)		  ;
(require* 'auto-complete-config)	  ;
(require* 'auto-complete-etags)		  ;
(require* 'auto-complete-extension)	  ;
(require* 'auto-dictionary)		  ;
(require* 'bbdb)			  ; Big Brother Database
(require* 'c-eldoc)			  ;
(require* 'cc-mode)			  ;
(require* 'cedet)			  ;
(require* 'cflow-mode)			  ; major mode for viewing cflow output files
(require* 'cl)				  ;
(require* 'color-theme)			  ; Color-theme
(require* 'company)			  ; Another aoto-complete plugins
(require* 'completion-ui)		  ;
(require* 'csharp-mode)			  ; C# mode derived mode
(require* 'ctagsfind)			  ;
(require* 'dabbrev)			  ;
(require* 'desktop)			  ;
(require* 'dirtrack)			  ; Directory Tracking by watching the prompt
(require* 'doxymacs)			  ; Doxygen for emacs
(require* 'dropdown-list)		  ;
(require* 'easymenu)			  ;
(require* 'eclim)			  ; an interface to the Eclipse IDE.
(require* 'ediff)
(require* 'el-get)			  ; Emacs-Lisp Manager
(require* 'eldoc)			  ; Emacs-Lisp Documents Browser
(require* 'eldoc-extension)		  ; Emacs-Lisp Documents Browser Extension
(require* 'elscreen)			  ; have multiple screens on emacs as well as GNU screen
(require* 'elscreen-color-theme)	  ;
(require* 'elscreen-dired)		  ;
(require* 'elscreen-dnd)		  ; enable drag and drop file to elscreen
(require* 'elscreen-gf)			  ;
(require* 'elscreen-goby)		  ;
(require* 'elscreen-howm)		  ;
(require* 'elscreen-server)		  ;
(require* 'elscreen-speedbar)		  ; elscreen with speedbar
(require* 'elscreen-w3m)		  ; elscreen with w3m
(require* 'elscreen-wl)			  ; elscreen with wanderlust
(require* 'emms)			  ; Emacs Multimedia System
(require* 'emms-setup)			  ; Setup script for EMMS
(require* 'emms-bookmarks)		  ; Bookmarks for Emms
(require* 'emms-browser)		  ; a track browser supporting covers and filtering
(require* 'emms-cache)			  ; persistence for emms-track
(require* 'emms-compat)			  ; Compatibility routines for EMMS
(require* 'emms-get-lyrics)		  ; Get the lyrics of the song emms is currently playing
(require* 'emms-history)		  ; save all playlists when exiting emacs
(require* 'emms-i18n)			  ; functions for handling coding systems
(require* 'emms-info)			  ; Retrieving track information
(require* 'emms-lyrics)			  ; Display lyrics synchronically
(require* 'emms-mode-line)		  ; Mode-Line and titlebar infos for emms
(require* 'emms-mode-line-icon)		  ; show an icon in the Emacs mode-line
(require* 'emms-player-mpg321-remote)	  ; play files with mpg321 -R
(require* 'emms-player-mplayer)		  ; mplayer support for EMMS
(require* 'emms-player-simple)		  ; A generic simple player
(require* 'emms-playing-time)		  ; Display emms playing time on mode line
(require* 'emms-streams)		  ; interface to add and play streams
(require* 'emms-tag-editor)		  ; Edit track tags.
(require* 'etags)			  ;
(require* 'executable)			  ;
(require* 'find-file)			  ;
(require* 'find-lisp)			  ; emulation of find in Emacs Lisp
(require* 'flyspell)			  ; on-the-fly spell checker
(require* 'framemove)			  ; directional frame selection routines
(require* 'gccsense)			  ; GCC's code analyzers
(require* 'gnugo)			  ; Run GNU Go in a buffer
(require* 'gobject-class)		  ; functions to easy GObject-based class developers
(require* 'grep)			  ;
(require* 'highlight-cl)		  ;
(require* 'highlight-parentheses)	  ; Highlight pair parentheses
(require* 'htmlize)			  ;
(require* 'hungry-delete)		  ;
(require* 'ibuf-ext)			  ; iBuffer Extension
(require* 'ibuffer)			  ; iBuffer
(require* 'ibuffer-git)			  ; show git status in ibuffer column
(require* 'ido)				  ; interactively do things with buffers and files
(require* 'idutils)			  ;
(require* 'info)			  ;
(require* 'ipa)				  ; In-place annotations
(require* 'linum)			  ;
(require* 'lusty-explorer)		  ; A good explorer tools
(require* 'magit)			  ;
(require* 'midnight)			  ; Use midnight-mode to auto-clean buffers
(require* 'mime)			  ; MIME library module
(require* 'mmm-auto)			  ; loading and enabling MMM Mode automatically
(require* 'mmm-mode)			  ; Allow Multiple Major Modes in a buffer
(require* 'multi-term)			  ;
(require* 'nav)				  ;
(require* 'newlisp)			  ; newlisp
(require* 'paredit)			  ; Use Paredit to balance parentheses
(require* 'popup-pos-tip)		  ;
(require* 'pos-tip)			  ;
(require* 'pp)				  ; pretty printer for Emacs Lisp
(require* 'ppindent)			  ;
(require* 'pretty-lambdada)		  ; Show lambda in smbol
(require* 'pymacs)			  ; Interface between Emacs Lisp and Python - Lisp part.
(require* 'python)			  ; Python
(require* 'qmake-mode)			  ; qmake mode for emacs
(require* 'quack)			  ; enhanced support for editing and running Scheme code
(require* 'rainbow-mode)		  ; Color the emacs buffer
(require* 'rcirc)			  ;
(require* 'rcirc-auto-away)		  ; Go /away after emacs is idle for a period of time.
(require* 'rcirc-color)			  ; color rcirc nicks
(require* 'rcirc-controls)		  ; color rcirc control sequences
(require* 'rcirc-dbus)			  ; rcirc dbus notifications
(require* 'rcirc-groups)		  ; an emacs buffer in rcirc-groups major mode
(require* 'rcirc-late-fix)		  ; Replace s/wrong/right strings on rcirc buffers
(require* 'rcirc-nonames-on-join)	  ; don't show 'JOIN' messages in rcirc
(require* 'rcirc-notify)		  ; rcirc libnotify popups
(require* 'rcirc-pounce)		  ; maintain a message queue for offline nicks
(require* 'recentf)			  ; Setup a menu of recently opened files
(require* 'rw-hunspell)			  ;
(require* 'rw-ispell)			  ;
(require* 'rw-language-and-country-codes) ;
(require* 'semactic/lex)		  ;
(require* 'semantic)			  ;
(require* 'semantic/bovine)		  ;
(require* 'semantic/bovine/c)		  ;
(require* 'semantic/bovine/gcc)		  ;
(require* 'semantic/db)			  ;
(require* 'semantic/db-el)		  ;
(require* 'semantic/db-file)		  ;
(require* 'semantic/db-find)		  ;
(require* 'semantic/db-global)		  ;
(require* 'semantic/db-ref)		  ;
(require* 'semantic/db-typecache)	  ;
(require* 'semantic/ia)			  ;
(require* 'session)			  ; Use Session to save current positions
(require* 'shell)			  ;
(require* 'shell-pop)			  ; Helps you pop up and pop out shell buffer easily
(require* 'slime)			  ; Superior Lisp Interaction Mode for Emacs
(require* 'smartchr)			  ; Emacs version of smartchr.vim
(require* 'smex)			  ; M-x interface with Ido-style fuzzy matching.
(require* 'smime)			  ; S/MIME interface
(require* 'speck)			  ; minor mode for spell checking
(require* 'ssh-config)			  ; Syntax Highlight ssh-configure files
(require* 'sunrise-commander)		  ; A commander looks like MC
(require* 'term)			  ;
(require* 'thingatpt)			  ; Get the thing at point
(require* 'thingatpt+)			  ; Extensions to `thingatpt.el'.
(require* 'tramp)			  ; Transparent Remote Access, Multiple Protocol
(require* 'undo-tree)			  ; Use undo-tree instead of redo.el
(require* 'unicad)			  ; Universal Charset Auto Detector
(require* 'uniquify)			  ; If two buffer have the same name, rename both
(require* 'url)				  ; Uniform Resource Locator retrieval tool
(require* 'vim)				  ; Use Vim-Mode
(require* 'w3m)				  ;
(require* 'w3m-lnum)			  ;
(require* 'windmove)			  ; Directional window-selection routines
(require* 'windows)			  ; Window manager for GNU Emacs.
(require* 'wl)				  ; Wanderlust is a mail and news client.
(require* 'woman)			  ; Use Woman
(require* 'x-dnd)			  ; Drag and drop support for X
(require* 'xcscope)			  ; Cscope interface for Emacs
(require* 'xcscope+)			  ; Providing an extension to xcscope
(require* 'yasnippet)			  ; Yet another snippet extension for Emacs
(require* 'zencoding-mode)		  ; Unfold CSS-selector-like expressions to markup
(require* 'vim-elscreen)		  ;
(require* 'mime-w3m)			  ; mime-view content filter for text
(require* 'pop3)			  ; Post Office Protocol (RFC 1460) interface
(require* 'gnus)			  ; a newsreader for GNU Emacs
(require* 'password-cache)		  ; Read passwords, possibly using a password cache
(require* 'server)			  ; Lisp code for GNU Emacs running as server process
(require* 'epa-file)			  ; the EasyPG Assistant, transparent file encryption
(require* 'env)				  ; functions to manipulate environment variables
(require* 'revive)			  ;
(require* 'mm-url)			  ; a wrapper of url functions/commands for Gnus
(require* 'egg)				  ; egg -- Emacs Got Git, A magit fork
(require* 'git-emacs)			  ; yet another git emacs mode for newbies
(require* 'spice-mode)			  ; major mode providing a spice mode hook for fontification
(require* 'identica-mode)		  ; Major mode for Identica
(require* 'elisp-depend)		  ; Parse depend libraries of elisp file.



;; environ
(cond
 (mac-p
  )
 (windows-p
  )
 (linux-p
  (require* 'notify)			  ; notification front-end
  (require* 'site-gentoo)		  ; site initialisation for Gentoo-installed packages
  ))









;;(require* 'anything-emms)		  ; Integrate EMMS with `anything.el'
;;(require* 'ipython)			; Adds support for IPython to python-mode.el
;;(require* 'matlab)			; Matlab







(provide '003-dependency)
;; 003-dependency.el ends here.
