;;
(setq el-get-dir "~/.emacs.d/lisp/")
(add-to-list 'el-get-recipe-path "~/.emacs.d/etc/recipes")

(setq el-get-sources
      '(
	ac-anything
	anything-complete
	anything-config
	anything-c-shell-history
	anything-dabbrev-expand
	anything
	anything-grep
	anything-gtags
	anything-include
	anything-ipa
	anything-kyr
	anything-match-plugin
	anything-menu
	anything-migemo
	anything-show-completion
	anything-slime
	anything-yaetags
	apel
	async-eval
	auto-complete-clang
	autocomplete
	auto-complete-etags
	auto-complete-extension
	c-eldoc
	company
	completion-ui
	csharp-mode
	doxymacs
	el-get
	emms
	gnugo
	gobject-class
	highlight-cl
	highlight-parentheses
	htmlize
	hungury-delete
	ipa
	ipython
	lusty-explorer
	magit
	multi-term
	nav
	newlisp-mode
	paredit
	popup-kill-ring
	popup-pos-tip
	pos-tip
	ppindent
	pylookup
	pymacs
	qmake-mode
	quack
	rainbow-mode
	revive
	smartchr
	ssh-config
	sunrise-commander
	undo-tree
	unicad
	vim-mode
	windows
	xcode-document-viewer
	yasnippet

	(:name xcscope+
	       :type emacswiki)

	(:name rcirc-color
	       :type emacswiki)
	(:name rcirc-controls
	       :type emacswiki)
	(:name redo+
	       :type emacswiki)
	(:name rw-hunspell
	       :type http
	       :url "http://www.mail-archive.com/gnu-emacs-sources@gnu.org/msg01709/rw-hunspell.el")
	(:name rw-ispell
	       :type http
	       :url "http://www.mail-archive.com/gnu-emacs-sources@gnu.org/msg01709/rw-ispell.el")
	(:name rw-language-and-country-codes
	       :type http
	       :url "http://www.mail-archive.com/gnu-emacs-sources@gnu.org/msg01709/rw-language-and-country-codes.el")
	(:name auto-dictionary
	       :type http
	       :url "http://nschum.de/src/emacs/auto-dictionary/auto-dictionary.el"
	       :features auto-dictionary)
	(:name speck
	       :type emacswiki)
	(:name zencoding-mode
	       :type git
	       :url "https://github.com/chrisdone/zencoding.git"
	       :build ("make"))
	(:name emacs-w3m
	       :type cvs
	       :module "emacs-w3m"
	       :url ":pserver:anonymous@cvs.namazu.org:/storage/cvsroot"
	       :build `("autoconf" ("./configure" ,(concat "--with-emacs=" el-get-emacs)) "make")
	       :info "doc")

	(:name ibuffer-git
	       :type git
	       :url "git://github.com/jrockway/ibuffer-git")

	(:name framemove
	       :type emacswiki)
	(:name eclim
	       :type git
	       :url "https://github.com/senny/emacs-eclim.git")

	(:name rcirc-notify
	       :type emacswiki)
	(:name rcirc-nonames-on-join
	       :type emacswiki)
	(:name rcirc-pounce
	       :type emacswiki)
	(:name rcirc-auto-away
	       :type emacswiki)
	(:name rcirc-late-fix
	       :type emacswiki)
	(:name rcirc-groups
	       :type emacswiki)
	(:name rcirc-dbus
	       :type emacswiki)
	(:name elscreen
	       :type git
	       :url "https://github.com/emacsmirror/elscreen.git")
	(:name notify
	       :type emacswiki)
	(:name anything-emms
	       :type emacswiki)
	(:name emms-get-lyrics
	       :type git
	       :url "https://github.com/talau/elisp.git")
	(:name mmm-mode
	       :type git
	       :url "git://github.com/purcell/mmm-mode.git")
	(:name smex
	       :type git
	       :url "https://github.com/nonsequitur/smex.git")
	(:name thingatpt+
	       :type emacswiki)
	(:name ascii
	       :type emacswiki)
	;;auctex
	;;color-theme			;

	;;  session
	;; emacs-w3m
	))

;;(el-get 'wait)



(provide '997-el-get)
;; 997-el-get.el ends here.
