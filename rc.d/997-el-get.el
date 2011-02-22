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
	anything-emms
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
	ascii
	async-eval
	auctex
	auto-complete-clang
	autocomplete
	auto-complete-etags
	auto-complete-extension
	auto-dictionary
	bbdb
	c-eldoc
	company
	completion-ui
	csharp-mode
	doxymacs
	eclim
	el-get
	elscreen-color-theme
	elscreen-dired
	elscreen-dnd
	elscreen
	elscreen-gf
	elscreen-goby
	elscreen-howm
	elscreen-server
	elscreen-speedbar
	elscreen-w3m
	elscreen-wl
	emacs-w3m
	emms
	emms-get-lyrics
	flim
	framemove
	gnugo
	gobject-class
	highlight-cl
	highlight-parentheses
	htmlize
	hungury-delete
	ibuffer-git
	ipa
	ipython
	lusty-explorer
	magit
	mmm-mode
	multi-term
	nav
	newlisp-mode
	notify
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
	rcirc-auto-away
	rcirc-color
	rcirc-controls
	rcirc-dbus
	rcirc-groups
	rcirc-late-fix
	rcirc-nonames-on-join
	rcirc-notify
	rcirc-pounce
	redo+
	revive
	rw-hunspell
	rw-ispell
	rw-language-and-country-codes
	semi
	smartchr
	smex
	speck
	ssh-config
	sunrise-commander
	thingatpt+
	undo-tree
	unicad
	vim-mode
	wanderlust
	windows
	xcode-document-viewer
	xcscope+
	yasnippet
	zencoding-mode
	(:name egg
	       :type git
	       :url "https://github.com/byplayer/egg.git"
	       :load-path (".")
	       :compile nil ;; egg uses eval at places which breaks compilation
	       :features egg)
	(:name git-emacs
	       :type git
	       :url "https://github.com/tsgates/git-emacs.git"
	       :features git-emacs)
	(:name identica-mode
	       :type git
	       :url "http://git.savannah.gnu.org/cgit/identica-mode.git")
	(:name twittering-mode
	       :type git
	       :url "git://github.com/hayamiz/twittering-mode.git")
	(:name elisp-depend
	       :type emacswiki)
	(:name sr-speedbar
	       :type emacswiki)
	;;  session
	;; emacs-w3m
	;;color-theme			;
	))

;;(el-get 'wait)



(provide '997-el-get)
;; 997-el-get.el ends here.
