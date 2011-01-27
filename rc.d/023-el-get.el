;;

(defalias 'install 'el-get-install)
(defalias 'remove  'el-get-remove)
(setq el-get-sources
      '(
	(:name pylookup
	       :type git
	       :url "git://github.com/tsgates/pylookup.git")
	(:name auto-complete-clang
	       :type git
	       :url "https://github.com/brianjcj/auto-complete-clang.git")
	(:name autocomplete
	       :type git
	       :url "http://github.com/m2ym/auto-complete.git")
	(:name auto-complete-etags
	       :type emacswiki)
	(:name popup-kill-ring
	       :type emacswiki)
	(:name auto-complete-extension
	       :type emacswiki)
	(:name el-get
	       :type git
	       :url "git://github.com/dimitri/el-get.git"
	       :features el-get
	       :load    "el-get.el"
	       :compile "el-get.el")
	(:name highlight-parentheses
	       :type http
	       :url "http://nschum.de/src/emacs/highlight-parentheses/highlight-parentheses.el"
	       :features highlight-parentheses)
	(:name ipython
	       :type http
	       :url "http://ipython.scipy.org/dist/ipython.el"
	       :features ipython
	       :after (lambda ()
			(setq py-python-command "ipython")))
	(:name magit
	       :type git
	       :url "http://github.com/philjackson/magit.git"
	       :info "."
	       ;; that used to be how to build it :build ("./autogen.sh" "./configure" "make")
	       :build ("make all")
	       :build/darwin `(,(concat "PATH=" invocation-directory ":$PATH make all"))
	       :features magit)
	(:name multi-term
	       :type emacswiki
	       :features multi-term)
	(:name paredit
	       :type http
	       :url "http://mumble.net/~campbell/emacs/paredit.el"
	       :features "paredit")
	(:name pos-tip
	       :type emacswiki)
	(:name pymacs
	       :type git
	       :url "http://github.com/pinard/Pymacs.git"
	       :build ("make"))
	(:name rainbow-mode
	       :type git
	       :url "git://git.naquadah.org/rainbow.git"
	       :features rainbow-mode)
	(:name undo-tree
	       :type http
	       :url "http://www.dr-qubit.org/undo-tree/undo-tree.el")
	(:name yasnippet
	       :type svn
	       :url "http://yasnippet.googlecode.com/svn/trunk/"
	       :features "yasnippet")
	(:name popup-pos-tip
	       :type emacswiki)
	(:name company
	       :type http-tar
	       :options ("jxf")
	       :url "http://nschum.de/src/emacs/company-mode/company-0.5.tar.bz2")
	(:name fic-mode
	       :type emacswiki)
	(:name hungury-delete
	       :type git
	       :url "http://github.com/nflath/hungry-delete.git")
	(:name c-eldoc
	       :type http
	       :url "http://www.emacswiki.org/emacs/download/c-eldoc.el")
	;; FIXME:
	(:name emms
	       :type git
	       :url "git://git.sv.gnu.org/emms.git"
	       :info "doc"
	       :load-path ("./lisp")
	       :features emms-setup
	       :build ("make autoloads" "make")
	       :build/darwin ("make EMACS=/Applications/Emacs.app/Contents/MacOS/Emacs autoloads all"))
	(:name smartchr
	       :type git
	       :url "http://github.com/imakado/emacs-smartchr.git")
	(:name xcode-document-viewer
	       :type git
	       :url "https://github.com/imakado/emacs-xcode-document-viewer.git")

	;;elscreen
	;;auctex
	;;color-theme			;

	;;  session
	;; emacs-w3m
	))

(provide '023-el-get)
;; 023-el-get.el ends here.
