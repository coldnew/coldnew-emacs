;;
(setq el-get-dir "~/.emacs.d/lisp/")
(add-to-list 'el-get-recipe-path "~/.emacs.d/etc/recipes")
(add-to-list 'load-path "~/.emacs.d/etc/recipes/")
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
	(:name lusty-explorer
	       :type emacswiki)

	(:name ssh-config
	       :type git
	       :url "git://github.com/renard/ssh-config-el.git"
	       :features ssh-config)
	(:name apel
	       :type cvs
	       :module "apel"
	       :url ":pserver:anonymous@cvs.m17n.org:/cvs/root"
	       :build
	       (mapcar
		(lambda (target)
		  (list el-get-emacs
			(split-string "-batch -q -no-site-file -l APEL-MK -f")
			target
			"prefix" "site-lisp" "site-lisp"))
		'("compile-apel" "install-apel"))
	       :load-path ("site-lisp/apel" "site-lisp/emu"))
	(:name doxymacs
	       :type git
	       :url "git://doxymacs.git.sourceforge.net/gitroot/doxymacs/doxymacs"
	       :load-path ("./lisp")
	       :build ("./bootstrap" "./configure" "make")
	       :features doxymacs
	       )
	(:name newlisp-mode
	       :type git
	       :url "https://github.com/may/newlisp-mode.git"
	       :features newlisp)
	(:name sunrise-commander
	       :type emacswiki)
	(:name nav
	       :type svn
	       :url "http://emacs-nav.googlecode.com/svn/trunk/"
	       :features nav)
	(:name anything
	       :type emacswiki)
	(:name anything-config
	       :type git
	       :url "git://repo.or.cz/anything-config.git")
	(:name anything-match-plugin
	       :type emacswiki)
	(:name anything-show-completion
	       :type emacswiki)
	(:name anything-migemo
	       :type emacswiki)
	(:name windows
	       :type http
	       :url "http://www.gentei.org/~yuuji/software/windows.el"
	       :features "windows")
	(:name anything-gtags
	       :type emacswiki)
	(:name anything-complete
	       :type emacswiki)
	(:name anything-ipa
	       :type emacswiki)
	(:name ipa
	       :type emacswiki)
	(:name revive
	       :type http
	       :url "http://www.gentei.org/~yuuji/software/revive.el")
	(:name ac-anything
	       :type emacswiki)
	(:name anything-grep
	       :type emacswiki)
	(:name anything-kyr
	       :type emacswiki)
	(:name anything-slime
	       :type emacswiki)
	(:name anything-menu
	       :type emacswiki)
	(:name anything-c-shell-history
	       :type emacswiki)
	(:name anything-include
	       :type emacswiki)
	(:name anything-yaetags
	       :type emacswiki)
	(:name anything-dabbrev-expand
	       :type emacswiki)
	(:name completion-ui
	       :type http-tar
	       :options ("xf")
	       :url "http://www.dr-qubit.org/download.php?file=predictive/completion-ui.tar.gz"
	       )
	(:name async-eval
	       :type http
	       :url "http://nschum.de/src/emacs/async-eval/async-eval.el")
	(:name unicad
	       :type emacswiki)
	(:name gobject-class
	       :type http
	       :url "http://blog.gustavobarbieri.com.br/old-website/gobject-class.el")
	(:name ppindent
	       :type emacswiki)
	(:name highlight-cl
	       :type emacswiki)
	(:name vim-mode
	       :type hg
	       :url "https://bitbucket.org/lyro/vim-mode")
	(:name qmake-mode
	       :type hg
	       :url "https://qmake-mode.googlecode.com/hg/ qmake-mode")
	csharp-mode
	gnugo
	htmlize
	quack
	;; (:name gnugo
	;;        :type emacswiki)
	;;elscreen
	;;auctex
	;;color-theme			;

	;;  session
	;; emacs-w3m
	))


;; BUG: After load this file, I can't use lusty-explorer, so rebind keymap
;;(vim:nmap (kbd "C-x C-f") 'find-file)
;;(vim:imap (kbd "C-x C-f") 'find-file)



(provide '997-el-get)
;; 997-el-get.el ends here.
