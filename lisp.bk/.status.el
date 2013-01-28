((ac-nrepl status "installed" recipe
	   (:name ac-nrepl :description "Nrepl completion source for Emacs auto-complete package" :type github :pkgname "purcell/ac-nrepl" :depends nrepl :features ac-nrepl))
 (ac-python status "installed" recipe
	    (:name ac-python :description "Simple Python Completion Source for Auto-Complete" :type http :url "http://chrispoole.com/downloads/ac-python.el" :features ac-python))
 (ac-slime status "installed" recipe
	   (:name ac-slime :website "https://github.com/purcell/ac-slime" :description "Emacs auto-complete plugin for Slime symbols" :type github :pkgname "purcell/ac-slime"))
 (ace-jump-mode status "installed" recipe
		(:name ace-jump-mode :website "https://github.com/winterTTr/ace-jump-mode/wiki" :description "A quick cursor location minor mode for emacs" :type github :pkgname "winterTTr/ace-jump-mode" :features ace-jump-mode))
 (alert status "installed" recipe
	(:name alert :description "Growl-style notification system for Emacs" :website "https://github.com/jwiegley/alert" :type github :pkgname "jwiegley/alert" :prepare
	       (progn
		 (autoload 'alert "alert" "Alert the user that something has happened.")
		 (autoload 'alert-add-rule "alert" "Programmatically add an alert configuration rule."))))
 (android-mode status "installed" recipe
	       (:name android-mode :website "https://github.com/remvee/android-mode" :description "Emacs minor mode for Android application development" :type github :pkgname "remvee/android-mode"))
 (ascii status "installed" recipe
	(:name ascii :auto-generated t :type emacswiki :description "ASCII code display." :website "https://raw.github.com/emacsmirror/emacswiki.org/master/ascii.el"))
 (auto-complete status "installed" recipe
		(:name auto-complete :website "http://cx4a.org/software/auto-complete/" :description "The most intelligent auto-completion extension." :type github :pkgname "auto-complete/auto-complete" :depends
		       (popup fuzzy)))
 (auto-complete-clang status "installed" recipe
		      (:name auto-complete-clang :website "https://github.com/brianjcj/auto-complete-clang" :description "Auto-complete sources for Clang. Combine the power of AC, Clang and Yasnippet." :type github :pkgname "brianjcj/auto-complete-clang" :depends auto-complete))
 (auto-complete-css status "removed" recipe nil)
 (auto-indent-mode status "installed" recipe
		   (:name auto-indent-mode :website "https://github.com/mlf176f2/auto-indent-mode.el" :description "Automatically Indent  when pressing return, pasting, and other customizable features." :type github :pkgname "mlf176f2/auto-indent-mode.el"))
 (bm status "installed" recipe
     (:name bm :auto-generated t :type elpa :description "Visible bookmarks in buffer. [source: github]"))
 (c-eldoc status "installed" recipe
	  (:name c-eldoc :auto-generated t :type elpa :description "helpful description of the arguments to C functions [source: github]"))
 (clojure-mode status "installed" recipe
	       (:name clojure-mode :website "https://github.com/technomancy/clojure-mode" :description "Emacs support for the Clojure language." :type github :pkgname "technomancy/clojure-mode"))
 (cmake-mode status "installed" recipe
	     (:name cmake-mode :website "http://www.itk.org/Wiki/CMake_Editors_Support" :description "Provides syntax highlighting and indentation for CMakeLists.txt and *.cmake source files." :type http :url "http://www.cmake.org/CMakeDocs/cmake-mode.el" :features "cmake-mode" :post-init
		    (progn
		      (add-to-list 'auto-mode-alist
				   '("CMakeLists\\.txt\\'" . cmake-mode))
		      (add-to-list 'auto-mode-alist
				   '("\\.cmake\\'" . cmake-mode)))))
 (ctags-update status "installed" recipe
	       (:name ctags-update :auto-generated t :type elpa :description "(auto) update TAGS in parent directory using exuberant-ctags [source: github]"))
 (ctypes status "installed" recipe
	 (:name ctypes :description "Enhanced Font lock support for custom defined type" :type elpa))
 (ecb-snapshot status "installed" recipe
	       (:name ecb-snapshot :auto-generated t :type elpa :description "Emacs Code Browser CVS snapshot"))
 (egg status "installed" recipe
      (:name egg :description "Egg is an Emacs interface to git. It's a suite composed of a minor-mode and various special-buffers presenting different UIs to help the user performing many git operations." :type github :pkgname "byplayer/egg" :load-path
	     (".")
	     :compile nil :features egg))
 (el-get status "installed" recipe
	 (:name el-get :website "https://github.com/dimitri/el-get#readme" :description "Manage the external elisp bits and pieces you depend upon." :type github :branch "4.stable" :pkgname "dimitri/el-get" :features el-get :info "." :load "el-get.el"))
 (eldoc-extension status "installed" recipe
		  (:name eldoc-extension :description "Some extension for eldoc" :website "" :type emacswiki :features eldoc-extension))
 (elscreen status "installed" recipe
	   (:name elscreen :description "This is a fork of ElScreen updated for Emacs 24 and package.el. " :website "https://github.com/shosti/elscreen" :type github :pkgname "shosti/elscreen"))
 (emacs-w3m status "installed" recipe
	    (:name emacs-w3m :description "A simple Emacs interface to w3m" :type cvs :module "emacs-w3m" :url ":pserver:anonymous@cvs.namazu.org:/storage/cvsroot" :build
		   `("autoconf"
		     ("./configure" ,(concat "--with-emacs=" el-get-emacs))
		     "make")
		   :build/windows-nt
		   ("sh /usr/bin/autoconf" "sh ./configure" "make")
		   :info "doc"))
 (erc-highlight-nicknames status "installed" recipe
			  (:name erc-highlight-nicknames :description "Highlights nicknames" :type emacswiki :features erc-highlight-nicknames))
 (evil status "installed" recipe
       (:name evil :website "http://gitorious.org/evil/pages/Home" :description "Evil is an extensible vi layer for Emacs. It\n                                   emulates the main features of Vim, and provides facilities\n                                   for writing custom extensions." :type git :url "git://gitorious.org/evil/evil.git" :features evil :depends undo-tree))
 (expand-region status "installed" recipe
		(:name expand-region :type github :pkgname "magnars/expand-region.el" :description "Expand region increases the selected region by semantic units. Just keep pressing the key until it selects what you want." :website "https://github.com/magnars/expand-region.el#readme" :features expand-region))
 (flymake-shell status "installed" recipe
		(:name flymake-shell :description "A flymake syntax-checker for shell script" :type git :url "https://github.com/purcell/flymake-shell.git" :features flymake-shell))
 (fuzzy status "installed" recipe
	(:name fuzzy :website "https://github.com/auto-complete/fuzzy-el" :description "Fuzzy matching utilities for GNU Emacs" :type github :pkgname "auto-complete/fuzzy-el"))
 (git-emacs status "installed" recipe
	    (:name git-emacs :description "Yet another git emacs mode for newbies" :type github :pkgname "tsgates/git-emacs" :features git-emacs))
 (go-mode status "installed" recipe
	  (:name go-mode :description "Major mode for the Go programming language" :type http :url "http://go.googlecode.com/hg/misc/emacs/go-mode.el?r=tip" :localname "go-mode.el" :features go-mode :post-init
		 (add-to-list 'auto-mode-alist
			      '("\\.go\\'" . go-mode))))
 (google-translate status "installed" recipe
		   (:name google-translate :auto-generated t :type elpa :description "Emacs interface to Google Translate [source: github]"))
 (google-weather status "installed" recipe
		 (:name google-weather :description "Fetch Google Weather forecasts." :type git :url "git://git.naquadah.org/google-weather-el.git" :features
			(google-weather org-google-weather)))
 (guess-offset status "installed" recipe
	       (:name guess-offset :auto-generated t :type elpa :description "Automatically determine c-basic-offset"))
 (guess-style status "installed" recipe
	      (:name guess-style :auto-generated t :type elpa :description "automatic setting of code style variables"))
 (helm status "required" recipe nil)
 (helm-etags-plus status "installed" recipe
		  (:name helm-etags-plus :type github :pkgname "jixiuf/helm-etags-plus" :features ctags-update helm-etags+))
 (helm-git status "installed" recipe
	   (:name helm-git :type github :pkgname "maio/helm-git" :features helm-git))
 (highlight-cl status "installed" recipe
	       (:name highlight-cl :description "Highlighting `cl' functions." :website "" :type emacswiki :features highlight-cl))
 (htmlize status "installed" recipe
	  (:name htmlize :website "http://www.emacswiki.org/emacs/Htmlize" :description "Convert buffer text and decorations to HTML." :type http :url "http://fly.srk.fer.hr/~hniksic/emacs/htmlize.el.cgi" :localname "htmlize.el" :feature htmlize))
 (hungry-delete status "installed" recipe
		(:name hungry-delete :description "hungry delete minor mode" :website "https://github.com/nflath/hungry-delete" :type git :url "https://github.com/nflath/hungry-delete.git" :features hungry-delete))
 (ibuffer-git status "installed" recipe
	      (:name ibuffer-git :description "show git status in ibuffer" :type git :url "git://github.com/jrockway/ibuffer-git" :features ibuffer-git))
 (iedit status "installed" recipe
	(:name iedit :description "Edit multiple regions with the same content simultaneously." :type emacswiki :features iedit))
 (jabber status "installed" recipe
	 (:name jabber :auto-generated t :type elpa :description "A Jabber client for Emacs."))
 (js2-mode status "installed" recipe
	   (:name js2-mode :website "https://github.com/mooz/js2-mode#readme" :description "An improved JavaScript editing mode" :type github :pkgname "mooz/js2-mode" :prepare
		  (autoload 'js2-mode "js2-mode" nil t)))
 (key-chord status "installed" recipe
	    (:name key-chord :description "map pairs of simultaneously pressed keys to commands" :type emacswiki :features key-chord))
 (less-css-mode status "installed" recipe
		(:name less-css-mode :auto-generated t :type elpa :description "Major mode for editing LESS CSS files (lesscss.org) [source: github]"))
 (linum-ace status "required" recipe nil)
 (linum-relative status "required" recipe nil)
 (lusty-explorer status "installed" recipe
		 (:name lusty-explorer :type github :pkgname "sjbach/lusty-emacs" :description "LustyExplorer is a fast and responsive way to manage files and buffers"))
 (magit status "installed" recipe
	(:name magit :website "https://github.com/magit/magit#readme" :description "It's Magit! An Emacs mode for Git." :type github :pkgname "magit/magit" :info "." :autoloads
	       ("50magit")
	       :build
	       (("make" "all"))
	       :build/darwin
	       `(("make" ,(format "EMACS=%s" el-get-emacs)
		  "all"))))
 (moz-repl status "installed" recipe
	   (:name moz-repl :description "Lets current buffer interact with inferior mozilla." :type http :url "http://github.com/bard/mozrepl/raw/master/chrome/content/moz.el"))
 (mu4e status "installed" recipe
       (:name mu4e :website "http://www.djcbsoftware.nl/code/mu/mu4e.html" :description "An emacs-based e-mail client which uses mu (http://www.djcbsoftware.nl/code/mu/) as its back-end: mu4e." :type github :pkgname "djcb/mu" :post-init
	      (setq mu4e-mu-binary
		    (expand-file-name "mu"
				      (expand-file-name "mu"
							(el-get-package-directory 'mu4e))))
	      :build
	      `(("autoreconf -i")
		("./configure")
		("make"))
	      :load-path "mu4e"))
 (multi-term status "installed" recipe
	     (:name multi-term :description "A mode based on term.el, for managing multiple terminal buffers in Emacs." :type emacswiki :features multi-term))
 (multi-web-mode status "installed" recipe
		 (:name "multi-web-mode" :description "Multi Web Mode is a minor mode which makes web editing in Emacs much easier" :type github :pkgname "fgallina/multi-web-mode"))
 (mumamo-noweb status "installed" recipe
	       (:name mumamo-noweb :auto-generated t :type emacswiki :description "MuMaMo noweb goo" :website "https://raw.github.com/emacsmirror/emacswiki.org/master/mumamo-noweb.el"))
 (nognus status "installed" recipe
	 (:name nognus :description "A newsreader for GNU Emacs" :type git :url "http://git.gnus.org/gnus.git" :build
		`(("./configure" ,(concat "--with-emacs="
					  (shell-quote-argument el-get-emacs)))
		  ("make"))
		:build/windows-nt
		`(,(concat "\"make.bat " invocation-directory "\""))
		:info "texi" :load-path
		("lisp")
		:autoloads nil :features gnus-load))
 (nrepl status "installed" recipe
	(:name nrepl :description "An Emacs client for nREPL, the Clojure networked REPL server." :type github :pkgname "kingtim/nrepl.el" :depends clojure-mode :features nrepl))
 (nrepl-ritz status "installed" recipe
	     (:name nrepl-ritz :auto-generated t :type elpa :description "nrepl extensions for ritz [source: github]"))
 (nxhtml status "installed" recipe
	 (:type github :pkgname "emacsmirror/nxhtml" :name nxhtml :type emacsmirror :description "An addon for Emacs mainly for web development." :build
		(list
		 (concat el-get-emacs " -batch -q -no-site-file -L . -l nxhtmlmaint.el -f nxhtmlmaint-start-byte-compilation"))
		:load "autostart.el"))
 (o-blog status "installed" recipe
	 (:name o-blog :type github :description "Stand alone org-mode blog exporter." :pkgname "renard/o-blog"))
 (offlineimap status "installed" recipe
	      (:name offlineimap :description "Run OfflineIMAP from Emacs" :type git :url "git://git.naquadah.org/offlineimap-el.git" :features offlineimap))
 (org-html5presentation status "installed" recipe
			(:name org-html5presentation :type http :website "https://gist.github.com/509761" :description "html5 presentation from org files" :url "https://raw.github.com/gist/509761/org-html5presentation.el" :autoloads t))
 (package status "installed" recipe
	  (:name package :description "ELPA implementation (\"package.el\") from Emacs 24" :builtin 24 :type http :url "http://repo.or.cz/w/emacs.git/blob_plain/1a0a666f941c99882093d7bd08ced15033bc3f0c:/lisp/emacs-lisp/package.el" :shallow nil :features package :post-init
		 (progn
		   (setq package-user-dir
			 (expand-file-name
			  (convert-standard-filename
			   (concat
			    (file-name-as-directory default-directory)
			    "elpa")))
			 package-directory-list
			 (list
			  (file-name-as-directory package-user-dir)
			  "/usr/share/emacs/site-lisp/elpa/"))
		   (make-directory package-user-dir t)
		   (unless
		       (boundp 'package-subdirectory-regexp)
		     (defconst package-subdirectory-regexp "^\\([^.].*\\)-\\([0-9]+\\(?:[.][0-9]+\\)*\\)$" "Regular expression matching the name of\n a package subdirectory. The first subexpression is the package\n name. The second subexpression is the version string."))
		   (setq package-archives
			 '(("ELPA" . "http://tromey.com/elpa/")
			   ("gnu" . "http://elpa.gnu.org/packages/")
			   ("marmalade" . "http://marmalade-repo.org/packages/")
			   ("SC" . "http://joseito.republika.pl/sunrise-commander/"))))))
 (paredit status "installed" recipe
	  (:name paredit :description "Minor mode for editing parentheses" :type http :url "http://mumble.net/~campbell/emacs/paredit.el" :features "paredit"))
 (po-mode status "installed" recipe
	  (:name po-mode :description "Major mode for GNU gettext PO files" :type http :url "http://cvs.savannah.gnu.org/viewvc/*checkout*/gettext/gettext/gettext-tools/misc/po-mode.el" :features po-mode :post-init
		 (progn
		   (add-to-list 'auto-mode-alist
				'("\\.po$" . po-mode))
		   (add-to-list 'auto-mode-alist
				'("\\.pot$" . po-mode)))))
 (popup status "installed" recipe
	(:name popup :website "https://github.com/auto-complete/popup-el" :description "Visual Popup Interface Library for Emacs" :type github :pkgname "auto-complete/popup-el"))
 (popwin status "installed" recipe
	 (:name popwin :description "Popup Window Manager." :website "https://github.com/m2ym/popwin-el" :type github :pkgname "m2ym/popwin-el"))
 (pretty-lambdada status "installed" recipe
		  (:name pretty-lambdada :description "Show the word `lambda' as the Greek letter." :website "" :type emacswiki :features pretty-lambdada))
 (projectile status "installed" recipe
	     (:name projectile :description "Project navigation and management library for Emacs" :type github :pkgname "bbatsov/projectile" :features projectile))
 (qmake-mode status "installed" recipe
	     (:name qmake-mode :type github :pkgname "coldnew/qmake-mode" :features qmake-mode))
 (qml-mode status "installed" recipe
	   (:name qml-mode :type github :pkgname "coldnew/qml-mode" :features qml-mode))
 (rainbow-delimiters status "installed" recipe
		     (:name rainbow-delimiters :website "https://github.com/jlr/rainbow-delimiters#readme" :description "Color nested parentheses, brackets, and braces according to their depth." :type github :pkgname "jlr/rainbow-delimiters" :features rainbow-delimiters))
 (rainbow-mode status "installed" recipe
	       (:name rainbow-mode :description "Colorize color names in buffers" :type elpa))
 (recentf-ext status "installed" recipe
	      (:name recentf-ext :description "Recentf extensions" :type emacswiki :features "recentf-ext"))
 (s status "installed" recipe
    (:name s :description "The long lost Emacs string manipulation library." :type github :pkgname "magnars/s.el" :features s))
 (sauron status "installed" recipe
	 (:name sauron :description "enhanced tracking of the world inside and outside your emacs" :website "https://github.com/djcb/sauron" :type github :pkgname "djcb/sauron" :prepare
		(autoload 'sauron-start "sauron" "Start sauron." t)))
 (shell-pop status "installed" recipe
	    (:name shell-pop :description "Helps you pop up and pop out shell buffer easily." :website "http://www.emacswiki.org/emacs/ShellPop" :type emacswiki :features "shell-pop"))
 (skype status "installed" recipe
	(:name skype :description "Skype UI for emacs users." :type github :pkgname "buzztaiki/emacs-skype" :features skype))
 (slime status "installed" recipe
	(:name slime :description "Major mode for editing Slim file" :features slime :type elpa))
 (smallurl status "installed" recipe
	   (:name smallurl :description "" :type http :url "http://repo.or.cz/w/ShellArchive.git/blob_plain/90c4e111279ed78ff871c660cff88af46e99c79c:/smallurl.el"))
 (smart-tab status "installed" recipe
	    (:name smart-tab :description "Intelligent tab completion and indentation." :type github :pkgname "genehack/smart-tab" :features smart-tab))
 (smartchr status "installed" recipe
	   (:type github :pkgname "emacsmirror/smartchr" :name smartchr :type emacsmirror :features smartchr :description "Emacs version of smartchr.vim"))
 (smarter-compile status "installed" recipe
		  (:name smarter-compile :description "an interface to `compile'" :features smarter-compile :type elpa))
 (smex status "installed" recipe
       (:name smex :description "M-x interface with Ido-style fuzzy matching." :type github :pkgname "nonsequitur/smex" :features smex :post-init
	      (smex-initialize)))
 (space-chord status "installed" recipe
	      (:name space-chord :description "key chord with Space" :type emacswiki :features space-chord))
 (sr-speedbar status "installed" recipe
	      (:name sr-speedbar :type emacswiki :description "Same frame speedbar" :post-init
		     (require 'sr-speedbar)))
 (undo-tree status "installed" recipe
	    (:name undo-tree :description "Treat undo history as a tree" :type git :url "http://www.dr-qubit.org/git/undo-tree.git" :prepare
		   (progn
		     (autoload 'undo-tree-mode "undo-tree.el" "Undo tree mode; see undo-tree.el for details" t)
		     (autoload 'global-undo-tree-mode "undo-tree.el" "Global undo tree mode" t))))
 (unicad status "installed" recipe
	 (:name unicad :description "" :type svn :url "http://unicad.googlecode.com/svn/trunk/"))
 (workgroups status "required" recipe nil)
 (xcscope status "installed" recipe
	  (:name xcscope :description "Cscope interface for (X)Emacs" :type github :pkgname "To1ne/xcscope" :features xcscope))
 (xcscope+ status "installed" recipe
	   (:name xcscope+ :description "Providing an extension to xcscope." :type emacswiki))
 (yasnippet status "installed" recipe
	    (:name yasnippet :website "https://github.com/capitaomorte/yasnippet.git" :description "YASnippet is a template system for Emacs." :type github :pkgname "capitaomorte/yasnippet" :features "yasnippet" :pre-init
		   (unless
		       (or
			(boundp 'yas/snippet-dirs)
			(get 'yas/snippet-dirs 'customized-value))
		     (setq yas/snippet-dirs
			   (list
			    (concat el-get-dir
				    (file-name-as-directory "yasnippet")
				    "snippets"))))
		   :post-init
		   (put 'yas/snippet-dirs 'standard-value
			(list
			 (list 'quote
			       (list
				(concat el-get-dir
					(file-name-as-directory "yasnippet")
					"snippets")))))
		   :compile nil :submodule nil))
 (zencoding-mode status "installed" recipe
		 (:name zencoding-mode :description "Unfold CSS-selector-like expressions to markup" :type github :pkgname "rooney/zencoding" :features zencoding-mode)))
