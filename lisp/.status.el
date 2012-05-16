((ac-slime status "installed" recipe
	   (:name ac-slime :website "https://github.com/purcell/ac-slime" :description "Emacs auto-complete plugin for Slime symbols" :type github :pkgname "purcell/ac-slime"))
 (ace-jump-mode status "installed" recipe
		(:name ace-jump-mode :website "https://github.com/winterTTr/ace-jump-mode/wiki" :description "A quick cursor location minor mode for emacs" :type github :pkgname "winterTTr/ace-jump-mode" :features ace-jump-mode))
 (android-mode status "installed" recipe
	       (:name android-mode :website "https://github.com/remvee/android-mode" :description "Emacs minor mode for Android application development" :type github :pkgname "remvee/android-mode"))
 (anything status "installed" recipe
	   (:name anything :website "http://www.emacswiki.org/emacs/Anything" :description "Open anything / QuickSilver-like candidate-selection framework" :type git :url "http://repo.or.cz/r/anything-config.git" :shallow nil :load-path
		  ("." "extensions" "contrib")
		  :features anything))
 (anything-traverse status "removed" recipe nil)
 (apel status "required" recipe nil)
 (ascii status "installed" recipe
	(:name ascii :description "ASCII code display." :website "" :type emacswiki :features ascii))
 (asciidoc status "removed" recipe nil)
 (auto-complete status "installed" recipe
		(:name auto-complete :website "http://cx4a.org/software/auto-complete/" :description "The most intelligent auto-completion extension." :type github :pkgname "m2ym/auto-complete" :depends
		       (popup fuzzy)
		       :load-path "." :post-init
		       (progn
			 (require 'auto-complete)
			 (add-to-list 'ac-dictionary-directories
				      (expand-file-name "dict"))
			 (require 'auto-complete-config)
			 (ac-config-default))))
 (auto-complete-clang status "installed" recipe
		      (:name auto-complete-clang :website "https://github.com/brianjcj/auto-complete-clang" :description "Auto-complete sources for Clang. Combine the power of AC, Clang and Yasnippet." :type github :pkgname "brianjcj/auto-complete-clang"))
 (auto-complete-emacs-lisp status "removed" recipe nil)
 (bash-completion status "installed" recipe
		  (:name bash-completion :description "" :website "" :type http :url "https://raw.github.com/szermatt/emacs-bash-completion/master/bash-completion.el" :features bash-completion))
 (c-eldoc status "installed" recipe
	  (:name c-eldoc :description "helpful description of the arguments to C functions" :website "" :type elpa :features c-eldoc))
 (cedet status "installed" recipe
	(:name cedet :website "http://cedet.sourceforge.net/" :description "CEDET is a Collection of Emacs Development Environment Tools written with the end goal of creating an advanced development environment in Emacs." :type bzr :url "bzr://cedet.bzr.sourceforge.net/bzrroot/cedet/code/trunk" :build
	       ("touch `find . -name Makefile`" "make")
	       :build/windows-nt
	       ("echo #!/bin/sh > tmp.sh & echo touch `/usr/bin/find . -name Makefile` >> tmp.sh & echo make FIND=/usr/bin/find >> tmp.sh" "sed 's/^M$//' tmp.sh  > tmp2.sh" "sh ./tmp2.sh" "rm ./tmp.sh ./tmp2.sh")))
 (center-cursor status "removed" recipe nil)
 (center-cursor-mode status "removed" recipe nil)
 (centered-cursor-mode status "installed" recipe
		       (:name centered-cursor-mode :description "" :website "" :type emacswiki :features centered-cursor-mode))
 (cljdoc status "installed" recipe
	 (:name cljdoc :description "eldoc mode for clojure" :type elpa))
 (clojure-mode status "installed" recipe
	       (:name clojure-mode :website "https://github.com/technomancy/clojure-mode" :description "Emacs support for the Clojure language." :type github :pkgname "technomancy/clojure-mode"))
 (ctypes status "installed" recipe
	 (:name ctypes :description "Enhanced Font lock support for custom defined type" :type elpa))
 (cursor-chg status "removed" recipe nil)
 (dot-mode status "removed" recipe nil)
 (doxymacs status "installed" recipe
	   (:name doxymacs :website "http://doxymacs.sourceforge.net/" :description "Doxymacs is Doxygen + {X}Emacs." :type git :url "git://doxymacs.git.sourceforge.net/gitroot/doxymacs/doxymacs" :load-path
		  ("./lisp")
		  :build
		  ("./bootstrap" "./configure" "make")
		  :features doxymacs))
 (dtrt-indent status "installed" recipe
	      (:name dtrt-indent :website "http://savannah.nongnu.org/projects/dtrt-indent/" :description "A minor mode that guesses the indentation offset originally used for creating source code files and transparently adjusts the corresponding settings in Emacs, making it more convenient to edit foreign files." :type git :url "git://git.savannah.nongnu.org/dtrt-indent.git" :features dtrt-indent :post-init
		     (dtrt-indent-mode 1)))
 (ecb status "installed" recipe
      (:name ecb :description "Emacs Code Browser" :type cvs :module "ecb" :url ":pserver:anonymous@ecb.cvs.sourceforge.net:/cvsroot/ecb" :build
	     `(("make" "CEDET=" ,(concat "EMACS="
					 (shell-quote-argument el-get-emacs))))))
 (ecb_snap status "removed" recipe nil)
 (egg status "installed" recipe
      (:name egg :description "Egg is an Emacs interface to git. It's a suite composed of a minor-mode and various special-buffers presenting different UIs to help the user performing many git operations." :type github :pkgname "byplayer/egg" :load-path
	     (".")
	     :compile nil :features egg))
 (el-get status "installed" recipe
	 (:name el-get :website "https://github.com/dimitri/el-get#readme" :description "Manage the external elisp bits and pieces you depend upon." :type github :branch "3.stable" :pkgname "dimitri/el-get" :features el-get :load "el-get.el"))
 (el-index status "removed" recipe nil)
 (el-kit status "removed" recipe nil)
 (el-screen status "removed" recipe nil)
 (eldoc-extension status "installed" recipe
		  (:name eldoc-extension :description "Some extension for eldoc" :website "" :type emacswiki :features eldoc-extension))
 (elscreen status "installed" recipe
	   (:name elscreen :description "This is a fork of ElScreen updated for Emacs 24 and package.el. " :website "https://github.com/shosti/elscreen" :type github :pkgname "shosti/elscreen"))
 (eproject status "installed" recipe
	   (:name eproject :description "File grouping (\"project\") extension for emacs" :type github :pkgname "jrockway/eproject" :load-path
		  ("." "lang")
		  :features eproject))
 (escreen status "removed" recipe nil)
 (etags-select status "installed" recipe
	       (:name etags-select :description "" :type elpa))
 (etags-table status "installed" recipe
	      (:name etags-table :description "" :type elpa))
 (evil status "installed" recipe
       (:name evil :website "http://gitorious.org/evil/pages/Home" :description "Evil is an extensible vi layer for Emacs. It\n       emulates the main features of Vim, and provides facilities\n       for writing custom extensions." :type git :url "https://git.gitorious.org/evil/evil.git" :features evil :depends undo-tree))
 (expand-region status "installed" recipe
		(:name expand-region :type github :pkgname "magnars/expand-region.el" :description "Expand region increases the selected region by semantic units. Just keep pressing the key until it selects what you want." :website "https://github.com/magnars/expand-region.el#readme" :prepare
		       (autoload 'er/expand-region "expand-region" nil t)))
 (fastnav status "installed" recipe
	  (:name fastnav :website "https://github.com/gleber/fastnav.el" :description "Fast navigation and editing routines." :type github :pkgname "gleber/fastnav.el" :load-path "." :post-init
		 (progn
		   (require 'fastnav))))
 (flymake-shell status "installed" recipe
		(:name flymake-shell :description "A flymake syntax-checker for shell script" :type git :url "https://github.com/purcell/flymake-shell.git" :features flymake-shell))
 (fuzzy status "installed" recipe
	(:name fuzzy :website "https://github.com/m2ym/fuzzy-el" :description "Fuzzy matching utilities for GNU Emacs" :type github :pkgname "m2ym/fuzzy-el" :features fuzzy))
 (gccsense status "removed" recipe nil)
 (git-emacs status "installed" recipe
	    (:name git-emacs :description "Yet another git emacs mode for newbies" :type github :pkgname "tsgates/git-emacs" :features git-emacs))
 (helm status "installed" recipe
       (:name helm :description "Emacs incremental and narrowing framework" :type github :pkgname "emacs-helm/helm" :features helm-config))
 (helm-etags-plus status "installed" recipe
		  (:name helm-etags-plus :type github :pkgname "jixiuf/helm-etags-plus" :features ctags-update helm-etags+))
 (helm-projectile status "removed" recipe nil)
 (hideshowvis status "installed" recipe
	      (:name hideshowvis :type elpa))
 (highlight-cl status "installed" recipe
	       (:name highlight-cl :description "Highlighting `cl' functions." :website "" :type emacswiki :features highlight-cl))
 (highlight-symbol status "removed" recipe nil)
 (hungry-delete status "installed" recipe
		(:name hungry-delete :description "hungry delete minor mode" :website "https://github.com/nflath/hungry-delete" :type git :url "https://github.com/nflath/hungry-delete.git" :features hungry-delete))
 (ibuffer-git status "installed" recipe
	      (:name ibuffer-git :description "show git status in ibuffer" :type git :url "git://github.com/jrockway/ibuffer-git" :features ibuffer-git))
 (icicles status "removed" recipe nil)
 (icicles-install status "removed" recipe nil)
 (ide-skel status "installed" recipe
	   (:name ide-skel :description "" :type emacswiki :depends tabbar :features ide-skel))
 (iedit status "installed" recipe
	(:name iedit :description "Edit multiple regions with the same content simultaneously." :type emacswiki :features iedit))
 (jabber status "installed" recipe
	 (:name cljdocjabber:wq :description "eldoc mode for clojure" :type elpa))
 (jump-char status "removed" recipe nil)
 (key-chord status "installed" recipe
	    (:name key-chord :description "map pairs of simultaneously pressed keys to commands" :type emacswiki :features key-chord))
 (linum+ status "removed" recipe nil)
 (lusty-explorer status "installed" recipe
		 (:name lusty-explorer :type emacswiki :description "LustyExplorer is a fast and responsive way to manage files and buffers"))
 (magit status "installed" recipe
	(:name magit :website "https://github.com/magit/magit#readme" :description "It's Magit! An Emacs mode for Git." :type github :pkgname "magit/magit" :info "." :build
	       ("make all")
	       :build/darwin
	       `(,(concat "PATH="
			  (shell-quote-argument invocation-directory)
			  ":$PATH make all"))))
 (minimap status "installed" recipe
	  (:name minimap :type git :url "http://randomsample.de/minimap.git"))
 (multi-term status "installed" recipe
	     (:name multi-term :description "A mode based on term.el, for managing multiple terminal buffers in Emacs." :type emacswiki :features multi-term))
 (nav status "installed" recipe
      (:name nav :description "Emacs mode for filesystem navigation" :type hg :url "https://emacs-nav.googlecode.com/hg" :features nav))
 (one-key status "installed" recipe
	  (:name one-key :description "OneKey is designed to help you remember keybindings. It saves you time and frees up your head!" :website "" :type emacswiki :features one-key))
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
			   ("marmalade" . "http://marmalade-repo.org/packages/"))))))
 (paredit status "installed" recipe
	  (:name paredit :description "Minor mode for editing parentheses" :type http :url "http://mumble.net/~campbell/emacs/paredit.el" :features "paredit"))
 (popup status "installed" recipe
	(:name popup :website "https://github.com/m2ym/popup-el" :description "Visual Popup Interface Library for Emacs" :type github :pkgname "m2ym/popup-el" :features popup))
 (pretty-lambdada status "installed" recipe
		  (:name pretty-lambdada :description "Show the word `lambda' as the Greek letter." :website "" :type emacswiki :features pretty-lambdada))
 (projectile status "installed" recipe
	     (:name projectile :description "Projectile is a project interaction library for Emacs" :features projectile :type github :pkgname bbatsov/projectile))
 (rainbow-delimiters status "installed" recipe
		     (:name rainbow-delimiters :website "https://github.com/jlr/rainbow-delimiters#readme" :description "Color nested parentheses, brackets, and braces according to their depth." :type github :pkgname "jlr/rainbow-delimiters" :features rainbow-delimiters))
 (rainbow-mode status "installed" recipe
	       (:name rainbow-mode :description "Colorize color names in buffers" :type elpa))
 (recentf-ext status "installed" recipe
	      (:name recentf-ext :description "Recentf extensions" :type emacswiki :features "recentf-ext"))
 (sauron status "installed" recipe
	 (:name sauron :description "enhanced tracking of the world inside and outside your emacs" :website "https://github.com/djcb/sauron" :type github :pkgname "djcb/sauron" :prepare
		(autoload 'sauron-start "sauron" "Start sauron." t)))
 (session status "removed" recipe nil)
 (shell-pop status "installed" recipe
	    (:name shell-pop :description "Helps you pop up and pop out shell buffer easily." :website "http://www.emacswiki.org/emacs/ShellPop" :type emacswiki :features "shell-pop"))
 (slime status "installed" recipe
	(:name slime :description "Major mode for editing Slim file" :features slime :type elpa))
 (smarter-compile status "installed" recipe
		  (:name smarter-compile :description "an interface to `compile'" :features smarter-compile :type elpa))
 (smex status "installed" recipe
       (:name smex :description "M-x interface with Ido-style fuzzy matching." :type github :pkgname "nonsequitur/smex" :features smex :post-init
	      (smex-initialize)))
 (smx status "removed" recipe nil)
 (space-chord status "installed" recipe
	      (:name space-chord :description "key chord with Space" :type emacswiki :features space-chord))
 (sr-speedbar status "installed" recipe
	      (:name sr-speedbar :type emacswiki :description "Same frame speedbar" :post-init
		     (require 'sr-speedbar)))
 (sunrise-commander status "installed" recipe
		    (:name sunrise-commander :description "Two-pane file manager for Emacs based on Dired and inspired by MC" :type emacswiki))
 (sunrise-x-tree status "removed" recipe nil)
 (switch-window status "installed" recipe
		(:name switch-window :description "A *visual* way to choose a window to switch to" :type github :pkgname "dimitri/switch-window" :features switch-window))
 (tabbar status "installed" recipe
	 (:name tabbar :type emacswiki :description "Display a tab bar in the header line" :lazy t :load-path "."))
 (tempbuf status "installed" recipe
	  (:name tempbuf :description "" :website "" :type emacswiki :features tempbuf))
 (traverselisp status "installed" recipe
	       (:name traverselisp :description "walk through directories and perform actions on files." :type emacswiki :features traverselisp))
 (treverselisp status "removed" recipe nil)
 (undo-tree status "installed" recipe
	    (:name undo-tree :description "Treat undo history as a tree" :type git :url "http://www.dr-qubit.org/git/undo-tree.git" :prepare
		   (progn
		     (autoload 'undo-tree-mode "undo-tree.el" "Undo tree mode; see undo-tree.el for details" t)
		     (autoload 'global-undo-tree-mode "undo-tree.el" "Global undo tree mode" t))))
 (unicad status "installed" recipe
	 (:name unicad :description "" :type svn :url "http://unicad.googlecode.com/svn/trunk/"))
 (volatile-highlights status "removed" recipe nil)
 (wanderlust status "removed" recipe nil)
 (winring status "removed" recipe nil)
 (xml-rpc-el status "installed" recipe
	     (:name xml-rpc-el :description "An elisp implementation of clientside XML-RPC" :type bzr :url "lp:xml-rpc-el"))
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
		   :compile nil :submodule nil)))
