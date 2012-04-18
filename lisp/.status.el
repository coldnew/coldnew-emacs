((auto-complete status "installed" recipe
		(:name auto-complete :website "http://cx4a.org/software/auto-complete/" :description "The most intelligent auto-completion extension." :type github :pkgname "m2ym/auto-complete" :depends
		       (popup fuzzy)
		       :load-path "." :post-init
		       (progn
			 (require 'auto-complete)
			 (add-to-list 'ac-dictionary-directories
				      (expand-file-name "dict"))
			 (require 'auto-complete-config)
			 (ac-config-default))))
 (el-get status "installed" recipe
	 (:name el-get :website "https://github.com/dimitri/el-get#readme" :description "Manage the external elisp bits and pieces you depend upon." :type github :branch "3.stable" :pkgname "dimitri/el-get" :features el-get :load "el-get.el"))
 (eldoc-extension status "installed" recipe
		  (:name eldoc-extension :description "Some extension for eldoc" :website "" :type emacswiki :features eldoc-extension))
 (fuzzy status "installed" recipe
	(:name fuzzy :website "https://github.com/m2ym/fuzzy-el" :description "Fuzzy matching utilities for GNU Emacs" :type github :pkgname "m2ym/fuzzy-el" :features fuzzy))
 (highlight-cl status "installed" recipe
	       (:name highlight-cl :description "Highlighting `cl' functions." :website "" :type emacswiki :features highlight-cl))
 (highlight-symbol status "installed" recipe
		   (:name highlight-symbol :description "Quickly highlight a symbol throughout the buffer and cycle through its locations." :type http :url "http://nschum.de/src/emacs/highlight-symbol/highlight-symbol.el" :features "highlight-symbol"))
 (hungry-delete status "installed" recipe
		(:name hungry-delete :description "hungry delete minor mode" :website "https://github.com/nflath/hungry-delete" :type git :url "https://github.com/nflath/hungry-delete.git" :features hungry-delete))
 (iedit status "installed" recipe
	(:name iedit :description "Edit multiple regions with the same content simultaneously." :type emacswiki :features iedit))
 (linum+ status "installed" recipe
	 (:name linum+ :description "Extension of linum" :type emacswiki :features linum+))
 (lusty-explorer status "installed" recipe
		 (:name lusty-explorer :type emacswiki :description "LustyExplorer is a fast and responsive way to manage files and buffers"))
 (multi-term status "installed" recipe
	     (:name multi-term :description "A mode based on term.el, for managing multiple terminal buffers in Emacs." :type emacswiki :features multi-term))
 (popup status "installed" recipe
	(:name popup :website "https://github.com/m2ym/popup-el" :description "Visual Popup Interface Library for Emacs" :type github :pkgname "m2ym/popup-el" :features popup))
 (pretty-lambdada status "installed" recipe
		  (:name pretty-lambdada :description "Show the word `lambda' as the Greek letter." :website "" :type emacswiki :features pretty-lambdada))
 (shell-pop status "installed" recipe
	    (:name shell-pop :description "Helps you pop up and pop out shell buffer easily." :website "http://www.emacswiki.org/emacs/ShellPop" :type emacswiki :features "shell-pop"))
 (undo-tree status "installed" recipe
	    (:name undo-tree :description "Treat undo history as a tree" :type git :url "http://www.dr-qubit.org/git/undo-tree.git" :prepare
		   (progn
		     (autoload 'undo-tree-mode "undo-tree.el" "Undo tree mode; see undo-tree.el for details" t)
		     (autoload 'global-undo-tree-mode "undo-tree.el" "Global undo tree mode" t))))
 (unicad status "installed" recipe
	 (:name unicad :description "" :type svn :url "http://unicad.googlecode.com/svn/trunk/"))
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
