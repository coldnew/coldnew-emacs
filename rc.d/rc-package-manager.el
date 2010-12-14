;;
(provide 'rc-package-manager)

(setq el-get-dir "~/.emacs.d/lisp/")
(setq el-get-recipe-path '("~/.emacs.d/etc/el-get/recipes"))
(when (require 'el-get nil 'noerror)
  (defalias 'install 'el-get-install)
  (defalias 'remove  'el-get-remove)
  (setq el-get-sources
	'(
	  el-get
	  yasnippet
	  auto-complete
	  auto-complete-etags
	  auto-complete-clang
	  auto-complete-extension
	  color-theme
	  highlight-parentheses
	  multi-term
	  elscreen
	  package
	  rainbow-mode
	  emms
	  auctex
	  c-eldoc
	  pos-tip
	  popup-pos-tip
	  undo-tree
	  vimpulse
	  vimpulse-surround
	  fic-mode
	  ;;	  session
	  ;;	  popup-kill-ring
	  hungury-delete
	  company
	  pylookup
	  pymacs
	  smartchr
	  ipython
	  xcode-document-viewer
	  paredit
	  paredit-viper-compat
	  ;;emacs-w3m
	  )))
