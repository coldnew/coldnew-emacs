
;;;; initial el-get
(provide 'init-el-get)

(setq el-get-dir "~/.emacs.d/lisp/")
(setq el-get-recipe-path '("~/.emacs.d/etc/el-get/recipes"))

(when (require 'el-get nil 'noerror)
  (defalias 'install 'el-get-install)
  (defalias 'remove  'el-get-remove)
  (setq el-get-sources 
	'(
	  el-get 
	  redo
	  redo+
	  yasnippet
	  pretty-lambdada
	  auto-complete
	  auto-complete-etags
	  auto-complete-clang
	  auto-complete-extension
	  ac-company
	  color-theme
	  shell-pop
	  template-simple
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
	  session
	  popup-kill-ring  
	  company
	  eldoc-extension
)))
