

(cond
 (linux-p
  (setq load-path (cons "~/.emacs.d/lisp/xref-linux/emacs" load-path))
  (setq exec-path (cons "~/.emacs.d/lisp/xref" exec-path))
  ;;  (load "xrefactory")
  (require 'xrefactory)
  )
 )


(provide 'rc-xrefactory)
;;
