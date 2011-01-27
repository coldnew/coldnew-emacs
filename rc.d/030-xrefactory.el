

(cond
 (linux-p
  (setq load-path (cons "~/.emacs.d/lisp/xref-linux/emacs" load-path))
  (setq exec-path (cons "~/.emacs.d/lisp/xref" exec-path))
  (require 'xrefactory)
  )
 )


(provide '030-xrefactory)
;; 030-xrefactory.el ends here.
