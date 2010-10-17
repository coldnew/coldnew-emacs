;; init slime


(setq-default inferior-lisp-program "/usr/bin/sbcl")
(when (require 'slime nil 'noerror)
  (slime-setup)
;; (when (require 'init-vim nil 'noerror)
  ;; (vim:imap (kbd "M-\)") 'slime-close-all-parens-in-sexp))
  )


(provide 'rc-slime)
