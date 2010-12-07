;; base on vimpulse-surround.el
(provide 'vim-surround)

(defvar vim:surround-pairs
  '((")" . ("(" . ")"))
    ))

(vim:defmotion vim:motion-inner-aa (inclusive count)
  "Selec `count'enclosing pars of <> exclusive."
  (vim:inner-block "\"" "\"" t (or count 1)))

()
 
(vim:omap (kbd "i\"") 'vim:motion-inner-aa)
