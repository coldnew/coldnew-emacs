;;
(eval-when-compile (require 'cl))



;; Auto-mode
(add-to-list 'auto-mode-alist '("\\.cir\\'" . spice-mode))
(add-to-list 'auto-mode-alist '("\\.sp\\'"  . spice-mode))
(add-to-list 'auto-mode-alist '("\\.spice\\'"  . spice-mode))



(provide 'lang-spice)
;; lang-spice.el ends here.
