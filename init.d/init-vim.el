;;;; initial vim-mode
(provide 'init-vim)

;;;; because vim-mode has many bugs , use viper and vimpulse instead
;; (when (require 'vim nil 'noerror)
;;  (vim-mode))

;;;;; Following are viper and vimpulse setting
(setq viper-mode t)
(setq viper-custom-file-name "~/.emacs.d/init.d/.viper.el")
(when (require 'viper nil noerror)
  (require 'vimpulse nil 'noerror)
  (defalias 'vim:nmap 'vimpulse-map)
  (defalias 'vim:omap 'vimpulse-omap)
  (defalias 'vim:vmap 'vimpulse-vmap)
  ;;FIXME: how to add vim:emap in here ?
  )

