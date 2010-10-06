;;;; initial vim-mode
(provide 'init-vim)

;; because vim-mode has many bugs , use viper and vimpulse instead
(when (require 'vim nil 'noerror)
  (vim-mode)

;; keymapping
(when (require 'undo-tree nil 'noerror)
     (vim:nmap (kbd "u") 'undo-tree-undo)
     (vim:nmap (kbd "C-r") 'undo-tree-redo))

;; tmp usage setting
(vim:defcmd vim:visual-toggle-comment (motion)
    "Toggles comments in the region."
    (comment-or-uncomment-region (vim:motion-begin-pos motion)
                                 (vim:motion-end-pos motion)))
(vim:vmap (kbd "M-;") 'vim:visual-toggle-comment)
  )

;;;;; Following are viper and vimpulse setting
;; (setq viper-mode t)
;; (setq viper-custom-file-name "~/.emacs.d/init.d/.viper.el")
;; (when (require 'viper nil noerror)
;;   (require 'vimpulse nil 'noerror)
;;   (defalias 'vim:nmap 'vimpulse-map)
;;   (defalias 'vim:omap 'vimpulse-omap)
;;   (defalias 'vim:imap 'vimpulse-imap)
;;   (defalias 'vim:vmap 'vimpulse-vmap)
;;   ;;FIXME: how to add vim:emap in here ?
;;   )

