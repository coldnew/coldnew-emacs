;;;; initial vim-mode
(provide 'rc-vim-mode)

;; because vim-mode has many bugs , use viper and vimpulse instead
(when (require 'vim nil 'noerror)
  (vim-mode)

  ;; keymapping
  (when (require 'undo-tree nil 'noerror)
    (global-undo-tree-mode)
    (vim:nmap (kbd "u") 'undo-tree-undo)
    (vim:nmap (kbd "C-r") 'undo-tree-redo))

;;  (vim:imap (kbd "RET") 'newline-and-indent)


  ;; tmp usage setting
  (vim:defcmd vim:visual-toggle-comment (motion)
    "Toggles comments in the region."
    (comment-or-uncomment-region (vim:motion-begin-pos motion)
				 (vim:motion-end-pos motion)))
  (vim:vmap (kbd "M-;") 'vim:visual-toggle-comment)

  (vim:defcmd vim:window-fullscreen (nonrepeatable)
    "Make the window full-screen."
    (let ((current-value (frame-parameter nil 'fullscreen)))
      (set-frame-parameter nil 'fullscreen
			   (if (equal 'fullboth current-value)
			       (if (boundp 'old-fullscreen) old-fullscreen nil)
			     (progn (setq old-fullscreen current-value)
				    'fullboth)))))

  (vim:wmap (kbd "C-w f") 'vim:window-fullscreen)

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
