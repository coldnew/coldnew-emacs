;;;; initial vim-mode
(provide 'rc-vim-mode)



;; because vim-mode has many bugs , use viper and vimpulse instead
;;;;; Following are viper and vimpulse setting
(setq viper-mode t)
(setq viper-custom-file-name "~/.emacs.d/rc.d/.viper.el")
(when (require 'viper nil noerror)
  (require 'vimpulse nil 'noerror)
  (require 'vimpulse-surround nil 'noerror)
  ;; make alias if one day we wish move to vim-mode
  ;; this will make things easily
  (defalias 'vim:nmap 'vimpulse-map)
  (defalias 'vim:omap 'vimpulse-omap)
  (defalias 'vim:imap 'vimpulse-imap)
  (defalias 'vim:vmap 'vimpulse-vmap)
  (defalias 'vim:local-nmap 'vimpulse-map-local)
  (defalias 'vim:local-imap 'vimpulse-imap-local)
  (defalias 'vim:local-vmap 'vimpulse-vmap-local)
  (defalias 'vim:local-omap 'vimpulse-omap-local)
  ;;   ;;FIXME: how to add vim:emap in here ?

  (when (require 'hungry-delete nil 'noerror)
    (define-key viper-insert-global-user-map (kbd "<backspace>") 'hungry-delete-backward))

  ;; Fix ibuffer-mode
  (add-hook 'ibuffer-mode-hook
	    '(lambda ()
	       (local-set-key (kbd "j") 'next-line)
	       (local-set-key (kbd "k") 'previous-line)
	       (local-set-key (kbd "q") 'quit-window)))
  (add-hook 'org-agenda-mode-hook
	    '(lambda ()
	       (local-set-key (kbd "C-w j") 'windmove-down)
	       (local-set-key (kbd "C-w k") 'windmove-up)
	       ))
  )

(defcmd window-fullscreen ()
  "Make the window full-screen."
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
			 (if (equal 'fullboth current-value)
			     (if (boundp 'old-fullscreen) old-fullscreen nil)
			   (progn (setq old-fullscreen current-value)
				  'fullboth)))))
;;FIXME:in vim-mode , we use vim:wmap
(vim:nmap (kbd "C-w f") 'window-fullscreen)


(when (require 'undo-tree nil 'noerror)
  (global-undo-tree-mode)
  (vim:nmap (kbd "u") 'undo-tree-undo)
  (vim:nmap (kbd "C-r") 'undo-tree-redo))


;;;;FIXME: bug on vim-mode
;;(when (require 'vim nil 'noerror)
;;(vim-mode)

;; keymapping

;;  (vim:imap (kbd "RET") 'newline-and-indent)


;; tmp usage setting
;; (vim:defcmd vim:visual-toggle-comment (motion)
;;   "Toggles comments in the region."
;;   (comment-or-uncomment-region (vim:motion-begin-pos motion)
;;				 (vim:motion-end-pos motion)))
;; (vim:vmap (kbd "M-;") 'vim:visual-toggle-comment)


;; )
