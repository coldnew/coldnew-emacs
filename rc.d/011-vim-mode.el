;;;; initial vim-mode

(vim-mode)				; start vim-mode

;; Global Keymapping
(vim:imap (kbd "RET") 'newline-and-indent)
(vim:vmap (kbd "M-;") 'vim:visual-toggle-comment)
(vim:wmap (kbd "C-w f") 'window-fullscreen)
(vim:nmap (kbd "<f1>") 'woman)
(vim:nmap (kbd "K") 'woman)
(vim:nmap (kbd "C-x C-b") 'ibuffer)
(vim:imap (kbd "C-x C-b") 'ibuffer)
(vim:nmap (kbd "<f3>") 'shell-pop)

(when (require 'undo-tree nil 'noerror)
  (global-undo-tree-mode)
  (vim:nmap (kbd "u") 'undo-tree-undo)
  (vim:nmap (kbd "C-r") 'undo-tree-redo))

  ;;;; Functions
(vim:defcmd vim:visual-toggle-comment (motion)
  "Toggles comments in the region."
  (comment-or-uncomment-region (vim:motion-begin-pos motion)
			       (vim:motion-end-pos motion)))

(defcmd window-fullscreen ()
  "Make the window full-screen."
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
			 (if (equal 'fullboth current-value)
			     (if (boundp 'old-fullscreen) old-fullscreen nil)
			   (progn (setq old-fullscreen current-value)
				  'fullboth)))))

(provide '011-vim-mode)
;; 011-vim-mode.el ends here.
