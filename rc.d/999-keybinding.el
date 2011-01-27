
;; Normal Map
(vim:nmap (kbd "<f1>") 'woman)
(vim:nmap (kbd "<f3>") 'shell-pop)
(vim:nmap (kbd "C-r") 'undo-tree-redo)
(vim:nmap (kbd "C-x C-b") 'ibuffer)
(vim:nmap (kbd "K") 'woman)
(vim:nmap (kbd "u") 'undo-tree-undo)
(vim:nmap (kbd "C-x C-f") 'lusty-file-explorer)
(vim:nmap (kbd "C-x b") 'lusty-buffer-explorer)

;; Insert Map
(vim:imap (kbd "C-n") 'auto-complete)
;;(vim:imap (kbd "C-n") 'ac-start)
(vim:imap (kbd "C-p") 'auto-complete)
(vim:imap (kbd "C-x C-b") 'ibuffer)
(vim:imap (kbd "RET") 'newline-and-indent)
(vim:imap (kbd "C-x C-f") 'lusty-file-explorer)
(vim:imap (kbd "C-x b") 'lusty-buffer-explorer)

;; Visual Map
(vim:vmap (kbd "M-;") 'vim:visual-toggle-comment)
;; Windows Map
(vim:wmap (kbd "C-w f") 'window-fullscreen)




(provide '999-keybinding)
;; 999-keybinding.el ends here.
