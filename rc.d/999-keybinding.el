
;; Normal Map
(vim:nmap (kbd "<f1>") 'woman)
(vim:nmap (kbd "<f3>") 'shell-pop)
(vim:nmap (kbd "C-r") 'undo-tree-redo)
(vim:nmap (kbd "C-x C-b") 'ibuffer)
(vim:nmap (kbd "C-x C-f") 'lusty-file-explorer)
(vim:nmap (kbd "C-x b") 'lusty-buffer-explorer)
(vim:nmap (kbd "C-x f") 'anything-for-files)
(vim:nmap (kbd "K") 'woman)
(vim:nmap (kbd "M-p") 'anything-show-kill-ring)
(vim:nmap (kbd "u") 'undo-tree-undo)
(vim:nmap (kbd "C-x k") 'kill-buffer)


;; Insert Map
(vim:imap (kbd "C-n") 'auto-complete)
(vim:imap (kbd "C-p") 'auto-complete)
(vim:imap (kbd "C-x C-b") 'ibuffer)
(vim:imap (kbd "RET") 'newline-and-indent)
(vim:imap (kbd "C-x C-f") 'lusty-file-explorer)
(vim:imap (kbd "C-x b") 'lusty-buffer-explorer)
(vim:imap (kbd "C-x f") 'anything-for-files)


;; Visual Map
(vim:vmap (kbd "M-;") 'vim:visual-toggle-comment)
;; Windows Map
(vim:wmap (kbd "C-w f") 'window-fullscreen)
(vim:wmap (kbd "C-x C-b") 'ibuffer)
(vim:wmap (kbd "C-x f") 'anything-for-files)




(provide '999-keybinding)
;; 999-keybinding.el ends here.
