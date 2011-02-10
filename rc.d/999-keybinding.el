
;; Normal Map
(vim:nmap (kbd "<f1>") 'woman)
(vim:nmap (kbd "<f4>") 'shell-pop)
(vim:nmap (kbd "C-c C-e") 'eval-and-replace)
(vim:nmap (kbd "C-r") 'undo-tree-redo)
(vim:nmap (kbd "C-x C-b") 'ibuffer)
(vim:nmap (kbd "C-x C-f") 'lusty-file-explorer)
(vim:nmap (kbd "C-x C-s") 'save-buffer-always)
(vim:nmap (kbd "C-x b") 'lusty-buffer-explorer)
(vim:nmap (kbd "C-x f") 'anything-for-files)
(vim:nmap (kbd "K") 'woman)
(vim:nmap (kbd "M-p") 'anything-show-kill-ring)
(vim:nmap (kbd "M-x") 'smex)
(vim:nmap (kbd "u") 'undo-tree-undo)
(vim:nmap (kbd "z.") 'find-file-at-point)

;; Insert Map
(vim:imap (kbd "C-c C-e") 'eval-and-replace)
(vim:imap (kbd "C-n") 'auto-complete)
(vim:imap (kbd "C-p") 'auto-complete)
(vim:imap (kbd "C-x C-b") 'ibuffer)
(vim:imap (kbd "C-x C-f") 'lusty-file-explorer)
(vim:imap (kbd "C-x C-s") 'save-buffer-always)
(vim:imap (kbd "C-x b") 'lusty-buffer-explorer)
(vim:imap (kbd "C-x f") 'anything-for-files)
(vim:imap (kbd "M-x") 'smex)
(vim:imap (kbd "RET") 'newline-and-indent)


;; Visual Map
(vim:vmap (kbd "M-;") 'vim:visual-toggle-comment)

;; Windows Map
(vim:wmap (kbd "C-w f") 'window-fullscreen)
(vim:wmap (kbd "C-x C-b") 'ibuffer)
(vim:wmap (kbd "C-x f") 'anything-for-files)
(vim:wmap (kbd "M-x") 'smex)

;; FIXME: I send this to author, one day to remove
(vim:mmap (kbd "<left>") 'vim:motion-left)
(vim:mmap (kbd "<right>") 'vim:motion-right)
(vim:mmap (kbd "<up>") 'vim:motion-up)
(vim:mmap (kbd "<down>") 'vim:motion-down)

(provide '999-keybinding)
;; 999-keybinding.el ends here.
