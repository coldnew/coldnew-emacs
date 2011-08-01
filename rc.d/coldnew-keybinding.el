;;
(eval-when-compile (require 'cl))


;;;;;;;; Packages Import
(require 'coldnew-macro)
(require 'coldnew-functions)
(require 'coldnew-commands)
(require 'coldnew-variables)
(require 'coldnew-vim)


;;;;;;;; Normal Map
(vim:nmap (kbd "C-x C-b") 'ibuffer)
(vim:nmap (kbd "C-x f") 'anything-find-file)
(vim:nmap (kbd "C-x C-f") 'lusty-file-explorer)
;;(vim:nmap (kbd "M-x") 'anything-execute-extended-command)
(vim:nmap (kbd "M-x") 'anything-M-x)
(vim:nmap (kbd "C-M-X") 'smex)
(vim:nmap (kbd "C-c C-e") 'eval-and-replace)
(vim:nmap (kbd "C-x C-s") 'save-buffer-always)
(vim:nmap (kbd "C-=") 'text-scale-increase)
(vim:nmap (kbd "C-+") 'text-scale-increase)
(vim:nmap (kbd "C--") 'text-scale-decrease)
(vim:nmap (kbd "<f4>") 'shell-pop)
(vim:nmap (kbd "C-x <f4>") 'switch-to-terminal)
(vim:nmap (kbd "C-x M-s") 'egg-status)
(vim:nmap (kbd "C-x M-l") 'egg-log)
(vim:nmap (kbd "C-;") 'iedit-mode)
(vim:nmap (kbd "u")   'undo-tree-undo)
(vim:nmap (kbd "C-r") 'undo-tree-redo)
(vim:nmap (kbd "M-p") 'anything-show-kill-ring)
(vim:nmap (kbd "M-K") 'sdcv-search-pointer+)
(vim:nmap (kbd "C-x M-S") 'scratch-toggle)
(vim:nmap (kbd "C-x M-i") 'ielm-toggle)
(vim:nmap (kbd "<f1>") 'woman)
(vim:nmap (kbd "K") 'anything-man-woman)

;;;;;;;; Insert Map
(vim:imap (kbd "C-x C-b") 'ibuffer)
(vim:imap (kbd "C-x C-f") 'lusty-file-explorer)
(vim:imap (kbd "M-x") 'anything-M-x)
;;(vim:imap (kbd "M-x") 'anything-execute-extended-command)
(vim:imap (kbd "C-c C-e") 'eval-and-replace)
(vim:imap (kbd "C-x C-s") 'save-buffer-always)
(vim:imap (kbd "C-=") 'text-scale-increase)
(vim:imap (kbd "C-+") 'text-scale-increase)
(vim:imap (kbd "C--") 'text-scale-decrease)
(vim:imap (kbd "<f4>") 'shell-pop)
(vim:imap (kbd "C-x <f4>") 'switch-to-terminal)
(vim:imap (kbd "C-x M-s") 'egg-status)
(vim:imap (kbd "C-x M-l") 'egg-log)
(vim:imap (kbd "<backspace>") 'hungry-delete-backward)
(vim:imap (kbd "DEL") 'hungry-delete-forward)
(vim:imap (kbd "C-n") 'auto-complete)
(vim:imap (kbd "C-p") 'auto-complete)
(vim:imap (kbd "C-x M-S") 'scratch-toggle)
(vim:imap (kbd "C-x M-i") 'ielm-toggle)
(vim:imap (kbd "<f1>") 'woman)
(vim:imap (kbd "C-u") 'universal-argument)


;;;;;;; Visual Map
(vim:vmap (kbd "M-;") 'vim:visual-toggle-comment)
(vim:vmap (kbd "C-;") 'iedit-mode)

;;;;;;;; Window Map
(vim:wmap (kbd "C-w f") 'fullscreen-window)
(vim:wmap (kbd "C-w J") 'windmove-down-fullscreen)
(vim:wmap (kbd "C-w H") 'windmove-left-fullscreen)
(vim:wmap (kbd "C-w L") 'windmove-right-fullscreen)
(vim:wmap (kbd "C-w K") 'windmove-up-fullscreen)

(vim:wmap (kbd "C-w C-s") 'sr-speedbar-toggle)
(vim:wmap (kbd "C-w C-d") 'nav-toggle)


;;;;;;;; Emacs Map
(global-set-key (kbd "<f1>") 'woman)
(global-set-key (kbd "<f4>") 'shell-pop)
(global-set-key (kbd "C-x <f4>") 'switch-to-terminal)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C-M-X") 'smex)
(global-set-key (kbd "C-c C-e") 'eval-and-replace)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-s") 'save-buffer-always)
(global-set-key (kbd "C-x M-l") 'egg-log)
(global-set-key (kbd "C-x M-s") 'egg-status)
(global-set-key (kbd "M-x") 'anything-M-x)
(global-set-key (kbd "C-x M-S") 'scratch-toggle)
(global-set-key (kbd "C-x M-i") 'ielm-toggle)

(provide 'coldnew-keybinding)
;; coldnew-keybinding.el ends here.
