;;
(provide 'rc-cedet)



;;;; Shell Popup
(when (require 'shell-pop nil 'noerror)
  (shell-pop-set-internal-mode "ansi-term")
  (shell-pop-set-internal-mode-shell "/bin/bash")
  (shell-pop-set-window-height 20)
  (shell-pop-set-window-position "bottom")
;;  (global-set-key (kbd "<f3>") 'shell-pop)
  (when (require 'rc-vim-mode nil 'noerror)
    (vim:nmap (kbd "<f3>") 'shell-pop)))


(when (require 'cedet nil 'noerror)
  )
