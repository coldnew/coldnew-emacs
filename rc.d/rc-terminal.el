;; init for terminal

(provide 'rc-terminal)


(when (require 'multi-term nil 'noerror)

  (setq multi-term-program "/bin/bash")

  )

;;;; Shel-pop
(when (require 'shell-pop nil 'noerror)
;;;; Kebinding
  ;; Emacs Kyebinding
  (global-set-key (kbd "<f3>") 'shell-pop)
  ;; Keybinding for vim-mode or Viper
  (when (require 'rc-vim nil 'noerror)
    (vim:nmap (kbd "<f3>") 'shell-pop)))
