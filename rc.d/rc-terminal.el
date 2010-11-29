;; init for terminal

(provide 'rc-terminal)


(when (require 'multi-term nil 'noerror)

  (setq multi-term-program "/bin/bash")

  )


(when (require 'shell-pop nil 'noerror)
  (when (require 'rc-vim nil 'noerror)
    (vim:nmap (kbd "<f3>") 'shell-pop)))
