
(when (require 'xcscope nil 'noerror)
  (setq cscope-do-not-update-database nil) ; 隨時更新 database

  (vim:nmap (kbd "C-, s") 'cscope-find-this-symbol)
  (vim:nmap (kbd "C-, g") 'cscope-find-global-definition)
  (vim:nmap (kbd "C-, G") 'cscope-find-global-definition-no-prompting)

  )





(provide 'rc-cscope)
;; rc-cscope.el ends here
