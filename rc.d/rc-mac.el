;; init for macosx

(provide 'rc-mac)


(cond (mac-p
       ;; Add binary PATH for Mac OSX
       (add-to-list 'exec-path "~/Gentoo/bin")
       (add-to-list 'exec-path "/usr/local/bin/")
       (add-to-list 'exec-path "/opt/local/bin/")
       (add-to-list 'exec-path "/usr/bin/")
       (add-to-list 'exec-path "/usr/X11/bin/")
       ))
