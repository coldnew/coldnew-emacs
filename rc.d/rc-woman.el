;; init woman
(provide 'rc-woman)

(when (require 'woman nil 'noerror)
  (setq woman-cache-filename
	(expand-file-name "~/.emacs.d/var/cache/woman.cache"))
  (setq woman-use-own-frame nil)

  (when (featurep 'vim)
    (vim:nmap (kbd "<f1>") 'woman)
    (vim:nmap (kbd "K") 'woman))

  )
