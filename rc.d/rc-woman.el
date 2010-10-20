;; init woman
(provide 'rc-woman)

(when (require 'woman nil 'noerror)
  (setq woman-cache-filename 
	(expand-file-name "~/.emacs.d/var/cache/woman.cache"))
  (setq woman-use-own-frame nil))
