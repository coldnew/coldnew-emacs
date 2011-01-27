
(eval-when-compile (require 'cl))

(yas/initialize)
(yas/load-directory "~/.emacs.d/etc/snippets")
(setq yas/prompt-functions '(yas/dropdown-prompt
			     yas/ido-prompt
			     yas/completing-prompt))

;; Auto add TEMPLATE in new file
(add-hook 'find-file-hook
	  '(lambda ()
	     (when (and (buffer-file-name)
			(not (file-exists-p (buffer-file-name)))
			(= (point-max) 1))
	       (insert "TEMPLATE")
	       (yas/expand))))


(provide '025-yasnippet)
;; 025-yasnippet.el ends here.
