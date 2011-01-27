
(eval-when-compile (require 'cl))

(yas/initialize)
(yas/load-directory "~/.emacs.d/etc/snippets")
(setq yas/prompt-functions '(yas/dropdown-prompt
			     yas/ido-prompt
			     yas/completing-prompt))


(provide '025-yasnippet)
;; 025-yasnippet.el ends here.
