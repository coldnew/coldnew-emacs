;;
(eval-when-compile (require 'cl))

;;;;;;;; Packages Import
(require 'coldnew-macro)
(require 'coldnew-functions)
(require 'coldnew-commands)
(require 'coldnew-variables)
(require 'coldnew-vim)



(when (require* 'woman)
  (setq woman-use-own-frame nil)
  (setq woman-cache-filename (concat emacs-cache-dir "woman.cache"))
  (setq woman-use-topic-at-point nil)
  ;; Colorful fonts
  (setq woman-fontify t)
  (setq woman-fill-column 100)
  )

(when (require* 'info)

  ;; Keybindings
  (add-hook 'Info-mode-hook
	    '(lambda ()
	       (vim:nmap (kbd "j")  'Info-next)
	       (vim:nmap (kbd "k") 'Info-prev)
	       (vim:nmap (kbd "C-f") 'Info-scroll-up)
	       (vim:nmap (kbd "C-b") 'Info-scroll-down)
	       ))
  )

(provide 'coldnew-help)
;; coldnew-help.el ends here.
