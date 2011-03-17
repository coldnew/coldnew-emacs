;;
(eval-when-compile (require 'cl))

;;;;;;;; Packages Import
(require 'coldnew-macro)
(require 'coldnew-functions)
(require 'coldnew-commands)
(require 'coldnew-variables)

;;;;;;;; Variables
(defvar emacs-default-shell "/bin/bash"
  "Default shell for cemacs.")
(defvar emacs-popup-shell-window-height 20
  "Window hight of popup shell.")
(defvar emacs-popup-shell-window-position "bottom"
  "Make popup shell window at buttom by default.")

;;;;;;;; Settings
;; Shell-pop
(when (require* 'shell-pop)
  (shell-pop-set-internal-mode "ansi-term")
  (shell-pop-set-internal-mode-shell emacs-default-shell)
  (shell-pop-set-window-height emacs-popup-shell-window-height)
  (shell-pop-set-window-position emacs-popup-shell-window-position)
  )

;; Multi-term
(setq multi-term-program emacs-default-shell)

;; Term
(setq term-default-bg-color nil)
(setq term-default-fg-color nil)

;;;; Keybindings
(add-hook 'term-mode-hook
	  '(lambda ()
	     (when (require* 'vim)
	       (vim:local-nmap (kbd "C-p") 'term-send-up)
	       (vim:local-nmap (kbd "C-n") 'term-send-down)
	       (vim:local-imap (kbd "<f4>") 'shell-pop)
	       (vim:local-imap (kbd "RET") 'term-send-raw)
	       )))


(provide 'coldnew-terminal)
;; coldnew-terminal.el ends here.
