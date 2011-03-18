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

;;;;;;;; Shell-pop
(when (require* 'shell-pop)
  (shell-pop-set-internal-mode "ansi-term")
  (shell-pop-set-internal-mode-shell emacs-default-shell)
  (shell-pop-set-window-height emacs-popup-shell-window-height)
  (shell-pop-set-window-position emacs-popup-shell-window-position)
  )

;;;;;;;; Multi-term
(when (require* 'multi-term)
  (setq multi-term-program emacs-default-shell))

;;;;;;;; Term
(setq term-default-bg-color nil)
(setq term-default-fg-color nil)

;;;;;;;;;; Keybindings
(add-hook 'term-mode-hook
	  '(lambda ()
	     (define-key term-raw-map (kbd "<f4>") 'shell-pop)
	     ))

;;;;;;;; Functions

;;;; Rewrite shell-pop function
;; I don't like use vim like key in shell,after shell popup
;; use emacs-key instead of vim-mode
;; TODO: use advice to rewrite this function.
;;
(defun shell-pop ()
  "Toggle vim-mode between shell-pop-up and shell-pop-down."
  (interactive)
  (if (equal (buffer-name) shell-pop-internal-mode-buffer)
      (progn
	(shell-pop-out)
	(vim-mode))
    (progn
      (shell-pop-up)
      (vim-mode -1))))



(provide 'coldnew-terminal)
;; coldnew-terminal.el ends here.
