;;
(eval-when-compile (require 'cl))


;;;;;; Settings
(setq enable-recursive-minibuffers t)
(setq max-mini-window-height .25)	; 2 lines high
(setq-default minibuffer-electric-default-mode t )

;;;;;; Keybindings
(add-hook 'minibuffer-setup-hook
	  '(lambda ()
	     (local-set-key (kbd "C-b")   'backward-kill-word)
	     (local-set-key (kbd "C-w")   'kill-word)
	     (local-set-key (kbd "C-u")   '(lambda () (interactive) (kill-line 0)))
	     (local-set-key (kbd "M-J")   'next-matching-history-element)
	     (local-set-key (kbd "M-K")   'previous-matching-history-element)
	     (local-set-key (kbd "M-b")   'backward-word)
	     (local-set-key (kbd "M-g")   'minibuffer-keyboard-quit)
	     (local-set-key (kbd "M-h")   'backward-char)
	     (local-set-key (kbd "M-j")   'next-history-element)
	     (local-set-key (kbd "M-k")   'previous-history-element)
	     (local-set-key (kbd "M-l")   'forward-char)
	     (local-set-key (kbd "M-w")   'forward-word)
	     ))


;; ;; Abort the minibuffer when using mouse
;; (add-hook 'mouse-leave-buffer-hook
;;	  '(lambda ()
;;	     (when (and (>= (recursion-depth) 1)
;;			(active-minibuffer-window))
;;	       (abort-recursive-edit))))

;;;;;; Functions
(defun switch-to-minibuffer-window ()
  "switch to minibuffer window (if active)"
  (interactive)
  (when (active-minibuffer-window)
    (select-window (active-minibuffer-window))))

;;;;;;;; Smex
(when (require* 'smex)
  ;; initialize smex
  (smex-initialize)

  ;; File that svae smex state
  ;;(setq smex-save-file "~/.emacs.d/var/cache/smex.cache")
  (setq smex-save-file (concat emacs-cache-dir "smex.cache"))

  ;; (defadvice smex (around make-aything-do-not-fighting-with activate)
  ;;   "Make anything won't conflict with smex."
  ;;   (when (require* 'anything)
  ;;     (anything-read-string-mode 0)
  ;;     ad-do-it
  ;;     ))
  )

(provide 'coldnew-minibuffer)
;; coldnew-minibuffer.el ends here.
