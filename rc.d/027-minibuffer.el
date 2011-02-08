
;;;;;; Settings
(setq enable-recursive-minibuffers t)
(setq max-mini-window-height .25)	; 2 lines high

;;;;;; Keybindings
(add-hook 'minibuffer-setup-hook
	  '(lambda ()
	     (local-set-key (kbd "C-b")   'backward-kill-word)
	     (local-set-key (kbd "C-w")   'kill-word)
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



(provide '027-minibuffer)
;; 027-minibuffer.el ends here.
