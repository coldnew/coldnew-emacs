;;; coldnew-minibuffer.el --- minibuffer setting
(eval-when-compile (require 'cl)

;;;; ---------------------------------------------------------------------------
;;;; initial setting
;;;; ---------------------------------------------------------------------------
(setq enable-recursive-minibuffers     t )
(setq max-mini-window-height         .25 ) ; 2 lines high
(setq minibuffer-electric-default-mode t )

;;;; ---------------------------------------------------------------------------
;;;; setup keybindings
;;;; ---------------------------------------------------------------------------
(define-key minibuffer-local-map (kbd "M-w") 'backward-kill-word)
(define-key minibuffer-local-map (kbd "C-w") 'kill-word)
(define-key minibuffer-local-map (kbd "C-u") '(lambda() (interactive) (kill-line 0))))
(define-key minibuffer-local-map (kbd "M-p") 'previous-history-element)
(define-key minibuffer-local-map (kbd "M-n") 'next-history-element)



(provide 'coldnew-minibuffer)
;; coldnew-minibuffer.el ends here.
