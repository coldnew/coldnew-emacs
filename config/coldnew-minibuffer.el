;;; coldnew-minibuffer.el --- minibuffer setting
(eval-when-compile (require 'cl))

;;;; ---------------------------------------------------------------------------
;;;; initial setting
;;;; ---------------------------------------------------------------------------
(setq enable-recursive-minibuffers     t )
(setq max-mini-window-height         .25 ) ; 2 lines high
(setq minibuffer-electric-default-mode t )

;;;; ---------------------------------------------------------------------------
;;;; setup keybindings
;;;; ---------------------------------------------------------------------------
(define-key minibuffer-local-map (kbd "M-l") 'backward-kill-word)
(define-key minibuffer-local-map (kbd "C-w") 'kill-word)
(define-key minibuffer-local-map (kbd "C-u") '(lambda() (interactive) (kill-line 0)))
(define-key minibuffer-local-map (kbd "M-p") 'previous-history-element)
(define-key minibuffer-local-map (kbd "M-n") 'next-history-element)
(define-key minibuffer-local-map (kbd "C-g") 'minibuffer-keyboard-quit)

;;;; ---------------------------------------------------------------------------
;;;; smex
;;;; ---------------------------------------------------------------------------
(require* 'smex)
(smex-initialize)
(setq smex-save-file (concat emacs-cache-dir "smex.dat"))


(provide 'coldnew-minibuffer)
;; coldnew-minibuffer.el ends here.
