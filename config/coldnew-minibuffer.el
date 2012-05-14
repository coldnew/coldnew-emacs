;;; coldnew-minibuffer.el --- minibuffer setting
(eval-when-compile (require 'cl))

;;;; ---------------------------------------------------------------------------
;;;; initial setting
;;;; ---------------------------------------------------------------------------
(setq enable-recursive-minibuffers     t )
(setq max-mini-window-height         .25 ) ; 2 lines high
(setq minibuffer-electric-default-mode t )

;;;; ---------------------------------------------------------------------------
;;;; Hooks
;;;; ---------------------------------------------------------------------------

;; Abort the minibuffer when using the mouse
(add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)

;;;; ---------------------------------------------------------------------------
;;;; setup keybindings
;;;; ---------------------------------------------------------------------------
(define-key minibuffer-local-map (kbd "M-l") 'backward-kill-word)
(define-key minibuffer-local-map (kbd "C-w") 'kill-word)
(define-key minibuffer-local-map (kbd "C-u") '(lambda() (interactive) (kill-line 0)))
(define-key minibuffer-local-map (kbd "M-p") 'previous-history-element)
(define-key minibuffer-local-map (kbd "M-n") 'next-history-element)
(define-key minibuffer-local-map (kbd "C-g") 'minibuffer-keyboard-quit)
(define-key minibuffer-local-map (kbd "C-r") 'resolve-sym-link)

;;;; ---------------------------------------------------------------------------
;;;; smex
;;;; ---------------------------------------------------------------------------
(require* 'smex)
(smex-initialize)
(setq smex-save-file (concat emacs-cache-dir "smex.dat"))

;;;; ---------------------------------------------------------------------------
;;;; ido
;;;; ---------------------------------------------------------------------------
(require 'ido)

;;;; functions
(defun coldnew/ido-setup-keybinding ()
  ;; use C-f as ido-next-match
  (define-key ido-completion-map (kbd "C-f") 'ido-next-match)
  )

;;;; hooks
(add-hook 'ido-setup-hook 'coldnew/ido-setup-keybinding)

;;;; ---------------------------------------------------------------------------
;;;; Functions
;;;; ---------------------------------------------------------------------------

(defun stop-using-minibuffer ()
  "kill the minibuffer"
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))

;;;; ---------------------------------------------------------------------------
;;;; Commands
;;;; ---------------------------------------------------------------------------
(defun resolve-sym-link ()
  "Replace the string at the point with the true path."
  (interactive)
  (beginning-of-line)
  (let* ((file (buffer-substring (point)
				 (save-excursion (end-of-line) (point))))
	 (file-dir (file-name-directory file))
	 (file-true-dir (file-truename file-dir))
	 (file-name (file-name-nondirectory file)))
    (delete-region (point) (save-excursion (end-of-line) (point)))
    (insert (concat file-true-dir file-name))))



(provide 'coldnew-minibuffer)
;; coldnew-minibuffer.el ends here.
