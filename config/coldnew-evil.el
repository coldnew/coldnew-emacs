;;; coldnew-evil.el ---
(eval-when-compile (require 'cl))


;;;; ---------------------------------------------------------------------------
;;;; evil
;;;; ---------------------------------------------------------------------------
(require 'evil)

;; change default cursor face
(setq evil-default-cursor '(:background (face-attribute 'cursor :background) box))

(setq evil-want-visual-char-semi-exclusive t)

(define-key evil-emacs-state-map (kbd "<escape>") 'evil-normal-state)
(define-key evil-emacs-state-map (kbd "C-[") 'evil-normal-state)

;;;; ---------------------------------------------------------------------------
;;;; Commands
;;;; ---------------------------------------------------------------------------

(defun evil-undefine ()
  "commands for undefine evil-keybinding"
  (interactive)
  (let (evil-mode-map-alist)
    (call-interactively (key-binding (this-command-keys)))))




(provide 'coldnew-evil)
;; coldnew-evil.el ends here.
