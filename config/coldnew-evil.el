;;; coldnew-evil.el ---
(eval-when-compile (require 'cl))


;;;; ---------------------------------------------------------------------------
;;;; evil
;;;; ---------------------------------------------------------------------------
(require 'evil)

;; Global enable evil-mode
(evil-mode t)

;; change default cursor face
(setq evil-default-cursor '(:background (face-attribute 'cursor :background) box))

(setq evil-want-visual-char-semi-exclusive t)

(add-to-list 'evil-emacs-state-modes 'git-branch-mode)
(add-to-list 'evil-emacs-state-modes 'term-mode)
(add-to-list 'evil-emacs-state-modes 'egg-status-buffer-mode)
(add-to-list 'evil-insert-state-modes 'egg-commit-buffer-mode)

;;;; ---------------------------------------------------------------------------
;;;; Commands
;;;; ---------------------------------------------------------------------------

(defun evil-undefine ()
  "commands for undefine evil-keybinding"
  (interactive)
  (let (evil-mode-map-alist)
    (call-interactively (key-binding (this-command-keys)))))



;;;; ---------------------------------------------------------------------------
;;;; Functions
;;;;---------------------------------------------------------------------------



;;;; ---------------------------------------------------------------------------
;;;; Macros
;;;; ---------------------------------------------------------------------------

(defmacro evil-define-key-insert (state map key name)
  "insert string in evil-mode."
  `(evil-define-key ,state ,map ,key
     '(lambda ()
	(interactive) (insert ,name) (if (featurep 'yasnippet) (yas/expand)))))




(provide 'coldnew-evil)
;; coldnew-evil.el ends here.
