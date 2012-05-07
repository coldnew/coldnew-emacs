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

;;; add all mode in emacs-state to insert state
(dolist (mode evil-emacs-state-modes)
  (add-to-list 'evil-insert-state-modes mode))


;; do not use evil-insert map keybinding, use emacs' keybinding
(setcdr evil-insert-state-map nil)
(define-key evil-insert-state-map
  (read-kbd-macro evil-toggle-key) 'evil-emacs-state)

;;; default state set to insert
(setq evil-default-state 'normal)

;;;; ---------------------------------------------------------------------------
;;;; state
;;;; ---------------------------------------------------------------------------


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

(defmacro evil:make-keymap (state key name)
  "Creates a new keymap with a certain `parent' keymap."
  `(define-key ,state ,key ,name))

(defmacro evil:nmap (key name)
  "mapping in the normal-mode keymap"
  `(evil:make-keymap evil-normal-state-map ,key ,name))

(defmacro evil:imap (key name)
  "mapping in the insert-mode keymap"
  `(evil:make-keymap evil-insert-state-map ,key ,name))

(defmacro evil:local-imap (keymap key name)
  "mapping in the insert-mode local keymap"
  `(evil-define-key 'insert ,keymap ,key ,name))


(evil:imap (kbd "<escape>") 'evil-normal-state)

(provide 'coldnew-evil)
;; coldnew-evil.el ends here.
