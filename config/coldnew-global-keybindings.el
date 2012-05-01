;;; coldnew-global-keybindings.el --- all global-keybindings setting

(eval-when-compile (require 'cl))

;;;;;;;; Normal
(define-key evil-normal-state-map (kbd "C-c C-f") 'my-anything-filelist)
(define-key evil-normal-state-map (kbd "C-s") 'my-anything-occur)
(define-key evil-normal-state-map (kbd "C-x vv") 'egg-next-action)
(define-key evil-normal-state-map (kbd "C-w") 'one-key-menu-window-navigation)
(define-key evil-normal-state-map (kbd "M-p") 'anything-show-kill-ring)
(define-key evil-normal-state-map (kbd "C-=") 'text-scale-increase)
(define-key evil-normal-state-map (kbd "C--") 'text-scale-decrease)
(define-key evil-normal-state-map (kbd "C-\'") 'er/expand-region)
(define-key evil-normal-state-map (kbd "C-0") 'ace-jump-mode)

(define-key evil-normal-state-map (kbd "C-n") 'next-line)
(define-key evil-normal-state-map (kbd "C-p") 'previous-line)


;;;;;;;; Insert
(define-key evil-insert-state-map (kbd "C-n") 'next-line)
(define-key evil-insert-state-map (kbd "C-p") 'previous-line)
(define-key evil-insert-state-map (kbd "C-b") 'backward-char)
(define-key evil-insert-state-map (kbd "C-f") 'forward-char)
(define-key evil-insert-state-map (kbd "M-f") 'forward-word)
(define-key evil-insert-state-map (kbd "M-b") 'backward-word)

(define-key evil-insert-state-map (kbd "C-l") 'hungry-delete-backward)
(define-key evil-insert-state-map (kbd "M-l") 'backward-delete-word)

(define-key evil-insert-state-map (kbd "C-x C-n") 'auto-complete)
(define-key evil-insert-state-map (kbd "C-s") 'my-anything-occur)
(define-key evil-insert-state-map (kbd "C-c C-f") 'my-anything-filelist)
(define-key evil-insert-state-map (kbd "C-x vv") 'egg-next-action)
(define-key evil-insert-state-map (kbd "M-p") 'anything-show-kill-ring)
(define-key evil-insert-state-map (kbd "C-\'") 'er/expand-region)
(define-key evil-insert-state-map (kbd "C-0") 'ace-jump-mode)

(key-chord-define evil-insert-state-map "vv"  'er/expand-region)
;; quickliy back-word and upcase/downcaseword
(key-chord-define evil-insert-state-map "bu"  'upcase-word-backward)
(key-chord-define evil-insert-state-map "bl"  'downcase-word-backward)
(key-chord-define evil-insert-state-map "bc"  'capitalize-word-backward)

(define-key evil-emacs-state-map (kbd "<escape>") 'evil-normal-state)
(define-key evil-emacs-state-map (kbd "C-s") 'my-anything-occur)
(define-key evil-emacs-state-map (kbd "C-c C-f") 'my-anything-filelist)
(define-key evil-emacs-state-map (kbd "C-x vv") 'egg-next-action)
(define-key evil-emacs-state-map (kbd "M-p") 'anything-show-kill-ring)
(define-key evil-emacs-state-map (kbd "C-=") 'text-scale-increase)
(define-key evil-emacs-state-map (kbd "C--") 'text-scale-decrease)
(define-key evil-emacs-state-map (kbd "C-\'") 'er/expand-region)
(define-key evil-emacs-state-map (kbd "C-0") 'ace-jump-mode)


;;;; ---------------------------------------------------------------------------
;;;; paredit-mode
;;;; ---------------------------------------------------------------------------




;; insert
(define-key evil-insert-state-map "(" 'paredit-open-round)
(define-key evil-insert-state-map ")" 'paredit-close-round)
(define-key evil-insert-state-map "[" 'paredit-open-square)
(define-key evil-insert-state-map "]" 'paredit-close-square)
(define-key evil-insert-state-map "{" 'paredit-open-curly)
(define-key evil-insert-state-map "}" 'paredit-close-curly)
(define-key evil-insert-state-map (kbd "\"")  'paredit-doublequote)
(define-key evil-insert-state-map (kbd "<delete>") 'paredit-forward-delete)
(define-key evil-insert-state-map (kbd "<backspace>") 'paredit-backward-delete)
(define-key evil-insert-state-map (kbd "C-d") 'paredit-forward-delete)
(define-key evil-insert-state-map (kbd "C-l") 'paredit-backward-delete)
(define-key evil-insert-state-map (kbd "M-d") 'paredit-forward-kill-word)
(define-key evil-insert-state-map (kbd "M-l") 'paredit-backward-kill-word)
(define-key evil-insert-state-map (kbd "M-0") 'paredit-close-round-and-newline)
(define-key evil-insert-state-map (kbd "M-\'") 'paredit-meta-doublequote)
(define-key evil-insert-state-map (kbd "C-k") 'paredit-kill)

(key-chord-define evil-insert-state-map "fp"  'paredit-forward)
(key-chord-define evil-insert-state-map "bp"  'paredit-backward)



;;;; ---------------------------------------------------------------------------
;;;; function-key
;;;; ---------------------------------------------------------------------------
(global-set-key (kbd "<f1>")     'woman)
(global-set-key (kbd "<f2>")     'shell-pop)
(global-set-key (kbd "C-x <f2>") 'multi-term)
(global-set-key (kbd "<f3>")     'call-last-kbd-macro)
(global-set-key (kbd "C-x <f3>") 'toggle-kbd-macro-recording-on)
(global-set-key (kbd "<f4>")     'sr-speedbar-toggle)


;;;; ---------------------------------------------------------------------------
;;;; Combination key
;;;; ---------------------------------------------------------------------------


(global-set-key (kbd "C-x C-f")  'lusty-file-explorer)
(global-set-key (kbd "C-x C-b")  'ibuffer)
(global-set-key (kbd "C-;")      'iedit-mode)
(global-set-key (kbd "C-x C-d")  'dired)
(global-set-key (kbd "C-x C-r")  'sudo-edit)
(global-set-key (kbd "C-x ,") 'undo-tree-undo)
(global-set-key (kbd "C-x .") 'undo-tree-redo)
;; (global-set-key (kbd "C-w")   'kill-word)
;; (global-set-key (kbd "M-w")   'backward-kill-word)
(global-set-key (kbd "C-g")   'keyboard-quit)
(global-set-key (kbd "C-c C-f") 'my-anything-filelist)
(global-set-key (kbd "<escape>") 'evil-mode)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-x M-x") 'anything-M-x)

(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-below)
(global-set-key (kbd "M-3") 'split-window-right)
(global-set-key (kbd "M-4") 'delete-window)
(global-set-key (kbd "M-0") 'other-window)

(global-set-key (kbd "C-x C-l") 'recenter-top-bottom)


;;;; ---------------------------------------------------------------------------
;;;; one-key
;;;; --------------------------------------------------------------------------
(global-set-key (kbd "C-x M-w") 'one-key-menu-window-navigation)

(global-set-key (kbd "C-w") 'one-key-menu-window-navigation)
(global-set-key (kbd "C-x M-f") 'one-key-menu-file-handle)



(provide 'coldnew-global-keybindings)
;; coldnew-global-keybindings.el ends here.
