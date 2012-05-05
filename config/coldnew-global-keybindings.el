;;; coldnew-global-keybindings.el --- all global-keybindings setting

(eval-when-compile (require 'cl))

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
;;;; Combination key (Ctrl prefix)
;;;; ---------------------------------------------------------------------------
(global-set-key (kbd "C-n") 'next-line)
(global-set-key (kbd "C-p") 'previous-line)
(global-set-key (kbd "C-b") 'backward-char)
(global-set-key (kbd "C-f") 'forward-char)
(global-set-key (kbd "C-l") 'hungry-delete-backward)
(global-set-key (kbd "C-;") 'iedit-mode)
(global-set-key (kbd "C-v") 'cua-set-rectangle-mark)

;;;; ---------------------------------------------------------------------------
;;;; Combination key (Ctrl-x prefix)
;;;; ---------------------------------------------------------------------------

(global-set-key (kbd "C-x C-f")  'lusty-file-explorer)
(global-set-key (kbd "C-x C-b")  'ibuffer)
(global-set-key (kbd "C-x C-d")  'dired)
(global-set-key (kbd "C-x C-r")  'sudo-edit)
(global-set-key (kbd "C-x ,") 'undo-tree-undo)
(global-set-key (kbd "C-x vv") 'egg-next-action)
(global-set-key (kbd "C-x M-x") 'anything-M-x)
(global-set-key (kbd "C-x o")   'switch-window)
(global-set-key (kbd "C-x C-l") 'recenter-top-bottom)


;;;; ---------------------------------------------------------------------------
;;;; Combination key (Ctrl-c prefix)
;;;; ---------------------------------------------------------------------------
(global-set-key (kbd "C-c C-f") 'my-anything-filelist)
(global-set-key (kbd "C-c C-s") 'save-buffer-always)
(global-set-key (kbd "C-c ,") 'undo-tree-redo)

;;;; ---------------------------------------------------------------------------
;;;; Combination key (Ctrl-Meta prefix)
;;;; ---------------------------------------------------------------------------
(global-set-key (kbd "C-M-,") 'beginning-of-buffer)
(global-set-key (kbd "C-M-.") 'end-of-buffer)


(global-set-key (kbd "C-e") 'move-end-of-line)
(global-set-key (kbd "C-a") 'move-beginning-of-line)
(global-set-key (kbd "C-M-v") 'traverse-scroll-down-other-window)
(global-set-key (kbd "C-M-V") 'traverse-scroll-up-other-window)

;;;; ---------------------------------------------------------------------------
;;;; Combination key (Meta prefix)
;;;; ---------------------------------------------------------------------------

(global-set-key (kbd "M-s") 'my-anything-occur)
(global-set-key (kbd "M-f") 'forward-word)
(global-set-key (kbd "M-b") 'backward-word)
(global-set-key (kbd "M-e") 'forward-sentence)
(global-set-key (kbd "M-a") 'backward-sentence)
(global-set-key (kbd "M-l") 'backward-delete-word)
(global-set-key (kbd "M-y") 'anything-show-kill-ring)
(global-set-key (kbd "M-n") 'scroll-up)
(global-set-key (kbd "M-p") 'scroll-down)
(global-set-key (kbd "M-v") 'er/expand-region)


(global-set-key (kbd "C-x C-n") 'auto-complete)
(global-set-key (kbd "C-c C-f") 'my-anything-filelist)



(global-set-key (kbd "C-0") 'ace-jump-mode)
(global-set-key (kbd "M-j") 'ace-jump-mode)
(global-set-key (kbd "C-.") 'repeat)


(global-set-key (kbd "%") 'match-paren)


(key-chord-define coldnew-editor-map "vv"  'er/expand-region)
;; quickliy back-word and upcase/downcaseword
(key-chord-define coldnew-editor-map "bu"  'upcase-word-backward)
(key-chord-define coldnew-editor-map "bl"  'downcase-word-backward)
(key-chord-define coldnew-editor-map "bc"  'capitalize-word-backward)
;; ;;; delete
(key-chord-define coldnew-editor-map "dt"  'zap-up-to-char)
(key-chord-define coldnew-editor-map "di"  'delete-between-pair)
(key-chord-define coldnew-editor-map "dl"  'kill-whole-line)
;; (key-chord-define coldnew-editor-map "da"  )



(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-below)
(global-set-key (kbd "M-3") 'split-window-right)
(global-set-key (kbd "M-4") 'delete-window)
(global-set-key (kbd "M-0") 'other-window)



;;;; ---------------------------------------------------------------------------
;;;; paredit-mode
;;;; ---------------------------------------------------------------------------



;; insert
(global-set-key "(" 'paredit-open-round)
(global-set-key ")" 'paredit-close-round)
(global-set-key "[" 'paredit-open-square)
(global-set-key "]" 'paredit-close-square)
(global-set-key "{" 'paredit-open-curly)
(global-set-key "}" 'paredit-close-curly)
(global-set-key (kbd "\"")  'paredit-doublequote)
(global-set-key (kbd "<delete>") 'paredit-forward-delete)
(global-set-key (kbd "<backspace>") 'paredit-backward-delete)
(global-set-key (kbd "C-d") 'paredit-forward-delete)
(global-set-key (kbd "C-l") 'paredit-backward-delete)
(global-set-key (kbd "M-d") 'paredit-forward-kill-word)
(global-set-key (kbd "M-l") 'paredit-backward-kill-word)
(global-set-key (kbd "M-0") 'paredit-close-round-and-newline)
(global-set-key (kbd "M-\'") 'paredit-meta-doublequote)
(global-set-key (kbd "C-k") 'paredit-kill)

;; (key-chord-define evil-insert-state-map "fp"  'paredit-forward)
;; (key-chord-define evil-insert-state-map "bp"  'paredit-backward)



;;;;;; Super
(global-set-key (kbd "s-n") 'windmove-down)
(global-set-key (kbd "s-p") 'windmove-up)
(global-set-key (kbd "s-f") 'windmove-right)
(global-set-key (kbd "s-b") 'windmove-left)

;;;; ---------------------------------------------------------------------------
;;;; one-key
;;;; --------------------------------------------------------------------------
(global-set-key (kbd "C-x M-w") 'one-key-menu-window-navigation)

;;(global-set-key (kbd "C-w") 'one-key-menu-window-navigation)
(global-set-key (kbd "C-x M-f") 'one-key-menu-file-handle)


(provide 'coldnew-global-keybindings)
;; coldnew-global-keybindings.el ends here.
