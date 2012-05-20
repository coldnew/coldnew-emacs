;;; coldnew-global-keybindings.el --- all global-keybindings setting

(eval-when-compile (require 'cl))

;;;; ---------------------------------------------------------------------------
;;;; Unset key
;;;; ---------------------------------------------------------------------------
(global-unset-key (kbd "C-x e"))
(global-unset-key (kbd "C-x d"))

;;;; ---------------------------------------------------------------------------
;;;; function-key
;;;; ---------------------------------------------------------------------------
(global-set-key (kbd "<f1>")     'woman)
(global-set-key (kbd "<f2>")     'shell-pop)
(global-set-key (kbd "C-x <f2>") 'multi-term)
(global-set-key (kbd "<f3>")     'call-last-kbd-macro)
(global-set-key (kbd "C-x <f3>") 'toggle-kbd-macro-recording-on)
(global-set-key (kbd "<f4>")     'sr-speedbar-toggle)

;; ;;;; ---------------------------------------------------------------------------
;; ;;;; Combination key (Ctrl prefix)
;; ;;;; ---------------------------------------------------------------------------
(global-set-key (kbd "C-n") 'next-line)
(global-set-key (kbd "C-p") 'previous-line)
(global-set-key (kbd "C-b") 'backward-char)
(global-set-key (kbd "C-f") 'forward-char)
(global-set-key (kbd "C-l") 'hungry-delete-backward)
(global-set-key (kbd "C-;") 'iedit-mode)
(global-set-key (kbd "C-v") 'coldnew/set-mark-command)
(global-set-key (kbd "C-/") 'undo-tree-undo)
;;(global-set-key (kbd "C-0") 'ace-jump-mode)
(global-set-key (kbd "C-e") 'move-end-of-line)
(global-set-key (kbd "C-a") 'move-beginning-of-line)
(global-set-key (kbd "C-]") 'coldnew/toggle-state)


(global-set-key (kbd "<escape>") 'coldnew/switch-to-command-mode)

;;;; ---------------------------------------------------------------------------
;;;; Combination key (Ctrl-x prefix)
;;;; ---------------------------------------------------------------------------

(global-set-key (kbd "C-x C-f")  'lusty-file-explorer)
(global-set-key (kbd "C-x b")    'ibuffer)
(global-set-key (kbd "C-x C-b")  'coldnew/helm-filelist)
(global-set-key (kbd "C-x C-d")  'dired)
(global-set-key (kbd "C-x C-r")  'sudo-edit)
(global-set-key (kbd "C-x vv") 'egg-next-action)
(global-set-key (kbd "C-x M-x") 'helm-M-x)
(global-set-key (kbd "C-x o")   'switch-window)
(global-set-key (kbd "C-x C-l") 'recenter-top-bottom)
(global-set-key (kbd "C-x C-n") 'auto-complete)
(global-set-key (kbd "C-x C-s") 'save-buffer-always)
(global-set-key (kbd "C-x f") 'fullscreen-window)

;; epa
(global-set-key (kbd "C-x er") 'epa-encrypt-region)
(global-set-key (kbd "C-x ef") 'epa-encrypt-file)
(global-set-key (kbd "C-x dr") 'epa-decrypt-region)
(global-set-key (kbd "C-x df") 'epa-decrypt-file)

;; elscreen


;;;; ---------------------------------------------------------------------------
;;;; Combination key (Ctrl-c prefix)
;;;; ---------------------------------------------------------------------------
(global-set-key (kbd "C-c C-h") 'coldnew/folding-toggle)


;;;; ---------------------------------------------------------------------------
;;;; Combination key (Ctrl-Meta prefix)
;;;; ---------------------------------------------------------------------------
(global-set-key (kbd "C-M-,") 'beginning-of-buffer)
(global-set-key (kbd "C-M-.") 'end-of-buffer)

(global-set-key (kbd "C-M-n") 'scroll-other-window)
(global-set-key (kbd "C-M-p") 'scroll-other-window-down)


;;;; ---------------------------------------------------------------------------
;;;; Combination key (Meta prefix)
;;;; ---------------------------------------------------------------------------

(global-set-key (kbd "M-s") 'coldnew/helm-occur)
(global-set-key (kbd "M-f") 'forward-word)
(global-set-key (kbd "M-b") 'backward-word)
(global-set-key (kbd "M-e") 'forward-sentence)
(global-set-key (kbd "M-a") 'backward-sentence)
(global-set-key (kbd "M-l") 'backward-delete-word)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "M-n") 'scroll-up)
(global-set-key (kbd "M-p") 'scroll-down)
(global-set-key (kbd "M-g") 'linum-ace-jump)
(global-set-key (kbd "M-v") 'er/expand-region)
(global-set-key (kbd "M-q") 'coldnew/switch-to-command-mode)
(global-set-key (kbd "M-/") 'undo-tree-redo)
(global-set-key (kbd "M-.") 'helm-etags+-select-one-key)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-below)
(global-set-key (kbd "M-3") 'split-window-right)
(global-set-key (kbd "M-4") 'delete-window)
(global-set-key (kbd "M-0") 'other-window)

(global-set-key (kbd "M-j") 'switch-window)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(global-set-key (kbd "<backspace>") 'hungry-delete-backward)
(global-set-key (kbd "<delete>") 'hungry-delete-forward)

(global-set-key (kbd "C-.") 'repeat)


(global-set-key (kbd "%") 'match-paren)


(key-chord-define coldnew-editor-map "vv"  'er/expand-region)
;; quickliy back-word and upcase/downcaseword
(key-chord-define coldnew-editor-map "bu"  'upcase-word-backward)
(key-chord-define coldnew-editor-map "bl"  'downcase-word-backward)
(key-chord-define coldnew-editor-map "bc"  'capitalize-word-backward)







;;;; ---------------------------------------------------------------------------
;;;; paredit-mode
;;;; ---------------------------------------------------------------------------

;; insert
(define-key coldnew-editor-map "(" 'paredit-open-round)
(define-key coldnew-editor-map ")" 'paredit-close-round)
(define-key coldnew-editor-map "[" 'paredit-open-square)
(define-key coldnew-editor-map "]" 'paredit-close-square)
(define-key coldnew-editor-map "{" 'paredit-open-curly)
(define-key coldnew-editor-map "}" 'paredit-close-curly)
(define-key coldnew-editor-map (kbd "\"")  'paredit-doublequote)
(define-key coldnew-editor-map (kbd "C-d") 'paredit-forward-delete)
(define-key coldnew-editor-map (kbd "C-l") 'paredit-backward-delete)
(define-key coldnew-editor-map (kbd "<backsapce>") 'paredit-backward-delete)
(define-key coldnew-editor-map (kbd "M-d") 'paredit-forward-kill-word)
(define-key coldnew-editor-map (kbd "M-l") 'paredit-backward-kill-word)
(define-key coldnew-editor-map (kbd "M-0") 'paredit-close-round-and-newline)
(define-key coldnew-editor-map (kbd "M-\'") 'paredit-meta-doublequote)
(define-key coldnew-editor-map (kbd "C-k") 'paredit-kill)

;; (key-chord-define evil-insert-state-map "fp"  'paredit-forward)
;; (key-chord-define evil-insert-state-map "bp"  'paredit-backward)



;; ;;;;;; Super
;; (global-set-key (kbd "s-n") 'windmove-down)
;; (global-set-key (kbd "s-p") 'windmove-up)
;; (global-set-key (kbd "s-f") 'windmove-right)
;; (global-set-key (kbd "s-b") 'windmove-left)

;;;; ---------------------------------------------------------------------------
;;;; one-key
;;;; --------------------------------------------------------------------------
;; (evil:nmap (kbd "C-w") 'one-key-menu-window-navigation)
;; (evil:imap (kbd "C-w") 'one-key-menu-window-navigation)

;;(global-set-key (kbd "C-w") 'one-key-menu-window-navigatio)
(global-set-key (kbd "C-x M-f") 'one-key-menu-file-handle)

;;;; ---------------------------------------------------------------------------
;;;; Alias
;;;; ---------------------------------------------------------------------------
(defalias 'coldnew/set-mark-command 'cua-set-mark-or-rectangle-mark)
(defalias 'coldnew/folding-toggle   'toggle-selective-display)


(provide 'coldnew-global-keybindings)
;; coldnew-global-keybindings.el ends here.
