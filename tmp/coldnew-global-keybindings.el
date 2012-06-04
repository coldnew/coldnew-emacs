;;; coldnew-global-keybindings.el --- all global-keybindings setting

(eval-when-compile (require 'cl))

;;;; ---------------------------------------------------------------------------
;;;; Unset key
;;;; ---------------------------------------------------------------------------


;;;; ---------------------------------------------------------------------------
;;;; function-key
;;;; ---------------------------------------------------------------------------
(global-set-key (kbd "C-x <f2>") 'multi-term)
(global-set-key (kbd "C-x <f3>") 'toggle-kbd-macro-recording-on)

;; ;;;; ---------------------------------------------------------------------------
;; ;;;; Combination key (Ctrl prefix)
;; ;;;; ---------------------------------------------------------------------------




(global-set-key (kbd "<escape>") 'coldnew/switch-to-command-mode)

;;;; ---------------------------------------------------------------------------
;;;; Combination key (Ctrl-x prefix)
;;;; ---------------------------------------------------------------------------



;; epa
(global-set-key (kbd "C-x er") 'epa-encrypt-region)
(global-set-key (kbd "C-x ef") 'epa-encrypt-file)
(global-set-key (kbd "C-x dr") 'epa-decrypt-region)
(global-set-key (kbd "C-x df") 'epa-decrypt-file)

;; elscreen


;;;; ---------------------------------------------------------------------------
;;;; Combination key (Ctrl-c prefix)
;;;; ---------------------------------------------------------------------------


;;;; ---------------------------------------------------------------------------
;;;; Combination key (Ctrl-Meta prefix)
;;;; ---------------------------------------------------------------------------



;;;; ---------------------------------------------------------------------------
;;;; Combination key (Meta prefix)
;;;; ---------------------------------------------------------------------------



;;;; ---------------------------------------------------------------------------
;;;; Combination key (Shift Prefix)
;;;; ---------------------------------------------------------------------------

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(global-set-key (kbd "<backspace>") 'hungry-delete-backward)

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


(provide 'coldnew-global-keybindings)
;; coldnew-global-keybindings.el ends here.
