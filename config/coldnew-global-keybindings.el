;;; coldnew-global-keybindings.el --- all global-keybindings setting

(eval-when-compile (require 'cl))

;;;; ---------------------------------------------------------------------------
;;;; function-key
;;;; ---------------------------------------------------------------------------
(global-set-key (kbd "<f1>")     'woman)
(global-set-key (kbd "<f2>")     'shell-pop)
(global-set-key (kbd "C-x <f2>") 'multi-term)

;;;; ---------------------------------------------------------------------------
;;;; Combination key
;;;; ---------------------------------------------------------------------------

(global-set-key (kbd "C-x C-f")  'lusty-file-explorer)
(global-set-key (kbd "C-x C-b")  'ibuffer)
(global-set-key (kbd "C-;")      'iedit-mode)
(global-set-key (kbd "C-=")      'text-scale-increase)
(global-set-key (kbd "C--")      'text-scale-decrease)
(global-set-key (kbd "C-x C-d")  'dired)

;;;; ---------------------------------------------------------------------------
;;;; one-key
;;;; --------------------------------------------------------------------------
(global-set-key (kbd "C-x M-w") 'one-key-menu-window-navigation)
(global-set-key (kbd "C-x M-f") 'one-key-menu-file-handle)



(provide 'coldnew-global-keybindings)
;; coldnew-global-keybindings.el ends here.
