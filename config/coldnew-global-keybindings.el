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
(global-set-key (kbd "C-x C-d")  'dired)
(global-set-key (kbd "C-x C-r")  'sudo-edit)
(global-set-key (kbd "C-x ,") 'undo-tree-undo)
(global-set-key (kbd "C-x .") 'undo-tree-redo)
(global-set-key (kbd "C-w")   'kill-word)
(global-set-key (kbd "M-w")   'backward-kill-word)
(global-set-key (kbd "C-g")   'keyboard-quit)
(global-set-key (kbd "C-c C-f") 'my-anything-filelist)
(global-set-key (kbd "<escape>") 'evil-mode)


;;;; ---------------------------------------------------------------------------
;;;; one-key
;;;; --------------------------------------------------------------------------
(global-set-key (kbd "C-x M-w") 'one-key-menu-window-navigation)

(global-set-key (kbd "C-w") 'one-key-menu-window-navigation)
(global-set-key (kbd "C-x M-f") 'one-key-menu-file-handle)



(provide 'coldnew-global-keybindings)
;; coldnew-global-keybindings.el ends here.
