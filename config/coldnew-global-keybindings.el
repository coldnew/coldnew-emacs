;;; coldnew-global-keybindings.el --- all global-keybindings setting

(eval-when-compile (require 'cl))

(global-set-key (kbd "C-x C-f") 'lusty-file-explorer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(provide 'coldnew-global-keybindings)
;; coldnew-global-keybindings.el ends here.
