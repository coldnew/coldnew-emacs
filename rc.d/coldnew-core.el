;;
(eval-when-compile (require 'cl))


;;;;;;;; Initial Settings

;; remove global setting
(setq inhibit-default-init t)

;; use visible instead of ring bell
(setq visible-bell t)

;; remove start-message after startup
(setq inhibit-startup-message t)

;; remove default scratch-message
(setq initial-scratch-message)

;; remove tool-bar
(tool-bar-mode -1)

;; remove scroll-bar
(scroll-bar-mode -1)

;; disable bllink cursor
(blink-cursor-mode -1)



(provide 'coldnew-core)
;; coldnew-core.el ends here.
