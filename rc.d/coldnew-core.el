;;
(eval-when-compile (require 'cl))


;;;;;;;; Initial Settings

;; Remove global setting
(setq inhibit-default-init t)

;; Use visible instead of ring bell
(setq visible-bell t)

;; Remove start-message after startup
(setq inhibit-startup-message t)

;; Remove default scratch-message
(setq initial-scratch-message)

;; Remove tool-bar
(tool-bar-mode -1)

;; Remove scroll-bar
(scroll-bar-mode -1)

;; Disable bllink cursor
(blink-cursor-mode -1)

;; Use y or n instead of yes and not
(fset 'yes-or-no-p 'y-or-n-p )

;; Remove menu-bar
(menu-bar-mode -1)

(setq initial-major-mode 'text-mode)

(setq warning-suppress-types nil)

(provide 'coldnew-core)
;; coldnew-core.el ends here.
