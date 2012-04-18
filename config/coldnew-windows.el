;;; coldnew-windows.el ---
(eval-when-compile (require 'cl))

(require 'coldnew-variables)

;;;; ---------------------------------------------------------------------------
;;;; Initial User-Interface Setting
;;;; ---------------------------------------------------------------------------
(cond
 ;; Resolution is 1920x1080
 (display-1920x1080-p
  (setq default-frame-alist
	(append (list
		 '(width  . 134)
		 '(height . 45)
		 '(top    . 90)
		 '(left   . 500))
		default-frame-alist)))
 ;; Resolution is 1280x1024
 (display-1280x1024-p
  (setq default-frame-alist
	(append (list
		 '(width  . 114)
		 '(height . 40)
		 '(top    . 90)
		 '(left   . 300))
		default-frame-alist)))
 ;; Resolution is 1280x800
 (display-1280x800-p
  (setq default-frame-alist
	(append (list
		 '(width  . 114)
		 '(height . 40)
		 '(top    . 90)
		 '(left   . 300))
		default-frame-alist)))
 ;; Default condition
 (t
  (setq default-frame-alist
	(append (list
		 '(width  . 100)
		 '(height . 40)
		 '(top    . 90)
		 '(left   . 100))
		default-frame-alist)))
 )

;;;; ---------------------------------------------------------------------------
;;;; winner-mode
;;;; ---------------------------------------------------------------------------
(require 'winner)
;; do not use default keybindings
(setq winner-dont-bind-my-keys t)
;; Enable winner-mode
(winner-mode t)




(provide 'coldnew-windows)
;; coldnew-windows.el ends here.
