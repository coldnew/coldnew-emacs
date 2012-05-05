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
;;;; switch-window
;;;; ---------------------------------------------------------------------------
(require* 'switch-window)


;;;; ---------------------------------------------------------------------------
;;;; winner-mode
;;;; ---------------------------------------------------------------------------
(require 'winner)
;; do not use default keybindings
(setq winner-dont-bind-my-keys t)
;; Enable winner-mode
(winner-mode t)


;;;; ---------------------------------------------------------------------------
;;;; Commands
;;;; ---------------------------------------------------------------------------

(defun fullscreen-window ()
  "Make the window full-screen."
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen))
	(old-value nil))
    (set-frame-parameter nil 'fullscreen
			 (if (equal 'fullboth current-value)
			     'old-value
			   (progn
			     (setq old-value current-value)
			     'fullboth)
			   ))))

(defun windmove-down-fullscreen ()
  "Select window below current one and make it fullscreen."
  (interactive)
  (if (windmove-down)
      (delete-other-windows)))

(defun windmove-up-fullscreen ()
  "Select window above the current one and make it fullscreen."
  (interactive)
  (if (windmove-up)
      (delete-other-windows)))

(defun windmove-left-fullscreen ()
  "Select window left to current one and make it fullscreen."
  (interactive)
  (if (windmove-left)
      (delete-other-windows)))

(defun windmove-right-fullscreen ()
  "Select window right to current one and make it fullscreen."
  (interactive)
  (if (windmove-right)
      (delete-other-windows)))





(provide 'coldnew-windows)
;; coldnew-windows.el ends here.
