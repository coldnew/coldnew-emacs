;; setting for display


;;;; Setting the window width and height
(cond
 ;; 1280 * 800
 ((and (= (display-pixel-width) 1280) (= (display-pixel-height) 800))
  (setq default-frame-alist
	(append (list
		 '(width  . 114)
		 '(height . 40)
		 '(top    . 90)
		 '(left   . 300))
		default-frame-alist)))
 ;; 1280 * 1024
 ((and (= (display-pixel-width) 1280) (= (display-pixel-height) 1024))
  (setq default-frame-alist
	(append (list
		 '(width  . 114)
		 '(height . 40)
		 '(top    . 90)
		 '(left   . 300))
		default-frame-alist)))
 ;; 1920 * 1080
 ((and (= (display-pixel-width) 1920) (= (display-pixel-height) 1080))
  (setq default-frame-alist
	(append (list
		 '(width  . 134)
		 '(height . 45)
		 '(top    . 90)
		 '(left   . 500))
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

(provide '012-display)
;; 012-display.el ends here.
