;;;; Setting the window width and height

;;;; Resolution
(defvar resolution-1280x800-p  (and (= (display-pixel-width) 1280) (= (display-pixel-height) 800)))
(defvar resolution-1280x1024-p (and (= (display-pixel-width) 1280) (= (display-pixel-height) 1024)))
(defvar resolution-1920x1080-p (and (= (display-pixel-width) 1920) (= (display-pixel-height) 1080)))

(cond
 ;; 1280 * 800
 (resolution-1280x800-p
  ;;  (and (= (display-pixel-width) 1280) (= (display-pixel-height) 800))
  (setq default-frame-alist
	(append (list
		 '(width  . 114)
		 '(height . 40)
		 '(top    . 90)
		 '(left   . 300))
		default-frame-alist)))
 ;; 1280 * 1024
 (resolution-1280x1024-p
  ;; (and (= (display-pixel-width) 1280) (= (display-pixel-height) 1024))
  (setq default-frame-alist
	(append (list
		 '(width  . 114)
		 '(height . 40)
		 '(top    . 90)
		 '(left   . 300))
		default-frame-alist)))
 ;; 1920 * 1080
 (;;resolution-1920x1080-p
  (and (= (display-pixel-width) 1920) (= (display-pixel-height) 1080))
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
		default-frame-alist))))

(provide '013-display)
;; 013-display.el ends here.
