;;
(eval-when-compile (require 'cl))


;;;;;;;; Packages Import
(require 'coldnew-macro)
(require 'coldnew-functions)
(require 'coldnew-commands)
(require 'coldnew-variables)


;;;;;;;; Modified window size according resolution
(cond
 ;; Resolution is 1920x1080
 (1920x1080-p
  (setq default-frame-alist
	(append (list
		 '(width  . 134)
		 '(height . 45)
		 '(top    . 90)
		 '(left   . 500))
		default-frame-alist)))
 ;; Resolution is 1280x1024
 (1280x1024-p
  (setq default-frame-alist
	(append (list
		 '(width  . 114)
		 '(height . 40)
		 '(top    . 90)
		 '(left   . 300))
		default-frame-alist)))
 ;; Resolution is 1280x800
 (1280x800-p
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





(provide 'coldnew-window)
;; coldnew-window.el ends here.
