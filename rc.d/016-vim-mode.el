;;;; initial vim-mode

(vim-mode)				; start vim-mode

;;;; Functions
(vim:defcmd vim:visual-toggle-comment (motion)
  "Toggles comments in the region."
  (comment-or-uncomment-region (vim:motion-begin-pos motion)
			       (vim:motion-end-pos motion)))

(defcmd window-fullscreen ()
  "Make the window full-screen."
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
			 (if (equal 'fullboth current-value)
			     (if (boundp 'old-fullscreen) old-fullscreen nil)
			   (progn (setq old-fullscreen current-value)
				  'fullboth)))))

(provide '016-vim-mode)
;; 016-vim-mode.el ends here.
