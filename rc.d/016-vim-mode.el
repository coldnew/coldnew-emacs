;;;; initial vim-mode

(vim-mode)				; start vim-mode

;;;; Functions
(vim:defcmd vim:visual-toggle-comment (motion)
  "Toggles comments in the region."
  (comment-or-uncomment-region (vim:motion-begin-pos motion)
			       (vim:motion-end-pos motion)))

(vim:defcmd vim:window-fullscreen (nonrepeatable)
  "Make the window full-screen."
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
			 (if (equal 'fullboth current-value)
			     (if (boundp 'old-fullscreen) old-fullscreen nil)
			   (progn (setq old-fullscreen current-value)
				  'fullboth)))))

(vim:defcmd vim:cmd-sudo ((argument:file file) nonrepeatable)
  "Edit file with sudo"
  (if file
      (find-file (concat "/sudo:root@localhost:" file))))

;; el-get
(vim:defcmd vim:cmd-el-get-install ((argument:text text) nonrepeatable)
  ""
  (if text
      (el-get-install text)))

(vim:defcmd vim:cmd-el-get-remove ((argument:text text) nonrepeatable)
  ""
  (if text
      (el-get-remove text)))

(vim:defcmd vim:cmd-el-get-update ((argument:text text) nonrepeatable)
  ""
  (if text
      (el-get-update text)))

(vim:emap "install" 'vim:cmd-el-get-install)
(vim:emap "remove" 'vim:cmd-el-get-remove)
(vim:emap "update" 'vim:cmd-el-get-update)


(provide '016-vim-mode)
;; 016-vim-mode.el ends here.
