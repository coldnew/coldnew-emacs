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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro vim:do-insert-yasnippet-command (vars string-case &rest body)
  (let ((commnd-list (nth 0 vars))
	(spec     (nth 1 vars))
	(keys     (nth 2 vars))
	(fn       (nth 3 vars))
	(examples (nth 4 vars)))
    `(dolist (,spec ,command-list)
       (if (stringp ,spec)
	   ,string-case
	 (let ((,keys (let ((k (car ,spec)))
			(cond ((stringp k) (list k))
			      ((listp k) k)
			      (t (error "Invalid vim-insert command %s."
					,spec)))))
	       (,fn (cadr ,spec))
	       (,examples (cddr ,spec)))
	   ,@body)))))

(defun vim:local-insert-define-keys (command-list)
  (vim:do-insert-yasnippet-command (command-list spec keys fn examples)
				   nil       ; string case
				   (dolist (key keys)
				     (vim:local-imap (read-kbd-macro key) (lambda () (interactive) (insert fn) (yas/expand)
									    )))))
;; (caar elisp:commands)
;; (cadar elisp:commands)

(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    ;;  (vim:local-insert-define-keys elisp:commands)
	    ))
(setq elisp:commands
      '(
	("M-i" "if")
	("M-r" "require")
	))

;; (setq elisp:commands
;;       '(
;;	("M-h" "require")
;;	("M-n" "if")
;;	))

;; (dolist (va elisp:commands)
;;   (vim:local-imap (read-kbd-macro (car va)) (lambda () (interactive) (insert (cadr va)) (yas/expand)))
;;   )



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
