;;
(eval-when-compile (require 'cl))

;;;;;; Keybindings
(add-hook 'ibuffer-hook
	  '(lambda ()
	     (vim:local-nmap (kbd "d") 'ibuffer-mark-for-delete)
	     (vim:local-nmap (kbd "x") 'ibuffer-do-kill-on-deletion-marks)
	     ))

;;;;;; Settings
(setq ibuffer-always-compile-formats         t )
(setq ibuffer-always-show-last-buffer        t )
(setq ibuffer-default-shrink-to-minimum-size t )
(setq ibuffer-expert                         t )
(setq ibuffer-show-empty-filter-groups     nil )
(setq ibuffer-use-other-window             nil )

;;;;;; Hooks
(add-hook 'ibuffer-mode-hook
	  (lambda ()
	    (hl-line-mode)		; Enable hight-line
	    (ibuffer-switch-to-saved-filter-groups "default")
	    (ibuffer-do-sort-by-filename/process)
	    ))

;;;;;; Buffer lists
(setq ibuffer-saved-filter-groups
      '(("default"
	 ("*Buffer*" (or (name . "^\\*Messages\\*$")
			 (name . "^TAGS\\(<[0-9]+>\\)?$")
			 (name . "^\\*Occur\\*$")
			 (name . "^\\*grep\\*$")
			 (name . "^\\*Compile-Log\\*$")
			 (name . "^\\*Backtrace\\*$")
			 (name . "^\\*Process List\\*$")
			 (name . "^\\*gud\\*$")
			 (name . "^\\*Kill Ring\\*$")
			 (name . "^\\*Completions\\*$")
			 (name . "^\\*tramp")
			 (name . "^\\*compilation\\*$")
			 (name . "^\\*CEDET Global\\*$")
			 (name . "^\\*Buffer List\\*$")
			 (name . "^\\*Anything Log\\*$")
			 (name . "^\\*anything for\\*$")
			 (name . "^\\**Loading Config Log\\*$")
			 (name . "^\\**Loading Library Log\\*$")
			 (name . "^\\*anything*")
			 (name . "^ipa*")
			 (name . "^\\*irc*")
			 (name . "^\\*el-get*")
			 ))
	 ("Version Control" (or (mode . svn-status-mode)
				(mode . svn-log-edit-mode)
				(name . "^\\*svn*\\*")
				(name . "^\\*vc*\\*$")
				(name . "^\\*Annotate")
				(name . "^\\*git-*")
				(name . "^\\*cvs*")
				(name . "^\\*vc-*")))
	 ("Help" (or (mode . woman-mode)
		     (mode . man-mode)
		     (mode . info-mode)
		     (mode . help-mode)))
	 ("IRC"   (or (mode . erc-mode)
		      (mode . rcirc-mode)))
	 ("Terminal" (or (mode . eshell-mode)
			 (mode . term-mode)
			 (mode . comint-mode)))
	 ("w3m"   (or (mode . w3m-mode)
		      (name . "^\\*w3m*")))
	 ("Org"   (mode . org-mode))
	 ("Shell Script" (or (mode . shell-mode)
			     (mode . python-mode)
			     (mode . perl-mode)
			     (mode . ruby-mode)))
	 ("C++ . C#" (or (mode . c++-mode)
			 (mode . csharpmode)))
	 ("C . Obj-C" (or (mode . c-mode)
			  (mode . objc-mode)))
	 ("Lisp"  (or (mode . newlisp-mode)
		      (mode . slime-mode)))
	 ("Emacs" (mode . emacs-lisp-mode))
	 )))

;; Following buffer will not show in iBuffer
(add-to-list 'ibuffer-never-show-predicates "^\\*Buffer List\\*$")

;; Reverse group list
(defadvice ibuffer-generate-filter-groups (after reverse-ibuffer-groups () activate)
  (setq ad-return-value (nreverse ad-return-value)))

;; Switching to ibuffer puts the cursor on the most recent buffer
(defadvice ibuffer (around ibuffer-point-to-most-recent) ()
  "Open ibuffer with cursor pointed to most recent buffer name"
  (let ((recent-buffer-name (buffer-name)))
    ad-do-it
    (ibuffer-jump-to-buffer recent-buffer-name)))
(ad-activate 'ibuffer)

;; Kill ibuffer after quit
(defadvice ibuffer-quit (after kill-ibuffer activate)
  "Kill the ibuffer buffer on exit."
  (kill-buffer "*Ibuffer*"))
(ad-activate 'ibuffer-quit)

;; visiting a file in ibuffer makes it "fullscreen"
(defadvice ibuffer-visit-buffer (after ibuffer-fs-after-visit (arg))
  "Delete other windows after visiting buffer"
  (delete-other-windows))
(ad-activate 'ibuffer-visit-buffer)

;; Use human readable Size column instead of original one
(define-ibuffer-column size-h
  (:name "Size" :inline t)
  (cond
   ((> (buffer-size) 1000) (format "%7.3fk" (/ (buffer-size) 1000.0)))
   ((> (buffer-size) 1000000) (format "%7.3fM" (/ (buffer-size) 1000000.0)))
   (t (format "%8d" (buffer-size)))))

;; integrate ibuffer with git
(when (featurep 'ibuffer-git)
  (setq ibuffer-formats
	'((mark modified read-only git-status-mini " "
		(name 18 18 :left :elide)
		" "
		(size-h 9 -1 :right)
		"  "
		(mode 16 16 :left :elide)
		"   "
		;; (eproject 16 16 :left :elide)
		;; " "
		(git-status 8 8 :left)
		"      "
		filename-and-process))))



(provide '022-ibuffer)
;; 022-ibuffer.el ends here.
