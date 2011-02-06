;;
(setq ibuffer-expert t)

;;;;;; Keybindings
(add-hook 'ibuffer-hook
	  '(lambda ()
	     (vim:local-nmap (kbd "d") 'ibuffer-mark-for-delete)
	     (vim:local-nmap (kbd "x") 'ibuffer-do-kill-on-deletion-marks)
	     ))
;;;;;; Hooks
(add-hook 'ibuffer-mode-hook
	  (lambda ()
	    (hl-line-mode)		; Enable hight-line
	    (ibuffer-switch-to-saved-filter-groups "default")
	    (ibuffer-do-sort-by-filename/process)
	    ))
;;;;;; Settings
(setq ibuffer-always-show-last-buffer t)

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

;; integrate ibuffer with git
(when (featurep 'ibuffer-git)
  (mark modified read-only git-status-mini " "
	(name 18 18 :left :elide)
	" "
	(size 9 -1 :right)
	" "
	(mode 16 16 :left :elide)
	" "
	(eproject 16 16 :left :elide)
	" "
	(git-status 8 8 :left)
	" " filename-and-process)
  )



(provide '022-ibuffer)
;; 022-ibuffer.el ends here.
