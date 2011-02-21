;;
(eval-when-compile (require 'cl))

;;;;;; Keybindings
(add-hook 'ibuffer-hook
	  '(lambda ()
	     ;; Normal map
	     (vim:local-nmap (kbd "d") 'ibuffer-do-delete)
	     (vim:local-nmap (kbd "s") 'ibuffer-do-sort-by-size)
	     ;; Insert map
	     (vim:local-imap (kbd "d") 'ibuffer-mark-for-delete)
	     (vim:local-imap (kbd "u") 'ibuffer-unmark-all)
	     (vim:local-imap (kbd "x") 'ibuffer-do-kill-on-deletion-marks)
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
			 (name . "^\\*scratch\\*$")
			 (name . "^\\*Warnings\\*$")
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
			 (name . "^\\*im.bitlbee.org\\*$")
			 (name . "^\\*el-get*")
			 (name . "^\\*EGG:*")
			 (name . "^\\*WoMan-Log\\*")
			 ))
	 ("Version Control" (or (mode . svn-status-mode)
				(mode . svn-log-edit-mode)
				(name . "^\\*svn*\\*")
				(name . "^\\*vc*\\*$")
				(name . "^\\*Annotate")
				(name . "^\\*git-*")
				(name . "^\\*cvs*")
				(name . "^\\*vc-*")
				(mode . egg-status-buffer-mode)
				(mode . egg-log-buffer-mode)
				(mode . egg-commit-buffer-mode)
				))
	 ("Help" (or (mode . woman-mode)
		     (mode . man-mode)
		     (mode . info-mode)
		     (mode . help-mode)))
	 ("IRC"   (or (mode . erc-mode)
		      (mode . rcirc-mode)))
	 ("Terminal" (or (mode . eshell-mode)
			 (mode . term-mode)
			 (mode . comint-mode)))
	 ("Text" (or (mode . text-mode)
		     (name . "*.txt$")))
	 ("w3m"   (or (mode . w3m-mode)
		      (name . "^\\*w3m*")))
	 ("Org"   (mode . org-mode))
	 ("Shell Script" (or (mode . shell-mode)
			     (mode . perl-mode)
			     (mode . ruby-mode)))
	 ("Python" (or (mode . python-mode)
		       (mode . ipython-mode)))
	 ("C++ . C#" (or (mode . c++-mode)
			 (mode . csharpmode)))
	 ("C"          (mode . c-mode))
	 ("Object-C"   (mode . objc-mode))
	 ("Snippet" (or (mode . snippet-mode)
			(name . "*.yas$")))
	 ("newLisp"  (mode . newlisp-mode))
	 ("Lisp"     (mode . slime-mode))
	 ("Emacs" (or (mode . emacs-lisp-mode)
		      (mode . lisp-interaction-mode)))
	 )))

;; Following buffer will not show in iBuffer
(add-to-list 'ibuffer-never-show-predicates "^\\*Buffer List\\*$")
(add-to-list 'ibuffer-never-show-predicates"^\\*CEDET Global\\*$")
(add-to-list 'ibuffer-never-show-predicates"^\\*MiniBuf-*")
(add-to-list 'ibuffer-never-show-predicates"^\\*Egg:Select Action\\*$")
(add-to-list 'ibuffer-never-show-predicates"^\\*Ido Completions\\*$")


;;;;;;;; Advice
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
