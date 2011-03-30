;;
(eval-when-compile (require 'cl))

;;;;;;;; Packages Import
(require 'coldnew-macro)
(require 'coldnew-functions)
(require 'coldnew-commands)
(require 'coldnew-variables)

;;;;;;;; Advice

;; Change all fundamental-mode buffer to lisp-interaction-mode.
(defadvice switch-to-buffer (after switch-to-buffer activate)
  "After switch-to-buffer, if tht buffer is Fundamental-mode, change it to lisp-interaction-mode"
  (if (equal major-mode 'fundamental-mode)
      (lisp-interaction-mode)))


;; Prevent to kill *scratch* and *Ibuffer*
(defadvice kill-buffer (around kill-buffer-around-advice activate)
  "Bury *scratch* or *Ibuffer* buffer instead of kill it "
  (let ((buffer-to-kill (ad-get-arg 0)))
    (if (or (equal buffer-to-kill "*scratch*")
	    (equal buffer-to-kill "*Ibuffer*"))
	(bury-buffer)
      ad-do-it)))


;;;;;;;; Uniquify
;; The library uniquify overrides Emacs' default mechanism for
;; making buffer names unique (using suffixes like <2>, <3> etc.)
;; with a more sensible behaviour which use parts of the file names
;; to make the buffer names distinguishable.
;;
(when (require* 'uniquify)
  (setq uniquify-buffer-name-style 'reverse)
  (setq uniquify-separator "/")
  ;; rename after killing uniquified
  (setq uniquify-after-kill-buffer-p t)
  ;; don't muck with special buffers
  (setq uniquify-ignore-buffers-re "^\\*"))

;;;;;;;; Ibuffer
;; Ibuffer is an advanced replacement for BufferMenu, which lets you operate
;; on buffers much in the same manner as Dired. The most important Ibuffer
;; features are highlighting and various alternate layouts.
;;
(when (require* 'ibuffer)
  ;; Settings
  (setq ibuffer-always-compile-formats         t )
  (setq ibuffer-always-show-last-buffer        t )
  (setq ibuffer-default-shrink-to-minimum-size t )
  (setq ibuffer-expert                         t )
  (setq ibuffer-show-empty-filter-groups     nil )
  (setq ibuffer-use-other-window             nil )

  ;; Keybindings
  (add-hook 'ibuffer-hook
	    '(lambda ()
	       (when (require* 'vim)
		 (vim:local-nmap (kbd "d") 'ibuffer-do-delete)
		 (vim:local-nmap (kbd "s") 'ibuffer-do-sort-by-size)
		 (vim:local-imap (kbd "d") 'ibuffer-mark-for-delete)
		 (vim:local-imap (kbd "u") 'ibuffer-unmark-all)
		 (vim:local-imap (kbd "x") 'ibuffer-do-kill-on-deletion-marks)
		 )
	       ))

;;;;;; Hooks
  (add-hook 'ibuffer-mode-hook
	    (lambda ()
	      (hl-line-mode)		; Enable highlight-line
	      (ibuffer-switch-to-saved-filter-groups "default")
	      (ibuffer-do-sort-by-filename/process)
	      ))

;;;;;; Buffer lists
  (setq ibuffer-saved-filter-groups
	'(("default"
	   ("*Buffer*" (or
			(name . "^TAGS\\(<[0-9]+>\\)?$")
			(name . "^\\**Loading Config Log\\*$")
			(name . "^\\**Loading Library Log\\*$")
			(name . "^\\*Anything Log\\*$")
			(name . "^\\*Backtrace\\*$")
			(name . "^\\*Buffer List\\*$")
			(name . "^\\*CEDET Global\\*$")
			(name . "^\\*Compile-Log\\*$")
			(name . "^\\*Completions\\*$")
			(name . "^\\*EGG:*")
			(name . "^\\*Kill Ring\\*$")
			(name . "^\\*Occur\\*$")
			(name . "^\\*Process List\\*$")
			(name . "^\\*Shell Command Output\\*")
			(name . "^\\*Warnings\\*$")
			(name . "^\\*anything for\\*$")
			(name . "^\\*anything*")
			(name . "^\\*compilation\\*$")
			(name . "^\\*el-get*")
			(name . "^\\*grep\\*$")
			(name . "^\\*gud\\*$")
			(name . "^\\*ielm\\*")
			(name . "^\\*im.bitlbee.org\\*$")
			(name . "^\\*irc*")
			(name . "^\\*scratch\\*$")
			(name . "^\\*tramp")
			(name . "^\\*wclock\\*$")
			(name . "^ipa*")
			(name . "^loaddefs.el$")
			(name . "^\\*Messages\\*$")
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
		       (mode . help-mode)
		       (name . "\\*Help\\*$")))
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
  (setq ibuffer-never-show-predicates
	'("^\\*Buffer List\\*$"
	  "^\\*CEDET Global\\*$"
	  "^\\*MiniBuf-*"
	  "^\\*Egg:Select Action\\*$"
	  "^\\*Ido Completions\\*$"
	  "^\\*SPEEDBAR\\*$"
	  "^\\*nav\\*$"
	  ))


  ;;;; Advice
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
     ((> (buffer-size) 1000) (format "%7.3fK" (/ (buffer-size) 1000.0)))
     ((> (buffer-size) 1000000) (format "%7.3fM" (/ (buffer-size) 1000000.0)))
     (t (format "%8dB" (buffer-size)))))

  ;; integrate ibuffer with git
  (when (require* 'ibuffer-git)
    (setq ibuffer-formats
	  '((mark modified read-only git-status-mini " "
		  (name 23 23 :left :elide)
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

  )

;;;;;;;; tempbuf
;; a minor mode that enables buffers to get automatically deleted in the
;; background when it can be deduced that they are no longer of any use.
;; It could be common for example to apply this mode to dired-mode buffers
;; or read-only buffers visiting files, relieving you from having to delete
;; each of them manually when the buffer list grows too large.
;;
(when (require* 'tempbuf)
  ;; Take following mode as temp buffer
  (add-hook 'custom-mode-hook 'turn-on-tempbuf-mode)
  (add-hook 'w3-mode-hook 'turn-on-tempbuf-mode)
  (add-hook 'w3m-mode-hook 'turn-on-tempbuf-mode)
  (add-hook 'Man-mode-hook 'turn-on-tempbuf-mode)
  (add-hook 'woman-mode-hook 'turn-on-tempbuf-mode)
  (add-hook 'help-mode-hook 'turn-on-tempbuf-mode)
  (add-hook 'view-mode-hook 'turn-on-tempbuf-mode)
  )

;;;;;;;; midnight
;; Midnight mode is a package that comes with Emacs for running configured
;; actions at every `midnight.
;;
(when (require* 'midnight)

  ;; Every day clean the buffer
  (setq clean-buffer-list-delay-general 1)

  ;; Kill anything, clean-buffer-list is very intelligent
  ;; at not killing unsaved buffer.
  (setq clean-buffer-list-kill-regexps '("^.*$"))

  ;; Prevent to kill buffers if match rules.
  (add-to-list 'clean-buffer-list-kill-never-regexps
	       '("^\\*EMMS Playlist\\*.*$"
		 ".*irc\\.freenode\\.net.*"
		 ".*irc\\.debian\\.org.*"
		 ".*im\\.bitlbee\\.org.*"
		 "^\\*ansi-term*"
		 "^\\*terminal*"
		 "^\\*Messages\\*$"
		 "^\\*scratch\\*$"
		 "^\\*w3m\\*$"
		 "^\\*Inferior*"
		 ))
  )


(provide 'coldnew-buffer)
;; coldnew-buffer.el ends here.
