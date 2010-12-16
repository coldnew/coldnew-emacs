;;
(provide 'rc-ibuffer)

(when (require 'ibuffer nil 'noerror)

  (when (require 'ibuffer-expert nil 'noerror)
    (setq ibuffer-expert t))

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
			   (name . "^\\*shell\\*$")
			   (name . "^\\*compilation\\*$")
			   (name . "^\\*CEDET Global\\*$")
			   (name . "^\\*Buffer List\\*$")))
	   ("Version Control" (or (mode . svn-status-mode)
				  (mode . svn-log-edit-mode)
				  (name . "^\\*svn-")
				  (name . "^\\*vc\\*$")
				  (name . "^\\*Annotate")
				  (name . "^\\*git-")
				  (name . "^\\*cvs")
				  (name . "^\\*vc-")))
	   ("Help" (or (mode . woman-mode)
		       (mode . man-mode)
		       (mode . info-mode)
		       (mode . help-mode)))
	   ("IRC"   (or (mode . erc-mode)
			(mode . rcirc-mode)))
	   ("Terminal" (or (mode . eshell-mode)
			   (mode . term-mode)))
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
	   ("Emacs" (mode . emacs-lisp-mode))
	   )))

  ;; Keybinding
  (when (require 'rc-vim-mode nil 'noerror)
    (vim:nmap (kbd "C-x C-b") 'ibuffer)
    (vim:imap (kbd "C-x C-b") 'ibuffer))

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

  (add-hook 'ibuffer-mode-hook
	    (lambda ()
	      (hl-line-mode)		; Enable hight-line
	      (ibuffer-switch-to-saved-filter-groups "default")))

  )
