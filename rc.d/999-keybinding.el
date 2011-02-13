
;; Normal Map
(vim:nmap (kbd "<f1>") 'woman)
(vim:nmap (kbd "<f4>") 'shell-pop)
(vim:nmap (kbd "C-c C-e") 'eval-and-replace)
(vim:nmap (kbd "C-r") 'undo-tree-redo)
(vim:nmap (kbd "C-x C-b") 'ibuffer)
(vim:nmap (kbd "C-x C-f") 'lusty-file-explorer)
(vim:nmap (kbd "C-x C-s") 'save-buffer-always)
(vim:nmap (kbd "C-x b") 'lusty-buffer-explorer)
(vim:nmap (kbd "C-x f") 'anything-for-files)
(vim:nmap (kbd "K") 'woman)
(vim:nmap (kbd "M-p") 'anything-show-kill-ring)
(vim:nmap (kbd "M-x") 'smex)
(vim:nmap (kbd "u") 'undo-tree-undo)
(vim:nmap (kbd "z.") 'find-file-at-point)
(vim:nmap (kbd "gs") 'switch-to-scratch-and-back)


;; Insert Map
(vim:imap (kbd "C-c C-e") 'eval-and-replace)
(vim:imap (kbd "C-n") 'auto-complete)
(vim:imap (kbd "C-p") 'auto-complete)
(vim:imap (kbd "C-x C-b") 'ibuffer)
(vim:imap (kbd "C-x C-f") 'lusty-file-explorer)
(vim:imap (kbd "C-x C-s") 'save-buffer-always)
(vim:imap (kbd "C-x b") 'lusty-buffer-explorer)
(vim:imap (kbd "C-x f") 'anything-for-files)
(vim:imap (kbd "M-x") 'smex)
(vim:imap (kbd "RET") 'newline-and-indent)

;; Visual Map
(vim:vmap (kbd "M-;") 'vim:visual-toggle-comment)

;; Windows Map
(vim:wmap (kbd "C-w f") 'vim:window-fullscreen)
(vim:wmap (kbd "C-x C-b") 'ibuffer)
(vim:wmap (kbd "C-x f") 'anything-for-files)
(vim:wmap (kbd "M-x") 'smex)

;; Motion Map
(vim:mmap (kbd "<left>") 'vim:motion-left)
(vim:mmap (kbd "<right>") 'vim:motion-right)
(vim:mmap (kbd "<up>") 'vim:motion-up)
(vim:mmap (kbd "<down>") 'vim:motion-down)

;; EX Map
(vim:emap "sudo" 'vim:cmd-sudo)







(vim:deflocalvar vim:cmd-sort-list nil
  "")

(vim:defcmd vim:cmd-sort ((count) (argument:text text) nonrepeatable)
  ""
  (setq vim:cmd-sort-list
	'(
	  ("u" "vim:cmd-sort-unique")
	  ))
  (if text
      (dolist (cmd vim:cmd-sort-list)
	(if (string= text (car cmd))
	    (eval (list (intern (cadr cmd))))))
    ;; do sort
    ))

(vim:defcmd vim:cmd-sort-unique (nonrepeatable)
  "Remove duplicate lines and sort lines in a buffer"
  (message "sort unique")
  )

(vim:emap "sort" 'vim:cmd-sort)
(vim:emap "sor" "sort")


(vim:deflocalvar vim:cmd-set-list nil
  "")
;; FIXME:can't use setq....
(vim:defcmd vim:cmd-set ((argument:text text) nonrepeatable)
  ""
  (setq vim:cmd-set-list
	'(("nu" "vim:cmd-toggle-linum-mode")))
  (if text
      (dolist (cmd vim:cmd-set-list)
	(if (string= text (car cmd))
	    (eval (list (intern (cadr cmd))))))))

(vim:defcmd vim:cmd-toggle-linum-mode (nonrepeatable)
  ""
  (linum-mode (if linum-mode -1 1)))

(vim:emap "set" 'vim:cmd-set)




(provide '999-keybinding)
;; 999-keybinding.el ends here.
