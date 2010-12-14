;; init org mode

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))


;;  Hooks
(add-hook 'org-mode-hook
	  '(lambda ()
	     (setq org-hide-leading-stars t)
	     (setq org-log-done t)
	     (setq org-log-done 'time)	; 對已完成事項加上時間
	     (setq org-tag-alist '(
				   ("Programming" . ?p)
				   ("Lab"         . ?l)
				   ("Home"        . ?h)
				   ))
	     (setq org-todo-keywords
		   '("TODO(T)" "STARTED(S)" "WAITING(W)" "|" "CANCELED(C)" "DONE(D)"))
	     (org-turn-on-iimage-in-org) ; display image on load
	     ))

;;;;; Keybinding
(add-hook 'org-mode-hook
	  '(lambda ()
	     (when (require 'rc-vim nil 'noerror)
	       (vim:nmap "\C-l" 'org-store-link) ;
	       (vim:nmap "\C-a" 'org-agenda)	 ; 進入日程表
	       (vim:nmap "\C-b" 'org-iswitchb)

	       (vim:imap (kbd "M-t") 'org-insert-todo-heading)
	       (vim:imap (kbd "C-t") 'org-insert-todo-heading-respect-content)
	       )))




;;;;;; Functions

;; function to setup images for display on load
(defun org-turn-on-iimage-in-org ()
  "display images in your org file"
  (interactive)
  (turn-on-iimage-mode)
  (set-face-underline-p 'org-link nil))








(provide 'rc-org-mode)
;; rc-org-mode.el ends here
