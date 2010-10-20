;; init org mode
(provide 'rc-org-mode)


(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))


;;  add a hook so we can display images on load
(add-hook 'org-mode-hook 
	  '(lambda () 
	     (setq org-hide-leading-stars t)
	     (setq org-log-done 'time)
	     (setq org-tag-alist '(
				   ("Programming" . ?p)
				   ("Lab"         . ?l)
				   ("Home"        . ?h)
				   ))

	     (when (require 'rc-vim nil 'noerror)
	       (vim:imap (kbd "M-t") 'org-insert-todo-heading)
	       (vim:imap (kbd "C-t") 'org-insert-todo-heading-respect-content))
	     
	     (org-turn-on-iimage-in-org)
	     ))



;;;;;; Functions

;; function to setup images for display on load
(defun org-turn-on-iimage-in-org ()
  "display images in your org file"
  (interactive)
  (turn-on-iimage-mode)
  (set-face-underline-p 'org-link nil))
