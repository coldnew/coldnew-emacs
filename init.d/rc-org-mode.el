;; init org mode
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; (when (require 'org nil 'noerror)
(setq org-hide-leading-stars t)
(setq org-log-done 'time)
(setq org-tag-alist '(
		      ("Programming" . ?p) 
		      ("Lab"         . ?l) 
		      ("Home"        . ?h)
		      ))    

(when (require 'init-vim nil 'noerror)
  (vim:imap (kbd "M-t") 'org-insert-todo-heading)
  (vim:imap (kbd "C-t") 'org-insert-todo-heading-respect-content))





(provide 'rc-org-mode)


;; )
