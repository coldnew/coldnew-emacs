;;
(eval-when-compile (require 'cl))

;;;;;;;; Packages Import
(require 'coldnew-macro)
(require 'coldnew-functions)
(require 'coldnew-commands)
(require 'coldnew-variables)





;;;;;;;; desktop
;;
(when (require* 'desktop)
  (setq desktop-path (list emacs-cache-dir))
  (setq desktop-dirname emacs-cache-dir)
  (setq desktop-base-file-name "desktop.dat")

  ;; Enable desktop
  (desktop-save-mode t)

  (defadvice desktop-owner (after pry-from-cold-dead-hands activate)
    "Don't allow dead emacsen to own the desktop file."
    (when (not (emacs-process-p ad-return-value))
      (setq ad-return-value nil)))


  (defun desktop-in-use-p ()
    (and (file-exists-p desktop-base-file-name) (file-exists-p desktop-base-lock-name)))

  (defun autosave-desktop ()
    (if (desktop-in-use-p) (desktop-save-in-desktop-dir)))

  ;; auto save desktop
  (add-hook 'after-init-hook
	    (lambda ()
	      (setq *desktop-saver-timer*
		    (run-with-timer 5 300 'autosave-desktop))))

  ;; Following modes are ignore and won't save to desktop
  (setq desktop-buffers-not-to-save
	(concat "\\("
		"^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
		"\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
		"\\)$"))
  (add-to-list 'desktop-modes-not-to-save 'fundamental-mode)
  (add-to-list 'desktop-modes-not-to-save 'dired-mode)
  (add-to-list 'desktop-modes-not-to-save 'Info-mode)
  (add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
  (add-to-list 'desktop-modes-not-to-save 'w3m-mode)
  (add-to-list 'desktop-modes-not-to-save 'view-mode)

  )




(provide 'coldnew-session)
;; coldnew-session.el ends here.
