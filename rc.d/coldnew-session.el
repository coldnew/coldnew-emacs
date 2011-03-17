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
  (setq desktop-path '("~/.emacs.d/var/cache/"))
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
  (add-to-list 'desktop-modes-not-to-save 'fundamental-mode)
  (add-to-list 'desktop-modes-not-to-save 'dired-mode)
  (add-to-list 'desktop-modes-not-to-save 'Info-mode)
  (add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)

  )


(provide 'coldnew-session)
;; coldnew-session.el ends here.
