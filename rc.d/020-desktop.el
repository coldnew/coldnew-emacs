;;


(setq desktop-path '("~/.emacs.d/var/cache/"))
(setq desktop-dirname "~/.emacs.d/var/cache/")
(setq desktop-base-file-name "desktop.cache")

(desktop-save-mode t)

(defadvice desktop-owner (after pry-from-cold-dead-hands activate)
  "Don't allow dead emacsen to own the desktop file."
  (when (not (emacs-process-p ad-return-value))
    (setq ad-return-value nil)))


(defun desktop-in-use-p ()
  (and (file-exists-p desktop-base-file-name) (file-exists-p desktop-base-lock-name)))

(defun autosave-desktop ()
  (if (desktop-in-use-p) (desktop-save-in-desktop-dir)))

(add-hook 'after-init-hook
	  (lambda ()
	    (setq *desktop-saver-timer*
		  (run-with-timer 5 300 'autosave-desktop))))

(add-to-list 'desktop-modes-not-to-save 'fundamental-mode)
(add-to-list 'desktop-modes-not-to-save 'dired-mode)
(add-to-list 'desktop-modes-not-to-save 'Info-mode)
(add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)


(provide '020-desktop)
;; 020-desktop.el ends here.
