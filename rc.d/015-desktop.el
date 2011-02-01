;;

(setq desktop-path '("~/.emacs.d/var/cache/"))
(setq desktop-dirname "~/.emacs.d/var/cache/")
(setq desktop-base-file-name "desktop.cache")

(desktop-save-mode         t )

(defun desktop-in-use-p ()
  (and (file-exists-p desktop-base-file-name) (file-exists-p desktop-base-lock-name)))

(defun autosave-desktop ()
  (if (desktop-in-use-p) (desktop-save-in-desktop-dir)))


(add-hook 'after-init-hook
	  (lambda ()
	    (setq *foo-desktop-saver-timer*
		  (run-with-timer 5 300 'autosave-desktop))))


(provide '015-desktop)
;; 015-desktop.el ends here.
