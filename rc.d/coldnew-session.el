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


;;;;;;;; midnight
;;
(when (require* 'midnight)

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
  (add-hook 'view-mode-hook 'turn-on-tempbuf-mode)
  )

(provide 'coldnew-session)
;; coldnew-session.el ends here.
