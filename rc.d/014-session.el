
;;;;;

(setq session-save-file "~/.emacs.d/var/cache/session.cache") ;
(setq session-globals-max-string  2048)
(setq session-registers-max-string 2048)
(setq history-length t)
(add-hook 'after-init-hook 'session-initialize)
(add-to-list 'session-globals-exclude 'org-mark-ring)


(provide '014-session)
;; 014-session.el ends here.
