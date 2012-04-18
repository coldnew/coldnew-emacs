;;; coldnew-backup.el --- default backup file config
(eval-when-compile (require 'cl))                                        

;;;; ---------------------------------------------------------------------------
;;;; initial setting
;;;; ---------------------------------------------------------------------------
(setq auto-save-interval  50)		; Number of input events between auto-saves
(setq auto-save-timeout   30)		; Number of seconds idle time before auto-save
(setq auto-save-visited-file-name t)	; auto-save buffer in the file it is visiting
(setq delete-by-moving-to-trash nil)	; delete file don't use system's trash can
(setq delete-auto-save-files      t)   	; delete auto-save file when bffer is saved or killed
(setq auto-save-default    t)		; auto-save of every file-visiting buffer

;; if emacs-backup-dir does not exist, create it
(if (not (file-exists-p emacs-backup-dir))
    (make-directory emacs-backup-dir t))

(setq backup-directory-alist `(("." . ,emacs-backup-dir)))
(setq version-control      t )		; enable version-control
(setq backup-by-copying    t )		; backup by copy
(setq kept-old-versions   10 )		; keep 10 old-version
(setq kept-new-versions   20 )		; keep 20 new-version
(setq delete-old-versions  t )		; delete non-of-above version

;; change auto-save-list setting
(setq auto-save-list-file-prefix (concat emacs-backup-dir "auto-saves-"))
(setq auto-save-file-name-transforms `((".*"  ,emacs-backup-dir)))


(provide 'coldnew-backup)                                             
;; coldnew-backup.el ends here.                                               
