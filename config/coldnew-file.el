;;; coldnew-file.el --- handle about file open, close save ...etc.
(eval-when-compile (require 'cl))

(require 'coldnew-snippets)




;;;; ---------------------------------------------------------------------------
;;;; hooks
;;;; ---------------------------------------------------------------------------

;; If save a newfile to nonexist directory, create the directory before save.
(add-hook 'before-save-hook
	  '(lambda ()
	     (or (file-exists-p  (file-name-directory buffer-file-name))
		 (make-directory (file-name-directory buffer-file-name) t))))

;; When visit emacs-lisp-dir, change file to view mode
;; this will avoid I change those plugins.
;; FIXME: this will cause .loaddef.el become readonly
;; (add-hook 'find-file-hook
;;	  '(lambda ()
;;	     (if (search (expand-file-name emacs-lisp-dir)
;;			 (directory-file-name (buffer-file-name)))
;;		 (view-mode))))

;; Auto add HEADER in new file
(add-hook 'find-file-hook
	  '(lambda ()
	     (when (and (buffer-file-name)
			(not (file-exists-p (buffer-file-name)))
			(= (point-max) 1))
	       (let ((header-snippet "HEADER")
		     (yas/fallback-behavior 'return-nil))
		 (insert header-snippet)
		 ;; if can't expand snippet, clear whole buffer
		 (if (not (yas/expand))
		     (delete-region (point-min) (point-max)))))))

;; Automatically update timestamp
(add-hook 'write-file-hooks 'time-stamp)

;;;; ---------------------------------------------------------------------------
;;;; ido
;;;; ---------------------------------------------------------------------------
;; (require 'ido)
;; (setq ido-enable-flex-matching t)
;; ;; (setq ido-everywhere t)
;; ;; (ido-mode 1)
;; (setq ido-use-filename-at-point 'guess)
;; (setq ido-file-extensions-order '(".org" ".txt" ".py" ".emacs" ".xml" ".el" ".ini" ".cfg" ".cnf"))
;; (setq ido-ignore-extensions t)

;;;; ---------------------------------------------------------------------------
;;;; lusty-explorer
;;;; ---------------------------------------------------------------------------
(require 'lusty-explorer)
  ;;;; Keybindings
(add-hook 'lusty-setup-hook
	  '(lambda ()
	     (define-key lusty-mode-map (kbd "RET") 'lusty-select-current-name)
	     ))

;; Make lusty-explorer use it's own completion, not anything-completion
(when (featurep 'anything)
  (add-to-list 'anything-completing-read-handlers-alist '(lusty-file-explorer . nil))
  (add-to-list 'anything-completing-read-handlers-alist '(lusty-buffer-explorer . nil)))

;; Make lusty-explorer use it's own completion, not helm-completion
(when (featurep 'helm)
  (add-to-list 'helm-completing-read-handlers-alist '(lusty-file-explorer . nil))
  (add-to-list 'helm-completing-read-handlers-alist '(lusty-buffer-explorer . nil)))



;;;; ---------------------------------------------------------------------------
;;;; Commands
;;;; ---------------------------------------------------------------------------

(defun sudo-edit (&optional arg)
  "Edit file with sudo in emacs"
  (interactive "p")
  (if (or arg (not buffer-file-name))
      ;; (find-file (concat "/sudo:root@localhost:" (anything-read-file-name "File: ")))
      (find-file (concat "/sudo:root@localhost:" (read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))


(provide 'coldnew-file)
;; coldnew-file.el ends here.
