;;; coldnew-tags.el --- some config for using tas
(eval-when-compile (require 'cl))

;;;; ---------------------------------------------------------------------------
;;;; gtags
;;;; ---------------------------------------------------------------------------
(require 'gtags)



;;;;;;;; Keybinding
(define-key gtags-mode-map (kbd "M-.") 'gtags-find-tag)
(define-key gtags-mode-map (kbd "M-,") 'gtags-find-rtag)

;;;;;;;; Functions
(defun gtags-create-or-update ()
  "create or update the gnu global tag file"
  (interactive)
  (if (not (= 0 (call-process "global" nil nil nil " -p"))) ; tagfile doesn't exist?
      (let ((olddir default-directory)
	    (topdir (read-directory-name
		     "gtags: top of source tree:" default-directory)))
	(cd topdir)
	(shell-command "gtags && echo 'created tagfile'")
	(cd olddir)) ; restore
    ;;  tagfile already exists; update it
    (shell-command "global -u && echo 'updated tagfile'")))

;;;; ---------------------------------------------------------------------------
;;;; etags
;;;; ---------------------------------------------------------------------------
(require 'etags)

(setq tags-revert-without-query 1)

(provide 'coldnew-tags)
;; coldnew-tags.el ends here.
