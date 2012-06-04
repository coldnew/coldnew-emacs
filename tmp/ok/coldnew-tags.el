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
(require* 'etags-table)
(require* 'etags-select)

(setq tags-revert-without-query 1)

;;;;(setq tags-table-list '("~/.emacs.d/TAGS"))
(add-to-list 'tags-table-list "~/.emacs.d/TAGS")
;; (setq etags-table-alist
;;       (list
;;        '("~/.emacs.d/tags/TAGS")))

;;; FIXME: I disable eproject...
(defun build-ctags ()
  (interactive)
  (message "building project tags")
  (let ((root (eproject-root)))
    (shell-command (concat "ctags -e -R --extra=+fq --exclude=db --exclude=test --exclude=.git --exclude=public -f " root "TAGS " root)))
  (visit-project-tags)
  (message "tags built successfully"))

(defun visit-project-tags ()
  (interactive)
  (let ((tags-file (concat (eproject-root) "TAGS")))
    (visit-tags-table tags-file)
    (message (concat "Loaded " tags-file))))





(provide 'coldnew-tags)
;; coldnew-tags.el ends here.
