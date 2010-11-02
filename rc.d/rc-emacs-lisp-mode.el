(provide 'rc-emacs-lisp-mode)

(eval-when-compile
  (require 'cl))
(require 'eldoc nil 'noerror)
(require 'eldoc-extension nil 'noerror)
(require 'highlight-parentheses nil 'noerror)

;;;;##########################################################################
;;;;  User Options, Variables
;;;;##########################################################################

(add-to-list 'auto-mode-alist '("\\.el$" . emacs-lisp-mode))


(add-hook 'emacs-lisp-mode-hook
	  '(lambda ()
	     ;; Highlight the parentheses
	     (when (featurep 'highlight-parentheses) (highlight-parentheses-mode))
	     ;; Enable eldoc
	     (when (featurep 'eldoc) (turn-on-eldoc-mode))
	     ;; Remodify auto-complete setting in auto-complete-config.el
	     (when (featurep 'auto-complete)
	       (setq ac-sources
		     '(ac-source-symbols ac-source-company-elisp ac-source-words-in-same-mode-buffers)))
	     ;; Enable pretty-lambda
	     (when (featurep 'pretty-lambda) (turn-on-pretty-lambda-mode))

	     ;; Hooks for emacs-lisp-mode
	     (byte-compile-when-save)
	     (remove-elc-when-visit)
	     (programming-common-hook)
	     (define-key emacs-lisp-mode-map [f5] 'eval-current-buffer)))

;;;;;; Functions

(defun remove-elc-when-visit ()
  "When visit, remove <filename>.elc"
  (make-local-variable 'find-file-hook)
  (add-hook 'find-file-hook
	    (lambda ()
	      (if (file-exists-p (concat buffer-file-name "c"))
		  (delete-file (concat buffer-file-name "c"))))))

(defun byte-compile-when-save()
  "When save, recompile it"
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
	    (lambda ()
	      (if (buffer-file-name)
		  (byte-compile-file buffer-file-name)))))



;; FIXME: remove one day
(defun ac-emacs-lisp-mode-setup ()
  (setq ac-sources '(ac-source-symbols ac-source-company-elisp
				       ac-source-words-in-same-mode-buffers)))
(add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)







;; ;; 該資料夾內沒有 Tags 檔案時自動建立,若有時則更新 Tags 檔
;; (defun etags-create-or-update ()
;;   "create or update the etag file"
;;   (interactive)
;;   ;; tagfile doesn't exist?
;;   (if (not (= 0 (call-process "global" nil nil nil " -p")))
;;       (let ((olddir default-directory)
;; 	    (topdir (read-directory-name
;; 		     "gtags: top of source tree:" default-directory)))
;; 	(cd topdir)
;; 	(shell-command "gtags && echo 'created tagfile'")
;; 	(cd olddir)) ; restore
;;     ;;  tagfile already exists; update it
;;     (shell-command "global -u && echo 'updated tagfile'")))



;; (defvar my-auto-update-tags-alist
;;   (list '("/some/path/to/TAGS" "command_to_build_tags")
;;         '("/another/path/to/TAGS" "another_build_command")))

;; (defun my-auto-update-tags ()
;;   "Automatically update TAGS files"
;;   (tags-table-check-computed-list)
;;   (let ((filename (buffer-file-name))
;;         build-cmd)
;;     (mapc (lambda (tag-file)
;;             (set-buffer tag-file)
;;             (when (member filename (tags-table-files))
;;               (setq build-cmd (cdr (assoc tag-file my-auto-update-tags-alist)))
;;               (when build-cmd
;;                 (call-process build-cmd nil 0))))
;;           tags-table-computed-list)))

;; (add-hook 'after-save-hook 'my-auto-update-tags)





;;; emacs-lisp.el ends here
