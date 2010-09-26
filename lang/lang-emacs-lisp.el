
(provide 'lang-emacs-lisp)
(eval-when-compile
  (require 'cl))

(require 'eldoc)
(require 'eldoc-extension)
(require 'highlight-parentheses)
;;(require 'pretty-lambdada)

;;;;##########################################################################
;;;;  User Options, Variables
;;;;##########################################################################

(add-to-list 'auto-mode-alist '("\\.el$" . emacs-lisp-mode))

(defun local/emacs-lisp-mode-hook()
  "local emacs lisp mode hook"
  (highlight-parentheses-mode)
  (turn-on-eldoc-mode)
;;  (eldoc-add-command 'paredit-backward-delete 'paredit-close-round)
  (when (require 'init-auto-complete nil 'noerror)
    (add-to-list 'ac-sources 'ac-source-company-elisp))

  ;;;; other keybinding
  (define-key emacs-lisp-mode-map [f5] 'eval-current-buffer)
  ;;   (define-key emacs-lisp-mode-map (kbd "RET") 'electrify-return-if-match)
  )

;; All hooks
;;(add-hook 'emacs-lisp-mode-hook 'byte-compile-when-save)
;;(add-hook 'emacs-lisp-mode-hook 'remove-elc-when-visit)
(add-hook 'emacs-lisp-mode-hook 'local/emacs-lisp-mode-hook)

(when (require 'pretty-lambdada nil 'noerror)
  (add-hook 'emacs-lisp-mode-hook 'pretty-lambda))


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

