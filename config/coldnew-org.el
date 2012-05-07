;;; coldnew-org.el --- org-mode config.
(eval-when-compile (require 'cl))

(require 'org-install)
;;(require 'org-src)
;;;;;;;; org-mode extensions
(add-to-list 'auto-mode-alist '("\\.txt$" . org-mode))
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-to-list 'auto-mode-alist '("\\.org_archive$" . org-mode))

;; ;;;;;;;; Config

;; ;; Set MobileOrg file path
;; ;; (setq org-mobile-directroy "~/Dropbox/")

(add-hook 'org-mode-hook
	  '(lambda ()

	     ;; do not show leading stars
	     (setq org-hide-leading-stars t)

	     ;; do not fold every content
	     (setq org-startup-folded nil)

	     ;; Latex Setting
	     (setq org-latex-to-pdf-process
		   '("xelatex -interaction nonstopmode -shell-escape %f"
		     "xelatex -interaction nonstopmode -shell-escape %f"))

	     ;;(setq org-latex-to-pdf-process '("texi2dvi --pdf --clean --verbose --batch %f"))
	     (setq org-src-window-setup 'current-window)

	     (setq org-src-fontify-natively t)
	     ))


;;;; ---------------------------------------------------------------------------
;;;; Hooks
;;;; ---------------------------------------------------------------------------

;;; FIXME: coldnew-editor-mode will cause error with org-mode
;; ;;; use coldnew-editor-mode
;;(add-hook 'org-mode-hook 'coldnew-editor-mode)

;;;; ---------------------------------------------------------------------------
;;;; REMOVE HOOKS
;;;; ---------------------------------------------------------------------------
;; remove builtin-define org-mode keybindings, this will prevent conflit
(add-hook 'org-mode-hook
	  '(lambda ()
	     (define-key org-mode-map (kbd "C-c C-f") nil)
	     ))
;;;; ---------------------------------------------------------------------------
;;;; Keybindings
;;;; ---------------------------------------------------------------------------
(add-hook 'org-mode-hook
	  '(lambda ()
	     (local-set-key (kbd "C-c C-f") 'my-anything-filelist)
	     (local-set-key (kbd "C-c C-c") 'org-edit-special)
	     ))


(add-hook 'org-src-mode-hook
	  '(lambda ()
	     (local-set-key (kbd "C-c C-c") 'org-edit-src-exit)
	     ))

(provide 'coldnew-org)
;; coldnew-org.el ends here.
