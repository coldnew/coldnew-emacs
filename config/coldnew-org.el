;;; coldnew-org.el --- org-mode config.
(eval-when-compile (require 'cl))

(require 'org-install)

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
;;;; Keybindings
;;;; ---------------------------------------------------------------------------

(provide 'coldnew-org)
;; coldnew-org.el ends here.
