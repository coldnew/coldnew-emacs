;;; coldnew-org.el --- org-mode config.
(eval-when-compile (require 'cl))

(require 'org-install)

;; ;;;;;;;; org-mode extensions
;; (add-to-list 'auto-mode-alist '("\\.txt$" . org-mode))
;; (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; (add-to-list 'auto-mode-alist '("\\.org_archive$" . org-mode))

;; ;;;;;;;; Config

;; ;; Set MobileOrg file path
;; ;; (setq org-mobile-directroy "~/Dropbox/")

;; ;;; do not show leading stars
;; (setq org-hide-leading-stars t)

;; ;;; Latex settings
;; (setq org-latex-to-pdf-process
;;       '("xelatex -interaction nonstopmode -shell-escape %f"
;;	"xelatex -interaction nonstopmode -shell-escape %f"))

;; (setq org-src-window-setup 'current-window)

;; (setq org-src-fontify-natively t)

;; ;;;; ---------------------------------------------------------------------------
;; ;;;; Hooks
;; ;;;; ---------------------------------------------------------------------------

;; ;;; use coldnew-editor-mode
;; (add-hook 'org-mode-hook 'coldnew-editor-mode)

;; ;;;; ---------------------------------------------------------------------------
;; ;;;; Keybindings
;; ;;;; ---------------------------------------------------------------------------
;; ;; Make windmove work in org-mode:
;; (add-hook 'org-shiftup-final-hook 'windmove-up)
;; (add-hook 'org-shiftleft-final-hook 'windmove-left)
;; (add-hook 'org-shiftdown-final-hook 'windmove-down)
;; (add-hook 'org-shiftright-final-hook 'windmove-right)

;; (add-hook 'org-mode-hook
;;	  (lambda ()
;;	    (org-set-local 'yas/trigger-key [tab])
;;	    (define-key yas/keymap [tab] 'yas/next-field-or-maybe-expand)))


(provide 'coldnew-org)
;; coldnew-org.el ends here.
