;;; coldnew-org.el --- org-mode config.
(eval-when-compile (require 'cl))

(require 'org-install)
(require 'org-table)
;;(require 'org-src)
;;;;;;;; org-mode extensions
(add-to-list 'auto-mode-alist '("\\.txt$" . org-mode))
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-to-list 'auto-mode-alist '("\\.org_archive$" . org-mode))

;; ;;;;;;;; Config

;; ;; Set MobileOrg file path
;; ;; (setq org-mobile-directroy "~/Dropbox/")
;; (defun my-buffer-face-mode-variable ()
;;   "Set font to a variable width (proportional) fonts in current buffer"
;;   (interactive)
;;   (setq buffer-face-mode-face '(:family "Hiragino Sans GB W3" :height 100))
;;   ;; (setq buffer-face-mode-face '(:family "DejaVu Sans" :height 100 :width semi-condensed))
;;   (buffer-face-mode))
;; (defun aa ()
;;   (interactive)
;;   (setq variable-pitch '(:family "Hiragino Sans GB W3" ))
;;   (variable-pitch-mode)
;;   )
;; (setq buffer-face-mode-face '(:family "DejaVu Sans" :height 100 :width semi-condensed))

;; do not show leading stars
(setq org-hide-leading-stars t)

;; do not fold every content
(setq org-startup-folded nil)
;; indent all at startup
(setq org-startup-indented t)
;; Make org-mode compatible with cua-mode
(setq org-CUA-compatible t)

;;; setting org-todo keywords
(setq org-todo-keywords
      '((sequence "TODO" "|" "DONE")
	(sequence "FIXME" "BUG" "KNOWCAUSE" "|" "FIXED")))

;;;;;;;TODO: need mode check
;; Latex Setting
(setq org-latex-to-pdf-process
      '("xelatex -interaction nonstopmode -shell-escape %f"
	"xelatex -interaction nonstopmode -shell-escape %f"))

;;(setq org-latex-to-pdf-process '("texi2dvi --pdf --clean --verbose --batch %f"))
(setq org-src-window-setup 'current-window)

(setq org-src-fontify-natively t)


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
	     (local-set-key (kbd "C-c C-l") 'org-store-link)
	     (local-set-key (kbd "C-c C-a") 'org-agenda)
	     (local-set-key (kbd "C-c C-t") 'org-todo)
	     (key-chord-define org-mode-map "cc"  'org-edit-special)
	     ))


(add-hook 'org-src-mode-hook
	  '(lambda ()
	     (local-set-key (kbd "C-c C-c") 'org-edit-src-exit)
	     (key-chord-define org-src-mode-map "cc"  'org-edit-src-exit)
	     ))




(provide 'coldnew-org)
;; coldnew-org.el ends here.
