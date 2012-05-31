;;; coldnew-org.el --- org-mode config.
(eval-when-compile (require 'cl))

(require 'org-install)
(require 'org-table)
;;(require 'org-src)
;;;;;;;; org-mode extensions
(add-to-list 'auto-mode-alist '("\\.txt$" . org-mode))
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-to-list 'auto-mode-alist '("\\.org_archive$" . org-mode))

;;;;;;;; Config

(setq org-directory "~/Dropbox/Org/")
;; FIXME: still buggy
;; (setq org-mobile-inbox-for-pull "~/Dropbox/Org/TODO.org")
;; (setq org-mobile-directroy (expand-file-name "~/Dropbox/MobileOrg/"))

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
      '((sequence "TODO" "STARTED" "WAITING" "|" "DONE" "CANCLED")
	(sequence "FIXME" "BUG" "KNOWCAUSE" "|" "FIXED")))

(setq org-todo-keyword-faces '(("TODO" :foreground "red" :weight bold)
			       ("STARTED" :foreground "green" :weight bold)
			       ("WAITING" :foreground "orange" :weight bold)
			       ("DONE" :foreground "forest green" :weight bold)
			       ("CANCELLED" :foreground "forest green" :weight bold)
			       ))

;;;;;;;TODO: need mode check
;; Latex Setting
(setq org-latex-to-pdf-process
      '("xelatex -interaction nonstopmode -shell-escape %f"
	"xelatex -interaction nonstopmode -shell-escape %f"))

;;(setq org-latex-to-pdf-process '("texi2dvi --pdf --clean --verbose --batch %f"))
(setq org-src-window-setup 'current-window)

(setq org-src-fontify-natively t)

(setq org-agenda-files (list "~/Dropbox/Org/"))

(setq org-log-done t)

(setq org-tag-alist '(
		      (:startgroup . nil) ("Business" . ?b) ("School" . ?s) ("Weintek" . ?w) ("Personal" . ?p) (:endgroup . nil)
		      ))
;; remember

;; ;; Load Org Remember Stuff
;; (require 'remember)
;; (org-remember-insinuate)

;; ;; keep clock running
;; (setq org-remember-clock-out-on-exit nil)
;; (setq org-remember-templates
;;       '(("TODO"  ?t "* TODO  %?\n %x\n %a" "~/Dropbox/Org/TODO.org"  "Tasks")
;;	("FIXME" ?f "* FIXME %?\n %x\n %a" "~/Dropbox/Org/FIXME.org" "Tasks")
;;	("IDEA"  ?i "* IDEA  %?\n %i\n %a" "~/Dropbox/Org/IDEA.org"  "Idea")
;;	))

;; capture
(setq org-default-notes-file (concat org-directory "TODO.org"))
(setq org-capture-templates '(("t" "TODO" entry (file+headline "" "Tasks")
			       "* TODO %?\n %i\n %a")
			      ;; ("n" "NOTE" entry (file+headline "" "Notes To Refile")
			      ;;  "* %?\n:PROPERTIES:\n :DateCreated: %T\n:END:\n#+begin_src\n%i\n#+end_src\n\n%a")
			      ))



;; (add-hook 'org-capture-mode-hook
;;        '(lambda ()
;;           (define-key coldnew/command-mode "c" 'org-capture-finalize)
;;           ))

;; writegood-mode

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

	     (define-key coldnew/command-mode-map "c" 'org-edit-special)
	     ;; (local-set-key (kbd "C-c C-l") 'org-store-link)
	     ;; (local-set-key (kbd "C-c C-a") 'org-agenda)
	     ;; (local-set-key (kbd "C-c C-t") 'org-todo)

	     (local-set-key (kbd "C-c b") 'org-metaleft)
	     (local-set-key (kbd "C-c f") 'org-metaright)
	     (local-set-key (kbd "C-c p") 'org-metaup)
	     (local-set-key (kbd "C-c n") 'org-metadown)

	     ))


(add-hook 'org-src-mode-hook
	  '(lambda ()
	     (define-key coldnew/command-mode-map "c" 'org-edit-src-exit)
	     (local-set-key (kbd "C-c C-c") 'org-edit-src-exit)
	     ))

(add-hook 'org-agenda-mode-hook
	  '(lambda ()
	     (local-set-key (kbd "C-g") 'org-agenda-exit)
	     ))

(global-set-key (kbd "C-c a") 'org-agenda)


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




(provide 'coldnew-org)
;; coldnew-org.el ends here.
