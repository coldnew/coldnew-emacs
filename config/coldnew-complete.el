;;; coldnew-complete.el ---
(eval-when-compile (require 'cl))


 ;;;; ---------------------------------------------------------------------------
 ;;;; auto complete
 ;;;; ---------------------------------------------------------------------------
(require 'auto-complete)
(require 'auto-complete-config)
(require* 'auto-complete-clang)

;; use default config
(ac-config-default)

;; desable fuzzy-match
(setq ac-use-fuzzy nil)

;; start auto-complete after insert 2 or more-key
(setq ac-auto-start 3)

;; Ignore case if completion target string doesn't include upper characters
(setq ac-ignore-case 'smart)

;; Enable auto-complete quick help
(setq ac-use-quick-help t)

;; After 0.01 sec, show help window
(setq ac-quick-help-delay 0.5)

;; Enable ac-comphist
(setq ac-use-comphist t)

;; Setting ac-comphist data
(setq ac-comphist-file (concat emacs-cache-dir "auto-complete.dat"))

;; Show menu
(setq ac-auto-show-menu t)

;; Enable ac-menu-map
(setq ac-use-menu-map t)

 ;;;; Keybindings
(define-key ac-menu-map (kbd "C-n") 'ac-next)
(define-key ac-menu-map (kbd "C-p") 'ac-previous)
(define-key ac-mode-map (kbd "C-c h") 'ac-last-quick-help)
(define-key ac-mode-map (kbd "C-c H") 'ac-last-help)
(define-key ac-completing-map "\t" 'ac-complete)
(define-key ac-complete-mode-map [tab] 'ac-expand)

 ;;;; ---------------------------------------------------------------------------
 ;;;; Helm
 ;;;; ---------------------------------------------------------------------------
(require* 'helm)
(require 'helm-config)

;; Use predefined configurations for `helm.el'
(setq helm-config t)

;; Enable helm globally
(helm-mode 1)

;; Enable dired binding
(helm-dired-bindings 1)

(require 'helm-etags+)
(require 'ctags-update)
(ctags-update-minor-mode 1)


;; ;;;; ---------------------------------------------------------------------------
;; ;;;; Commands
;; ;;;; ---------------------------------------------------------------------------

(defun coldnew/helm-filelist ()
  "Preconfigured `anything' to open files/buffers/bookmarks instantly.
 This is a replacement for `anything-for-files'.
 See `anything-c-filelist-file-name' docstring for usage."
  (interactive)
  (helm-other-buffer
   '(
     helm-c-source-buffers-list
     helm-c-source-recentf
     helm-c-source-ffap-line
     helm-c-source-ffap-guesser
     helm-c-source-bookmarks
     helm-c-source-file-cache
     helm-c-source-projectile-files-list
     helm-c-source-files-in-current-dir
     helm-c-source-locate)
   "*coldnew/filelist*"))


(defun coldnew/helm-occur ()
  "I don't like highlight when goto lines."
  (interactive)
  ;; FIXME: is there more elegent way to make temp face?
  (set (make-local-variable 'face-remapping-alist) '((helm-selection-line nil)))
  (helm-occur))

(defun helm-c-occur-get-line (s e)
  "rewrite `helm-c-occur-get-line' to make it color on line-number."
  (concat (propertize (format "%7d" (line-number-at-pos (1- s))) 'face '((:foreground "red")))
	  (format ": %s" (buffer-substring s e))))





(provide 'coldnew-complete)
;; coldnew-complete.el ends here.
