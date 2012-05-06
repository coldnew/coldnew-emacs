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
(setq ac-auto-start 2)

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
;;;; anything
;;; ---------------------------------------------------------------------------
(require 'anything-config)
(require 'anything-match-plugin)

;; Use predefined configurations for `anything.el'
(setq anything-config t)

;; Enable anything globally
(ac-mode 1)

;; Path of file where history information is stored.
(setq anything-c-adaptive-history-file (concat emacs-cache-dir "anything.cache"))
;; Setting ignore file regexp
(setq anything-c-boring-file-regexp
      (concat anything-c-boring-file-regexp
	      (rx
	       (and (or ".out" ".elc") eol))))

;;;;;;;; keybinding
(define-key anything-occur-map (kbd "C-r") 'anything-occur-run-query-replace-regexp)

;;;; ---------------------------------------------------------------------------
;;;; Helm
;;;; ---------------------------------------------------------------------------
(require* 'helm)
(require 'helm-config)

;; Use predefined configurations for `helm.el'
(setq helm-config t)

;; Enable helm globally
(helm-mode 1)

;;;; ---------------------------------------------------------------------------
;;;; Commands
;;;; ---------------------------------------------------------------------------


(defun my-anything-filelist ()
  "Preconfigured `anything' to open files/buffers/bookmarks instantly.

This is a replacement for `anything-for-files'.
See `anything-c-filelist-file-name' docstring for usage."
  (interactive)
  (anything
   :prompt "Switch to: "
   :source
   '(
     anything-c-source-recentf
     anything-c-source-ffap-line
     anything-c-source-ffap-guesser
     ;; anything-c-source-buffers-list
     anything-c-source-bookmarks
     anything-c-source-file-cache
     anything-c-source-filelist)))

(defun my-anything-occur ()
  "I don't like highlight when goto lines."
  (interactive)
  (let ((anything-match-line-overlay-face nil)
	(recenter-redisplay nil))
    (anything-occur))
  (recenter))

(defun anything-c-occur-get-line (s e)
  "rewrite `anything-c-occur-get-line' to make it color on line-number."
  (concat (propertize (format "%7d" (line-number-at-pos (1- s))) 'face '((:foreground "red")))
	  (format ": %s" (buffer-substring s e))))






(provide 'coldnew-complete)
;; coldnew-complete.el ends here.
