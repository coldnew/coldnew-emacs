;;; coldnew-eshell.el ---
(eval-when-compile (require 'cl))

(require 'eshell)
(require 'em-dirs)
(require 'em-hist)
(require 'em-prompt)
(require 'em-term)
(require 'em-cmpl)

;;;; ---------------------------------------------------------------------------
;;;; Config
;;;; ---------------------------------------------------------------------------

;;; Setup prompt function
(setq eshell-prompt-function
      '(lambda ()
	 (concat
	  user-login-name "@" system-name " "
	  (if (search (directory-file-name (expand-file-name (getenv "HOME"))) (eshell/pwd))
	      (replace-regexp-in-string (expand-file-name (getenv "HOME")) "~" (eshell/pwd))
	    (eshell/pwd))
	  (if (= (user-uid) 0) " # " " $ ")
	  )))

;;; change history file path
(setq eshell-last-dir-ring-file-name (concat emacs-cache-dir "eshell-lastdir"))
(setq eshell-history-file-name (concat emacs-cache-dir "eshell-history"))

;; other setting
(setq eshell-save-history-on-exit t)
(setq eshell-ask-to-save-last-dir nil)
(setq eshell-history-size 512)
(setq eshell-hist-ignoredups t)
(setq eshell-cmpl-cycle-completions nil)
(setq eshell-scroll-to-bottom-on-output t)
(setq eshell-show-maximum-output t)

;;;; ---------------------------------------------------------------------------
;;;; Hooks
;;;; ---------------------------------------------------------------------------

;;; Make eshell prompt more colorful
(add-to-list 'eshell-output-filter-functions 'coldnew/colorfy-eshell-prompt)

;; my auto-complete for elisp
(add-hook 'eshell-mode-hook 'auto-complete-mode)
(add-hook 'eshell-mode-hook 'ac-eshell-mode-setup)

;; use helm to complete esehll
(when (featurep 'helm)
  (add-hook 'eshell-mode-hook
	    #'(lambda ()
		(define-key eshell-mode-map
		  [remap pcomplete]
		  'helm-esh-pcomplete))))

;;;; ---------------------------------------------------------------------------
;;;; Keybindings
;;;; ---------------------------------------------------------------------------

;; (define-key eshell-command-map (kbd "C-a") 'eshell-bol)
;; (add-hook 'eshell-mode-hook
;;	  '(lambda ()
;;	     (define-key evil-insert-state-local-map (kbd "C-a") 'eshell-bol)
;;	     (define-key evil-insert-state-local-map (kbd "C-b") 'eshell-backward-argument)
;;	     (define-key evil-insert-state-local-map (kbd "C-e") 'eshell-show-maximum-output)
;;	     (define-key evil-insert-state-local-map (kbd "C-f") 'eshell-forward-argument)
;;	     (define-key evil-insert-state-local-map (kbd "C-o") 'eshell-kill-output)
;;	     (define-key evil-insert-state-local-map (kbd "C-r") 'eshell-show-output)
;;	     ;; (define-key evil-insert-state-local-map (kbd "C-t") 'eshell-truncate-buffer)
;;	     (define-key evil-insert-state-local-map (kbd "C-u") 'eshell-kill-input)
;;	     (define-key evil-insert-state-local-map (kbd "M-l") 'backward-kill-word)
;;	     (define-key evil-insert-state-local-map (kbd "M-l") 'backward-delete-char)
;;	     ))


;;;; ---------------------------------------------------------------------------
;;;; eshell/command
;;;; ---------------------------------------------------------------------------

;; find-file
;; (defun eshell/ef (file) (find-file file))
(defun eshell/ef (&rest args) (eshell/emacs args))

;; ediff
(defun eshell/ed (file1 file2) (ediff file1 file2))

;; clear
(defun eshell/clear ()
  "Clears the shell buffer ala Unix's clear or DOS' cls"
  (interactive)
  ;; the shell prompts are read-only, so clear that for the duration
  (let ((inhibit-read-only t))
    ;; simply delete the region
    (delete-region (point-min) (point-max))))


(defun eshell/info (subject)
  "Read the Info manual on SUBJECT."
  (let ((buf (current-buffer)))
    (Info-directory)
    (let ((node-exists (ignore-errors (Info-menu subject))))
      (if node-exists
	  0
	;; We want to switch back to *eshell* if the requested
	;; Info manual doesn't exist.
	(switch-to-buffer buf)
	(eshell-print (format "There is no Info manual on %s.\n"
			      subject))
	1))))

(defun eshell/emacs (&rest args)
  "Open a file in emacs. Some habits die hard."
  (if (null args)
      ;; If I just ran "emacs", I probably expect to be launching
      ;; Emacs, which is rather silly since I'm already in Emacs.
      ;; So just pretend to do what I ask.
      (bury-buffer)
    ;; We have to expand the file names or else naming a directory in an
    ;; argument causes later arguments to be looked for in that directory,
    ;; not the starting directory
    (mapc #'find-file (mapcar #'expand-file-name (eshell-flatten-list (reverse args))))))



;;;; ---------------------------------------------------------------------------
;;;; Autocomplete
;;;; ---------------------------------------------------------------------------

;; define ac-source for eshell-pcomplete
(ac-define-source eshell-pcomplete
  '((candidates . pcomplete-completions)
    (cache)
    (symbol . "f")))

(defun ac-eshell-mode-setup ()
  "auto-complete settings for eshell-mode"
  (setq ac-sources
	'(
	  ac-source-eshell-pcomplete
	  ;; ac-source-symbols
	  ;; ac-source-variables
	  ;; ac-source-functions
	  ;; ac-source-features
	  ;; ac-source-filename
	  ;; ac-source-files-in-current-dir
	  ;; ac-source-words-in-same-mode-buffers
	  )))

;;;; ---------------------------------------------------------------------------
;;;; Functions
;;;; ---------------------------------------------------------------------------
(defun coldnew/colorfy-eshell-prompt ()
  (interactive)
  (let* ((mpoint)
	 (user-string-regexp (concat "^" user-login-name "@" system-name)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward (concat user-string-regexp ".*[$#]") (point-max) t)
	(setq mpoint (point))
	(overlay-put (make-overlay (point-at-bol) mpoint) 'face '(:foreground "dodger blue")))
      (goto-char (point-min))
      (while (re-search-forward user-string-regexp (point-max) t)
	(setq mpoint (point))
	(overlay-put (make-overlay (point-at-bol) mpoint) 'face '(:foreground "green3"))
	))))



(provide 'coldnew-eshell)
;; coldnew-eshell.el ends here.
