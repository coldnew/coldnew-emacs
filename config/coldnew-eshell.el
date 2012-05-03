;;; coldnew-eshell.el ---
(eval-when-compile (require 'cl))

(require 'eshell)

;;; Setup prompt function
(setq eshell-prompt-function
      '(lambda ()
	 (concat
	  user-login-name "@" system-name " "
	  (if (string= (eshell/pwd) (directory-file-name (expand-file-name (getenv "HOME"))))
	      "~"
	    (eshell/pwd))
	  (if (= (user-uid) 0) " # " " $ ")
	  )))
;;; Make eshell prompt more colorful
(add-to-list 'eshell-output-filter-functions 'coldnew/colorfy-eshell-prompt)

;;;; key
;; (define-key eshell-command-map (kbd "C-a") 'eshell-bol)
(add-hook 'eshell-mode-hook
	  '(lambda ()
	     (define-key evil-insert-state-local-map (kbd "C-a") 'eshell-bol)
	     ))

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
