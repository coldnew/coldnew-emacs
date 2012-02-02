;;
(eval-when-compile (require 'cl))

;;;;;;;; Packages Import
(require 'coldnew-macro)
(require 'coldnew-functions)
(require 'coldnew-commands)
(require 'coldnew-variables)
(require 'coldnew-evil)


;;;;see
;; https://github.com/ZaneA/Dotfiles/blob/master/.emacs#L206

(defun evil-mode-string ()
  (let ((evil-state-string (substring evil-mode-line-tag 2 3)))
    (setq evil-state-string-face
	  (cond
	   ((string= "N" evil-state-string) 'mode-line-evil-state-string-N)
	   ((string= "I" evil-state-string) 'mode-line-evil-state-string-I)
	   ((string= "V" evil-state-string) 'mode-line-evil-state-string-V)
	   ((string= "E" evil-state-string) 'mode-line-evil-state-string-E)
	   ))
    (concat "<" (propertize evil-state-string 'face evil-state-string-face) ">")
    ))

(defun mode-line-major-mode ()
  "Get major-mode name with << >>."
  (concat "<< " (propertize mode-name 'face 'mode-line-mode-name-face) " >>")
  )

;;TODO: add mode line face
;; (setq mode-line-in-non-selected-windows nil)
;;;;;;;; Settings
;; Make all mode-line use mode-line-format as default
(setq-default mode-line-format
	      '((" "
		 mode-line-mule-info
		 ;; read-only or modified status
		 (:eval
		  (cond (buffer-read-only
			 (propertize "RO" 'face 'mode-line-read-only-face))
			((buffer-modified-p)
			 (propertize "**" 'face 'mode-line-modified-face))
			(t "--")))
		 "   "
		 ;;		 mode-line-modified
		 ;;	 mode-line-frame-identification
		 (when (featurep 'evil)
		   (:eval (evil-mode-string)))
		 "   "
		 mode-line-buffer-identification
		 " "
		 ;; line and column
		 "("
		 (:eval (propertize "%02l" 'face 'font-lock-type-face))
		 ","
		 (:eval (propertize "%02c" 'face 'font-lock-type-face))
		 ")"
		 "   "

		 ;; major-mode name
		 (:eval (mode-line-major-mode))

		 "   "
		 (vc-mode vc-mode)
		 "   "
		 ;; relative position, size of file
		 "["
		 (:eval (propertize "%p" 'face 'font-lock-constant-face)) ;; % above top
		 "/"
		 (:eval (propertize "%I" 'face 'font-lock-constant-face)) ;; size
		 "] "
		 s
		 ;; "   "
		 ;; (when (require* 'pomodoro)
		 ;;   pomodoro-display-string)
		 ;; (which-func-mode ("" which-func-format ""))
		 ;; mode-line-position
		 ;; display-time-string
		 )))



;; (defun vimpulse-set-mode-line-face ()
;;   (unless (minibufferp (current-buffer))
;;     (set-face-background 'mode-line
;;                          (cdr (assq viper-current-state
;;                                     '((vi-state       . "Black")
;;                                       (insert-state   . "Red")
;;                                       (emacs-state    . "Wheat")
;;                                       (operator-state . "Green")
;;                                       (visual-state   . "Blue")))))))

;; (add-hook 'viper-vi-state-hook    'vimpulse-set-mode-line-face)
;; (setq visible-bell nil)
;; (add-hook 'viper-insert-state-hook      'vimpulse-set-mode-line-face)
;; (add-hook 'viper-emacs-state-hook       'vimpulse-set-mode-line-face)
;; (add-hook 'vimpulse-operator-state-hook 'vimpulse-set-mode-line-face)
;; (add-hook 'vimpulse-visual-state-hook   'vimpulse-set-mode-line-face)
;; (defadvice set-buffer (after vimpulse-mode-line-face activate)
;;   (vimpulse-set-mode-line-face))
;; (defadvice find-file (after vimpulse-mode-line-face activate)
;;   (vimpulse-set-mode-line-face))
;; (defadvice kill-buffer (after vimpulse-mode-line-face activate)
;;   (vimpulse-set-mode-line-face))
;; (defadvice switch-to-buffer (after vimpulse-mode-line-face activate)
;;   (vimpulse-set-mode-line-face))
;; (defadvice select-window (after vimpulse-mode-line-face activate)
;;   (vimpulse-set-mode-line-face))
;; (defadvice delete-window (after vimpulse-mode-line-face activate)
;;   (vimpulse-set-mode-line-face))
;; http://d.hatena.ne.jp/mizchi/20110224/1298520369



(provide 'coldnew-modeline)
;; coldnew-modeline.el ends here.
