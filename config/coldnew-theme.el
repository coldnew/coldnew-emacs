;;; coldnew-theme.el --- default theme settings

;;;; ---------------------------------------------------------------------------
;;;; initial setting
;;;; ---------------------------------------------------------------------------

;; set them directory
(setq custom-theme-directory emacs-themes-dir)

;; load coldnew-night-theme
(load-theme 'coldnew-night t)

;;;; ---------------------------------------------------------------------------
;;;; faces
;;;; ---------------------------------------------------------------------------
;;;;;;;; Face
(defface mode-line-read-only-face
  '((t (:foreground "#C82829" :bold t)))
  "face for mode-name-string in modeline."
  :group 'mode-line)

(defface mode-line-modified-face
  '((t (:inherit 'font-lock-function-name-face :bolt t)))
  "face for mode-name-string in modeline."
  :group'mode-lin)

(defface mode-line-mode-name-face
  '((t (:inherit font-lock-keyword-face)))
  "face for mode-name-string in modeline."
  :group 'mode-line)

(defface mode-line-normal-state-face
  '((t (:inherit font-lock-function-name-face)))
  "face for emacs normal state")

(defface font-lock-escape-char-face
  '((((class color)) (:foreground "seagreen2")))
  "highlight c escapes char like vim"
  :group 'font-lock-faces)


(provide 'coldnew-theme)
;; coldnew-theme.el ends here.
