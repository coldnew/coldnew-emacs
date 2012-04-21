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

(defface mode-line-evil-state-string-N
  '((t (:inherit font-lock-function-name-face)))
  "face for vim-string in normal-map on mode-line."
  :group 'mode-line)

(defface mode-line-evil-state-string-I
  '((t (:inherit font-lock-constant-face)))
  "face for vim-string in insert-map on mode-line."
  :group 'mode-line)

(defface mode-line-evil-state-string-V
  '((t (:inherit font-lock-variable-name-face)))
  "face for vim-string in visual-map on mode-line."
  :group 'mode-line)

(defface mode-line-evil-state-string-E
  '((t (:inherit font-lock-string-face)))
  "face for vim-string in emacs-map on mode-line."
  :group 'mode-line)

(defface font-lock-escape-char-face
  '((((class color)) (:foreground "seagreen2")))
  "highlight c escapes char like vim"
  :group 'font-lock-faces)


(provide 'coldnew-theme)
;; coldnew-theme.el ends here.
