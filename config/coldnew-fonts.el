;;; coldnew-fonts.el ---
(eval-when-compile (require 'cl))

;; Font type setting
(defvar emacs-english-font "Inconsolata"
  "The font name of English.")
(defvar emacs-cjk-font "LiHei Pro"
  "The font name for CJK.")
(defvar emacs-symbol-font "Monaco"
  "The font name for Synbol.")

;; font size setting
(defvar emacs-english-font-size 12
  "Default English font size.")
(defvar emacs-cjk-font-size 10
  "Default CJK font size.")
(defvar emacs-symbol-font-size 10
  "Default Symbol font size.")

;; Use my defined font under X
(cond ((eq window-system 'x)
       ;; Setting English Fonts
       (if (font-exist-p emacs-english-font)
	   (set-frame-font (format "%s-%s" (eval emacs-english-font) (eval emacs-english-font-size))))

       ;; Setting Chinese Fonts
       (if (font-exist-p emacs-cjk-font)
	   (set-fontset-font (frame-parameter nil 'font)
			     'han (format "%s-%s" (eval emacs-cjk-font) (eval emacs-cjk-font-size))))

       ;; Setting Symbol Fonts
       (if (font-exist-p emacs-symbol-font)
	   (set-fontset-font (frame-parameter nil 'font)
			     'symbol (format "%s-%s" (eval emacs-symbol-font) (eval emacs-symbol-font-size))))
       ))


;; list text sample
(setq-default list-faces-sample-text
	      (concat
	       "ABCDEFTHIJKLMNOPQRSTUVWXYZ abcdefghijklmnopqrstuvwxyz 11223344556677889900"
	       "ABCDEFTHIJKLMNOPQRSTUVWXYZ abcdefghijklmnopqrstuvwxyz 壹貳參肆伍陸柒捌玖零"
	       ))


(provide 'coldnew-fonts)
;; coldnew-fonts.el ends here.
