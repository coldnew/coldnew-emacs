;;
(eval-when-compile (require 'cl))


;; ;;;; 建立新的 fontset
;; (create-fontset-from-fontset-spec
;;  "-*-liberation mono-medium-r-*--14-*-*-*-*-*-fontset-coldnew")

;; ;;;; 設定其他編碼的字型
;; (set-fontset-font "fontset-coldnew"     ; 中文字體
;;		  'han (font-spec :family "LiHei Pro" :size 16))

;; (set-fontset-font "fontset-coldnew"     ; 符號
;;		  'symbol (font-spec :family "Monaco" :size 20 ))

;; ;;; 使用自己建立的 fontset
;; (set-frame-font "fontset-coldnew")

;; ;;;; 讓新開的 frame 使用特定的 fontset
;; (add-to-list 'default-frame-alist '(font . "fontset-coldnew"))

;;;; 字型顯示樣本
(setq-default list-faces-sample-text
	      (concat
	       "ABCDEFTHIJKLMNOPQRSTUVWXYZ abcdefghijklmnopqrstuvwxyz 11223344556677889900"
	       "ABCDEFTHIJKLMNOPQRSTUVWXYZ abcdefghijklmnopqrstuvwxyz 壹貳參肆伍陸柒捌玖零"
	       ))

(defvar emacs-english-font "Inconsolata"
  "The font name of English.")
(defvar emacs-cjk-font "LiHei Pro"
  "The font name for CJK.")
(defvar emacs-symbol-font "Monaco"
  "The font name for Synbol.")

(defvar emacs-english-font-size 13
  "Default English font size.")
(defvar emacs-cjk-font-size 11
  "Default CJK font size.")
(defvar emacs-symbol-font-size 10
  "Default Symbol font size.")

(set-frame-font (format "%s-%s" (eval emacs-english-font) (eval emacs-english-font-size)))

(set-fontset-font (frame-parameter nil 'font)
		  'han (format "%s-%s" (eval emacs-cjk-font) (eval emacs-cjk-font-size)))

(set-fontset-font (frame-parameter nil 'font)
		  'symbol (format "%s-%s" (eval emacs-symbol-font) (eval emacs-symbol-font-size)))


(provide 'coldnew-fonts)
;; coldnew-fonts.el ends here.
