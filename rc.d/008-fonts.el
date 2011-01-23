;;; rc-fonts.el ---
(eval-when-compile (require 'cl))

;;;; 建立新的 fontset
(create-fontset-from-fontset-spec
 "-*-liberation mono-medium-r-*--14-*-*-*-*-*-fontset-coldnew")

;;;; 設定其他編碼的字型
(set-fontset-font "fontset-coldnew"     ; 中文字體
		  'han (font-spec :family "LiHei Pro" :size 16))

(set-fontset-font "fontset-coldnew"     ; 符號
		  'symbol (font-spec :family "Monaco" :size 20 ))

;;; 使用自己建立的 fontset
(set-frame-font "fontset-coldnew")

;;;; 讓新開的 frame 使用特定的fontset
(add-to-list 'default-frame-alist '(font . "fontset-coldnew"))

;;;; 字型顯示樣本
(setq-default list-faces-sample-text
	      "ABCDEFTHIJKLMNOPQRSTUVWXYZ abcdefghijklmnopqrstuvwxyz 11223344556677889900
     ABCDEFTHIJKLMNOPQRSTUVWXYZ abcdefghijklmnopqrstuvwxyz 壹貳參肆伍陸柒捌玖零")



(provide '008-fonts)
;;; 008-fonts.el ends here
