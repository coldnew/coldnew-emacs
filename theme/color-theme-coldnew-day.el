;;; color-theme-coldnew-day.el ---

;; Copyright 2010 Yen-Chin,Lee
;;
;; Author: Yen-Chin,Lee <coldnew.tw@gmail.com>
;; Version: $Id: color-theme-coldnew-day.el,v 0.0 2010/09/28 16:00:50 coldnew Exp $
;; Keywords:
;; X-URL: not distributed yet

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'color-theme-coldnew-night)

;;; Code:

(provide 'color-theme-coldnew-day)
(eval-when-compile
  (require 'cl)
  (require 'color-theme))


;;;;##########################################################################
;;;;  User Options, Variables
;;;;##########################################################################

(defun color-theme-coldnew-day ()
  "Color theme modify by coldnew, update:2010-09-01."
  (interactive)
  (let ( (_Fg             "#202020")
	 (_Bg             "#E9E9E9")
	 (_Green          "#95e454")
	 (_Green+1        "#cae682")
	 (_Green+2        "#4BC98A")
	 (_Red-2          "#e5786d")
	 (_Red-1          "#e53f3f")
	 (_Red            "#E52210")
	 (_Red+1          "#E91303")
	 (_Red+3          "#EA0000")
	 (_RedGray-6      "#624646")
	 (_Blue-5         "#2e3436")
	 (_Blue-4         "#565968")
	 (_Blue-3         "#7b8793")
	 (_Blue-2         "#7f6bff")
	 (_Blue-1         "#64a8d8")
	 (_Blue           "#8ac6f2")
	 (_Blue+1         "#4871F2")
	 (_Blue+2         "#4064f2")
	 (_Blue+6         "#4D85FF")
	 (_Skyblue-1      "#b1c3d4")
	 (_Magenta        "#cc99cc")
	 (_Orange-1       "#f57900")
	 (_Orange         "#e65c00")
	 (_Orange+1       "#e9b96e")
	 (_Orange+2       "#ffc125")
	 (_Purple-1       "#ad7fa8")
	 (_Purple         "#cc99cc")
	 (_Purple+2       "#b184cb")
	 (_Purple+3       "#A67CBE")
	 (_Pink-1         "#f283b6")
	 (_Pink           "#F6B3DF")
	 (_Gray-7         "#282828")
	 (_Gray-6         "#2A2A2A")
	 (_Gray-5         "#2E2E2E")
	 (_Gray-3         "#343434")
	 (_Gray-2         "#3F3F3F")
	 (_Gray-1         "#444444")
	 (_Gray           "#424242")
	 (_Gray+1         "#99968b")
	 (_Black-1        "#0c0c0c")
	 (_White-2        "#CDCDCD")
	 (_White-4        "#C2C2C2"))

    (color-theme-install
     `(color-theme-coldnew-day
       ( (background-color . "#f8f8f7")
	 (background-mode  . light)
	 (border-color     . "#f8f8f7")
	 (cursor-color     . "#A1A1A1")
	 (foreground-color . "#000000"))
       ;; Font Lock
       (font-lock-builtin-face        ; 內建的顏色
	((t (:foreground "#9226b5"))))
       (font-lock-comment-delimiter-face ;
	((t (:foreground ,_Gray+1   :italic t :slant italic))))
       (font-lock-comment-face        ; 註解文字的顏色
	((t (:foreground "#27a027"   :italic t :slant italic))))
       (font-lock-constant-face       ; 常數的顏色
	((t (:foreground ,_Red-1              :bold t))))
       (font-lock-doc-face            ; 解釋文檔的顏色
	((t (:foreground ,_Gray+1))))
       (font-lock-function-name-face  ; 函數的顏色
	((t (:foreground "#102cc1" :italic t :bold t))))
       (font-lock-keyword-face        ; 關鍵字的顏色
	((t (:foreground "#b415c1"))))
       (font-lock-negation-char-face  ;
	((t (:foreground ,_Red))))
       (font-lock-preprocessor-face   ; 預處理字的顏色
	((t (:foreground ,_Blue))))
       (font-lock-regexp-grouping-backslash ;
	((t (:bold t :weight bold))))
       (font-lock-regexp-grouping-construct ;
	((t (:bold t ,_Green))))
       (font-lock-string-face         ; 字串的顏色
	((t (:foreground "#c7292b" :italic t ))))
       (font-lock-type-face           ; 類型的顏色
	((t (:foreground "#ca1273"))))
       (font-lock-variable-name-face  ; 變數的顏色
	((t (:foreground "#418b90"))))
       (font-lock-warning-face        ; 警告文字的顏色
	((t (:foreground ,_Red+1 :bold t))))

       ;; (font-lock-operator
       ;;   ((t (:foreground "#B1CAAA" :weight bold))))
       ;; (font-lock-pseudo-keyword
       ;;   ((t (:foreground "#dfdf8f" :weight bold))))

       (minibuffer-prompt ((t (:foreground "#7299ff" :bold t))))
       (comint-highlight-prompt ((t (:foreground ,_Red :bold t))))

       (region                        ; 選中區域的顏色
	((t (:background "#9ec2fa"))))

       (fringe                        ; 窗口邊緣
	((t (:background "#969696"))))

       ;; Mode Line
       (mode-line                     ; 使用中的buffer
	((t (:foreground "#ffffff" :background "#595959"
			 :box (:line-width 2 :color ,_Purple+2)))))
       (mode-line-inactive            ; 非使用中的buffer
	((t (:foreground ,_Blue-3 :background ,_Gray-3
			 :box (:line-width 2 :color ,_Blue-4)))))
       (mode-line-buffer-id           ; buffer 名稱
	((t (:foreground ,_White-2 :weight bold))))

       (hl-line ((t (:background ,_Gray-1))))


       (isearch ((t (:background ,_Orange-1 :foreground ,_Blue-2))))
       (isearch-lazy-highlight-face ((t (:foreground ,_Blue-2 :background ,_Orange+1))))

;;;; Show Paren
       (show-paren-mismatch-face
	((t (:background ,_Black-1 :foreground ,_Red+1 :weight bold))))
       (show-paren-match-face
	((t (:background ,_Orange :foreground ,_White-2 :weight bold))))



;;;; Org-mode
       (org-date ((t (:foreground ,_Blue+6 :bold t))))
       (org-agenda-date ((t (:foreground ,_Blue))))
       (org-agenda-date-weekend ((t (:bold t :foreground ,_Orange :weight bold))))
       (org-hide ((t (:foreground ,_Bg))))
       (org-todo ((t (:foreground ,_Pink :bold t))))
       (org-hide ((t (:foreground ,_Bg))))
       (org-done ((t (:foreground ,_Green+2 :bold t))))
       (org-level-1 ((t (:foreground ,_Blue :bold t))))
       (org-level-2 ((t (:foreground "#ee9a49"))))
       (org-level-3 ((t (:foreground "#ff83fa"))))
       (org-level-4 ((t (:foreground "#ffa500"))))
       (org-level-5 ((t (:foreground "#ff4040"))))

       ;; Parenthesis Matching
       (paren-face-match ((t (:inherit show-paren-match-face))))
       (paren-face-match-light ((t (:inherit show-paren-match-face))))
       (paren-face-mismatch ((t (:inherit show-paren-mismatch-face))))
       (show-paren-match-face ((t (:background ,_Orange :foreground "white" :bold t))))
       (show-paren-mismatch-face ((t (:background ,_Purple-1 :foreground ,_Blue-2))))

       ;; Auto Complete
       (ac-candidate-face
	((t (:background "white" :foreground "black"))))
       (ac-selection-face
	((t (:background ,_Green+1 :foreground "white" :weight bold))))

       ;; ;; Highlight-change-mode
       ;; (highlight-changes
       ;; 	((t (:background ,_Gray-5))))
       ;; (highlight-changes-delete
       ;; 	((t (:background ,_RedGray-6 :foreground nil))))


       ;; ecb
       (ecb-default-highlight-face
	((t (:background ,_Green+1 :foreground ,_Black-1 :weight bold))))


       ;; w3m
       (w3m-underline
	((t (:foreground ,_Blue))))
       (w3m-history-current-url
	((t (:foreground ,_Purple+2))))
       (w3m-anchor			; 未訪問的標題
	((t (:foreground ,_Blue :underline t))))
       (w3m-arrived-anchor		; 已訪問過的標題
	((t ((:foreground ,_Purple+2 :underline t)))))
       (w3m-current-anchor		; 當前標題
	((t ((:box (:line-width -1 :color ,_Gray-2) :weight bold)))))
       (w3m-form
	((t (:foreground ,_Gray+1
			 :underline "gray"))))

	;;;; TODO: read `http://www.emacswiki.org/emacs/LazyCatTheme.el' to add more

;;;; Woman

       (woman-italic-face ((t (:slant italic :weight bold))))
       (woman-unknown ((t (:foreground ,_Red+3 :weight bold))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

       (woman-addition ((t (:foreground "cadet blue"))))
       (woman-bold ((t (:inherit bold :foreground "CadetBlue3"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


















;;;; WhiteSpace Mode
       (whitespace-space              ; 顯示 Space
	((((class color) (background dark))
	  (:background "gray83" :foreground "black"))))
       (whitespace-tab                ; 顯示 TAB
	((((class color) (background dark))
	  (:background "gray57" :foreground "gray77"))))

;;;; Eshell
       (eshell-ls-directory ((((class color) (background dark))
			      (:foreground "blue2" :weight bold))))
       (eshell-ls-readonly ((((class color) (background dark))
			     (:foreground "white"))))
;;;; ElScreen
       (elscreen-tab-background-face
	((t (:background "#272729" ))))
       (elscreen-tab-control-face
	((t (:foreground "white" :background "black" :weight extra-bold))))
       (elscreen-tab-current-screen-face
	((t (:background "#250628" :foreground "Gray90" :weight bold))))
       (elscreen-tab-other-screen-face
	((t (:background "#1D1D1F" :foreground "Gray85" :weight bold))))

;;;; Dired
       (diredp-date-time              ; 修改時間
	((t (:foreground "Grey60"))))
       (diredp-deletion               ; 刪除標記
	((t (:background "Black" :foreground "red"))))
       (diredp-deletion-file-name     ; 刪除文件
	((t (:foreground "red"))))
       (diredp-dir-heading            ; 目錄
	((t (:background "Black" :foreground "Gold"))))
       (diredp-dir-priv               ; 目錄掩碼
	((t (:background "Black" :foreground "DodgerBlue"))))
       (diredp-display-msg            ; 路徑
	((t (:foreground "Gold"))))
       (diredp-exec-priv              ; 可執行掩碼
	((t (:background "Black" :foreground "DeepSkyBlue3"))))
       (diredp-file-name              ; 文件
	((t (:foreground "Green3"))))
       (diredp-file-suffix            ; 文件擴展名
	((t (:foreground "Green4"))))
       (diredp-flag-mark              ; 選中標記
	((t (:background "Black" :foreground "Cyan"))))
       (diredp-flag-mark-line         ; 選中文件
	((t (:background "Black" :foreground "Cyan"))))
       (diredp-ignored-file-name      ; 忽略的文件
	((t (:foreground "grey40"))))
       (diredp-no-priv                ; 無掩碼
	((t (:background "Black" :foreground "Green"))))
       (diredp-other-priv             ; 其他掩碼
	((t (:background "Black" :foreground "khaki"))))
       (diredp-rare-priv              ; 稀有的掩碼
	((t (:background "Black" :foreground "Red"))))
       (diredp-read-priv              ; 讀取掩碼
	((t (:background "Black" :foreground "IndianRed"))))
       (diredp-write-priv             ; 寫入掩碼
	((t (:background "Black" :foreground "Gold3"))))

;;;; ibuffer
       (ibuffer-deletion ((t (:foreground "#dfaf8f" :weight bold))))
       (ibuffer-help-buffer ((t (:inherit font-lock-comment))))
       (ibuffer-marked ((t (:foreground "#f0dfaf" :weight bold))))
       (ibuffer-special-buffer ((t (:inherit font-lock-doc))))

;;;; rcirc
       (rcirc-other-nick
	((((class color) (min-colors 88) (background dark)) nil)))
       (rcirc-prompt
	((((min-colors 88) (background dark)) (:foreground "gold2"))))
       (rcirc-server
	((((class color) (min-colors 88) (background dark)) (:foreground "purple2"))))
       (rcirc-server-prefix
	((default (:foreground "seagreen")) (((class color) (min-colors 16)) nil)))
       (rcirc-timestamp
	((t (:foreground "green3" :weight bold))))



       ))))


;;; color-theme-coldnew-night.el ends here
