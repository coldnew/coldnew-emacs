;;; rc-base.el ---
(eval-when-compile (require 'cl))

;;;; 個人資訊設定
(setq-default user-mail-address "coldnew.tw@gmail.com" )
(setq-default user-full-name    "Yen-Chin,Lee"         )
(setq-default user-nickname     "coldnew"              )

;;;; 移除掉的預設功能
(setq-default visible-bell                  t ) ; 關閉出錯時的警告鈴聲
(setq-default inhibit-startup-message       t ) ; 關閉 Emacs 啟動時的螢幕閃爍
(setq-default gnus-inhibit-startup-message  t ) ; 去掉 GNU 引導介面
(setq-default ring-bell-function (lambda () t)) ; 關閉 console 下的螢幕閃爍

(setq-default confirm-nonexistent-file-or-buffer nil)
(setq ido-create-new-buffer 'always)
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
	    kill-buffer-query-functions))

;;;; Debugging emacs
(setq-default max-specpdl-size    32000 )
(setq-default max-lisp-eval-depth 20000 )
(setq debug-on-error t)
(setq debug-on-quit  t)

;;;; 基本外觀設置
(menu-bar-mode          t ) ; 移除菜單欄
(blink-cursor-mode     -1 )    ; 關閉游標閃爍
(scroll-bar-mode       -1 )    ; 去掉滾動條，使用鼠標滾輪
(tool-bar-mode         -1 )    ; 去掉工具欄
(transient-mark-mode    t )    ; 高亮顯示要拷貝的區域

;;;; 其他基礎設置
(fset 'yes-or-no-p 'y-or-n-p )   ; 所有問題使用 y/n 回答
(icomplete-mode            t )   ; 用  M-x 選命令時，給予可選的命令提示
(auto-compression-mode     t )   ; 打開壓縮文件時自動解壓縮
(show-paren-mode           t )   ; 顯示括號匹配
(global-auto-revert-mode   t )   ; 自動重讀修改過的檔案
(auto-image-file-mode      t )   ; 圖片顯示功能
(iimage-mode               t )
;; (desktop-save-mode         t )
(mouse-avoidance-mode 'animate ) ; 鼠標自動避開游標

;;;; 編輯環境
(setq-default initial-scratch-message       nil )
(setq-default major-mode    'text-mode ) ; 預設使用 text-mode
(setq-default line-spacing                    4 ) ; 設定行間距為4格
(setq-default fill-column                   100 ) ; 顯示默認的文檔的寬度為100列
(setq-default kill-ring-max                 300 ) ; 設定刪除保存紀錄為300
;; (setq show-paren-style     'expression )
(setq-default track-eol                       t ) ; 在行尾上下移動時保持在行尾
(setq-default kill-whole-line                 t ) ; 在行首使用C-k時，同時刪除該行
(setq-default global-font-lock-mode           t ) ; 啟用全域語法高亮
(setq-default global-auto-revert-mode         t ) ; 自動重新整理 Buffer
(setq-default transient-mark-mode             t ) ; 高亮選中區塊
(setq-default require-final-newline           t ) ; 在文檔最後自動插入一行空白
(setq-default delete-trailing-whitespace      t ) ; 刪除每行最後的空格
(setq-default x-stretch-cursor                t ) ; 全形符號時游標延展
(setq-default x-select-enable-clipboard       t ) ; 支持 Emacs 和外部程序的黏貼
(setq-default mouse-yank-at-point             t ) ; 允許滑鼠中鍵黏貼
(setq-default line-number-mode                t ) ; 顯示行號
(setq-default column-number-mode              t ) ; 顯示列號
(setq-default partial-completion-mode         t ) ; 啟用自動補全函數和變量
(setq-default enable-recursive-minibuffers    t ) ; 可以遞迴的使用 mini-buffer
(setq-default completion-ignore-case          t ) ; 自動補全忽略大小寫
(setq-default scroll-preserve-screen-position t ) ;
(setq-default minibuffer-electric-default-mode t ) ; 啟用部份補全
(setq-default view-read-only t ) ; 開啟惟讀檔時啟用 view-mode

(cua-mode                   t )         ; 啟用 cua-mode
(setq-default cua-enable-cua-keys nil )	; 不使用 cua-mode 裡的按鍵
(delete-selection-mode      t )         ; 啟用 delsel-mode
(visual-line-mode           t )         ; 折行時不拆開一個 "字"

;; uniquify changes conflicting buffer names from file<2> etc
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;;  當 emacs 建立新檔案至不存在的資料夾時，於儲存前自動建立該資料夾
(add-hook 'before-save-hook
	  '(lambda ()
	     (or (file-exists-p  (file-name-directory buffer-file-name))
		 (make-directory (file-name-directory buffer-file-name) t))))

(provide '003-base)
;;; 003-base.el ends here
