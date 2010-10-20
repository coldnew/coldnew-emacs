;; init lang.cpp

(provide 'rc-cpp-mode)
(eval-when-compile
  (require 'cl))


;;(defun local/cpp-mode-hook ()
 ;; (setq c-basic-offset             8 ) ; 縮排設定
 ;; (setq tab-width                  4 ) ; TAB 寬度為4個空格
 ;; (setq indent-tabs-mode         nil ) ; 禁止在縮排中插入制表符
 ;; (setq c-max-one-liner-length   100 ) ; 最大格數為 100 格
 ;; (setq compilation-window-height 10 ) ; 編譯結果視窗高度
  ;;(c-set-style "linux")                ; C 語言風格為 linux
  ;;(linum-mode);
;;)

;;(add-hook 'c++-mode-hook 'local/cpp-mode-hook)


(add-hook 'c++-mode-hook 
	  '(lambda ()
	  (setq c-basic-offset             8 ) ; 縮排設定
	  (setq tab-width                  4 ) ; TAB 寬度為4個空格
	  (setq indent-tabs-mode         nil ) ; 禁止在縮排中插入制表符
	  (setq c-max-one-liner-length   100 ) ; 最大格數為 100 格
	  (setq compilation-window-height 10 ) ; 編譯結果視窗高度
	  (c-set-style "linux")                ; C 語言風格為 linux
	  (linum-mode)


	  ))
