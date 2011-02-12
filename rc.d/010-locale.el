;;; rc-locale.el ---

(eval-when-compile (require 'cl))

(prefer-coding-system           'utf-8 ) ; 設定系統編碼
(setq language-environment      'utf-8 ) ; 設定語言環境
(setq buffer-file-coding-system 'utf-8 ) ; 文件保存時的編碼設置
(setq keyboard-coding-system    'utf-8 ) ; 鍵盤編碼設定
(setq terminal-coding-system    'utf-8 ) ; 設定終端機的編碼
(setq buffer-file-coding-system 'utf-8 ) ; buffer內文字的編碼
(setq selection-coding-system   'utf-8 ) ; 選擇區域內編碼
(setq clipboard-coding-system   'utf-8 ) ; 剪貼簿編碼設定
(setq file-name-coding-system   'utf-8 ) ; 使用 utf-8 編碼顯示文件名
(setq system-time-locale       "en_US" ) ; 設定時間顯示使用英文


(provide '010-locale)
;;; 010-locale.el ends here
