
(setq anything-quick-update t)                        ;不顯示提示窗口外的變量, 加快刷新速度
(setq anything-candidate-number-limit 10000)          ;候選數量限制
(setq anything-c-yas-display-key-on-candidate t)      ;補全 YAsnippet 時顯示 YAsnippet 的名稱
(setq anything-etags-enable-tag-file-dir-cache t)     ;使用緩存的tag文件
(setq anything-c-use-standard-keys nil)               ;不使用標準按鍵
(setq anything-etags-cache-tag-file-dir "~/.emacs.d/var/cache/") ;tag緩存文件的目錄
;;(anything-etags-generate-tag-buffer)                  ;啟動時生成 tag buffer
(setq anything-c-google-suggest-url                   ;設置Google建議允許訪問的URL
      "http://www.google.com.tw/search?q=%s&ie=utf-8&oe=utf-8")

(setq anything-sources                  ;Anything 源列表
      '(
	anything-c-source-ffap-guesser             ;光標處的文件
	anything-c-source-buffers                  ;Buffer
	anything-c-source-recentf                  ;最近打開的文件列表
	anything-c-source-yasnippet                ;代碼補全別名
	anything-c-source-file-name-history        ;文件名歷史
	anything-c-source-locate                   ;本地文件
	anything-c-source-files-in-current-dir+    ;當前目錄的文件
	anything-c-source-elisp-library-catalog    ;查找加載的庫
	anything-c-source-w3m-bookmarks            ;w3m 書籤
	anything-c-source-extended-command-history ;Emacs命令歷史
	anything-c-source-info-elisp               ;Info Elisp
	anything-c-source-info-cl                  ;Info Common-Lisp
	anything-c-source-info-pages               ;Info Pages
	anything-c-source-fixme                    ;FIX ME
	anything-c-source-semantic                 ;Sematic Tag
	anything-c-source-etags-select             ;etags
	anything-c-source-gtags-select             ;Gtags
	anything-c-source-emacs-commands           ;Emacs 命令相關的
	anything-c-source-complex-command-history  ;複雜命令歷史
	anything-c-source-complete-shell-history   ;Shell歷史
	anything-c-source-occur                    ;occur 搜索, (後面一點, 增加性能)
	anything-c-source-man-pages                ;man
	anything-c-source-emacs-process            ;進程
	anything-c-source-call-source              ;call source
	anything-c-source-customize-face           ;自定義顏色
	anything-c-source-bbdb                     ;bbdb
	anything-c-source-colors                   ;顏色
	anything-c-source-buffer-not-found         ;創建buffer
	anything-c-source-tracker-search           ;Tracker桌面搜索
	anything-c-source-calculation-result       ;計算結果
	anything-c-source-evaluation-result        ;執行表達式結果
	anything-c-source-kill-ring                ;Kill ring
	anything-c-source-files-in-current-dir
	anything-c-source-recentf
	anything-c-source-emacs-commands
	anything-c-source-include
	))


(setq anything-include-save-file "~/.emacs.d/var/cache/anything-include.cache")
(setq anything-include-max-saved-items 100)



(provide '032-anything)
;; 032-anything.el ends here.
