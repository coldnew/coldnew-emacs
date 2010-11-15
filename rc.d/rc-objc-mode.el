;; Object C
;; http://forum.ubuntu.org.cn/viewtopic.php?f=68&t=260070
(add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@interface"      . objc-mode))
(add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@protocol"       . objc-mode))
(add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@implementation" . objc-mode))
;; ac-company で company-xcode を有効にする
(ac-company-define-source ac-source-company-xcode company-xcode)

;; objc-mode で補完候補を設定
(setq ac-modes (append ac-modes '(objc-mode)))
;; hook
(add-hook 'objc-mode-hook
	  (lambda ()
	    ;;  (define-key objc-mode-map (kbd "\t") 'ac-complete)
	    ;; XCode を利用した補完を有効にする
	    (push 'ac-source-company-xcode ac-sources)
	    ;; C++ のキーワード補完をする Objective-C++ を利用する人だけ設定してください
	    ;; (push 'ac-source-c++-keywords ac-sources)
	    ))


;;http://github.com/kurain/kurain-dotfiles/blob/162ba05b04e14ff232c54f23e0f96300a9215922/.emacs.d/conf/init-objc.el
