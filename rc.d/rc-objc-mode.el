;; Object C
;; http://forum.ubuntu.org.cn/viewtopic.php?f=68&t=260070
(add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@interface"      . objc-mode))
(add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@protocol"       . objc-mode))
(add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@implementation" . objc-mode))

;;;; When Run on MacOS X
(when mac-p
  (require 'xcode nil 'noerror)
  (require 'xcode-document-viewer nil 'noerror)

  ;; Document Viewer
  (setq xcdoc:document-path "/Developer/Platforms/iPhoneOS.platform/Developer/Documentation/DocSets/com.apple.adc.documentation.AppleiPhone3_1.iPhoneLibrary.docset")
  (setq xcdoc:open-w3m-other-buffer t)

  ;; ac-company で company-xcode を有効にする
  (ac-company-define-source ac-source-company-xcode company-xcode)
  ;; objc-mode で補完候補を設定
  (setq ac-modes (append ac-modes '(objc-mode)))
  (add-hook 'objc-mode-hook
	    '(lambda ()
	       ;; XCode を利用した補完を有効にする
	       (push 'ac-source-company-xcode ac-sources)
	       (when (require 'vim nil 'noerror)
		 (vim:nmap (kbd "<f9>") 'xcode:build-and-run))
	       ))
  )


;; hook
(add-hook 'objc-mode-hook
	  '(lambda ()
	     ;;  (define-key objc-mode-map (kbd "\t") 'ac-complete)
	     ;; C++ のキーワード補完をする Objective-C++ を利用する人だけ設定してください
	     ;; (push 'ac-source-c++-keywords ac-sources)
	     (setq c-basic-offset             8 ) ; 縮排設定
	     (setq tab-width                  8 ) ; TAB 寬度為4個空格
	     (setq indent-tabs-mode         nil ) ; 禁止在縮排中插入制表符
	     (setq c-max-one-liner-length   100 ) ; 最大格數為 100 格
	     (setq compilation-window-height 10 ) ; 編譯結果視窗高度
	     (c-set-style "linux")                ; C++ 語言風格為 linux
	     (programming-common-hook)

	     ;; hook for cpp-mode
	     (programming-common-hook)	; programming common hook
	     (find-source-or-header)	; switch between sorece and header

	     ))
;; Keybinding
(add-hook 'objc-mode-hook
	  '(lambda ()
	     ;; Insert smart char
	     (vim:local-imap (kbd "=")   'objc-mode:insert-equal)
	     (vim:local-imap (kbd ".")   'objc-mode:insert-pointer)
	     (vim:local-imap (kbd ">")   'objc-mode:insert-greater-or-shift)
	     (vim:local-imap (kbd "<")   'objc-mode:insert-lesser-or-shift)
	     ;;         (define-key objc-mode-map (kbd "C-c w") 'xcdoc:ask-search)
	     ))



;;;; Functions

(defcmd objc-mode:insert-equal ()
  "insert eaual with extra space."
  (cond ((in-string-p) (insert "="))
	((search-backward " = "  nil t) (delete-char 3) (insert " == "))
	((search-backward " == " nil t) (delete-char 4) (insert " = "))
	(t (insert " = "))))

(defcmd objc-mode:insert-pointer ()
  "insert . or -> if not in string."
  (cond ((in-string-p) (insert "."))
	((search-backward "->" nil t) (delete-char 2) (insert "."))
	((search-backward "."  nil t) (delete-char 1) (insert "->"))
	(t (insert "."))))

(defcmd objc-mode:insert-greater-or-shift ()
  "insert > or >> if not in string."
  (cond ((in-string-p) (insert ">"))
	((search-backward ">" nil t) (delete-char 1) (insert ">>"))
	((search-backward ">>"  nil t) (delete-char 2) (insert ">"))
	(t (insert ">"))))

(defcmd objc-mode:insert-lesser-or-shift ()
  "insert < or << if not in string."
  (cond ((in-string-p) (insert "<"))
	((search-backward "<" nil t) (delete-char 1) (insert "<<"))
	((search-backward "<<"  nil t) (delete-char 2) (insert "<"))
	(t (insert "<"))))


;;;;;; Mac OSX special Setting
;;; Those Funtcions for MacOs
(when mac-p
  (defun xcode:build-and-run ()
    (interactive)
    (do-applescript
     (format (concat "tell application \"Xcode\" to activate \r"
		     "tell application \"System Events\" \r"
		     "     tell process \"Xcode\" \r"
		     "		key code 36 using {command down} \r"
		     "      end tell \r"
		     "end tell \r"
		     ))))
  )

;;;; alian regexp
;; M-x align twice
;; make following :
;;   NSTimer *timer =
;;        [NSTimer timerWithTimeInterval:1.0
;;                 target:self
;;                 selector:@selector(callback:)
;;                 userInfo:nil
;;                 repeats:YES];
;; look like :
;;     NSTimer *timer =
;;        [NSTimer timerWithTimeInterval:1.0
;;                                target:self
;;                              selector:@selector(callback:)
;;                              userInfo:nil
;;                               repeats:YES];
;;
(obj-c-colons
 (regexp . "^\\(\\s-*[^:]+\\):")
 (justify . t)
 (repeat . t)
 (modes obj-c-mode))

(add-to-list 'align-rules-list
	     '(obj-c-colons
	       (regexp . "^\\(\\s-*[^:]+\\):")
	       (justify . t)
	       (repeat . t)
	       (modes obj-c-mode)))

;;http://github.com/kurain/kurain-dotfiles/blob/162ba05b04e14ff232c54f23e0f96300a9215922/.emacs.d/conf/init-objc.el
