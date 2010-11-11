;; init lang.cpp

(provide 'rc-cpp-mode)
(eval-when-compile
  (require 'cl))

(require 'find-file nil 'noerror)
(require 'smartchr  nil 'noerror)


(add-hook 'c++-mode-hook
	  '(lambda ()
	     (setq c-basic-offset             8 ) ; 縮排設定
	     (setq tab-width                  4 ) ; TAB 寬度為4個空格
	     (setq indent-tabs-mode         nil ) ; 禁止在縮排中插入制表符
	     (setq c-max-one-liner-length   100 ) ; 最大格數為 100 格
	     (setq compilation-window-height 10 ) ; 編譯結果視窗高度
	     (c-set-style "linux")                ; C++ 語言風格為 linux

	     ;; hook for cpp-mode
	     (programming-common-hook)	; programming common hook
	     (find-source-or-header)	; switch between sorece and header
	     ))

;;;; Functions

(defun find-source-or-header ()
  "find source or header file."
  (when (and (featurep 'vim)
	     (featurep 'find-file))
    (add-to-list ff-other-file-alist '((("\\.cpp$"   (".h" ".hpp")))))
    (vim:nmap (kbd ",o") 'ff-find-other-file)
    (vim:nmap (kbd ",h") 'ff-find-related-file)))





;; (defun insert-char-smart ()
;;   "insert character more smart."
;;   (when (featurep 'smartchr)
;;     (vim:imap (kbd "=") (smartchr '(" = " " == "  "=")))
;;     (vim:imap (kbd ";") (smartchr '(";" ik:insert-eol))))) 