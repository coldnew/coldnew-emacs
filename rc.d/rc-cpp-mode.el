;; init lang.cpp

(provide 'rc-cpp-mode)
(eval-when-compile
  (require 'cl))

(require 'find-file nil 'noerror)
(require 'smartchr  nil 'noerror)
(require 'yasnippet nil 'noerror)


;; Coding-Style Setting
(add-hook 'c++-mode-hook
	  '(lambda ()
	     (setq c-basic-offset             8 ) ; 縮排設定
	     (setq tab-width                  4 ) ; TAB 寬度為4個空格
	     (setq indent-tabs-mode         nil ) ; 禁止在縮排中插入制表符
	     (setq c-max-one-liner-length   100 ) ; 最大格數為 100 格
	     (setq compilation-window-height 10 ) ; 編譯結果視窗高度
	     (c-set-style "linux")                ; C++ 語言風格為 linux
	     (c-toggle-auto-state t)		  ; Auto indent on insertion of a curly brace
	     ;;
	     (add-to-list ff-other-file-alist '((("\\.cpp$"   (".h" ".hpp")))))
	     ;; hook for cpp-mode
	     (programming-common-hook)	; programming common hook
	     ))

;;;; Keybinding
(add-hook 'c++-mode-hook
	  '(lambda ()
	     (when (require 'vim nil 'noerror)
	       (vim:nmap (kbd ",o") 'ff-find-other-file)
	       (vim:nmap (kbd ",h") 'ff-find-related-file)
	       (vim:imap (kbd "=")   'cpp-mode:insert-equal)
	       (vim:imap (kbd "M-i") 'cpp-mode:insert-inc-or-if)
	       (vim:imap (kbd "M-d") 'cpp-mode:insert-do-while)
	       )))


;;;; Functions



;;;;; Keybinding Functions

(defcmd cpp-mode:insert-inc-or-if ()
  "If at the start of line. add `inc' and expand it,
else add `if' and expand it."
  (let* ((current (point))
	 (begin (line-beginning-position)))
    (if (equal current begin)
	(insert "inc")
      (insert "if"))
    (yas/expand)))

(defcmd cpp-mode:insert-equal ()
  "insert equal for easy."
  (if (featurep 'smartchr)
      (smartchr '(" = " " == "  "="))
    (self-insert-command)))

(defcmd cpp-mode:insert-do-while ()
  "insert do{...} while()."
  (insert "do")
  (yas/expand))


;; (vim:imap (kbd ";") (smartchr '(";" ik:insert-eol)))))
