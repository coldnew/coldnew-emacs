;;
(provide 'rc-c-mode)

;; Coding-Style Setting
(add-hook 'c-mode-hook
	  '(lambda ()
	     (c-set-style "linux")                ; C 語言風格為 linux
	     (setq c-basic-offset             8 ) ; 縮排設定
	     (setq tab-width                  4 ) ; TAB 寬度為4個空格
	     (setq indent-tabs-mode         nil ) ; 禁止在縮排中插入制表符
	     (setq c-max-one-liner-length   100 ) ; 最大格數為 100 格
	     (setq compilation-window-height 10 ) ; 編譯結果視窗高度
	     ))

;;;; Keybindings
(add-hook 'c-mode-hook
	  '(lambda ()
	     (vim:nmap (kbd ",o") 'ff-find-other-file)
	     (vim:nmap (kbd ",h") 'ff-find-related-file)
	     (vim:imap (kbd "=")   'c-mode:insert-equal)
	     (vim:imap (kbd "M-i") 'c-mode:insert-inc-or-if)
	     (vim:imap (kbd "M-d") 'c-mode:insert-do-while)
	     ))
;;;; Hooks
(add-hook 'c-mode-hook
	  '(lambda ()
	     (add-to-list ff-other-file-alist '((("\\.c$"   (".h")))))
	     (programming-common-hook)	; programming common hook
	     ))


;;;; Functions

(defcmd c-mode:insert-inc-or-if ()
  "If at the start of line. add `inc' and expand it,
else add `if' and expand it."
  (let* ((current (point))
	 (begin (line-beginning-position)))
    (if (eq current begin)
	(insert "inc")
      (insert "if"))
    (yas/expand)))

(defcmd c-mode:insert-equal ()
  "insert equal for easy."
  (if (featurep 'smartchr)
      (smartchr '(" = " " == "  "="))
    (self-insert-command)))

(defcmd c-mode:insert-do-while ()
  "insert do{...} while()."
  (insert "do")
  (yas/expand))
