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
	     (c-toggle-auto-state t)		  ;;; Auto indent on insertion of a curly brace
	     ;; hook for cpp-mode
	     (programming-common-hook)	; programming common hook
	     (find-source-or-header)	; switch between sorece and header
	     ))

;;;; Keybinding
(add-hook 'c++-mode-hook
	  '(lambda ()
	     (when (require 'vim nil 'noerror)
	       (vim:imap (kbd "=") (smartchr '(" = " " == "  "=")))
	       ;;(vim:imap (kbd "M-i") (lambda () (interactive) (insert "inc") (yas/expand)))
	       (vim:imap (kbd "M-i") 'insert-inc-or-if))
	     ))


;;;; FIXME: no use
;; (defun insert-inc-or-if ()
;;   (interactive)
;;   (if  (equal (line-number-at-pos) (line-beginning-position))
;;       (message "begin")
;;     (message "what"))
;;	    (> (current-indentation) 0))
;;      (insert "inc")
;;  (insert "if"))
;;(yas/expand)

(defun insert-inc-or-if ()
  (interactive)
  ""
  ;;  (indent-for-tab-command)
  (let ((current (point)))
    (beginning-of-line)
    (let ((begin (point)))
      (if (equal current begin)
	  (insert "inc")
	;;(message "include")
	(insert "if")
	;;(message "if")
	)
      (yas/expand)
      (goto-char current)
      )))

(vim:imap (kbd "M-c") 'insert-inc-or-if)


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
