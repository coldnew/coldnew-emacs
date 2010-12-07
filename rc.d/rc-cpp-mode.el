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
	     (c-set-style "linux")                ; C++ 語言風格為 linux
	     (setq c-basic-offset             8 ) ; 縮排設定
	     (setq tab-width                  4 ) ; TAB 寬度為4個空格
	     (setq indent-tabs-mode         nil ) ; 禁止在縮排中插入制表符
	     (setq c-max-one-liner-length   100 ) ; 最大格數為 100 格
	     (setq compilation-window-height 10 ) ; 編譯結果視窗高度
	     (c-toggle-auto-state t)          ; Auto indent on insertion of a curly brace
	     ))

;;;; Keybinding
(add-hook 'c++-mode-hook
	  '(lambda ()
	     (vim:nmap (kbd ",o") 'ff-find-other-file)
	     (vim:nmap (kbd ",h") 'ff-find-related-file)
	     (vim:imap (kbd "M-i") 'cpp-mode:insert-inc-or-if)
	     (vim:imap (kbd "M-d") 'cpp-mode:insert-do-while)
	     (vim:imap (kbd "M-m") 'cpp-mode:insert-main-function)
	     ;; FIXME:
	     ;; (vim:imap (kbd "=")   'cpp-mode:insert-equal)
	     ;; (vim:imap (kbd ".")   'c-mode:insert-pointer)
	     ;; (vim:imap (kbd ">")   'c-mode:insert-greater-or-shift)
	     ;; (vim:imap (kbd "<")   'c-mode:insert-lesser-or-shift)

	     ))

;;;; Hooks
(add-hook 'c++-mode-hook
	  '(lambda ()
	     ;;
	     ;; hook for cpp-mode
	     (programming-common-hook)	; programming common hook

	     ))

;;;; Support for Qt4
(cond
 (mac-p
  (setq qt4-base-dir "/usr/include/qt4")
  (semantic-add-system-include qt4-base-dir 'c++-mode)
  (add-to-list 'auto-mode-alist (cons qt4-base-dir 'c++-mode))
  (add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "/Qt/qconfig.h"))
  (add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "/Qt/qconfig-dist.h"))
  (add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "/Qt/qglobal.h"))))

;;;; Auto-Mode
(add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cc\\'"  . c++-mode))


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

(defcmd cpp-mode:insert-do-while ()
  "insert do{...} while()."
  (insert "do")
  (yas/expand))

(defcmd cpp-mode:insert-main-function ()
  "insert main()."
  (let* ((current (point))
	 (begin (line-beginning-position)))
    (if (equal current begin)
	(insert "main")
      )
    (yas/expand)))


;; FIXME:
;; (defcmd cpp-mode:insert-equal ()
;;   "insert equal for easy."
;;   (if (and (featurep 'smartchr)
;;     (not (in-string-p)))
;;       (smartchr '(" = " " == "  "="))
;;     (self-insert-command)))

;; (defcmd cpp-mode:insert-pointer ()
;;   "insert . or -> for easy."
;;   (if (and (featurep 'smartchr)
;;     (not (in-string-p)))
;;       (smartchr '("." "->"))
;;     (self-insert-command)))

;; (defcmd cpp-mode:insert-greater-or-shift ()
;;   "insert > or >> for easy."
;;   (if (and (featurep 'smartchr)
;;     (not (in-string-p)))
;;       (smartchr '(">" ">>"))
;;     (self-insert-command)))

;; (defcmd cpp-mode:insert-lesser-or-shift ()
;;   "insert < or << for easy."
;;   (if (and (featurep 'smartchr)
;;     (not (in-string-p)))
;;       (smartchr '("<" "<<"))
;;     (self-insert-command)))


;; (vim:imap (kbd ";") (smartchr '(";" ik:insert-eol)))))
