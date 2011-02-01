;; init lang.cpp

(provide 'lang-cpp)
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
	     (vim:local-nmap (kbd ",o") 'ff-find-other-file)
	     (vim:local-nmap (kbd ",h") 'ff-find-related-file)
	     ;; Insert yasnippet
	     (vim:local-imap (kbd "M-i") 'cpp-mode:insert-inc-or-if)
	     (vim:local-imap (kbd "M-d") 'cpp-mode:insert-do-while)
	     (vim:local-imap (kbd "M-m") 'cpp-mode:insert-main-function)

	     ))

;;;; Hooks
(add-hook 'c++-mode-hook
	  '(lambda ()
	     ;; Use my define programming-common environment
	     (programming-common-hook)
	     ;; Use my define cc-mode common environment
	     (cc-mode-common-hook)

	     ))

;;;; Support for Qt4
(setq c-C++-access-key "\\<\\(slots\\|signals\\|private\\|protected\\|public\\)\\>[ \t]*[(slots\\|signals)]*[ \t]*:")
(font-lock-add-keywords 'c++-mode '(("\\<\\(Q_OBJECT\\|public slots\\|public signals\\|private slots\\|private signals\\|protected slots\\|protected signals\\)\\>" . font-lock-constant-face)))

;; (cond
;;  (mac-p
;;   (setq qt4-base-dir "/usr/include/qt4")
;;   (semantic-add-system-include qt4-base-dir 'c++-mode)
;;   (add-to-list 'auto-mode-alist (cons qt4-base-dir 'c++-mode))
;;   (add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "/Qt/qconfig.h"))
;;   (add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "/Qt/qconfig-dist.h"))
;;   (add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "/Qt/qglobal.h"))))

;;;; Auto-Mode
(add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cc\\'"  . c++-mode))


;;;; Functions



;;;;; Keybinding Functions

;; insert yasnippet
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
	(insert "main"))
    (yas/expand)))
