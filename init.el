;;;; init file

(require 'site-gentoo nil 'noerror)        ; 讀取 Gentoo 安裝的外掛資訊
(setq-default inhibit-default-init t )     ; 關閉全域初始化
(setq-default debug-on-error     nil )
(setq-default custom-file "~/.emacs.d/custom.el")

;;;;;;; start server for emacsclient
(message "* --[ start the emacs server ]-- *")
(when (require 'server nil 'noerror)
  (unless (server-running-p)
    (server-start)))

;;;;;;; 運行環境辨別
(defvar mac-p     (eq system-type 'darwin))
(defvar linux-p   (and (eq system-type 'gnu/linux) (not mac-p)))
(defvar cygwin-p  (eq system-type 'cygwin))
(defvar windows-p (eq system-type 'windows-nt))
(defvar emacs23-p (equal emacs-major-version 23))
(defvar emacs24-p (equal emacs-major-version 24))

;;;;;;;; 將指定目錄裡的東西全部加入清單
(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
    (let* ((my-lisp-dir "~/.emacs.d/")
	   (default-directory my-lisp-dir))
      (setq load-path (cons my-lisp-dir load-path))
      (normal-top-level-add-subdirs-to-load-path)))

;;;;; binary path


;;;;;; load package initial setting

(require '000-macro)			; All Macros I use
(require '001-environment)		; Environment Setting
(require '003-dependency)		; All libraries included in
(require '005-base)			; Basic emacs config
(require '006-function)			; All functions I use
(require '007-backup)			; Configure Backup Process
(require '008-fonts)			; Setting Fonts
(require '009-locale)			; Setting Locales
(require '010-color-theme)		; Color-themes
(require '011-vim-mode)			; Use Vim Keybindings
(require '012-display)			; Configure window's size
(require '013-woman)			; Woman-mode Settings
(require '014-session)			; Store current positions
(require '022-ibuffer)			; Call buffer-list
(require '023-el-get)			; emacs lisp manager
(require '024-elpa)			; emacs lisp manager
(require '025-yasnippet)
(require '026-auto-complete)
(require '027-minibuffer)
(require '028-uniquify)




(require 'rc-cedet)
(require 'rc-common-hook)

(cond (emacs23-p
       (require 'rc-ecb)))
(require 'rc-org-mode)
(require 'rc-smartchr)
(require 'rc-w3m)
(require 'rc-ielm-mode)
(require 'rc-find-file)
(require 'rc-xrefactory)
(require 'rc-ccmode-common)

;;(require 'circuit-mode)

(require '999-keybinding)		; Global Keybindings, must in the last line.

;; FIXME: bug?
;; BUG: where is the fucking symbol function?
;;(require 'rc-slime)
;;(require 'magit)

;;(setq-default header-line-format mode-line-format) ; Copy mode-line to top
;;(setq-default mode-line-format nil) ; Remove mode-line
(defcmd show-mode-line ()
  (if mode-line-format
      (setq mode-line-format nil)
    (setq mode-line-format	t)))
