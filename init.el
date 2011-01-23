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
(cond (mac-p (require 'rc-mac)))


;;;;;; load package initial setting

(require '000-macro)
(require '003-base)
(require '005-dependency)
(require '006-function)
(require '007-backup)
(require '008-fonts)
(require '009-locale)
(require '010-color-theme)
(require '011-vim-mode)
(require '012-display)
(require '013-woman)
(require '022-ibuffer)
(require 'rc-minibuffer)
(require 'rc-cedet)
(require 'rc-common-hook)
(require 'rc-complete)

(cond (emacs23-p
       (require 'rc-ecb)))

(require 'rc-matlab-mode)
(require 'rc-org-mode)
(require 'rc-session)
(require 'rc-smartchr)
(require 'rc-w3m)
(require 'rc-ielm-mode)
(require 'rc-find-file)
(require 'rc-xrefactory)
(require 'rc-ccmode-common)

(require 'circuit-mode)



;; FIXME: bug?
;;(require 'rc-package-manager)
;; BUG: where is the fucking symbol function?
;;(require 'rc-slime)
;;(require 'magit)

;;(setq-default header-line-format mode-line-format) ; Copy mode-line to top
;;(setq-default mode-line-format nil) ; Remove mode-line
(defcmd show-mode-line ()
  (if mode-line-format
      (setq mode-line-format nil)
    (setq mode-line-format	t)))
