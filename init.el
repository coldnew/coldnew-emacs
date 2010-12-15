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


(require 'rc-macro)
(require 'rc-backup)
(require 'rc-base)
(require 'rc-ibuffer)
(require 'rc-minibuffer)
(require 'rc-c-mode)
(require 'rc-cedet)
(require 'rc-color-theme)
(require 'rc-common-hook)
(require 'rc-complete)
(require 'rc-cpp-mode)
(require 'rc-ecb)
(require 'rc-emacs-lisp-mode)
(require 'rc-fonts)
(require 'rc-locale)
(require 'rc-matlab-mode)
(require 'rc-org-mode)
(require 'rc-session)
(require 'rc-woman)
(require 'rc-function)
(require 'rc-smartchr)
(require 'rc-w3m)
(require 'rc-display)
(require 'rc-vim-mode)
(require 'rc-ielm-mode)
(require 'rc-find-file)


(require 'circuit-mode)



;; FIXME: bug?
;;(require 'rc-package-manager)
;; BUG: where is the fucking symbol function?
;;(require 'rc-slime)
