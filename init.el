;;; init.el ---

(setq custom-file "~/.emacs.d/custom.el")
(require 'site-gentoo nil 'noerror)        ; 讀取 Gentoo 安裝的外掛資訊
(setq-default inhibit-default-init t )	   ; 關閉全域初始化
(setq-default debug-on-error     nil )

;;;;;;; start server for emacsclient
(message "* --[ start the emacs server ]-- *")
(when (require 'server nil 'noerror)
  (unless (server-running-p)
    (server-start)))

;;;;;;; 運行環境辨別
(defvar mac-p     (eq window-system 'mac))
(defvar linux-p   (eq system-type 'gnu/linux))
(defvar cygwin-p  (eq system-type 'cygwin))
(defvar windows-p (eq system-type 'windows-nt))

;;;;;;;; 將指定目錄裡的東西全部加入清單
(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
    (let* ((my-lisp-dir "~/.emacs.d/")
	   (default-directory my-lisp-dir))
      (setq load-path (cons my-lisp-dir load-path))
      (normal-top-level-add-subdirs-to-load-path)))

;;;;;;;;; 設定預設emacs窗口大小
(setq default-frame-alist '((width . 170) (height . 50)))


;;;;;; load package initial setting
(require 'rc-backup)
(require 'rc-base)
(require 'rc-buffer)
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
(require 'rc-slime)
(require 'rc-vim-mode)
(require 'rc-woman)


;;FIXME: bug?
;;(require 'rc-package-manager)

