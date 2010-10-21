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

;;;;;;;; 將指定目錄裡的東西全部加入清單
(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
    (let* ((my-lisp-dir "~/.emacs.d/")
	   (default-directory my-lisp-dir))
      (setq load-path (cons my-lisp-dir load-path))
      (normal-top-level-add-subdirs-to-load-path)))


;;;;;; load package initial setting
(require 'rc-backup)
(require 'rc-base)
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
(require 'rc-org-mode)
(require 'rc-package-manager)
(require 'rc-session)
(require 'rc-slime)
(require 'rc-vim-mode)
(require 'rc-woman)


;;;test
