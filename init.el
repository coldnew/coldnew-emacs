(require 'site-gentoo nil 'noerror)        ; 讀取 Gentoo 安裝的外掛資訊
(setq-default inhibit-default-init t )	   ; 關閉全域初始化
(setq-default debug-on-error     nil )
(setq-default custom-file "~/.emacs.d/custom.el")

;;;;;;; start server for emacsclient
(message "* --[ start the emacs server ]-- *")
(when (require 'server nil 'noerror)
  (unless (server-running-p)
    (server-start)))

;;;;;;; 運行環境辨別
(defvar mac-p     (eq system-type 'darwin))
(defvar linux-p   (eq system-type 'gnu/linux))
(defvar cygwin-p  (eq system-type 'cygwin))
(defvar windows-p (eq system-type 'windows-nt))

;;;;;;;; 將指定目錄裡的東西全部加入清單
(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
    (let* ((my-lisp-dir "~/.emacs.d/")
	   (default-directory my-lisp-dir))
      (setq load-path (cons my-lisp-dir load-path))
      (normal-top-level-add-subdirs-to-load-path)))

;;;;;; Macro
;; (defmacro require-maybe (feature &optional file)
;;   "*Try to require FEATURE, but don't signal an error if `require' fails."
;;   `(require ,feature ,file 'noerror))

(defmacro when-available (func foo)
  "*Do something if FUNCTION is available."
  `(when (fboundp ,func) ,foo))

(defmacro require-maybe (feature &optional file)
  "*Try to require FEATURE, but don't signal an error if `require' fails."
  `(let ((require-result (require ,feature ,file 'noerror)))
     (with-current-buffer (get-buffer-create "*Startup Log*")
       (let* ((startup-log-format-string-prefix "%-20s--------[")
	      (startup-log-format-string-postfix "%s")
	      (startup-status (if require-result "LOADED" "FAILED"))
	      (startup-status-face `(face (:foreground
					   ,(if require-result "green" "red")))))
	 (insert (format startup-log-format-string-prefix ,feature))
	 (let ((start-pos (point)))
	   (insert (format startup-log-format-string-postfix startup-status))
	   (add-text-properties start-pos (point) startup-status-face)
	   (insert "]\n"))))
     require-result))

;; (defvar tes-font-lock
;;   (eval-when-compile
;;     `(
;;       ,(concat "(require-maybe)\\>"
;; 	       "[ \t']*\\(\\sw+\\)?")
;;       (1 font-lock-keyword-face)
;;       (2 font-lock-constant-face nil t)
;;       )
;;     )
;;   )

;;;;;;;;; 設定預設emacs窗口大小
(cond
 (mac-p   (setq default-frame-alist '((width . 20) (height . 10))))
 (linux-p (setq default-frame-alist '((width . 170) (height . 50)))))

;;;;;; load package initial setting
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
(require 'rc-vim-mode)
(require 'rc-woman)
(require 'rc-function)
(require 'rc-smartchr)
(require 'rc-w3m)
(require 'rc-macro)


;; FIXME: bug?
;;(require 'rc-package-manager)
;; BUG: where is the fucking symbol function?
;;(require 'rc-slime)
