;;
;;;;;;; 運行環境辨別
(defvar mac-p     (eq system-type 'darwin))
(defvar linux-p   (and (eq system-type 'gnu/linux) (not mac-p)))
(defvar cygwin-p  (eq system-type 'cygwin))
(defvar windows-p (eq system-type 'windows-nt))
(defvar emacs23-p (equal emacs-major-version 23))
(defvar emacs24-p (equal emacs-major-version 24))

;;;; OS independent environ setting
(cond
 ;; If running on Mac OSX
 (mac-p
  ;; Add binary PATH for Mac OSX
  (add-to-list 'exec-path "~/Gentoo/bin") ; Gentoo prefix
  (add-to-list 'exec-path "/usr/local/bin/")
  (add-to-list 'exec-path "/opt/local/bin/")
  (add-to-list 'exec-path "/usr/bin/")
  (add-to-list 'exec-path "/usr/X11/bin/"))
 ;; If running in Windows
 (windows-p
  )
 ;; Default is Linux
 (t
  ))

;;;; Global environment Setting
(setenv "GPG_AGENT_INFO" nil)

;;;; Alias
(defalias 'irc 'erc)

;;;; Following setting must run before load the libraries.

;; ;; el-get.el
;; (setq el-get-dir "~/.emacs.d/lisp/")
;; (setq el-get-recipe-path "~/.emacs.d/etc/recipes")

;; ;; auto-complete.el
;; (setq ac-comphist-file "~/.emacs.d/var/cache/auto-complete.cache")


(provide '002-environment)
;; 002-environment.el ends here.
