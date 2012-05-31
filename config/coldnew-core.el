;;; coldnew-core.el --- core defun and variables setting
(eval-when-compile (require 'cl))

;;;; ------------------------------------------------------------------------------
;;;;   Initial User-Interface setting
;;;; ------------------------------------------------------------------------------
(setq inhibit-startup-message t  ) ;; Remove start-message after startup
(setq initial-scratch-message "" ) ;; Remove default scratch-message
(setq inhibit-default-init     t ) ;; Remove global setting
(setq visible-bell             t ) ;; Use visible instead of ring bell
(tool-bar-mode                -1 ) ;; Remove tool-bar
(scroll-bar-mode              -1 ) ;; Remove scroll-bar
(blink-cursor-mode            -1 ) ;; Disable bllink cursor
(tool-bar-mode                -1 ) ;; Remove tool-bar
(menu-bar-mode                -1 ) ;; Remove menu-bar
(fset 'yes-or-no-p 'y-or-n-p )    ;; Use y or n instead of yes and not

;; nice scrolling
(setq scroll-margin                   0 )
(setq scroll-conservatively      100000 )
(setq scroll-preserve-screen-position 1 )
(setq scroll-up-aggressively       0.01 )
(setq scroll-down-aggressively     0.01 )

;; timestamp
(setq time-stamp-active      t ) ; do enable time-stamps
(setq time-stamp-line-limit 10 ) ; check first 10 buffer lines for Time-stamp:
(setq time-stamp-format "%04y-%02m-%02d %02H:%02M:%02S (%u)") ; date format

;; TODO: add comment
(setq stack-trace-on-error t)
(setq imenu-auto-scan t)
;;(setq redisplay-dont-pause t)

;; xrelated srtting
(setq x-select-enable-clipboard t)
(setq select-active-regions t)
(setq x-gtk-use-system-tooltips nil)	; disable gtk-tooltip

(setq max-lisp-eval-depth '40000)
(setq max-specpdl-size 5)  ; default is 1000, reduce the backtrace level
(setq debug-on-error t)    ; now you should get a backtrace
;;;; -------------------------------------------------------------------------------
;;;;   Global Variables Setting
;;;; -------------------------------------------------------------------------------
(defvar emacs-dir "~/.emacs.d/"
  "The top-level emacs-configure directory.")
(defvar emacs-config-dir (concat emacs-dir "config/")
  "directory to place emacs configure.")
(defvar emacs-themes-dir (concat emacs-dir "themes/")
  "directory to place emacs theme.")
(defvar emacs-lisp-dir   (concat emacs-dir "lisp/")
  "directory to place lisp packages from internet.")
(defvar emacs-elpa-dir   (concat emacs-lisp-dir "elpa/")
  "directory to place ELPA lisp packages from internet.")
(defvar emacs-snippets-dir (concat emacs-dir "snippets/")
  "directory to place yasnippet files.")
(defvar emacs-recipes-dir (concat emacs-dir "recipes/")
  "directory to place local el-get recepies.")

(defvar emacs-custom-file (concat emacs-dir "custom.el")
  "store customize UI config.")

(defvar emacs-bin-dir    (concat emacs-dir "bin/")
  "directory to place binary staff.")
(defvar emacs-cache-dir  (concat emacs-dir "cache/")
  "cache file directory.")
(defvar emacs-backup-dir (concat emacs-dir "backup/")
  "directory to backup files.")
(defvar emacs-authinfo-file (concat emacs-dir ".authinfo.gpg")
  "file that save secret")

;; (defvar emacs-log-dir (concat emacs-var-dir "log/")
;;   "log file directory."

;;;; Shell setting
(defvar emacs-default-shell "/bin/bash"
  "Default shell for cemacs.")
(defvar emacs-popup-shell-window-height 30
  "Window hight of popup shell.")
(defvar emacs-popup-shell-window-position "bottom"
  "Make popup shell window at buttom by default.")




;;;; ---------------------------------------------------------------------------
;;;; Environment setting
;;;; ---------------------------------------------------------------------------
(require 'coldnew-variables)

(defun change-mouse-to-left ()
  (interactive)
  (shell-command "xmodmap -e \"pointer = 3 2 1\""))

(defun change-mouse-to-right ()
  (interactive)
  (shell-command "xmodmap -e \"pointer = 1 2 3\""))

(defun swap-ctrl-caps ()
  "swap control and capslock"
  (shell-command "setxkbmap -option ctrl:swapcaps"))

(defun make-caps-as-ctrl ()
  "make capslock as control-key"
  (shell-command "setxkbmap -option ctrl:nocaps"))

;; only disable capslock and make it as control
(cond ((eq window-system 'x)
       ;; make caps lock a control key
       (make-caps-as-ctrl)
       (change-mouse-to-left)))

;; Set PATH
;; TODO: make it work on every platform, now only has Linux support
(setenv "PATH"
	(concat
	 emacs-bin-dir ":"
	 "~/.lein/bin" ":"
	 (getenv "PATH")
	 ))

(setq exec-path (cons emacs-bin-dir exec-path))



(provide 'coldnew-core)
;; coldnew-core.el ends here.
