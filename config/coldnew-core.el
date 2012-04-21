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
(fset 'yes-or-no-p 'y-or-n-p )	  ;; Use y or n instead of yes and not

;; nice scrolling
(setq scroll-margin                   0 )
(setq scroll-conservatively      100000 )
(setq scroll-preserve-screen-position 1 )



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

;; (defvar emacs-bin-dir    (concat emacs-dir "bin/")
;;   "directory to place binary staff.")
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

(defun swap-ctrl-caps ()
  "swap control and capslock"
  (shell-command "setxkbmap -option ctrl:swapcaps"))

(defun make-caps-as-ctrl ()
  "make capslock as control-key"
  (shell-command "setxkbmap -option ctrl:nocaps"))

;; Swap control and Capslock in xwindow
(cond ((eq window-system 'x)
       ;; make caps lock a control key
       (make-caps-as-ctrl)))





(provide 'coldnew-core)
;; coldnew-core.el ends here.
