;;;; init
(eval-when-compile (require 'cl))

(require 'site-gentoo nil 'noerror)        ; 讀取 Gentoo 安裝的外掛資訊
(setq-default inhibit-default-init t )     ; 關閉全域初始化
(setq-default debug-on-error     nil )
(setq-default custom-file "~/.emacs.d/rc.d/custom.el")

;;;;;;; start server for emacsclient
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

;;;;;; Load Path
(add-to-list 'load-path "~/.emacs.d/rc.d/")
(add-to-list 'load-path "~/.emacs.d/theme/")
(let ((default-directory "~/.emacs.d/lisp/"))
  (normal-top-level-add-subdirs-to-load-path))
(let ((default-directory "~/.emacs.d/local-lisp/"))
  (normal-top-level-add-subdirs-to-load-path))


;;;;;; Functions


(defmacro require* (feature &optional file)
  "*Try to require FEATURE, but don't signal an error if `require' fails."
  `(let ((require-result (require ,feature ,file 'noerror)))
     (with-current-buffer (get-buffer-create "*Loading Library Log*")
       (let* ((startup-log-format-string-prefix "\t%-30s\t\t\t[")
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

(defmacro requiref (feature &optional file)
  "*Try to require FEATURE, but don't signal an error if `require' fails."
  `(let ((require-result (require ,feature ,file 'noerror)))
     (with-current-buffer (get-buffer-create "*Loading Config Log*")
       (let* ((startup-log-format-string-prefix "\t Loading %-20s\t <-------------------------\t\t [")
	      (startup-log-format-string-postfix "%s")
	      (startup-status (if require-result "SUCCESS" "FAILED"))
	      (startup-status-face `(face (:foreground
					   ,(if require-result "green" "red")))))
	 (insert (format startup-log-format-string-prefix ,feature))
	 (let ((start-pos (point)))
	   (insert (format startup-log-format-string-postfix startup-status))
	   (add-text-properties start-pos (point) startup-status-face)
	   (insert "]\n"))))
     require-result))

;;;; Extra font-lock
(font-lock-add-keywords 'emacs-lisp-mode
			'(("(\\(require\\*\\)\\s [ \t']*\\(\\sw+\\)?"
			   (1 font-lock-keyword-face)
			   (2 font-lock-constant-face nil t))
			  ("(\\(requiref\\)\\s [ \t']*\\(\\sw+\\)?"
			   (1 font-lock-keyword-face)
			   (2 font-lock-constant-face nil t))
			  ))


;; Load external config files
(let ((init-folder "~/.emacs.d/rc.d"))
  (if (file-readable-p init-folder)
      (dolist (config-file (directory-files init-folder t ".*\.elc?$"))
	;;(load-file config-file)
	(requiref (intern (file-name-sans-extension (file-name-nondirectory config-file))))
	)))
