;;;; init
(eval-when-compile (require 'cl))

;;(setq-default inhibit-default-init t )     ; 關閉全域初始化
(setq-default debug-on-error     nil )
(setq-default custom-file "~/.emacs.d/rc.d/000-custom.el")


;; Load Path
(let* ((emacs-dir "~/.emacs.d/")
       (lisp-dir '("rc.d/" "theme/" "lisp/" "local-lisp/")))
  (dolist (lisp-path lisp-dir)
    (let* ((load-dir (concat emacs-dir lisp-path))
	   (default-directory load-dir))
      (setq load-path (cons load-dir load-path))
      (normal-top-level-add-subdirs-to-load-path))))

;; Loading my emacs configures
(let ((init-folder "~/.emacs.d/rc.d/")
      (authinfo-file (expand-file-name "~/.emacs.d/.authinfo.gpg")))
  (if (file-readable-p init-folder)
      (dolist (config-file (directory-files init-folder t ".*\.elc?$"))
	(let* ((feature (file-name-sans-extension (file-name-nondirectory config-file)))
	       (loading-result (require (intern feature) nil 'noerror)))
	  (with-current-buffer (get-buffer-create "*Loading Config Log*")
	    (insert (format "\t Loading %-20s\t \n" (concat init-folder feature ".el")))))))
  ;; After loading all my config file, read my authorization file
  (if (file-exists-p authinfo-file) (load-file authinfo-file))
  (message "\t --- Loading All Emacs Configures Finish ---"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-compression-mode t)
 '(blink-cursor-mode nil)
 '(custom-file nil)
 '(global-auto-revert-mode t)
 '(global-font-lock-mode t)
 '(gnus-inhibit-startup-message t)
 '(inhibit-default-init t)
 '(inhibit-startup-screen t)
 '(initial-scratch-message "")
 '(kill-whole-line t)
 '(major-mode (quote lisp-interaction-mode))
 '(menu-bar-mode t)
 '(mouse-avoidance-mode (quote animate) nil (avoid))
 '(mouse-yank-at-point t)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(track-eol t)
 '(transient-mark-mode t)
 '(visible-bell t)
 '(x-select-enable-clipboard t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
