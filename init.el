;;; init.el --- configuration entry point.
(eval-when-compile (require 'cl))

(message "\n\nEmacs is powering up... Be patient, Master %s!\n\n" (getenv "USER"))

;; add directories to emacs's `load-path' recursively.
(let* ((emacs-dir "~/.emacs.d/")
       (lisp-dir '("lisp/" "local-lisp/" "config/")))
  (dolist (lisp-path lisp-dir)
    (let* ((load-dir (concat emacs-dir lisp-path))
	   (default-directory load-dir))
      (setq load-path (cons load-dir load-path))
      (normal-top-level-add-subdirs-to-load-path))))

;; a shortcut to reload emacs setting
(global-set-key (kbd "<f5>") '(lambda () (interactive) (load-file "~/.emacs.d/init.el") (desktop-revert) (delete-other-windows)))

;; load the core stuff
(require 'coldnew-core)
(require 'coldnew-variables)
(require 'coldnew-packages)
(require 'coldnew-macros)
(require 'coldnew-depends)
(require 'coldnew-functions)
(require 'coldnew-evil)
(require 'coldnew-tags)
(require 'coldnew-editor)

;; config changes made through the customize UI will be store here
(setq custom-file emacs-custom-file)

;; loading all emacs configures
(if (file-readable-p emacs-config-dir)
    (with-current-buffer (get-buffer-create "*Loading Log*")
      ;; header file
      (insert (concat "\n" (make-string 80 ?=)))
      (insert (propertize "\n Loading emacs config status \n" 'face '(:foreground "gold2")))
      (insert (concat (make-string 80 ?=) "\n"))
      ;; loading file and test status
      (dolist (config-file (directory-files emacs-config-dir t "^[^#].*el$"))
	(let* ((feature (file-name-sans-extension (file-name-nondirectory config-file)))
	       (config-file-name (file-name-nondirectory config-file))
	       (loading-result (require (intern feature) nil 'noerror))
	       (loading-status (if loading-result "LOADED" "FAILED")))

	  (insert (format " %-20s %s\t\t [ " config-file-name
			  (make-string (- 40 (length config-file-name)) ? )
			  ))
	  (insert (propertize loading-status 'face  `(:foreground ,(if loading-result "green" "red"))))
	  (insert " ]\n"))))

  ;; After loading allemacs config file, readauthorization fil
  (if (file-exists-p emacs-authinfo-file) (load-file emacs-authinfo-file))

  (message "\t --- Loading All Emacs Confis Finish ---")
  )


(message "\n\nEmacs is ready to serve you, Master %s!\n\n" (getenv "USER"))
