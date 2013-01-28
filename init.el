;;; init.el --- configuration entry point.
(eval-when-compile (require 'cl))

(message "\nEmacs is powering up... Be patient, Master %s!\n" (getenv "USER"))

;; add directories to emacs's `load-path' recursively.
(let* ((emacs-dir "~/.emacs.d/")
       (lisp-dir '("lisp/" "themes/")))
  (dolist (lisp-path lisp-dir)
    (let* ((load-dir (concat emacs-dir lisp-path))
	   (default-directory load-dir))
      (setq load-path (cons load-dir load-path))
      (normal-top-level-add-subdirs-to-load-path))))

;; define a reload command
(defun reload-emacs ()
  "reload my emacs settings"
  (interactive)
  (load-file "~/.emacs.d/init.el") (desktop-revert) (delete-other-windows))

;; When eval org-babel, do not confirm
(setq org-confirm-babel-evaluate nil)

;; Load up org-mode and org-babel
(require 'org-install)
(require 'ob-tangle)

;; Load up all literate org-mode files in config directory
(mapc #'org-babel-load-file (directory-files "~/.emacs.d/" t "\\.org$"))

;; After loading allemacs config file, read authorization file
(if (file-exists-p emacs-authinfo-file) (load-file emacs-authinfo-file))


(message "\nEmacs is ready to serve you, Master %s!\n" (getenv "USER"))

;;; init.el ends here.
(put 'narrow-to-region 'disabled nil)
