;;; init.el --- configuration entry point.
(eval-when-compile (require 'cl))

(message "\nEmacs is powering up... Be patient, Master %s!\n" (getenv "USER"))

;; Add directories to emacs's `load-path' recursively.
;; if path does not exist, create directory.
(let* ((emacs-dir "~/.emacs.d/")
       (lisp-dir '("lisp/" "themes/" "local-lisp/")))
  (dolist (lisp-path lisp-dir)
    (if (not (file-exists-p lisp-path)) (make-directory (concat emacs-dir lisp-path t)))
    (let* ((load-dir (concat emacs-dir lisp-path))
	   (default-directory load-dir))
      (setq load-path (cons load-dir load-path))
      (normal-top-level-add-subdirs-to-load-path))))

;; define a reload command
(defun reload-emacs ()
  "reload my emacs settings"
  (interactive)
  (load-file "~/.emacs.d/init.el") (desktop-revert) (delete-other-windows))

;; Load up org-mode and org-babel
(require 'org-install)
(require 'ob-tangle)

;; Load config.org from ~/.emacs.d/
(org-babel-load-file (expand-file-name "config.org" "~/.emacs.d/"))

;; Done and done!!
(message "\nEmacs is ready to serve you, Master %s!\n" (getenv "USER"))

;;; init.el ends here.
