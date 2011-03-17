

;; Load Path Setting
(let* ((emacs-dir "~/.emacs.d/")
       (lisp-dir '("lisp/" "local-lisp/" "rc.d/")))
  (dolist (lisp-path lisp-dir)
    (let* ((load-dir (concat emacs-dir lisp-path))
	   (default-directory load-dir))
      (setq load-path (cons load-dir load-path))
      (normal-top-level-add-subdirs-to-load-path))))





(require 'coldnew-init)
