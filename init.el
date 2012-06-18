;;; init.el --- configuration entry point.
(eval-when-compile (require 'cl))

(message "\n\nEmacs is powering up... Be patient, Master %s!\n\n" (getenv "USER"))

;; add directories to emacs's `load-path' recursively.
(let* ((emacs-dir "~/.emacs.d/")
       (lisp-dir '("lisp/" "local-lisp/")))
  (dolist (lisp-path lisp-dir)
    (let* ((load-dir (concat emacs-dir lisp-path))
           (default-directory load-dir))
      (setq load-path (cons load-dir load-path))
      (normal-top-level-add-subdirs-to-load-path))))

;; a shortcut to reload emacs setting
(global-set-key (kbd "<f5>") '(lambda () (interactive) (load-file "~/.emacs.d/init.el") (desktop-revert) (delete-other-windows)))

;; add safe local variable
(add-to-list 'safe-local-variable-values '(org-confirm-babel-evaluate . nil))


;; load up org-mode and org-babel
(require 'org-install)
(require 'ob-tangle)

;; load up all literate org-mode files in config directory
(mapc #'org-babel-load-file (directory-files "~/.emacs.d/" t "\\.org$"))
;; (let* ((emacs-dir "~/.emacs.d/")
;;        (emacs-config-file (expand-file-name (concat emacs-dir "config.org"))))
;;   (org-babel-load-file emacs-config-file))


;; After loading allemacs config file, readauthorization fil
;;(if (file-exists-p emacs-authinfo-file) (load-file emacs-authinfo-file))


(message "\n\nEmacs is ready to serve you, Master %s!\n\n" (getenv "USER"))
