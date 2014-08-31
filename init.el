;;; init.el --- configuration entry point.
(eval-when-compile (require 'cl))

(message "\nEmacs is powering up... Be patient, Master %s!\n" (getenv "USER"))

;; Since I use Cask to mantain emacs config, check if it exist
(unless (file-exists-p "~/.cask/cask.el")
  (error "~/.cask/cask.el not found!!! Please install Cask first."))

;; load cask and pallet
(require 'cask "~/.cask/cask.el")
(cask-initialize)

;; Use pallet to install packages in emacs
(require 'pallet)

;; Define emacs-dir where all the files live.
(defvar emacs-dir
  (file-name-directory (or load-file-name (buffer-file-name)))
  "Define where user load this init.el, this variable will be `~/.emacs.d/' in many case.")

;; Add directories to emacs's `load-path' recursively.
;; if path does not exist, create directory.
(let* ((lisp-dir '("themes/" "local-lisp/")))
  (dolist (lisp-path lisp-dir)
    (if (not (file-exists-p lisp-path)) (make-directory (concat emacs-dir lisp-path) t))
    (let* ((load-dir (concat emacs-dir lisp-path))
           (default-directory load-dir))
      (setq load-path
            (append
             (let ((load-path (copy-sequence load-path)))
               (append
                (copy-sequence (normal-top-level-add-to-load-path '(".")))
                (normal-top-level-add-subdirs-to-load-path)))
             load-path)))))

;;;; Create some dir in ramdisk
;; I bind `backup', `cache', `log' dir to /tmp/.emacs.d/, when emacs
;; startup just check if these dir exist or not, create the dir if not
;; exist.
(let ((pdir "/tmp/.emacs.d"))
    (make-directory (concat pdir "/backup") t)
    (make-directory (concat pdir "/cache")  t)
    (make-directory (concat pdir "/log")    t))

;; Make customize-ui write file to ~/.emacs.d/custom.el
(setq custom-file (concat emacs-dir "/custom.el"))

;; define a reload command
(defun reload-emacs ()
  "reload my emacs settings"
  (interactive)
  (load-file (concat emacs-dir "init.el")) (delete-other-windows))
;;  (load-file (concat emacs-dir "init.el")) (desktop-revert) (delete-other-windows))

(defun eval-buffer-until-error ()
  "Evaluate emacs buffer until error occured."
  (interactive)
  (goto-char (point-min))
  (while t (eval (read (current-buffer)))))

;; Load up org-mode and org-babel
(require 'org)
(setq org-confirm-babel-evaluate nil)

;; Load config.org from emacs-dir
(org-babel-load-file (expand-file-name "config.org" emacs-dir))

;; Load my personal secret config which can't write on config.org file
(let ((secret-config "~/.secret.el.gpg"))
  (when (file-exists-p secret-config) (load-file secret-config)))

;; Some personal config NEED TO REMOVE
(let ((personal-config "~/.personal.el"))
  (when (file-exists-p personal-config) (load-file personal-config)))

;; Done and done!!
(message "\nEmacs is ready to serve you, Master %s!\n" (getenv "USER"))

;;(require 'remote-emacsclient)
;;(update-tramp-emacs-server-port-forward tramp-default-method)

;;; init.el ends here.
