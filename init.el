;;; init.el --- configuration entry point.
(eval-when-compile (require 'cl))

(message "\nEmacs is powering up... Be patient, Master %s!\n" (getenv "USER"))

;; Define emacs-dir where all the files live.
(defvar emacs-dir
  (file-name-directory
   (or load-file-name (buffer-file-name)))
  "Define where user load this init.el, this variable will be `~/.emacs.d/' in many case.")

;; Add directories to emacs's `load-path' recursively.
;; if path does not exist, create directory.
(let* ((lisp-dir '("lisp/" "themes/" "local-lisp/")))
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
             load-path
             )))))

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

;; Done and done!!
(message "\nEmacs is ready to serve you, Master %s!\n" (getenv "USER"))

;;; init.el ends here.
