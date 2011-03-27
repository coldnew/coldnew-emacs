;;

(eval-when-compile (require 'cl))


;;;;;;;; Packages Import
(require 'coldnew-macro)
(require 'coldnew-functions)

;;;;;;;; System Variables
(defvar emacs-config-dir "~/.emacs.d/rc.d/"
  "directory to place emacs configure.")
(defvar emacs-etc-dir    "~/.emacs.d/etc/"
  "directory to place other stuff.")
(defvar emacs-var-dir    "~/.emacs.d/var/"
  "directory to place cache and backup files.")
(defvar emacs-lisp-dir   "~/.emacs.d/lisp/"
  "directory to place lisp package from internet.")

(defvar emacs-cache-dir  (concat emacs-var-dir "cache/")
  "cache file directory.")
(defvar emacs-backup-dir (concat emacs-var-dir "backup/")
  "directory to back up files.")


;;;;;;;; Variables
(defvar mac?     (eq system-type 'darwin))
(defvar linux?   (and (eq system-type 'gnu/linux) (not (eq system-type 'drawin))))
(defvar cygwin?  (eq system-type 'cygwin))
(defvar windows? (eq system-type 'windows-nt))
(defvar emacs23? (equal emacs-major-version 23))
(defvar emacs24? (equal emacs-major-version 24))

(defvar new-file? (and (buffer-file-name)
		       (not (file-exists-p (buffer-file-name)))
		       (= (point-max) 1)))

(defvar 1280x800?   (and (= (display-pixel-width) 1280)
			 (= (display-pixel-height) 800)))
(defvar 1280x1024?  (and (= (display-pixel-width) 1280)
			 (= (display-pixel-height) 1024)))
(defvar 1920x1080?  (and (= (display-pixel-width) 1920)
			 (= (display-pixel-height) 1080)))

;;;;;;;; Face
;; TODO: add face for numbers
;;(defface font-lock-number-face )





(provide 'coldnew-variables)
;; coldnew-variables.el ends here.
