;;

(eval-when-compile (require 'cl))

(defvar emacs-cache-dir "~/.emacs.d/var/cache/"
  "cache file directory.")
(defvar emacs-backup-dir "~/.emacs.d/var/backup/"
  "directory to back up files.")
(defvar emacs-etc-dir "~/.emacs.d/etc/"
  "directory to place other stuff.")
(defvar emacs-lisp-dir "~/.emacs.d/lisp/"
  "directory to place lisp package from internet.")
(defvar emacs-config-dir "~/.emacs.d/rc.d/"
  "directory to place emacs configure.")


;;;;;;;; Variables
;; ;; TODO: remove
;; (defvar mac-p     (eq system-type 'darwin))
;; (defvar linux-p   (and (eq system-type 'gnu/linux) (not mac-p)))
;; (defvar cygwin-p  (eq system-type 'cygwin))
;; (defvar windows-p (eq system-type 'windows-nt))
;; (defvar emacs23-p (equal emacs-major-version 23))
;; (defvar emacs24-p (equal emacs-major-version 24))

;; (defvar new-file-p (and (buffer-file-name)
;;			(not (file-exists-p (buffer-file-name)))
;;			(= (point-max) 1)))

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



(provide 'coldnew-variables)
;; coldnew-variables.el ends here.
