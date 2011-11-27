;;

(eval-when-compile (require 'cl))

;;;;;;;; Packages Import
(require 'coldnew-macro)
(require 'coldnew-functions)

;;;;;;;; System Variables
(defvar emacs-dir "~/.emacs.d/"
  "The top-level emacs-configure directory.")
(defvar emacs-config-dir "~/.emacs.d/rc.d/"
  "directory to place emacs configure.")
(defvar emacs-etc-dir    "~/.emacs.d/etc/"
  "directory to place other stuff.")
(defvar emacs-var-dir    "~/.emacs.d/var/"
  "directory to place cache and backup files.")
(defvar emacs-lisp-dir   "~/.emacs.d/lisp/"
  "directory to place lisp package from internet.")

(defvar emacs-recipes-dir (concat emacs-etc-dir "recipes/")
  "directory to place local el-get recepies.")
(defvar emacs-snippets-dir (concat emacs-etc-dir "snippets/")
  "directory to place yasnippet files.")
(defvar emacs-cache-dir  (concat emacs-var-dir "cache/")
  "cache file directory.")
(defvar emacs-backup-dir (concat emacs-var-dir "backup/")
  "directory to back up files.")
(defvar emacs-log-dir (concat emacs-var-dir "log/")
  "log file directory.")

(defvar emacs-default-shell "/bin/bash"
  "Default shell for cemacs.")
(defvar emacs-popup-shell-window-height 30
  "Window hight of popup shell.")
(defvar emacs-popup-shell-window-position "bottom"
  "Make popup shell window at buttom by default.")

;;;;;;;; Variables
(defvar mac?     (eq system-type 'darwin)
  "Return nil if OS is not Mac.")
(defvar linux?   (and (eq system-type 'gnu/linux) (not (eq system-type 'drawin)))
  "Return nil if OS is not Linux.")
(defvar cygwin?  (eq system-type 'cygwin)
  "Return nil if OS is not CygWin.")
(defvar windows? (eq system-type 'windows-nt)
  "Return nil if OS is not Windows.")

(defvar root? (zerop (user-real-uid))
  "Return nil if user is not root user.")

(defvar 1280x800?   (and (= (display-pixel-width) 1280)
			 (= (display-pixel-height) 800))
  "Return nil if current display's resolution is not 1280x800")

(defvar 1280x1024?  (and (= (display-pixel-width) 1280)
			 (= (display-pixel-height) 1024))
  "Return nil if current display's resolution is not 1280x1024")

(defvar 1920x1080?  (and (= (display-pixel-width) 1920)
			 (= (display-pixel-height) 1080))
  "Return nil if current display's resolution is not 1920x1080")

;;;;;;;; Face
;; TODO: add face for numbers
;;(defface font-lock-number-face )

;; (defface font-lock-successful-face
;;   '((t (:foreground "green")))
;;   ""
;;   :group font-lock-)


(defface mode-line-vim-face
  '((t (:inherit font-lock-keyword-face)))
  "face for vim-mode strig in mode-line"
  :group 'mode-line)

(defface mode-line-mode-name-face
  '((t (:inherit font-lock-keyword-face)))
  "face for mode-name-string in modeline."
  :group 'mode-line)

(defface mode-line-vim-string-N
  '((t (:inherit font-lock-function-name-face)))
  "face for vim-string in normal-map on mode-line."
  :group 'mode-line)

(defface mode-line-vim-string-I
  '((t (:inherit font-lock-constant-face)))
  "face for vim-string in insert-map on mode-line."
  :group 'mode-line)

(defface mode-line-vim-string-V
  '((t (:inherit font-lock-variable-name-face)))
  "face for vim-string in visual-map on mode-line."
  :group 'mode-line)

(defface mode-line-vim-string-E
  '((t (:inherit font-lock-string-face)))
  "face for vim-string in emacs-map on mode-line."
  :group 'mode-line)



;; TODO: need to test this function
(defvar new-file? (and (buffer-file-name)
		       (not (file-exists-p (buffer-file-name)))
		       (= (point-max) 1)))


(provide 'coldnew-variables)
;; coldnew-variables.el ends here.
