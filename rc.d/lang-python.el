;; ;; init for python mode


;; (add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
;; (add-to-list 'interpreter-mode-alist '("python" . python-mode))

;; ;;; Initialize Pymacs
;; (autoload 'pymacs-apply "pymacs")
;; (autoload 'pymacs-call "pymacs")
;; (autoload 'pymacs-eval "pymacs" nil t)
;; (autoload 'pymacs-exec "pymacs" nil t)
;; (autoload 'pymacs-load "pymacs" nil t)
;; ;;; Initialize Rope
;; (pymacs-load "ropemacs" "rope-")
;; (setq ropemacs-enable-autoimport t)
;; (setq ropemacs-enable-shortcuts nil)

;; ;; ipython
;; (cond
;;  (linux-p
;;   (setq ipython-command "/usr/bin/ipython")
;;   (setq py-python-command-args '("-i" "-colors" "Linux"))))

;; ;;pylookup
;; (defvar pylookup-dir "~/.emacs.d/lisp/pylookup")
;; (add-to-list 'load-path pylookup-dir)

;; ;; load pylookup when compile time
;; (eval-when-compile (require 'pylookup))

;; ;; set executable file and db file
;; (setq pylookup-program (concat pylookup-dir "/pylookup.py"))
;; (setq pylookup-db-file (concat pylookup-dir "/pylookup.db"))

;; ;; to speedup, just load it on demand
;; (autoload 'pylookup-lookup "pylookup"
;;   "Lookup SEARCH-TERM in the Python HTML indexes." t)
;; (autoload 'pylookup-update "pylookup"
;;   "Run pylookup-update and create the database at `pylookup-db-file'." t)

(provide 'lang-python)
;; lang-python.el ends here.
