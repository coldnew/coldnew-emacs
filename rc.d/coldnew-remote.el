;;
(eval-when-compile (require 'cl))

;;;;;;;; Packages Import
(require 'coldnew-macro)
(require 'coldnew-functions)
(require 'coldnew-commands)
(require 'coldnew-variables)
(require 'tramp)



;; (defun find-file-hook-root-header-warning ()
;;   (when (and buffer-file-name (string-match "root@localhost" buffer-file-name))
;;     (find-file-root-header-warning)))
;; (add-hook 'find-file-hook 'find-file-hook-root-header-warning)

;; (defface find-file-root-header-face
;;   '((t (:foreground "white" :background "red3")))
;;   "*Face use to display header-lines for files opened as root.")

;; (defun find-file-root-header-warning ()
;;   "*Display a warning in header line of the current buffer.
;;    This function is suitable to add to `find-file-root-hook'."
;;   (let* ((warning "WARNING: EDITING FILE AS ROOT!")
;;	 (space (+ 6 (- (frame-width) (length warning))))
;;	 (bracket (make-string (/ space 2) ?-))
;;	 (warning (concat bracket warning bracket)))
;;     (setq header-line-format
;;	  (propertize  warning 'face 'find-file-root-header-face))))

;; (add-hook 'find-file-root-hook 'find-file-root-header-warning)


(provide 'coldnew-remote)
;; coldnew-remote.el ends here.
