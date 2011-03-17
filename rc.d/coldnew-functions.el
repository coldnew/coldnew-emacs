;;
(eval-when-compile (require 'cl))



(defun show-buffer-major-mode (buffer-or-string)
  "Returns the major mode associated with a buffer."
  (with-current-buffer buffer-or-string major-mode))

(defun emacs-process-p (pid)
  "If pid is the process ID of an emacs process, return t, else nil.
Also returns nil if pid is nil."
  (when pid
    (let ((attributes (process-attributes pid)) (cmd))
      (dolist (attr attributes)
	(if (string= "comm" (car attr))
	    (setq cmd (cdr attr))))
      (if (and cmd (or (string= "emacs" cmd) (string= "emacs.exe" cmd))) t))))

(defun get-current-line()
  "Current line string"
  (buffer-substring (save-excursion (beginning-of-line) (point))
		    (save-excursion (end-of-line) (point))))

(defun line-length()
  "Length of a line in number of characters"
  (length (buffer-substring (save-excursion (beginning-of-line) (point))
			    (save-excursion (end-of-line) (point)))))





(provide 'coldnew-functions)
;; coldnew-functions.el ends here.
