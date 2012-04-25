;;; coldnew-function.el ---
(eval-when-compile (require 'cl))


(defun emacs-process-p (pid)
  "If pid is the process ID of an emacs process, return t, else nil.
Also returns nil if pid is nil."
  (when pid
    (let ((attributes (process-attributes pid)) (cmd))
      (dolist (attr attributes)
	(if (string= "comm" (car attr))
	    (setq cmd (cdr attr))))
      (if (and cmd (or (string= "emacs" cmd) (string= "emacs.exe" cmd))) t))))

(defun get-ip-address (&optional dev)
  "get the IP-address for device DEV (default: eth0)"
  (let ((dev (if dev dev "eth0")))
    (format-network-address (car (network-interface-info dev)) t)))

;;;; ---------------------------------------------------------------------------
;;;; Buffer
;;;; ---------------------------------------------------------------------------
(defun get-buffers-matching-mode (mode)
  "Returns a list of buffers where their major-mode is equal to MODE"
  (let ((buffer-mode-matches '()))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
	(if (eq mode major-mode)
	    (add-to-list 'buffer-mode-matches buf))))
    buffer-mode-matches))

;;;; ---------------------------------------------------------------------------
;;;; date
;;;; ---------------------------------------------------------------------------
(defun current-date-time ()
  "return current date in `%Y-%m-%d' format, ex:`2012-04-25'."
  (let ((system-time-locale "en_US")
	(format "%Y-%m-%d"))
    (format-time-string "%Y-%m-%d")))


;;;; ---------------------------------------------------------------------------
;;;; Advice
;;;; ---------------------------------------------------------------------------

(defadvice kill-emacs (around recompile-emacs-config activate)
  "Before exit emacs, recompile emacs-config"
  (let ((config-dir emacs-config-dir))
    (byte-recompile-directory config-dir 0) ad-do-it))



(provide 'coldnew-functions)
;; coldnew-functions.el ends here.
