;;
(eval-when-compile (require 'cl))

;;;;;;;; Packages Import
(require 'coldnew-macro)
(require 'coldnew-functions)
(require 'coldnew-commands)
(require 'coldnew-variables)

;;;;;;;; Advice

(defadvice kill-emacs (around recompile-emacs-config activate)
  "Before exit emacs, recompile emacs-config"
  (let ((config-dir emacs-config-dir))
    (byte-recompile-directory config-dir 0) ad-do-it))

(defadvice save-buffers-kill-terminal (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (flet ((process-list ())) ad-do-it))

(defadvice switch-to-buffer (after switch-to-buffer activate)
  "After switch-to-buffer, if tht buffer is Fundamental-mode, change it to lisp-interaction-mode"
  (if (equal major-mode 'fundamental-mode)
      (lisp-interaction-mode)))

(defadvice kill-buffer (around kill-buffer-around-advice activate)
  "bury *scratch* or *Ibuffer* buffer instead of kill it "
  (let ((buffer-to-kill (ad-get-arg 0)))
    (if (or (equal buffer-to-kill "*scratch*")
	    (equal buffer-to-kill "*Ibuffer*"))
	(bury-buffer)
      ad-do-it)))

(provide 'coldnew-advice)
;; coldnew-advice.el ends here.
