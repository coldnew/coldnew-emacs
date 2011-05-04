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

(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single
   line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
       (message "Copied line")
       (list (line-beginning-position)
	     (line-beginning-position 2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single
   line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
       (list (line-beginning-position)
	     (line-beginning-position 2)))))

(provide 'coldnew-advice)
;; coldnew-advice.el ends here.
