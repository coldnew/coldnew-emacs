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

;; ;; Change all fundamental-mode buffer to lisp-interaction-mode.
;; (defadvice switch-to-buffer (after switch-to-buffer activate)
;;   "After switch-to-buffer, if tht buffer is Fundamental-mode, change it to lisp-interaction-mode"
;;   (if (equal major-mode 'fundamental-mode)
;;       (lisp-interaction-mode)))

;; Prevent to kill *scratch* and *Ibuffer*
(defadvice kill-buffer (around kill-buffer-around-advice activate)
  "Bury *scratch* or *Ibuffer* buffer instead of kill it "
  (let ((buffer-to-kill (ad-get-arg 0)))
    (if (or (equal buffer-to-kill "*scratch*")
	    (equal buffer-to-kill "*Ibuffer*"))
	(bury-buffer)
	ad-do-it)))



(defadvice lusty-file-explorer (around lusty-buffer-explorer activate)
  "temporary fix ecb bug"
  (let ((ecb-active-p ecb-minor-mode))
    (if ecb-active-p
	(ecb-hide-ecb-windows))
    ad-do-it
    (if ecb-active-p
	(ecb-show-ecb-windows)))
  )




(provide 'coldnew-advice)
;; coldnew-advice.el ends here.
