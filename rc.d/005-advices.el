;;
(eval-when-compile (require 'cl))

;;;; Advices
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (flet ((process-list ())) ad-do-it))

;;
(defadvice kill-buffer (around kill-buffer-around-advice activate)
  "bury *scratch* buffer instead of kill it "
  (let ((buffer-to-kill (ad-get-arg 0)))
    (if (equal buffer-to-kill "*scratch*")
	(bury-buffer)
      ad-do-it)))

(provide '005-advices)
;; 005-advices.el ends here.
