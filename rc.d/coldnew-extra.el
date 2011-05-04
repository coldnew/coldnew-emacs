;;
(eval-when-compile (require 'cl))

;; tea-time
;; Tea-time is an extention to emacs.
;; Kind of analog of gnome applet tea-time http://det.cable.nu/teatime/index.rbx?r=2.8.0

;; It allows you to set up time intervals and after this
;; interval is elapsed, Emacs will notify you with sound and    notification.
;; It could be useful if you make a tea or if you would like to
;; be more productive by setting time limit for a task.

;; If available, notification would be done with great tool
;; mumbles ( http://www.mumbles-project.org )
;; If not, then simply use standard emacs message.
;;
(when (require 'tea-time)
  )



(provide 'coldnew-extra)
;; coldnew-extra.el ends here.
