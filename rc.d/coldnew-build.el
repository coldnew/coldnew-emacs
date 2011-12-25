;;
(eval-when-compile (require 'cl))

;;;;;;;; Packages Import
(require 'coldnew-macro)
(require 'coldnew-functions)
(require 'coldnew-commands)
(require 'coldnew-variables)

(defun global-compilation-hook ()
  "Global compilation setting."

  ;; Make compilaction buffer always scrolls to follow output as it comes in.
  (setq compilation-scroll-output t)

  ;; Auto jump to the first error.
  (setq compilation-auto-jump-to-first-error t)

  ;;

  )


(defun notify-compilation-result(buffer msg)
  "Notify that the compilation is finished,
   close the *compilation* buffer if the compilation is successful,
   and set the focus back to Emacs frame"
  (if (string-match "^finished" msg)
      (progn
       (delete-windows-on buffer)
       (message (propertize "COMPILATION SUCCESSFUL :-) " 'face 'font-lock-warning-face))
       ;;	(tooltip-show "\n Compilation Successful :-) \n ")
       )
      (tooltip-show "\n Compilation Failed :-( \n "))
  ;; FIXME: When I use dualscreen, following functiokn will make error,
  ;;        after compilation, current frame will jump to another DISPLAY
  ;;  (setq current-frame (car (car (cdr (current-frame-configuration)))))
  ;; (select-frame-set-input-focus current-frame)
  )

(add-to-list 'compilation-finish-functions 'notify-compilation-result)





(provide 'coldnew-build)
;; coldnew-build.el ends here.
