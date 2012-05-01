;;; coldnew-tabbar.el ---
(eval-when-compile (require 'cl))


;; (require 'tabbar)
;; (tabbar-mode 1)


;; (defun tabbar-buffer-groups (buffer)
;;   "Return the list of group names BUFFER belongs to.
;;  Return only one group for each buffer."
;;   (with-current-buffer (get-buffer buffer)
;;     (cond
;;      ((string-equal "*" (substring (buffer-name) 0 1))
;;       '("Emacs Buffer"))
;;      ((eq major-mode 'dired-mode)
;;       '("Dired"))
;;      (t
;;       '("User Buffer"))
;;      )))

;; (setq tabbar-buffer-groups-function 'tabbar-buffer-groups)




(provide 'coldnew-tabbar)
;; coldnew-tabbar.el ends here.
