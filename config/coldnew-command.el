;;; coldnew-command.el ---
;;; Time-stamp: <2012-04-26 03:38:29 (coldnew)>
(eval-when-compile (require 'cl))

(require 'coldnew-functions)

;;;; ---------------------------------------------------------------------------
;;;; Date
;;;; ---------------------------------------------------------------------------

(defun insert-date-time ()
  "Insert current-date."
  (interactive)
  (insert (current-date-time)))

;;;; ---------------------------------------------------------------------------
;;;; Search
;;;; ---------------------------------------------------------------------------
(defun multi-occur-in-this-mode ()
  "Show all lines matching REGEXP in buffers with this major mode."
  (interactive)
  (multi-occur
   (get-buffers-matching-mode major-mode)
   (car (occur-read-primary-args)))
  (select-window (get-buffer-window "*Occur*")))




;; TODO: move me to other files
(add-hook 'occur-mode-hook 'hl-line-mode)

(provide 'coldnew-command)
;; coldnew-command.el ends here.
