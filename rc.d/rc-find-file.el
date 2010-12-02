;;

(when (require 'find-file nil 'noerror)

;;;; hooks
  (add-hook 'find-file-hook
	    '(lambda ()
	       (
		(find-file:find-proper-mode)
		)))
;;;; modes
  (push ".m" (cadr (assoc "\\.h\\'" cc-other-file-alist)))
  (add-to-list ff-other-file-alist '((("\\.c$"   (".h")))))
  (add-to-list ff-other-file-alist '((("\\.cpp$"   (".h" ".hpp")))))

;;;; Functions
  (defun find-file:find-proper-mode ()
    ;; only run on .h files
    (when (string-match "\\.h\\'" (buffer-file-name))
      (save-window-excursion
	(save-excursion
	  (let* ((alist (append auto-mode-alist nil))  ;; use whatever auto-mode-alist has
		 (ff-ignore-include t)                 ;; operate on buffer name only
		 (src (ff-other-file-name))            ;; find the src file corresponding to .h
		 re mode)
	    ;; go through the association list
	    ;; and find the mode associated with the source file
	    ;; that is the mode we want to use for the .h file
	    (while (and alist
			(setq mode (cdar alist))
			(setq re (caar alist))
			(not (string-match re src)))
	      (setq alist (cdr alist)))
	    (when mode (funcall mode)))))))

  )

(provide 'rc-find-file)
;; rc-find-file.el ends here.
