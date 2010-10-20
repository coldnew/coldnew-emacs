;;
(provide 'rc-common-hook)


(defun coldnew/show-too-long-lines ()
  "highlight too long lines."
  (font-lock-add-keywords nil '(("^[^\n]\\{100\\}\\(.*\\)$" 1 font-lock-warning-face t))))



;; nuke whitespaces when writing to a file
(add-hook 'before-save-hook 'whitespace-cleanup)
