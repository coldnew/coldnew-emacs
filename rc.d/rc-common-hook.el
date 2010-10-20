;;
(provide 'rc-common-hook)


(defun coldnew/show-too-long-lines ()
  "highlight too long lines."
  (font-lock-add-keywords nil '(("^[^\n]\\{100\\}\\(.*\\)$" 1 font-lock-warning-face t))))

(defun coldnew/show-prog-keywords ()
  "highlight additional keywords."
  (font-lock-add-keywords nil '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))
  (font-lock-add-keywords nil '(("\\<\\(DONE\\):" 1 font-lock-doc-face t))))


(defun programming-common-hook ()
  "the meta functions for programmer use."
  (linum-mode)
  (coldnew/show-prog-keywords)
  (coldnew/show-too-long-lines))


;; nuke whitespaces when writing to a file
(add-hook 'before-save-hook 'whitespace-cleanup)
