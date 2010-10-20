;;
(provide 'rc-common-hook)


(defun coldnew/show-too-long-lines ()
  "highlight too long lines."
  (font-lock-add-keywords nil '(("^[^\n]\\{100\\}\\(.*\\)$" 1 font-lock-warning-face t))))

(defun coldnew/show-prog-keywords ()
  "highlight additional keywords."
  (font-lock-add-keywords nil '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))
  (font-lock-add-keywords nil '(("\\<\\(DONE\\):" 1 font-lock-doc-face t))))

(defun coldnew/highlight-changes-remove-after-save ()
  "Remove previous changes after save."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
	    (lambda ()
		(highlight-changes-remove-highlight (point-min) (point-max)))))



(defun programming-common-hook ()
  "the meta functions for programmer use."
  (linum-mode)					; 顯示行號
  (highlight-changes-mode)			; 顯示修改過的行
  (coldnew/show-prog-keywords)			; 高亮特殊字
  (coldnew/show-too-long-lines)			; 高亮過長行
  (coldnew/highlight-changes-remove-after-save) ; 存檔後移除之前修改的顯示
 )

;(add-hook 'write-file-hooks 'coldnew/highlight-changes-remove-after-save)


;; nuke whitespaces when writing to a file
(add-hook 'before-save-hook 'whitespace-cleanup)
