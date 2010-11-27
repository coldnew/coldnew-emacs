;;
(provide 'rc-common-hook)



(defun programming-common-hook ()
  "The meta functions for programmer use."
  (linum-mode)				; 顯示行號
  (highlight-changes-mode)		; 顯示修改過的行
  (show-prog-keywords)			; 高亮特殊字
  (show-too-long-lines)			; 高亮過長行
  (highlight-changes-remove-after-save) ; 存檔後移除之前修改的顯示
  (set-newline-and-indent)		; 行尾按ENTER自動縮排
  (indent-file-when-save)		; 儲存檔案時自動縮排
  (use-hungry-delete)			; 啟用 hungry-delete mode
  )


;; nuke whitespaces when writing to a file
(add-hook 'before-save-hook 'whitespace-cleanup)

;;;; Functions

(defun show-too-long-lines ()
  "highlight too long lines."
  (font-lock-add-keywords nil '(("^[^\n]\\{100\\}\\(.*\\)$" 1 font-lock-warning-face t))))

(defun show-prog-keywords ()
  "highlight additional keywords."
  (font-lock-add-keywords nil '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))
  (font-lock-add-keywords nil '(("\\<\\(DONE\\):" 1 font-lock-doc-face t))))

(defun font-lock-fontify-numbers ()
  "Use this function as a hook to fontify numbers as constant"
  (font-lock-add-keywords nil
			  '(("[^a-zA-Z_]\\(0x[0-9a-fA-F]+\\)" 1 font-lock-constant-face) ; hexa
			    ("[^a-zA-Z_]\\(-?[0-9]+\\.[0-9]+\\)" 1 font-lock-constant-face) ; float
			    ("[^a-zA-Z_1-9]\\(-?[0-9]+L?\\)" 1 font-lock-constant-face)))) ; int

(defun highlight-changes-remove-after-save ()
  "Remove previous changes after save."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
	    (lambda ()
	      (highlight-changes-remove-highlight (point-min) (point-max)))))

(defun set-newline-and-indent ()
  (when (featurep'vim)
    (vim:imap (kbd "RET") 'newline-and-indent)))

(defun indent-file-when-save ()
  "When save, indent whole file."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
	    (lambda ()
	      (indent-region (point-min) (point-max) nil)
	      (save-buffer))))

(defun use-hungry-delete ()
  "Use hungry delete mode"
  (when (require 'hungry-delete nil 'noerro)
    (turn-on-hungry-delete-mode)))
