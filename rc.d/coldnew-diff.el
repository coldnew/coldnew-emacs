;;
(eval-when-compile (require 'cl))

;;;;;;;; Packages Import
(require 'coldnew-macro)
(require 'coldnew-functions)
(require 'coldnew-commands)
(require 'coldnew-variables)
(require 'coldnew-evil)

;;;;;;;; Loding libraries
(require 'ediff)
(require 'diff)


;;;;;;;; diff-mode
(setq diff-)


(evil-define-key 'motion ediff-mode-map "[c" 'ediff-next-difference)

(defun command-line-diff (switch)
  (let ((file1 (pop command-line-args-left))
	(file2 (pop command-line-args-left)))
    (ediff file1 file2)))

(add-to-list 'command-switch-alist '("-diff" . command-line-diff))

(provide 'coldnew-diff)
;; coldnew-diff.el ends here.u
