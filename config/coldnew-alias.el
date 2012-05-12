;;; coldnew-alias.el --- alias commands
(eval-when-compile (require 'cl))

;;;; ---------------------------------------------------------------------------
;;;; Keybindings
;;;; ---------------------------------------------------------------------------

(defalias 'coldnew/next-line 'next-line)
(defalias 'coldnew/previous-line 'previous-line)


(defalias 'coldnew/set-mark-command 'cua-set-mark-or-rectangle-mark)
(defalias 'coldnew/folding-toggle   'toggle-selective-display)

;;;; ---------------------------------------------------------------------------
;;;; Version Control
;;;; ---------------------------------------------------------------------------


(provide 'coldnew-alias)
;; coldnew-alias.el ends here.
