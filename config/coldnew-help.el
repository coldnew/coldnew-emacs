;;; coldnew-help.el --- setting for woman, info ...etc.
(eval-when-compile (require 'cl))

;;;; ---------------------------------------------------------------------------
;;;; woman
;;;; ---------------------------------------------------------------------------
(require 'woman)
(setq woman-cache-filename (concat emacs-cache-dir "woman.cache"))
(setq woman-use-topic-at-point nil)
;; Colorful fonts
(setq woman-fontify t)
(setq woman-fill-column 100)


;;;; ---------------------------------------------------------------------------
;;;; info
;;;; ---------------------------------------------------------------------------
(require 'info)


(provide 'coldnew-help)
;; coldnew-help.el ends here.
