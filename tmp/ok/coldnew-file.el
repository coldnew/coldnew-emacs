;;; coldnew-file.el --- handle about file open, close save ...etc.
(eval-when-compile (require 'cl))

(require 'coldnew-snippets)

;;;; ---------------------------------------------------------------------------
;;;; hooks
;;;; ---------------------------------------------------------------------------



;; When visit emacs-lisp-dir, change file to view mode
;; this will avoid I change those plugins.
;; FIXME: this will cause .loaddef.el become readonly
;; (add-hook 'find-file-hook
;;	  '(lambda ()
;;	     (if (search (expand-file-name emacs-lisp-dir)
;;			 (directory-file-name (buffer-file-name)))
;;		 (view-mode))))


;; Automatically update timestamp
(add-hook 'write-file-hooks 'time-stamp)

;;;; ---------------------------------------------------------------------------
;;;; ido
;;;; ---------------------------------------------------------------------------
;; (require 'ido)
;; (setq ido-enable-flex-matching t)
;; ;; (setq ido-everywhere t)
;; ;; (ido-mode 1)
;; (setq ido-use-filename-at-point 'guess)
;; (setq ido-file-extensions-order '(".org" ".txt" ".py" ".emacs" ".xml" ".el" ".ini" ".cfg" ".cnf"))
;; (setq ido-ignore-extensions t)




(provide 'coldnew-file)
;; coldnew-file.el ends here.
