;;; coldnew-lang-clojure.el ---
(eval-when-compile (require 'cl))

(require 'clojure-mode)
(require 'slime)
(require 'ac-slime)
(require 'cljdoc)

;;;; clojure-mode extensions
(add-to-list 'auto-mode-alist '("\\.clj$"  . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs$" . clojure-mode))

;;;; ---------------------------------------------------------------------------
;;;; Hooks
;;;; ---------------------------------------------------------------------------

;; use my lisp-common-setting
(add-hook 'clojure-mode-hook 'coldnew-lisp-common-setting)

;; make fn more pretty
(add-hook 'clojure-mode-hook 'pretty-fn)


;;;; ---------------------------------------------------------------------------
;;;; Keybindings
;;;; ---------------------------------------------------------------------------


;;;; ---------------------------------------------------------------------------
;;;; Auto Complete
;;;; ---------------------------------------------------------------------------

;;;; ---------------------------------------------------------------------------
;;;; Flymake
;;;; ---------------------------------------------------------------------------

;;;; ---------------------------------------------------------------------------
;;;; Functions
;;;; ---------------------------------------------------------------------------
(defun pretty-fn ()
  (font-lock-add-keywords nil `(("(\\(\\<fn\\>\\)"
				 (0 (progn (compose-region (match-beginning 1)
							   (match-end 1)
							   "\u0192"
							   'decompose-region)))))))

(defun clojure-mode:define-function ()
  (interactive)
  (let ((name (symbol-at-point)))
    (backward-paragraph)
    (insert "\n(defn " (symbol-name name) "\n  [])\n")
    (backward-char 3)))

;;;; ---------------------------------------------------------------------------
;;;; Commands
;;;; ---------------------------------------------------------------------------


(provide 'coldnew-lang-clojure)
;; coldnew-lang-clojure.el ends here.
