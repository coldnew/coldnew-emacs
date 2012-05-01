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

(defun slime-java-describe (symbol-name)
  "Get details on Java class/instance at point."
  (interactive (list (slime-read-symbol-name "Java Class/instance: ")))
  (when (not symbol-name)
    (error "No symbol given"))
  (save-excursion
    (set-buffer (slime-output-buffer))
    (unless (eq (current-buffer) (window-buffer))
      (pop-to-buffer (current-buffer) t))
    (goto-char (point-max))
    (insert (concat "(clojure.contrib.repl-utils/show " symbol-name ")"))
    (when symbol-name
      (slime-repl-return)
      (other-window 1))))

(defun slime-javadoc (symbol-name)
  "Get JavaDoc documentation on Java class at point."
  (interactive (list (slime-read-symbol-name "JavaDoc info for: ")))
  (when (not symbol-name)
    (error "No symbol given"))
  (set-buffer (slime-output-buffer))
  (unless (eq (current-buffer) (window-buffer))
    (pop-to-buffer (current-buffer) t))
  (goto-char (point-max))
  (insert (concat "(clojure.contrib.javadoc/javadoc " symbol-name ")"))
  (when symbol-name
    (slime-repl-return)
    (other-window 1)))




;;;; ---------------------------------------------------------------------------
;;;; Commands
;;;; ---------------------------------------------------------------------------
(defun clojure-mode:define-function ()
  (interactive)
  (let ((name (symbol-at-point)))
    (backward-paragraph)
    (insert "\n(defn " (symbol-name name) "\n  [])\n")
    (backward-char 3)))


(provide 'coldnew-lang-clojure)
;; coldnew-lang-clojure.el ends here.
