;;; coldnew-editor.el --- enhanced core editing experience.
(eval-when-compile (require 'cl))

(require 'coldnew-complete)

;;;; ---------------------------------------------------------------------------
;;;; minor-mode
;;;; ---------------------------------------------------------------------------
(define-globalized-minor-mode global-coldnew-editor-mode coldnew-editor-mode coldnew-editor-on)

(defun coldnew-editor-on  () (coldnew-editor-mode   t))
(defun coldnew-editor-off () (coldnew-editor-mode nil))

(defun coldnew-editor-mode-hook () "Hooks for coldnew-editor-mode.")

(defvar coldnew-editor-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for coldnew-editor-mode.")

;;;###autoload
(define-minor-mode coldnew-editor-mode
  "Minor mode for coldnew's editor."
  :init-value t
  :lighter " coldnew-editor"
  :global t
  :keymap coldnew-editor-mode-map
  (if coldnew-editor-mode
      (run-hooks 'coldnew-editor-mode-hook)
    (coldnew-editor-off)))

;;;; ---------------------------------------------------------------------------
;;;; Initial Editor Setting
;;;; ---------------------------------------------------------------------------
(setq indent-tabs-mode nil )		; don't use tabs to indent
(setq tab-width          8 )		; default tab-width is 8
(setq line-spacing       4 )		; Additional space between lines
(setq fill-column      100 )		; column beyond which automatic line-wrapping shold happen
(setq kill-ring-max    300 )		; Maximum lenght of kill-ring
(setq major-mode 'org-mode )		; Use org-mode as default major-mode
(setq require-final-newline t )		; Auto add a newline at the end of line

;; Enable global font-lock
(global-font-lock-mode t)
;; Revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)
;; Show matching parentheses all the time
(show-paren-mode t)
;; Enable auto-complete-mode
(global-auto-complete-mode t)
;; Enable coldnew-editor-minor-mode
(global-coldnew-editor-mode t)

;;;; ---------------------------------------------------------------------------
;;;; Hooks
;;;; ---------------------------------------------------------------------------

;; After save buffer, indent whole file.
(add-hook 'coldnew-editor-mode-hook 'indent-file-after-save)
;; Before save buffer, cleanup whitespace
(add-hook 'coldnew-editor-mode-hook 'cleanup-whitespace-before-save)
;; Enable line-number
(add-hook 'coldnew-editor-mode-hook 'linum-mode)
;; Enable highlight-symbol-mode
(add-hook 'coldnew-editor-mode-hook 'highlight-symbol-mode)
;; Enable hungry-delete-mode
(add-hook 'coldnew-editor-mode-hook 'hungry-delete-mode)
;; use electric-indent-mode
(add-hook 'coldnew-editor-mode-hook 'electric-indent-mode)
;; highlight special keywords like TODO, BUF
(add-hook 'coldnew-editor-mode-hook 'highlight-additional-keywords)
;; highlight fontify numbers and constant
(add-hook 'coldnew-editor-mode-hook 'highlight-fontify-numbers)
;; highlight escape char in string
(add-hook 'coldnew-editor-mode-hook 'highlight-escape-char)

;;;; ---------------------------------------------------------------------------
;;;; Keybindings
;;;; ---------------------------------------------------------------------------



;;;; ---------------------------------------------------------------------------
;;;; unicad
;;;; ---------------------------------------------------------------------------
(require 'unicad)

;;;; ---------------------------------------------------------------------------
;;;; hungry-delete
;;;; ---------------------------------------------------------------------------
(require 'hungry-delete)

;;;; ---------------------------------------------------------------------------
;;;; highlight-symbol
;;;; ---------------------------------------------------------------------------
(require 'highlight-symbol)

;;;; ---------------------------------------------------------------------------
;;;; undo-tree
;;;; ---------------------------------------------------------------------------
(require 'undo-tree)
;; Enable undo-tree globally
(global-undo-tree-mode)

;;;; ---------------------------------------------------------------------------
;;;; iedit
;;;; ---------------------------------------------------------------------------
(require 'iedit)

;;;; ---------------------------------------------------------------------------
;;;; linum+
;;;; ---------------------------------------------------------------------------
(require 'linum+)
;; define line number format when `linum-format' is `dynamic'.
(setq linum+-dynamic-format "%%%dd")
;; define line number format when `linum-format' is `smart'.
(setq linum+-smart-format   "%%%dd")

;;;; ---------------------------------------------------------------------------
;;;; pretty-lambdada
;;;; ---------------------------------------------------------------------------
(require 'pretty-lambdada)

;;;; ---------------------------------------------------------------------------
;;;; Common language setting
;;;; ---------------------------------------------------------------------------

;;;; lisp common setting
(defun coldnew-lisp-common-setting ()
  "coldnew's common setting for lisp-like mode"

  ;; Use Greek character lambda insteda of string
  (turn-on-pretty-lambda-mode)
  )


;;;; ---------------------------------------------------------------------------
;;;; Functions
;;;; ---------------------------------------------------------------------------

(defun indent-file-after-save ()
  "Indent whole file after saved."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
	    '(lambda ()
	       (indent-region (point-min) (point-max) nil)
	       (save-buffer))))

(defun cleanup-whitespace-before-save ()
  "Cleanup whitespaces before save to a file."
  (make-local-variable 'before-save-hook)
  (add-hook 'before-save-hook
	    '(lambda ()
	       (whitespace-cleanup))))

(defun highlight-additional-keywords ()
  "Highlight additional keywords."
  (font-lock-add-keywords nil '(("\\<\\(FIXME\\|BUG\\):" 1 font-lock-warning-face t)))
  (font-lock-add-keywords nil '(("\\<\\(NOTE\\):" 1 'org-todo t)))
  (font-lock-add-keywords nil '(("\\<\\(TODO\\):" 1 'org-todo t)))
  (font-lock-add-keywords nil '(("\\<\\(DONE\\):" 1 'org-done t)))
  )

(defun highlight-fontify-numbers ()
  "Use this function as a hook to fontify numbers as constant"
  (font-lock-add-keywords nil
			  '(
			    ;; hexadecimal
			    ("\\b\\(0x[0-9a-fA-F]+\\)" 1 font-lock-constant-face)
			    ;; float
			    ("\\b\\([+-]?[0-9]+\\.[0-9]+\\)" 1 font-lock-constant-face)
			    ;; int
			    ("[\`^(\{\[,\+\-\*/\%=\s-]\\(-?[0-9]+U?L?L?\\)" 1 font-lock-constant-face)
			    )))

(defun highlight-escape-char ()
  "Use this function as a hook to fontify escape char."
  (font-lock-add-keywords nil
			  '(
			    ("\\\\\\(?:[abfnrtv'\"?\\0]\\|x[a-fA-F]\\{2\\}\\|[0-7]\\{3\\}\\)"
			     0 'font-lock-escape-char-face prepend)
			    )))



(provide 'coldnew-editor)
;; coldnew-editor.el ends here.
