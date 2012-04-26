;;; coldnew-editor.el --- enhanced core editing experience.
(eval-when-compile (require 'cl))

(require 'coldnew-complete)

;;;; ---------------------------------------------------------------------------
;;;; minor-mode
;;;; ---------------------------------------------------------------------------

(defun coldnew-editor-hook () "Hooks for coldnew-editor-mode.")

(defvar coldnew-editor-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for coldnew-editor-mode.")

(define-minor-mode coldnew-editor-mode
  "Minor mode for coldnew's editor."
  :init-value t
  :lighter " coldnew-editor"
  ;;  :global t
  :keymap coldnew-editor-map
  (if coldnew-editor-mode
      (progn
	(run-hooks 'coldnew-editor-hook)
	(evil-local-mode 1))))

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
;; Enable delete-selection-mode
(delete-selection-mode t)

;;;; ---------------------------------------------------------------------------
;;;; Hooks
;;;; ---------------------------------------------------------------------------
;; After save buffer, indent whole file.
(add-hook 'coldnew-editor-hook 'indent-file-after-save)
;; Before save buffer, cleanup whitespace
(add-hook 'coldnew-editor-hook 'cleanup-whitespace-before-save)
;; Enable line-number
(add-hook 'coldnew-editor-hook 'linum-mode)
;; use electric-indent-mode
(add-hook 'coldnew-editor-hook 'electric-indent-mode)
;; highlight special keywords like TODO, BUF
(add-hook 'coldnew-editor-hook 'highlight-additional-keywords)
;; highlight fontify numbers and constant
(add-hook 'coldnew-editor-hook 'highlight-fontify-numbers)
;; highlight escape char in string
(add-hook 'coldnew-editor-hook 'highlight-escape-char)

;;;; ---------------------------------------------------------------------------
;;;; Keybindings
;;;; ---------------------------------------------------------------------------

;;;; ---------------------------------------------------------------------------
;;;; paredit
;;;; ---------------------------------------------------------------------------
(require 'paredit)


;;;; ---------------------------------------------------------------------------
;;;; dtrt-indent
;;;; ---------------------------------------------------------------------------
(require 'dtrt-indent)
(add-hook 'coldnew-editor-hook '(lambda () (dtrt-indent-mode t)))

;;;; ---------------------------------------------------------------------------
;;;; cua
;;;; ---------------------------------------------------------------------------
(require 'cua-base)
(setq cua-enable-cua-keys nil)		; don't add C-x, C-c, C-v
(cua-mode t)				; Enable cua-mode

;;;; ---------------------------------------------------------------------------
;;;; unicad
;;;; ---------------------------------------------------------------------------
(require 'unicad)

;;;; ---------------------------------------------------------------------------

;;;; hungry-delete
;;;; ---------------------------------------------------------------------------
(require 'hungry-delete)
(add-hook 'coldnew-editor-hook 'hungry-delete-mode)

;;;; ---------------------------------------------------------------------------
;;;; highlight-symbol
;;;; ---------------------------------------------------------------------------
(require 'highlight-symbol)
(add-hook 'coldnew-editor-hook 'highlight-symbol-mode)

;;;; ---------------------------------------------------------------------------
;;;; undo-tree
;;;; ---------------------------------------------------------------------------
(require 'undo-tree)
;; Enable undo-tree globally
(global-undo-tree-mode)
;; keybinding
(define-key undo-tree-visualizer-map (kbd "C-g") 'undo-tree-visualizer-quit)

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
;;;; rainbow-delimiters
;;;; ---------------------------------------------------------------------------
(require 'rainbow-delimiters)

;;;; ---------------------------------------------------------------------------
;;;; doxymacs
;;;; ---------------------------------------------------------------------------
(require 'doxymacs)


;;;; ---------------------------------------------------------------------------
;;;; Common language setting
;;;; ---------------------------------------------------------------------------

;;;; lisp common setting
(defun coldnew-lisp-common-setting ()
  "coldnew's common setting for lisp-like mode"

  ;; Use coldnew's editor mode
  (coldnew-editor-mode)

  ;; Use Greek character lambda insteda of string
  (turn-on-pretty-lambda-mode)

  ;; Color nested parentheses, brackets, and braces according to their dept
  (rainbow-delimiters-mode)
  )

;;;; cc-mode common setting
(defun coldnew-cc-mode-common-setting ()
  "coldnew's common setting for cc-mode"

  ;; Use coldnew's editor mode
  (coldnew-editor-mode)

  ;; Color nested parentheses, brackets, and braces according to their dept
  (rainbow-delimiters-mode)

  ;; enable doxygen
  (doxymacs-mode t)
  (doxymacs-font-lock)
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
