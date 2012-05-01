;;; coldnew-editor.el --- enhanced core editing experience.
(eval-when-compile (require 'cl))

(require 'coldnew-complete)

;;;; ---------------------------------------------------------------------------
;;;; minor-mode
;;;; ---------------------------------------------------------------------------

(defvar coldnew-editor-hook nil
  "Hooks for coldnew-editor-mode.")

(defvar coldnew-editor-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for coldnew-editor-mode.")

(define-minor-mode coldnew-editor-mode
  "Minor mode for coldnew's editor."
  :init-value t
  :lighter " coldnew-editor"
  :keymap coldnew-editor-map
  (run-hooks 'coldnew-editor-hook))

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
;; Color nested parentheses, brackets, and braces according to their dept
(add-hook 'coldnew-editor-hook 'rainbow-delimiters-mode)
;; Add spaces between Chinese and English character.
(add-hook 'before-save-hook 'insert-space-between-english-chinese)

;;;; ---------------------------------------------------------------------------
;;;; Keybindings
;;;; ---------------------------------------------------------------------------

;;;; ---------------------------------------------------------------------------
;;;; key-chord
;;;; ---------------------------------------------------------------------------
(require 'key-chord)
(key-chord-mode 1)

;;;; ---------------------------------------------------------------------------
;;;; space-chord
;;;; ---------------------------------------------------------------------------
(require 'space-chord)

;;;; ---------------------------------------------------------------------------
;;;; projectile
;;;; ---------------------------------------------------------------------------
(require 'projectile)
(projectile-global-mode)
(setq projectile-enable-caching t)

;;;; ---------------------------------------------------------------------------
;;;; paredit
;;;; ---------------------------------------------------------------------------
(require 'paredit)

(defun paredit-blink-paren-match (another-line-p)
  "redefine this function, i don't like paredit to blikn math paren")

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
;;;; slime
;;;; ---------------------------------------------------------------------------
(require 'slime)
(require 'ac-slime)
;; Save REPL history to emacs-cache-dir
(setq slime-repl-history-file (concat emacs-cache-dir "slime-hist.dat"))

;; REPL history size set to 300
(setq slime-repl-history-size 300)

;; Use global programming mode
(add-hook 'slime-repl-mode-hook 'programming-mode)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)


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

  ;; keybindings
  (define-key evil-insert-state-local-map (kbd ";") 'paredit-semicolon)
  (define-key evil-insert-state-local-map (kbd "M-;") 'paredit-comment-dwim)
  (define-key evil-insert-state-local-map (kbd "C-j") 'paredit-newline)
  (define-key evil-insert-state-local-map (kbd "M-9") 'paredit-wrap-round)
  (define-key evil-insert-state-local-map (kbd "M-s") 'paredit-splice-sexp)
  (key-chord-define evil-insert-state-local-map "bs"  'paredit-splice-sexp-killing-backward)
  (key-chord-define evil-insert-state-local-map "fs"  'paredit-splice-sexp-killing-forward)
  (define-key evil-insert-state-local-map (kbd "C-0") 'paredit-forward-slurp-sexp)
  (define-key evil-insert-state-local-map (kbd "C-]") 'paredit-forward-barf-sexp)
  (define-key evil-insert-state-local-map (kbd "C-9") 'paredit-backward-slurp-sexp)
  (define-key evil-insert-state-local-map (kbd "C-[") 'paredit-backward-barf-sexp)
  (key-chord-define evil-insert-state-local-map "ps"  'paredit-split-sexp)
  (key-chord-define evil-insert-state-local-map "pj"  'paredit-join-sexp)
  )


;;;; cc-mode common setting
(defun coldnew-cc-mode-common-setting ()
  "coldnew's common setting for cc-mode"

  ;; Use coldnew's editor mode
  (coldnew-editor-mode)

  ;; enable doxygen
  (doxymacs-mode t)
  (doxymacs-font-lock)

  ;; gtags
  (gtags-mode t)
  (if-not (string-match "/usr/src/linux/" (expand-file-name default-directory))
	  (gtags-create-or-update))

  ;; keybindings
  (define-key evil-normal-state-local-map (kbd "C-x C-o") 'ff-find-other-file)
  (define-key evil-insert-state-local-map (kbd "C-x C-o") 'ff-find-other-file)
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
	       (whitespace-cleanup)
	       (delete-trailing-whitespace))))

(defun highlight-additional-keywords ()
  "Highlight additional keywords."
  (font-lock-add-keywords nil '(("\\<\\(FIXME\\|BUG\\|HACK\\):" 1 font-lock-warning-face t)))
  (font-lock-add-keywords nil '(("\\<\\(NOTE\\):" 1 'org-level-2 t)))
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

(defun insert-space-between-english-chinese ()
  "Insert a space between English words and Chinese charactors"
  (save-excursion
    (beginning-of-buffer)
    (while (re-search-forward "\\(\\cc\\)\\([a-zA-Z0-9]\\)" nil t)
      (replace-match "\\1 \\2" nil nil))
    (beginning-of-buffer)
    (while (re-search-forward "\\([a-zA-Z0-9]\\)\\(\\cc\\)" nil t)
      (replace-match "\\1 \\2" nil nil))
    (beginning-of-buffer)
    (while (re-search-forward "\\([。，！？；：「」（）、]\\) \\([a-zA-Z0-9]\\)" nil t)
      (replace-match "\\1\\2" nil nil))
    (beginning-of-buffer)
    (while (re-search-forward "\\([a-zA-Z0-9]\\) \\([。，！？；：「」（）、]\\)" nil t)
      (replace-match "\\1\\2" nil nil))))




(provide 'coldnew-editor)
;; coldnew-editor.el ends here.
