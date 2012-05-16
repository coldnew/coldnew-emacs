;;; coldnew-editor.el --- enhanced core editing experience.
(eval-when-compile (require 'cl))

(require 'misc)
(require 'coldnew-complete)

;;;; ---------------------------------------------------------------------------
;;;; minor-mode
;;;; ---------------------------------------------------------------------------
;;(require* 'centered-cursor-mode)
(require 'key-chord)

(defvar coldnew-editor-hook nil
  "Hooks for coldnew-editor-mode.")

(defvar coldnew-editor-state "Emacs"
  "default editor mode is Emacs-mode")

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

(defun coldnew/disable-mode-according-state ()
  (cond
   ((string= "View"  coldnew-editor-state) (view-mode -1))
   ((string= "Command"  coldnew-editor-state) (coldnew/command-mode -1))
   ))

(defun coldnew/switch-to-emacs-mode ()
  (interactive)
  ;; disable other state according mode
  (coldnew/disable-mode-according-state)
  (setq coldnew-editor-state "Emacs"))

(defun coldnew/switch-to-command-mode ()
  (interactive)
  ;; disable other state according mode
  (coldnew/disable-mode-according-state)
  (setq coldnew-editor-state "Command")
  (coldnew/command-mode 1))

(defun coldnew/switch-to-emacs-mode-append ()
  (interactive)
  (coldnew/switch-to-emacs-mode)
  (unless (eolp) (forward-char)))
(require* 'fastnav)

(defvar coldnew/command-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    ;; simulate vim keys
    (define-key map "i" 'coldnew/switch-to-emacs-mode)
    (define-key map "a" 'coldnew/switch-to-emacs-mode-append)
    (define-key map "h" 'backward-char)
    (define-key map "j" 'next-line)
    (define-key map "k" 'previous-line)
    (define-key map "l" 'forward-char)
    (define-key map "%" 'match-paren)
    (define-key map "bl" 'beginning-of-line)
    (define-key map "yy" 'kill-ring-save)
    (define-key map "x" 'delete-char)
    (define-key map "dd" 'kill-whole-line)
    (define-key map "u" 'undo-tree-undo)
    (define-key map (kbd "C-r") 'undo-tree-redo)
    (define-key map "." 'repeat)
    (define-key map (kbd "C-f") 'View-scroll-page-forward)
    (define-key map (kbd "C-b") 'View-scroll-page-backward)
    (define-key map (kbd "0") 'coldnew/beginning-of-line-or-digit-argument)
    (define-key map (kbd "$") 'end-of-line)

    ;; my keymap
    (define-key map (kbd "z") 'zap-up-to-char)
    (define-key map (kbd "Z") 'zap-up-to-char-backward)
    (define-key map (kbd "b") 'beginning-of-line)
    (define-key map (kbd "e") 'end-of-line)
    (define-key map (kbd "n") 'View-scroll-page-forward)
    (define-key map (kbd "p") 'View-scroll-page-backward)
    (define-key map (kbd "<SPC>") 'ace-jump-mode)
    (define-key map (kbd "d(")  '(lambda () (interactive) (delete-between-pair ?\()))
    (define-key map (kbd "d\"") '(lambda () (interactive) (delete-between-pair ?\")))
    (define-key map (kbd "d[")  '(lambda () (interactive) (delete-between-pair ?\[)))
    (define-key map (kbd "d{")  '(lambda () (interactive) (delete-between-pair ?\{)))
    (define-key map (kbd "[") 'beginning-of-buffer)
    (define-key map (kbd "]") 'end-of-buffer)
    map)
  "Keymap for coldnew-editor-Mode.")

(define-minor-mode coldnew/command-mode
  "Minor mode like vi's normal mode"
  :init-value nil
  :global t
  :lighter " "
  :keymap coldnew/command-mode-map
  (if coldnew/command-mode
      (progn
	(key-chord-mode 1))
    (progn
      (key-chord-mode -1))))

(add-hook 'post-command-hook 'coldnew/set-mode-according-state)

(defvar coldnew/buffer-state-alist
  '((eshell-mode . "Emacs")
    (term-mode   . "Emacs")
    ))

(defun coldnew/set-mode-according-state ()
  (let* ((mode major-mode)
	 (state (cdr-safe (assoc mode coldnew/buffer-state-alist))))
    (if (minibufferp) (setq state "Emacs"))
    (cond
     ((string= "Command" state) (coldnew/switch-to-command-mode))
     ((string= "Emacs"   state) (coldnew/switch-to-emacs-mode)) )
    ))

(defun coldnew/beginning-of-line-or-digit-argument ()
  "Feeds a 0 count or moves the cursor to the beginning of the line."
  (interactive)
  (if (and current-prefix-arg
	   (not (zerop (prefix-numeric-value current-prefix-arg))))
      (call-interactively 'digit-argument)
    (call-interactively 'beginning-of-line)))

;;;; ---------------------------------------------------------------------------
;;;; Initial Editor Setting
;;;; ---------------------------------------------------------------------------
(setq indent-tabs-mode nil )          ; don't use tabs to indent
(setq tab-width          8 )          ; default tab-width is 8
(setq line-spacing       4 )          ; Additional space between lines
(setq fill-column      100 ) ; column beyond which automatic line-wrapping shold happen
(setq kill-ring-max    300 ) ; Maximum lenght of kill-ring
(setq require-final-newline  t ) ; Auto add a newline at the end of line
(setq next-line-add-newlines t ) ;
(setq shift-select-mode      t ) ; Enable shift-select mode

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
;;;; View-mode
;;;; ---------------------------------------------------------------------------

;;; view-mode is a `read-only' mode
(setq view-read-only nil)

(define-key view-mode-map "i" 'coldnew/switch-to-emacs-mode)
(define-key view-mode-map "h" 'backward-char)
(define-key view-mode-map "j" 'next-line)
(define-key view-mode-map "k" 'previous-line)
(define-key view-mode-map "l" 'forward-char)
(define-key view-mode-map (kbd "C-f") 'View-scroll-page-forward)
(define-key view-mode-map (kbd "C-b") 'View-scroll-page-backward)


;;;; ---------------------------------------------------------------------------
;;;; center-cursor
;;;; ---------------------------------------------------------------------------
;;(require* 'centered-cursor-mode)
;;(global-centered-cursor-mode +1)

;;;; ---------------------------------------------------------------------------
;;;; minimap
;;;; ---------------------------------------------------------------------------
;; minimap is really funny :)
;;(require* 'minimap)

;;;; ---------------------------------------------------------------------------
;;;; zone
;;;; ---------------------------------------------------------------------------

;; Uncomment this if you'd like your Emacs session to do amusing
;; things after 3 minutes of idle time.  Hitting a key will stop the
;; madness :^) .
;;
(require 'zone)
(setq zone-idle (* 60 300 1000))
(zone-when-idle zone-idle)


;;;; ---------------------------------------------------------------------------
;;;; hideshow
;;;; ---------------------------------------------------------------------------
(require 'hideshow)
(require* 'hideshowvis)

;;; enable following mode to use hideshow
(dolist (hook (list 'emacs-lisp-mode-hook
		    'c++-mode-hook
		    'c-mode-hook))
  (add-hook hook 'hideshowvis-enable))


;;;; ---------------------------------------------------------------------------
;;;; key-chord
;;;; ---------------------------------------------------------------------------
(require 'key-chord)
;;(key-chord-mode 1)
(setq key-chord-two-keys-delay 0.05)

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
(require 'cua-rect)
(setq cua-enable-cua-keys nil)		; don't add C-x, C-c, C-v
(cua-mode t)				; Enable cua-mode


;;;; ---------------------------------------------------------------------------
;;;; hungry-delete
;;;; ---------------------------------------------------------------------------
(require 'hungry-delete)
(add-hook 'coldnew-editor-hook 'hungry-delete-mode)

;;;; ---------------------------------------------------------------------------
;;;; undo-tree
;;;; ---------------------------------------------------------------------------
(require 'undo-tree)
;; Enable undo-tree globally
(global-undo-tree-mode)
;; keybinding
(define-key undo-tree-visualizer-map (kbd "C-g") 'undo-tree-visualizer-quit)

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
  ;; (define-key evil-insert-state-local-map (kbd "M-s") 'paredit-splice-sexp)
  (key-chord-define evil-insert-state-local-map "bs"  'paredit-splice-sexp-killing-backward)
  (key-chord-define evil-insert-state-local-map "fs"  'paredit-splice-sexp-killing-forward)
  (define-key evil-insert-state-local-map (kbd "C-0") 'paredit-forward-slurp-sexp)
  (define-key evil-insert-state-local-map (kbd "C-]") 'paredit-forward-barf-sexp)
  (define-key evil-insert-state-local-map (kbd "C-9") 'paredit-backward-slurp-sexp)
  ;; (define-key evil-insert-state-local-map (kbd "C-[") 'paredit-backward-barf-sexp)
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
    (goto-char (point-min))
    (while (or (re-search-forward "\\(\\cc\\)\\([a-zA-Z0-9]\\)" nil t)
	       (re-search-forward "\\([a-zA-Z0-9]\\)\\(\\cc\\)" nil t))
      (replace-match "\\1 \\2" nil nil))
    (goto-char (point-min))
    (while (or (re-search-forward "\\([。，！？；：「」（）、]\\) \\([a-zA-Z0-9]\\)" nil t)
	       (re-search-forward "\\([a-zA-Z0-9]\\) \\([。，！？；：「」（）、]\\)" nil t))
      (replace-match "\\1\\2" nil nil))))



;;;; ---------------------------------------------------------------------------
;;;; Commands
;;;; ---------------------------------------------------------------------------

(defun select-forwards-to-before-match (match)
  "Selects forwards to just before next match, uses
select-region-to-before-match"
  (interactive "MSelect forwards to just before: ")
  (select-region-to-before-match match 'forwards))

(defun select-backwards-to-before-match (match)
  "Selects backwards to just before next match, uses
select-region-to-before-match"
  (interactive "MSelect backwards to just before: ")
  (select-region-to-before-match match 'backwards))

(defun kill-forwards-to-before-match (match)
  "Selects forwards to just before next match, uses
select-region-to-before-match, then kills that region."
  (interactive "MKill forwards to just before: ")
  (let* ((positions (select-region-to-before-match match 'forwards))
	 (start (car positions))
	 (end (cadr positions)))
    (kill-region start end)))

(defun kill-backwards-to-before-match (match)
  "Selects backwards to just before next match, uses
select-region-to-before-match, then kills that region."
  (interactive "MKill backwards to just before: ")
  (let* ((positions (select-region-to-before-match match 'backwards))
	 (start (car positions))
	 (end (cadr positions)))
    (kill-region start end)))

(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
	((looking-at "\\s\)") (forward-char 1) (backward-list 1))
	(t (self-insert-command (or arg 1)))))

(defun delete-between-pair (char)
  "Delete in between the given pair"
  (interactive "cDelete between char: ")
  (let ((pair-char))
    (search-backward-to-char char)
    (forward-char 1)
    (cond
     ((char-equal char ?\() (setq pair-char ?\)))
     ((char-equal char ?\") (setq pair-char ?\"))
     ((char-equal char ?\') (setq pair-char ?\'))
     ((char-equal char ?\[) (setq pair-char ?\]))
     ((char-equal char ?\{) (setq pair-char ?\}))
     ((char-equal char ?\<) (setq pair-char ?\>)))
    (zap-up-to-char 1 pair-char)))

(defun cua-set-mark-or-rectangle-mark (&optional arg)
  "toggle between cua-set-mark or cua-rectangle-mark"
  (interactive "P")
  (if (or (not mark-active) arg)
      (cua-set-mark arg)
    (cua-toggle-rectangle-mark)))


;;FIXME: temp add, remove one day
(defun hime-agent ()
  ""
  (interactive)
  (start-process "hime-agent" nil "hime-agent"))

(defadvice kill-ring-save (before slickcopy activate compile)
  "When called interactively with no active region, copy a single line
instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
	   (line-beginning-position 2)))))

(defadvice kill-region (before slickcut activate compile)
  "When called interactively with no active region, kill a single line
instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
	   (line-beginning-position 2)))))

(defun zap-up-to-char-backward (arg char)
  (interactive "p\ncZap up to char backward: ")
  (zap-up-to-char (- arg) char))


(provide 'coldnew-editor)
;; coldnew-editor.el ends here.
