;;
(eval-when-compile (require 'cl))

;;;;;;;; Packages Import
(require 'coldnew-macro)
(require 'coldnew-functions)
(require 'coldnew-commands)
(require 'coldnew-variables)
(require 'coldnew-evil)
(require 'coldnew-anything)
(require 'coldnew-complete)
(require 'coldnew-snippets)
(require 'coldnew-evil)
(require 'coldnew-cedet)
(require 'coldnew-project)
(require 'coldnew-build)


;;;;;;;; Settings
(setq line-spacing                    4 )
(setq fill-column                   100 )
(setq kill-ring-max                 300 )
(setq line-number-mode                t )
(setq column-number-mode              t )

;; Auto add a newline at the end of document
(setq require-final-newline           t )

;; Enable global font-lock
(global-font-lock-mode                t )

;; Auto revert file if file modified
(global-auto-revert-mode              1 )

;; Use lisp-interaction-mode as default major mode
(setq-default major-mode 'lisp-interaction-mode )

;;;;;;;; Programming Mode
(defun programming-mode ()
  "Programming mode is use for all programming languages."

  ;; Enable flymake
  (flymake-mode t)

  ;; Enable line-number
  (linum-mode)

  ;; Enable Auto-Complete
  (auto-complete-mode t)

  ;; Before save to a file, cleanup whitespaces
  (cleanup-whitespace-before-save)

  ;; Enable hungry delete
  (use-hungry-delete)

  ;; After save to file, auto indent whole file
  (indent-file-after-save)

  ;; Make Enter key always do newline-and-indent
  (make-ret-newline-and-indent)

  ;; Highlight toolong lines
  ;;  (highlight-toolong-lines)

  ;; Highlight spicial keywords like TODO, BUG
  (highlight-additional-keywords)

  ;; Highlight fontify numbers ad constant
  (highlight-fontify-numbers)

  ;; Highlight escape char in string
  (highlight-escape-char)

  ;; ;; Use auto-pair
  ;; (use-autopair-mode)

  ;; ;; Use electric-pair-mode
  ;; (electric-pair-mode)

  ;; Use paredit in elisp
  (use-paredit-mode)

  ;; Use electric-indent-mode
  (electric-indent-mode)

  ;; Show matching parentheses all the time
  (show-paren-mode t)

  ;; Enable global-compilation setting
  (global-compilation-hook)

  ;; Enable highlight-symbol
  (highlight-symbol-mode)

  )

;;;;;;;; Highlight-symbol
;; automatic and manual symbol highlighting;
;;
(when (require* 'highlight-symbol))



;;;;;;;; cua
;; CUA package provides a complete emulation of the
;; standard CUA key bindings (Motif/Windows/Mac GUI) for selecting and
;; manipulating the region where S-<movement> is used to highlight &
;; extend the region.
;;
(when (require* 'cua-base)
  ;; Enable cua-mode
  (cua-mode t)
  ;; disable default-keybindings in cua-mode
  (setq cua-enable-cua-keys nil)
  )

;;;;;;;; Smartchr
;; emacs version of smartchr.vim
;;
(when (require* 'smartchr))

;;;;;;;; Unicad
;; An elisp port of Mozilla Universal Charset Auto Detector
;;
(when (require* 'unicad))

;;;;;;;; Undo-tree
;;
(when (require 'undo-tree)
  (global-undo-tree-mode))

;;;;;;;; linum+
;; linum+ is an extention for smart control width of line number
;; displayed on linum-mode. If visible line number of current bufffer is
;; from 1 to 50, then width of line number is 2, and visible line number
;; of current buffer is from 100 to 150, then width of line number is 3.
;;
(when (require* 'linum+)
  ;; define line number format when `linum-format' is `dynamic'.
  (setq linum+-dynamic-format "%%%dd")
  ;; define line number format when `linum-format' is `smart'.
  (setq linum+-smart-format   "%%%dd")
  )

;;;;;;;; linum-relative
;; display relative line number in the left margin
;;
;; (when (require 'linum-relative)
;;   (setq linum-relative-current-symbol "->")
;;   )

;;;;;;;; rainbow-mode
;; Displays color names with colored background.
;;
(when (require* 'rainbow-mode)
  ;; ;; Auto enable rainbow-mode if the file is emacs's color-theme and any css file.
  (add-hook 'find-file-hook
	    '(lambda ()
	       ;; On following situation will enable rainbow-mode automatically
	       ;; if rainbow-mode does not start yet.
	       (if (and (not (rainbow-mode))
			(or
			 ;; Emacs's color-theme file
			 (string-match "\\w*-theme.el" (buffer-file-name))
			 ;; CSS file
			 (equal major-mode 'css-mode)))
		   ;; Enable rainbow-mode
		   (rainbow-mode))
	       )))

;;;;;;;; iedit-mode
;; This package provides a more intuitive way of replace-string operation
;; (at least for me):
;; - Mark the content in the buffer
;; - Start iedit minor mode - by press C-;
;;   All of the same contents in the buffer are highlighted
;; - Edit one of the them
;;   The change is applied to all other contents simultaneously
;; - Finish - by pressing C-; again
;;
(when (require* 'iedit))

;;;;;;;; nav
;; Emacs mode for filesystem navigation
;;
(when (require* 'nav)
  ;; Ignore following regexp
  (setq nav-boring-file-regexps '("\\.elc$"  "^[.].*$" "\\.pyc$" "\\.bak$" "\\.o$" "\\~$"
				  "\\.out$"))
  )

;;;;;;;; pomodoro
;; Pomodoro Technique for emacs
;; The Pomodoro Technique® is a way to get the most out of time management.
;; Turn time into a valuable ally to accomplish what we want to do and chart
;; continuous improvement in the way we do it.
;;
(when (require* 'pomodoro)
  ;; auto start pomodoro
  ;;  (pomodoro)
  )

;;;;;;;; Speedbar
(when (require* 'speedbar)
  (require* 'sr-speedbar)
  (setq sr-speedbar-right-side nil)
  )

;;;;;;;; Wrap-region
;; Wrap Region is a minor mode for Emacs that wraps a region with punctuations.
;; For "tagged" markup modes, such as HTML and XML, it wraps with tags.
;;
(when (require* 'wrap-region)
  ;; Enable wrap-region by default.
  (wrap-region-mode t))

;;;;;;;; acejump
;;
;;
(when (require 'ace-jump-mode)

  ;; TODO: Temporary add this function
  (defun ace-jump-char-smae-line-mode (query-char)
    "AceJump char mode in the same line"
    (interactive (list (read-char "Query Char:")))
    (if (ace-jump-query-char-p query-char)
	(progn
	 (setq ace-jump-query-char query-char)
	 (setq ace-jump-current-mode 'ace-jump-char-mode)
	 (ace-jump-do (regexp-quote (make-string 1 query-char)) (point-at-bol) (point-at-eol)
		      ))
	(error "[AceJump] Non-printable char")))
  )

;;;;;;;; Hungry-delete
;;
(require* 'hungry-delete)

;;;;;;;; ASCII code display.
;; This package provides a way to display ASCII code on a window, that is,
;; display in another window an ASCII table highlighting the current character
;; code.
;;
(require 'ascii)


;;;;;;;; Functions
;; FIXME: remove this unuse function
(defun use-hungry-delete ()
  "Use hungry delete mode"
  (when (require* 'hungry-delete)
    (turn-on-hungry-delete-mode)

    ;; TODO: fix this function
    (defadvice hungry-delete-backward (before delete-empty-pair activate)
      (let ((pair-alist '(
			  (?\( . ?\))
			  (?\' . ?\')
			  (?\" . ?\")
			  (?[  . ?])
			  (?{  . ?})
			  (?$  . ?$) )))
	(if (eq (cdr (assq (char-before) pair-alist)) (char-after))
	    (and (char-after) (delete-char 1))
	    )
	)
      )))

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

(defun make-ret-newline-and-indent ()
  "Always make Enter key do newline-and-indent."
  (define-key evil-insert-state-local-map (kbd "RET") 'newline-and-indent))

(defun highlight-toolong-lines ()
  "If a line exist more than 100 chars, highlight this line."
  (font-lock-add-keywords nil '(("^[^\n]\\{100\\}\\(.*\\)$" 1 font-lock-warning-face t))))

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
			    ))
  )


;; (defun use-autopair-mode ()
;;   "Enable autopair for all mode."
;;   (when (require* 'autopair)
;;     ;; Use auto-pair+ for more functions
;;     (require* 'auto-pair+)
;;     ;; Enable autopair-mode
;;     (autopair-mode 1)
;;     ;; Make autopair work with paredit-mode
;;     (when (require* 'paredit)
;;       (defadvice paredit-mode (around disable-autopairs-around (arg) activate)
;;         "Disable autopairs mode if paredit-mode is turned on"
;;         ad-do-it
;;         (if (null ad-return-value)
;;             (autopair-mode 1)
;;             (autopair-mode 0)
;;             ))
;;       )
;;     ))


(when (require* 'paredit)
  ;; Make eldoc work with Paredit
  (when (require* 'eldoc)
    (eldoc-add-command 'paredit-backward-delete 'paredit-close-round))
  ;; Enable Paredit in vim-mode
  (define-minor-mode use-paredit-mode
    "Enable paredit mode."
    :keymap (make-sparse-keymap))
  ;; Normal Map
  (evil-define-key 'normal use-paredit-mode-map (kbd "C-9") 'paredit-forward)
  (evil-define-key 'normal use-paredit-mode-map (kbd "C-0") 'paredit-backward)
  (evil-define-key 'normal use-paredit-mode-map (kbd "C-(") 'paredit-forward-slurp-sexp)
  (evil-define-key 'normal use-paredit-mode-map (kbd "M-(") 'paredit-backward-slurp-sexp)
  (evil-define-key 'normal use-paredit-mode-map (kbd "C-)") 'paredit-forward-barf-sexp)
  (evil-define-key 'normal use-paredit-mode-map (kbd "M-)") 'paredit-backward-barf-sexp)
  ;; Insert Map
  (evil-define-key 'insert use-paredit-mode-map (kbd "(")  'paredit-open-round)
  (evil-define-key 'insert use-paredit-mode-map (kbd ")")  'paredit-close-round)
  (evil-define-key 'insert use-paredit-mode-map (kbd "[")  'paredit-open-square)
  (evil-define-key 'insert use-paredit-mode-map (kbd "]")  'paredit-close-square)
  (evil-define-key 'insert use-paredit-mode-map (kbd "{")  'paredit-open-curly)
  (evil-define-key 'insert use-paredit-mode-map (kbd "}")  'paredit-close-curly)
  (evil-define-key 'insert use-paredit-mode-map (kbd "\"") 'paredit-doublequote)
  (evil-define-key 'insert use-paredit-mode-map (kbd "M-(") 'paredit-wrap-sexp)
  (evil-define-key 'insert use-paredit-mode-map (kbd "C-(") 'paredit-splice-sexp)
  (evil-define-key 'insert use-paredit-mode-map (kbd "M-)") 'paredit-close-round-and-newline)
  (evil-define-key 'insert use-paredit-mode-map (kbd "C-)") 'paredit-split-sexp)
  (evil-define-key 'insert use-paredit-mode-map (kbd "C-j") 'paredit-join-sexps)
  (evil-define-key 'insert use-paredit-mode-map (kbd "M-\"") 'paredit-meta-doublequote)
  (evil-define-key 'insert use-paredit-mode-map (kbd "<delete>") 'paredit-forward-delete)
  (evil-define-key 'insert use-paredit-mode-map (kbd "C-d") 'paredit-forward-delete)
  (evil-define-key 'insert use-paredit-mode-map (kbd "<backspace>") 'paredit-backward-delete)
  (evil-define-key 'insert use-paredit-mode-map (kbd "C-l") 'paredit-backward-delete)
  )


(defadvice paredit-backward-delete (around paredit-backward-delete activate)
  "Intergrated paredit-backward-delete with hungry-delete."
  ad-do-it
  (when (featurep 'hungry-delete)
    (if (eq (char-before) ?\ )
	(hungry-delete-backward))))


(defadvice paredit-forward-delete (around paredit-forward-delete activate)
  "Intergrated paredit-forward-delete with hungry-delete."
  ad-do-it
  (when (featurep 'hungry-delete)
    (if (eolp)
	(hungry-delete-forward))))


;;;;;;;; expand-region
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)


;; (defun use-paredit-mode ()
;;   "Enable paredit-mode and rebind the keybinding to vim-mode when use it."
;;   (when (require* 'paredit)
;;     ;; Make eldoc work with Paredit
;;     (when (require* 'eldoc)
;;       (eldoc-add-command 'paredit-backward-delete 'paredit-close-round))
;;     ;; Enable Paredit in vim-mode
;;     ;; Normal Map
;;     (define-key evil-normal-state-local-map (kbd "C-9") 'paredit-forward)
;;     (define-key evil-normal-state-local-map (kbd "C-0") 'paredit-backward)
;;     (define-key evil-normal-state-local-map (kbd "C-(") 'paredit-forward-slurp-sexp)
;;     (define-key evil-normal-state-local-map (kbd "M-(") 'paredit-backward-slurp-sexp)
;;     (define-key evil-normal-state-local-map (kbd "C-)") 'paredit-forward-barf-sexp)
;;     (define-key evil-normal-state-local-map (kbd "M-)") 'paredit-backward-barf-sexp)
;;     ;; Insert Map
;;     (define-key evil-insert-state-local-map (kbd "(")  'paredit-open-round)
;;     (define-key evil-insert-state-local-map (kbd ")")  'paredit-close-round)
;;     (define-key evil-insert-state-local-map (kbd "[")  'paredit-open-square)
;;     (define-key evil-insert-state-local-map (kbd "]")  'paredit-close-square)
;;     (define-key evil-insert-state-local-map (kbd "{")  'paredit-open-curly)
;;     (define-key evil-insert-state-local-map (kbd "}")  'paredit-close-curly)
;;     (define-key evil-insert-state-local-map (kbd "\"") 'paredit-doublequote)
;;     (define-key evil-insert-state-local-map (kbd "M-(") 'paredit-wrap-sexp)
;;     (define-key evil-insert-state-local-map (kbd "C-(") 'paredit-splice-sexp)
;;     (define-key evil-insert-state-local-map (kbd "M-)") 'paredit-close-round-and-newline)
;;     (define-key evil-insert-state-local-map (kbd "C-)") 'paredit-split-sexp)
;;     (define-key evil-insert-state-local-map (kbd "C-j") 'paredit-join-sexps)
;;     (define-key evil-insert-state-local-map (kbd "M-\"") 'paredit-meta-doublequote)
;;     (define-key evil-insert-state-local-map (kbd "<delete>") 'paredit-forward-delete)
;;     (define-key evil-insert-state-local-map (kbd "C-d") 'paredit-forward-delete)
;;     (define-key evil-insert-state-local-map (kbd "<backspace>") 'paredit-backward-delete)
;;     (define-key evil-insert-state-local-map (kbd "C-l") 'paredit-backward-delete)
;;     ))



;;;;;;;; Functions



;; (defun load-tags-cache (file)
;;   (if (file-exists-p file)
;;       (with-current-buffer (find-file-noselect file)
;;         (goto-char (point-min))
;;         (setq tags-completion-table (read (current-buffer)))
;;         (kill-buffer))
;;     (tags-completion-table)
;;     (save-tags-cache file))

;; (defun save-tags-cache (file)
;;   (with-temp-buffer
;;     (insert (prin1-to-string tags-completion-table))
;;     (write-file file)))

;; ;; Example usage
;; (defvar tags-completion-table-file \\\"~/.emacs.d/tags-completion-table\\\")
;; (load-tags-cache tags-completion-table-file)



;; (defun font-lock-system-command (&optional limit)
;;   ""
;;   (and  (search-forward-regexp "\\<[a-zA-Z\\-]+\\>" limit t)
;;	(executable-find
;;   (buffer-substring-no-properties (car (bounds-of-thing-at-point 'word))
;;                   (cdr (bounds-of-thing-at-point 'word)))
;;   )))



;; ; (font-lock-add-keywords 'sh-mode
;; ;;			'((font-lock-system-command . font-lock-type-face)))

(defvar font-lock-system-command-face 'font-lock-system-command-face)

(defface font-lock-system-command-face
  '((((class color)) (:foreground "red")))
  "I am comment"
  :group 'font-lock-faces)


(defun font-lock-system-command (&optional limit)
  ""
  (and  (search-forward-regexp "\\<[a-zA-Z\\-]+\\>" limit t)
	(executable-find
	 (buffer-substring-no-properties (car (bounds-of-thing-at-point 'word))
					 (cdr (bounds-of-thing-at-point 'word)))
	 )))

(font-lock-add-keywords
 'sh-mode
 '((font-lock-system-command . font-lock-system-command-face)))

(font-lock-add-keywords
 'emacs-lisp-mode
 '((font-lock-system-command . 'font-lock-system-command-face)))



(provide 'coldnew-editor)
;; coldnew-editor.el ends here.
