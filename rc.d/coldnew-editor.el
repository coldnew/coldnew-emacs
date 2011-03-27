;;
(eval-when-compile (require 'cl))

;;;;;;;; Packages Import
(require 'coldnew-macro)
(require 'coldnew-functions)
(require 'coldnew-commands)
(require 'coldnew-variables)
(require 'coldnew-complete)
(require 'coldnew-snippets)


;;;;;;;; Settings
(setq line-spacing                    4 )
(setq fill-column                   100 )
(setq kill-ring-max                 300 )
(setq line-number-mode                t )
(setq column-number-mode              t )

;; Auto add a newline at the end of document
(setq require-final-newline           t )

;; Enable global font-lock
(setq global-font-lock-mode           t )

;; Auto revert file if file modified
(setq global-auto-revert-mode         t )

;; Use lisp-interaction-mode as default major mode
(setq-default major-mode 'lisp-interaction-mode )


;;;;;;;; Programming Mode
(defun programming-mode ()
  "Programming mode is use for all programming languages."

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
  (highlight-toolong-lines)

  ;; Highlight spicial keywords like TODO, BUG
  (highlight-additional-keywords)

  ;; Highlight fontify numbers ad constant
  (highlight-fontify-numbers)

  ;; Use paredit-mode
  (use-paredit-mode)

  )


;;;;;;;; Advices
(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single
   line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position)
	   (line-beginning-position 2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single
   line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
	   (line-beginning-position 2)))))

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
  (setq-default cua-enable-cua-keys nil))

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



;;;;;;;; rainbow-mode
;; Displays color names with colored background.
;;
(when (require* 'rainbow-mode)
  ;; Auto enable rainbow-mode if the file is emacs's color-theme and any css file.
  (add-hook 'find-file-hook
	    '(lambda ()
	       ;; On following situation will enable rainbow-mode automatically
	       ;; if rainbow-mode does not start yet.
	       (if (and (not (rainbow-mode))
			(or
			 ;; Emacs's color-theme file
			 (string-match "color-theme-\\w*\\.el" (buffer-file-name))
			 ;; CSS file
			 (equal major-mode 'css-mode)))
		   ;; Enable rainbow-mode
		   (rainbow-mode))
	       ))
  )

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


;;;;;;;; textmate
;; TextMate has some very nice to use bindings on quotes, brackets,
;; parentheses etc, which Emacs lacks out of the box.
;; You can use skeleton-pairs to insert pairs, but what about deleting
;; them? What if you (or a colleague) press the closing bracket key
;; accidentally (muscle memory and all)?
;; textmate-mode provides more sensible behaviour for the keys
;; ", ', (, [, {, and their closing pairs.
;;
(when (require* 'textmate)
  (tm/initialize))


;;;;;;;; Functions
(defun use-hungry-delete ()
  "Use hungry delete mode"
  (when (require* 'hungry-delete)
    (turn-on-hungry-delete-mode)))

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
  (when (require* 'vim)
    (vim:local-imap (kbd "RET") 'newline-and-indent)))

(defun highlight-toolong-lines ()
  "If a line exist more than 100 chars, highlight this line."
  (font-lock-add-keywords nil '(("^[^\n]\\{100\\}\\(.*\\)$" 1 font-lock-warning-face t))))

(defun highlight-additional-keywords ()
  "Highlight additional keywords."
  (font-lock-add-keywords nil '(("\\<\\(FIXME\\|BUG\\):" 1 font-lock-warning-face t)))
  (font-lock-add-keywords nil '(("\\<\\(TODO\\):" 1 'org-todo t)))
  (font-lock-add-keywords nil '(("\\<\\(DONE\\):" 1 'org-done t)))
  )

(defun highlight-fontify-numbers ()
  "Use this function as a hook to fontify numbers as constant"
  (font-lock-add-keywords nil
			  '(
			    ;; hexadecimal
			    ("\\<\\(0x[0-9a-fA-F]+\\)"       1 font-lock-constant-face)
			    ;; float
			    ("\\<\\([+-]?[0-9]+\\.[0-9]+\\)" 1 font-lock-constant-face)
			    ;; int
			    ("\\<\\([+-]?[0-9]+\\)\\b"       1 font-lock-constant-face)
			    )))

(defun use-paredit-mode ()
  "Enable paredit-mode and rebind the keybinding to vim-mode when use it."
  (when (require* 'paredit)
    ;; Make eldoc work with Paredit
    (when (require* 'eldoc)
      (eldoc-add-command 'paredit-backward-delete 'paredit-close-round))
    ;; Enable Paredit in vim-mode
    (when (require* 'vim)


      ))
  )



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
;; (defvar tags-completion-table-file "~/.emacs.d/tags-completion-table")
;; (load-tags-cache tags-completion-table-file)

(provide 'coldnew-editor)
;; coldnew-editor.el ends here.
