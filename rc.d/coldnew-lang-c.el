;;
(eval-when-compile (require 'cl))


;;;;;;;; Packages Import
(require 'coldnew-editor)
(require 'cc-mode)

;;;;;;;; c-mode extensions
(add-to-list 'auto-mode-alist '("\\.h$" . c-mode))
(add-to-list 'auto-mode-alist '("\\.c$" . c-mode))


;;;;;;;; Auto Complete Settings
(when (require* 'auto-complete)
  (require* 'auto-complete-clang)
  ;; Setting my c-mode auto-complete source
  (defun ac-c-mode-setup ()
    "auto-complete settings for c-mode."
    (setq ac-sources '(
		       ac-source-clang
		       ac-source-dictionary
		       ac-source-abbrev
		       ac-source-semantic
		       ac-source-filename
		       ac-source-files-in-current-dir
		       ac-source-words-in-same-mode-buffers
		       ))
    ;; Default clang completion flags
    ;;    (setq clang-completion-flags
    (setq ac-clang-flags
	  (split-string
	   (concat
	    "-pthread -I./ -I../ "
	    (shell-command-to-string "pkg-config --cflags-only-I opencv gtk+-3.0")
	    )))
    ))

;;;;;;;; Coding-Style Setting
(add-hook 'c-mode-hook
	  '(lambda ()

	     ;; Use linux-kernel style
	     (c-set-style "linux")

	     ;; Setting indentation lvel
	     (setq c-basic-offset 8)

	     ;; Make TAB equivilent to 8 spaces
	     (setq tab-width 8)

	     ;; Use spaces to indent instead of tabs.
	     (setq indent-tabs-mode nil)

	     ;; Indent the continuation by 2
	     (setq c-continued-statement-offset 2)

	     ;; Brackets should be at same indentation level as the statements they open
	     ;; for example:
	     ;;                 if (0)        becomes        if (0)
	     ;;                     {                        {
	     ;;                        ;                         ;
	     ;;                     }                        }
	     (c-set-offset 'substatement-open '0)

	     ;; make open-braces after a case
	     (c-set-offset 'case-label '+)
	     ;; TODO:test this function
	     ;; Handle longname argument in functions
	     (c-set-offset 'arglist-intro '+)
	     ;; make open-braces after a case: statement indent to 0 (default was '+)
	     ;;           (c-set-offset 'statement-case-open 8)
	     ))

;;;;;;;; Hooks
(add-hook 'c-mode-hook
	  '(lambda ()

	     ;; Enable Auto Complete
	     (when (require* 'auto-complete)
	       (ac-c-mode-setup))

	     ;; Enable c-eldoc
	     (setq c-eldoc-includes "`pkg-config gtk+-3.0 opencv --cflags --libs` -I./ -I../")
	     (when (require* 'c-eldoc)
	       (c-turn-on-eldoc-mode))

	     ;; Automatically determine c-basic-offset
	     (when (require* 'guess-offset))

	     ;; Use global programming mode
	     (programming-mode)


	     ))

;;;; Keybindings
(add-hook 'c-mode-hook
	  '(lambda ()
	     ;; Normal map
	     (define-key evil-normal-state-local-map (kbd "C-x C-o") 'ff-find-other-file)
	     ;; Insert map
	     (define-key evil-insert-state-local-map (kbd "M-i") 'c-mode:insert-inc-or-if) ; insert "#include <>" or "if () {...}"
	     (define-key evil-insert-state-local-map (kbd "M-d") 'c-mode:insert-do-while)  ; insert "do {...} while()"
	     (define-key evil-insert-state-local-map (kbd "M-m") 'c-mode:insert-main-function) ; insert "int main () {...}"
	     ))





;;;;;;;; make cedet integrated with c
(when (require 'cedet)
  (semantic-add-system-include "/usr/include" 'c-mode)
  )


;;;; Other Settings
(setq c-mode:include-dirs		; Setting include directories
      '(
	"/usr/include"
	))


;;;; Functions

;; insert yasnippet
(defun c-mode:insert-inc-or-if ()
  "If at the start of line. add `inc' and expand it,
else add `if' and expand it."
  (interactive)
  (let* ((current (point))
	 (begin (line-beginning-position)))
    (if (eq current begin)
	(progn
	 (c-mode:insert-include)
	 (newline-and-indent))
	(progn
	 (insert "if")
	 (yas/expand)))))

;; FIXME: We don't want directories also show in completion
(define-skeleton c-mode:insert-include
  "generate include<>" ""
  > "#include <"
  (completing-read "Enter include file："
		   (mapcar #'(lambda (f) (list f ))
			   (apply 'append
				  (mapcar
				   #'(lambda (dir)
				       (directory-files dir))
				   c-mode:include-dirs
				   ))))
  ">")




(defun c-mode:insert-do-while ()
  "insert do{...} while()."
  (interactive)
  (insert "do")
  (yas/expand))

(defun c-mode:insert-main-function ()
  "insert main()."
  (interactive)
  (let* ((current (point))
	 (begin (line-beginning-position)))
    (if (equal current begin)
	(insert "main"))
    (yas/expand)))



(provide 'coldnew-lang-c)
;; coldnew-lang-c.el ends here.
