;;
(eval-when-compile (require 'cl))

;;;;;;;; Packages Import
(require 'coldnew-editor)

;;;;;;;; c++-mode extensions
(add-to-list 'auto-mode-alist '("\\.hpp$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cpp$" . c++-mode))
(add-to-list 'magic-mode-alist
	     `(,(lambda ()
		  (and (string= (file-name-extension buffer-file-name) "h")
		       (re-search-forward "\\W\\(class\\|template\\namespace\\)\\W"
					  magic-mode-regexp-match-limit t)))
	       . c++-mode))

;;;;;;;; Auto Complete Settings
(when (require* 'auto-complete)
  (require* 'auto-complete-clang)
  ;; Setting my c-mode auto-complete source
  (defun ac-cpp-mode-setup ()
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


;;;;;;;; Flymake
(defun flymake-cpp-init ()
  (flymake-generic-init-makefile "g++" '("-Wall" "-Wextra" "-pedantic" "-fsyntax-only")))

(add-to-list 'flymake-allowed-file-name-masks
	     '(".+\\cpp$"
	       flymake-cpp-init
	       flymake-simple-cleanup
	       flymake-get-real-file-name))

;;;;;;;; Coding-Style Setting
(add-hook 'c++-mode-hook
	  '(lambda ()

	     ;; TODO: add comment here
	     (setq c-macro-shrink-window-flag t)
	     (setq c-macro-preprocessor "cpp")
	     (setq c-macro-cppflags " ")
	     (setq c-macro-prompt-flag t)

	     ;; Use linux-kernel style
	     (c-set-style "linux")

	     ;; Setting indentation lvel
	     (setq c-basic-offset 4)

	     ;; Make TAB equivilent to 4 spaces
	     (setq tab-width 4)

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

	     ))

;;;;;;;; Hooks
(add-hook 'c++-mode-hook
	  '(lambda ()

	     ;; Enable Auto Complete
	     (when (require* 'auto-complete)
	       (ac-cpp-mode-setup))

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
(add-hook 'c++-mode-hook
	  '(lambda ()
	     ;; Normal map
	     (define-key evil-normal-state-local-map (kbd "C-x C-o") 'ff-find-other-file)
	     ;; Insert map
	     (define-key evil-insert-state-local-map (kbd "M-i") 'cpp-mode:insert-inc-or-if) ; insert "#include <>" or "if () {...}"
	     (define-key evil-insert-state-local-map (kbd "M-d") 'cpp-mode:insert-do-while)  ; insert "do {...} while()"
	     (define-key evil-insert-state-local-map (kbd "M-m") 'cpp-mode:insert-main-function) ; insert "int main () {...}"
	     ))





;;;;;;;; make cedet integrated with c
(when (require 'cedet)
  (semantic-add-system-include "/usr/include" 'c++-mode)
  )


;;;; Other Settings
(setq cpp-mode:include-dirs		; Setting include directories
      '(
	"/usr/include"
	))


;;;; Functions

;; insert yasnippet
(defun cpp-mode:insert-inc-or-if ()
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
(define-skeleton cpp-mode:insert-include
  "generate include<>" ""
  > "#include <"
  (completing-read "Enter include file："
		   (mapcar #'(lambda (f) (list f ))
			   (apply 'append
				  (mapcar
				   #'(lambda (dir)
				       (directory-files dir))
				   cpp-mode:include-dirs
				   ))))
  ">")




(defun cpp-mode:insert-do-while ()
  "insert do{...} while()."
  (interactive)
  (insert "do")
  (yas/expand))

(defun cpp-mode:insert-main-function ()
  "insert main()."
  (interactive)
  (let* ((current (point))
	 (begin (line-beginning-position)))
    (if (equal current begin)
	(insert "main"))
    (yas/expand)))



(provide 'coldnew-lang-cpp)
;; coldnew-lang-cpp.el ends here.
