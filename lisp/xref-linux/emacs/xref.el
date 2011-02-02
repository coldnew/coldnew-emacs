;;; xref.el - (X)Emacs interface to Xrefactory (autoloaded functions)

;; Copyright (C) 1997-2007 Marian Vittek, Xref-Tech
 
;; This  file  is  part  of  Xrefactory  software;  it  implements  an
;; interface  between  xref  task   and  (X)Emacs  editors.   You  can
;; distribute it  under the  terms of the  GNU General  Public License
;; version 2 as published by the Free Software Foundation.  You should
;; have received a  copy of the GNU General  Public License along with
;; this program; if not, write  to the Free Software Foundation, Inc.,
;; 59  Temple Place  - Suite  330,  Boston, MA  02111-1307, USA.   The
;; content of  this file is  copyrighted by Xref-Tech. This  file does
;; not contain any code  written by independent developers.  Xref-Tech
;; reserves  all rights  to make  any  future changes  in this  file's
;; license conditions.

;; The  GNU  GPL  license  applies  only  to  the  files  xref.el  and
;; xrefactory.el.    Distribution  of   other  parts   of  Xrefactory,
;; especially xref executable  and its source code is  governed by the
;; Xrefactory License Agreement.

;; This  file   may  invoke   functions  which  were   implemented  by
;; independent developers.  All credit for those  functions belongs to
;; the   authors,  contributors  and   maintainers  of   the  packages
;; containing  those functions.  This  concerns mainly  the 'compile',
;; 'comint', 'vc' and 'browse-url' packages.  Xrefactory is not an IDE
;; but  we have  included the  "Emacs IDE"  submenu  into Xrefactory's
;; (X)Emacs interface  in order  to allow users  of those  packages to
;; profit from  Xrefactory's project  management.  Of course,  we also
;; acknowledge the developers and maintainers of GNU Emacs and XEmacs.

(provide 'xref)

(load "xrefprotocol")

(autoload 'xref-refactoring-documentation "xrefdoc" "Not documented" t)

(defvar xref-debug-mode nil)
(defvar xref-debug-preserve-tmp-files nil)

;;(toggle-debug-on-error)

(if (eq xref-platform 'windows)
	(progn
	  (defvar xref-slash ?\\)
	  (defvar xref-find-file-on-mouse-delimit "[^A-Za-z0-9_\\.~-]")
	  (defvar xref-path-separator ?\;)
	  (defvar xref-run-batch-file (format "%s/xref%s.bat" xref-tmp-dir xref-user-identification))
	  )

  ;; a linux/unix platform
  (defvar xref-slash ?/)
  (defvar xref-find-file-on-mouse-delimit "[^A-Za-z0-9_/.~-]")
  (defvar xref-path-separator ?\:)
  (defvar xref-run-batch-file (format "%s/xref%s.sh" xref-tmp-dir xref-user-identification))
)

(if (string-match "XEmacs" emacs-version)
	(defvar xref-running-under 'xemacs)
  (defvar xref-running-under 'emacs)
)

;; also check some debugging configuratios, in order to not release them
(if (or xref-debug-mode xref-debug-preserve-tmp-files)
	(progn
	  (message "WARNING: Debugging configuration is on!")
	  (beep 1)
	  (sit-for 2)
))

(defvar xref-directory-dep-prj-name "++ Automatic (directory dependent) ++")
(defvar xref-abandon-deletion "++ Cancel (no deletion) ++")
(defvar xref-c-suffixes '(".c" ".h" ".tc" ".th"))
(defvar xref-cpp-suffixes '(".cpp" ".h" ".C" ".cc" ".CC" ".tc" ".th"))
(defvar xref-java-suffixes '(".java"))

(defvar xref-run-this-option "runthis")
(defvar xref-run-option "run")

(random t)
(defvar xref-server-tmp-file-counter (random 1000))
(defvar xref-server-tmp-file-increment (1+ (random 10)))
(defvar xref-ide-last-compile-commands nil)
(defvar xref-ide-last-run-commands nil)


(defun xref-server-get-new-tmp-file-name ()
  (let ((res))
	(setq res (format "%s/xref%s%d.tmp" xref-tmp-dir xref-user-identification xref-server-tmp-file-counter))
	(setq xref-server-tmp-file-counter (+ xref-server-tmp-file-counter xref-server-tmp-file-increment))
	res
))

(defun xref-modal-buffer-name (title)
  (let ((res))
	(setq res (format " xref %s (modal)" title))
	res
))


;; process descriptions are cons (process . (pending-output . synced-flag))
(defvar xref-server-process nil)  
(defvar xref-refactorer-process nil)
(defvar xref-tags-process nil)

(defvar xref-server-tasks-ofile (xref-server-get-new-tmp-file-name))
(defvar xref-tags-tasks-ofile (format "%s/xref%s.log" xref-tmp-dir xref-user-identification))

(defvar xref-ppc-synchro-record (format "<%s>" PPC_SYNCHRO_RECORD))
(defvar xref-ppc-synchro-record-len (length xref-ppc-synchro-record))
(defvar xref-ppc-progress (format "<%s>" PPC_PROGRESS))
(defvar xref-ppc-progress-len (length xref-ppc-progress))

(defvar xref-run-buffer-no-stars "run")
(defvar xref-run-buffer (format "*%s*" xref-run-buffer-no-stars))
(defvar xref-compilation-buffer "*compilation*")
(defvar xref-log-view-buffer " *xref-log*")
(defvar xref-server-answer-buffer "*xref-server-answer*")
(defvar xref-completions-buffer "*completions*")
(defvar xref-tag-results-buffer "*xref-search-results*")
(defvar xref-class-tree-buffer " *class-tree*")
(defvar xref-project-list-buffer " *project-list*")
(defvar xref-vc-log-buffer "*VC-log*")
(defvar xref-cvs-shell-log-buffer "*cvs-shell-log*")
(defvar xref-extraction-buffer " *code-extraction*")

(defvar xref-selection-modal-buffer (xref-modal-buffer-name " *selection"))
(defvar xref-confirmation-modal-buffer (xref-modal-buffer-name " *confirmation"))
(defvar xref-error-modal-buffer (xref-modal-buffer-name " *error"))
(defvar xref-info-modal-buffer (xref-modal-buffer-name " *info"))

(defvar xref-info-buffer " *info*")
(defvar xref-browser-info-buffer " *browser-info*")
(defvar xref-symbol-resolution-buffer " *symbols/classes*")
(defvar xref-references-buffer " *references*")

(defvar xref-registration-url "http://www.xref-tech.com/xrefactory/license.html")

(defvar xref-completions-windows-counter 0)
(defvar xref-completions-dispatch-data nil)
(defvar xref-class-tree-dispatch-data nil)
(defvar xref-refactorer-dispatch-data nil)
(defvar xref-global-dispatch-data nil)
(defvar xref-active-project nil)

(defvar xref-refactoring-block "")

(defvar xref-refactoring-beginning-marker (make-marker))
(defvar xref-refactoring-beginning-offset 0)
(defvar xref-moving-refactoring-marker (make-marker))
(defvar xref-moving-refactoring-line 0)
(defvar xref-undo-marker (make-marker))
(defvar xref-extraction-marker (make-marker))
(defvar xref-extraction-marker2 (make-marker))
(defvar xref-completion-marker (make-marker))
(defvar xref-completion-pop-marker (make-marker))
(defvar xref-completion-id-before-point "")
(defvar xref-completion-id-after-point "")
(defvar xref-completion-auto-search-list nil)
;; this is used only in automated test suites
(defvar xref-renaming-default-name nil)
(defvar xref-full-auto-update-allowed nil)
(defvar xref-full-auto-update-perform nil)

(defvar xref-resolution-dialog-explication "\n")

(defvar xref-standard-help-message "Type ? for help.")

(defvar xref-backward-pass-identifier-regexp "\\(\\`\\|[^A-Za-z0-9_$]\\)")
(defvar xref-forward-pass-identifier-regexp "\\(\\'\\|[^A-Za-z0-9_$]\\)")


(defvar xref-c-keywords-regexp
"[^A-Za-z0-9_$]\\(auto\\|break\\|c\\(ase\\|har\\|on\\(st\\|tinue\\)\\)\\|d\\(efault\\|o\\(uble\\)?\\)\\|e\\(lse\\|num\\|xtern\\)\\|f\\(loat\\|or\\)\\|goto\\|i\\(f\\|nt\\)\\|long\\|re\\(gister\\|turn\\)\\|s\\(hort\\|i\\(gned\\|zeof\\)\\|t\\(atic\\|ruct\\)\\|witch\\)\\|typedef\\|un\\(ion\\|signed\\)\\|vo\\(id\\|latile\\)\\|while\\)[^A-Za-z0-9_$]"
)

(defvar xref-cpp-keywords-regexp
"[^A-Za-z0-9_$]\\(?:a\\(?:sm\\|uto\\)\\|b\\(?:ool\\|reak\\)\\|c\\(?:a\\(?:se\\|tch\\)\\|har\\|lass\\|on\\(?:st\\(?:_cast\\)?\\|tinue\\)\\)\\|d\\(?:e\\(?:fault\\|lete\\)\\|o\\(?:uble\\)?\\|ynamic_cast\\)\\|e\\(?:lse\\|num\\|x\\(?:plicit\\|tern\\)\\)\\|f\\(?:alse\\|loat\\|or\\|riend\\)\\|goto\\|i\\(?:f\\|n\\(?:line\\|t\\)\\)\\|long\\|mutable\\|n\\(?:amespace\\|ew\\)\\|operator\\|p\\(?:r\\(?:ivate\\|otected\\)\\|ublic\\)\\|re\\(?:gister\\|interpret_cast\\|turn\\)\\|s\\(?:hort\\|i\\(?:gned\\|zeof\\)\\|t\\(?:atic\\(?:_cast\\)?\\|ruct\\)\\|witch\\)\\|t\\(?:emplate\\|h\\(?:is\\|row\\)\\|r\\(?:ue\\|y\\)\\|ype\\(?:def\\|id\\|name\\)\\)\\|u\\(?:n\\(?:ion\\|signed\\)\\|sing\\)\\|v\\(?:irtual\\|o\\(?:id\\|latile\\)\\)\\|w\\(?:char_t\\|hile\\)\\)[^A-Za-z0-9_$]"
)

(defvar xref-java-keywords-regexp
"[^A-Za-z0-9_$]\\(?:abstract\\|b\\(?:oolean\\|reak\\|yte\\)\\|c\\(?:a\\(?:se\\|tch\\)\\|har\\|lass\\|on\\(?:st\\|tinue\\)\\)\\|d\\(?:efault\\|o\\(?:uble\\)?\\)\\|e\\(?:lse\\|xtends\\)\\|f\\(?:inal\\(?:ly\\)?\\|loat\\|or\\)\\|goto\\|i\\(?:f\\|mp\\(?:lements\\|ort\\)\\|n\\(?:stanceof\\|t\\(?:erface\\)?\\)\\)\\|long\\|n\\(?:ative\\|ew\\)\\|p\\(?:ackage\\|r\\(?:ivate\\|otected\\)\\|ublic\\)\\|return\\|s\\(?:hort\\|tatic\\|uper\\|witch\\|ynchronized\\)\\|t\\(?:h\\(?:is\\|rows?\\)\\|r\\(?:ansient\\|y\\)\\)\\|vo\\(?:id\\|latile\\)\\|while\\)[^A-Za-z0-9_$]"
)


;; somewhere they don't have the 'regexp-opt' function ...
;;
;;(defvar xref-c-keywords-regexp
;;  (eval-when-compile
;;	(concat "[^A-Za-z0-9_$]"
;;			(regexp-opt
;;			 '("auto" "extern" "register" "static" "typedef" "struct"
;;			   "union" "enum" "signed" "unsigned" "short" "long"
;;			   "int" "char" "float" "double" "void" "volatile" "const"
;;			   "break" "continue" "do" "else" "for" "if" "return"
;;			   "switch" "while" "goto" "case" "default" "sizeof"
;;			   ) t )
;;			"[^A-Za-z0-9_$]")
;;	))
;;(defvar xref-cpp-keywords-regexp
;;  (eval-when-compile
;;	(concat "[^A-Za-z0-9_$]"
;;			(regexp-opt
;;			 '(
;;			   "asm" "do" "inline" "short" "typeid"
;;			   "auto" "double" "int" "signed" "typename"
;;			   "bool" "dynamic_cast" "long" "sizeof" "union"
;;			   "break" "else" "mutable" "static" "unsigned"
;;			   "case" "enum" "namespace" "static_cast" "using"
;;			   "catch" "explicit" "new" "struct" "virtual"
;;			   "char" "extern" "operator" "switch" "void"
;;			   "class" "false" "private" "template" "volatile"
;;			   "const" "float" "protected" "this" "wchar_t"
;;			   "const_cast" "for" "public" "throw" "while"
;;			   "continue" "friend" "register" "true"
;;			   "default" "goto" "reinterpret_cast" "try"
;;			   "delete" "if" "return" "typedef"
;;			   ) t )
;;			"[^A-Za-z0-9_$]")
;;	))
;;(defvar xref-java-keywords-regexp
;;  (eval-when-compile
;;	(concat "[^A-Za-z0-9_$]"
;;			(regexp-opt
;;			 '("abstract" "boolean" "break" "byte" "case" "catch" 
;;			   "char" "class" "const" "continue"
;;			   "default" "do" "double" "else" "extends" "final"
;;			   "finally" "float" "for" "goto"
;;			   "if" "implements" "import" "instanceof" "int" "interface"
;;			   "long" "native" "new" "package"
;;			   "private" "protected" "public" "return" "short" "static"
;;			   "super" "switch" "synchronized" "this"
;;			   "throw" "throws" "transient" "try" "void" "volatile" "while"
;;			   ) t )
;;			"[^A-Za-z0-9_$]")
;;	))

(defun xref-keywords-regexp ()
  (let ((res))
  (if xref-highlight-java-keywords
	  (setq res xref-java-keywords-regexp)
	;;(setq res xref-c-keywords-regexp)
	(setq res xref-cpp-keywords-regexp)
	)
  res
))

(defvar xref-font-lock-compl-keywords
    (cons 
	 (cons (xref-keywords-regexp) '(xref-keyword-face))
	  '(("^\\([a-zA-Z0-9_ ]*\\):" xref-list-pilot-face)
		(": *( *[0-9 ]* *) *\\([a-zA-Z0-9_ ]*\\):" xref-list-classname-face)
		)
	  )
    "Default expressions to highlight in Xref completion list modes.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; refactorings codes, names and functions

(defvar xref-refactorings-assoc-list
  (list
   (list PPC_AVR_NO_REFACTORING "No refactoring" 'xref-no-refactoring nil)
   (list PPC_AVR_RENAME_SYMBOL "Rename Symbol" 'xref-rename-symbol nil)
   (list PPC_AVR_RENAME_CLASS "Rename Class" 'xref-rename-class nil)
   (list PPC_AVR_RENAME_PACKAGE "Rename Package" 'xref-rename-package nil)
   (list PPC_AVR_ADD_PARAMETER "Add Parameter" 'xref-add-parameter nil)
   (list PPC_AVR_DEL_PARAMETER "Delete Parameter" 'xref-del-parameter nil)
   (list PPC_AVR_MOVE_PARAMETER "Move Parameter" 'xref-move-parameter nil)
   (list PPC_AVR_EXTRACT_METHOD "Extract Method" 'xref-extract-method nil)
   (list PPC_AVR_EXTRACT_FUNCTION "Extract Function" 'xref-extract-function nil)
   (list PPC_AVR_EXTRACT_MACRO "Extract Macro" 'xref-extract-macro nil)
   (list PPC_AVR_MOVE_STATIC_FIELD "Move Static Field" 'xref-move-static-field nil)
   (list PPC_AVR_MOVE_STATIC_METHOD "Move Static Method" 'xref-move-static-method nil)
   (list PPC_AVR_MOVE_FIELD "Move Field" 'xref-move-field nil)
   (list PPC_AVR_PULL_UP_FIELD "Pull Up Field" 'xref-pull-up-field nil)
   (list PPC_AVR_PULL_UP_METHOD "Pull Up Method" 'xref-pull-up-method nil)
   (list PPC_AVR_PUSH_DOWN_FIELD "Push Down Field" 'xref-push-down-field nil)
   (list PPC_AVR_PUSH_DOWN_METHOD "Push Down Method" 'xref-push-down-method nil)
   (list PPC_AVR_MOVE_CLASS "Move Class" 'xref-move-class nil)
   (list PPC_AVR_MOVE_CLASS_TO_NEW_FILE "Move Class to New File" 'xref-move-class-to-new-file nil)
   (list PPC_AVR_MOVE_ALL_CLASSES_TO_NEW_FILE "Move file" 'xref-move-file nil)
   (list PPC_AVR_ENCAPSULATE_FIELD "Encapsulate Field" 'xref-encapsulate-field nil)
   (list PPC_AVR_SELF_ENCAPSULATE_FIELD "Self Encapsulate Field" 'xref-self-encapsulate-field nil)
   (list PPC_AVR_TURN_DYNAMIC_METHOD_TO_STATIC "Turn Virtual Method to Static" 'xref-turn-dynamic-method-to-static nil)
   (list PPC_AVR_TURN_STATIC_METHOD_TO_DYNAMIC "Turn Static Method to Virtual" 'xref-turn-static-method-to-dynamic nil)
   (list PPC_AVR_ADD_TO_IMPORT "Add import" 'xref-add-to-imports nil)
   (list PPC_AVR_REDUCE_NAMES "Reduce names" 'xref-reduce-names nil)
   (list PPC_AVR_EXPAND_NAMES "Expand names" 'xref-expand-names nil)
   (list PPC_AVR_ADD_ALL_POSSIBLE_IMPORTS "Add all used imports" 'xref-reduce-long-names-in-the-file nil)
   (list PPC_AVR_SET_MOVE_TARGET "Set Target for Next Moving Refactoring" 'xref-set-moving-target-position nil)
   (list PPC_AVR_UNDO "Undo Last Refactoring" 'xref-undo-last-refactoring nil)
))
   
   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; buffer locals
;;

;;
(defvar xref-this-buffer-type 'source-file) ;or  'symbol-list 'reference-list 'completion 'tag-search-results
(make-variable-buffer-local 'xref-this-buffer-type)
(defvar xref-this-buffer-dispatch-data nil)
(make-variable-buffer-local 'xref-this-buffer-dispatch-data)
(defvar xref-this-buffer-filter-level xref-default-symbols-filter)
(make-variable-buffer-local 'xref-this-buffer-filter-level)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; frame locals
;;

(if (eq xref-running-under 'emacs)
	(progn
	  (defvar xref-this-frame-dispatch-data nil)
	  (make-variable-frame-local 'xref-this-frame-dispatch-data)
	  ))

(defun xref-get-this-frame-dispatch-data ()
  (let ((res))
	(if (eq xref-running-under 'emacs)
		(setq res xref-this-frame-dispatch-data)
	  (setq res (frame-property (selected-frame) 'xref-this-frame-dispatch-data nil))
	  )
	res
))

(defun xref-set-this-frame-dispatch-data (dispatch-data)
  (if (eq xref-running-under 'emacs)
	  (setq xref-this-frame-dispatch-data dispatch-data)
	(set-frame-property (selected-frame) 
						'xref-this-frame-dispatch-data 
						dispatch-data)
	)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun xref-add-bindings-for-chars (keymap chars fun)
  (let ((i) (len))
	(setq i 0)
	(setq len (length chars))
	(while (< i len)
	  (define-key keymap (substring chars i (+ i 1)) fun)
	  (setq i (+ i 1))
	  )
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       Xrefactory local keymaps                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar xref-query-replace-map (make-sparse-keymap "Xref-query-replace"))
(define-key xref-query-replace-map "y" 'answer-yes)
(define-key xref-query-replace-map "Y" 'answer-yes)
(define-key xref-query-replace-map "n" 'answer-no)
(define-key xref-query-replace-map "N" 'answer-no)
(define-key xref-query-replace-map "\e" 'answer-no)
(define-key xref-query-replace-map "\C-]" 'answer-abort)
(define-key xref-query-replace-map "\C-g" 'answer-abort)
(define-key xref-query-replace-map "a" 'answer-all)
(define-key xref-query-replace-map "A" 'answer-all)
(define-key xref-query-replace-map "c" 'answer-confirmed)
(define-key xref-query-replace-map "C" 'answer-confirmed)

(defun xref-bind-default-button (map fun)
  (if (eq xref-running-under 'xemacs)
	  (progn
		(define-key map 'button2 'xref-undefined)
		(define-key map 'button2up fun)
		(if xref-bind-left-mouse-button
			(progn
			  (define-key map 'button1 'xref-undefined)
			  (define-key map 'button1up fun)
			  )))
	;; emacs
	(define-key map [mouse-2] fun)
	(if xref-bind-left-mouse-button
		(progn
		  (define-key map [mouse-1] fun)
		  )))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                         Completion local keymap

(defvar xref-completion-mode-map (make-keymap)
  "Keymap for xref buffer containing completions."
)
(xref-add-bindings-for-chars xref-completion-mode-map
							 " `[];',./-=\\~!@#%^&*()+|{}:\"<>?"
							 'xref-completion-auto-switch)
(define-key xref-completion-mode-map " " 'xref-interactive-completion-goto)
(define-key xref-completion-mode-map [(backspace)] 'xref-completion-auto-search-back)
(define-key xref-completion-mode-map "\b" 'xref-completion-auto-search-back)
(define-key xref-completion-mode-map [(delete)] 'xref-completion-auto-search-back)
(define-key xref-completion-mode-map [iso-lefttab] 'xref-completion-auto-search-back) ; Emacs
(define-key xref-completion-mode-map [iso-left-tab] 'xref-completion-auto-search-back) ; XEmacs
(define-key xref-completion-mode-map "\C-w" 'xref-completion-auto-search-w)
(define-key xref-completion-mode-map [tab] 'xref-completion-auto-search-s)
(define-key xref-completion-mode-map "\t" 'xref-completion-auto-search-s)
(define-key xref-completion-mode-map "\C-b" 'xref-interactive-completion-browse)
(define-key xref-completion-mode-map [(shift left)] 'xref-scroll-right)
(define-key xref-completion-mode-map [(shift right)] 'xref-scroll-left)
(define-key xref-completion-mode-map "\?" 'xref-interactive-completion-help)
(define-key xref-completion-mode-map xref-escape-key-sequence 'xref-interactive-completion-escape)
(define-key xref-completion-mode-map "\C-g" 'xref-interactive-completion-close)
(define-key xref-completion-mode-map "\C-q" 'xref-interactive-completion-escape)
;; can't bind, anything to Alt-xx because escape is bind to single key!
;;(define-key xref-completion-mode-map "\M-q" 'xref-interactive-completion-escape)
;;(define-key xref-completion-mode-map [(control return)] 'xref-interactive-completion-goto)
;;(define-key xref-completion-mode-map [(control space)] 'xref-interactive-completion-goto)
;;(define-key xref-completion-mode-map "\C- " 'xref-interactive-completion-goto)
(define-key xref-completion-mode-map "\C-m" 'xref-interactive-completion-select)
;; those two functions are bind to F5 and C-F5, so maybe this is useless
(define-key xref-completion-mode-map "\C-u" 'xref-interactive-completion-previous)
(define-key xref-completion-mode-map "\C-d" 'xref-interactive-completion-next)
(if (eq xref-running-under 'xemacs)
	(progn
	  ;; XEmacs
	  (define-key xref-completion-mode-map 'button3 'xref-popup-xemacs-completion-menu)
	  (define-key xref-completion-mode-map 'button3up 'xref-undefined)
	  (define-key xref-completion-mode-map [(control button2up)] 'xref-interactive-completion-mouse-browse)
	  (define-key xref-completion-mode-map [(control button2)] 'xref-undefined)
	  )
  ;; Emacs
  (define-key xref-completion-mode-map [mouse-3] 'xref-compl-3bmenu)
  (define-key xref-completion-mode-map [S-mouse-2] 'xref-interactive-completion-mouse-browse)
  )
(xref-bind-default-button xref-completion-mode-map 'xref-interactive-completion-mouse-select)
(xref-add-bindings-for-chars 
 xref-completion-mode-map
 "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_$1234567890"
 'xref-completion-auto-search)
(xref-add-bindings-to-keymap xref-completion-mode-map)
(define-key xref-completion-mode-map "q" 'xref-interactive-completion-q)


;;;;;;;;;;;;;;;;;;;;; mouse3 menu ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar xref-compl-3bmenu (make-sparse-keymap "Completions Menu"))
(fset 'xref-compl-3bmenu (symbol-value 'xref-compl-3bmenu))
(define-key xref-compl-3bmenu [xref-compl-3bclose] '("Close" . xref-interactive-completion-close))
(define-key xref-compl-3bmenu [xref-compl-3bexit] '("Close and Return" . xref-interactive-completion-escape))
(define-key xref-compl-3bmenu [xref-compl-3bselect] '("Complete" . xref-interactive-completion-mouse-select))
(define-key xref-compl-3bmenu [xref-compl-3bnext] '("Forward" . xref-interactive-completion-mouse-next))
(define-key xref-compl-3bmenu [xref-compl-3bprev] '("Backward" . xref-interactive-completion-mouse-previous))
(define-key xref-compl-3bmenu [xref-compl-3binspect] '("Inspect Definition" . xref-interactive-completion-mouse-goto))
(define-key xref-compl-3bmenu [xref-compl-3bjdoc] '("JavaDoc" . xref-interactive-completion-mouse-browse))

(defvar xref-xemacs-compl-3bmenu
  '("Completions Menu"
   ["JavaDoc"                xref-interactive-completion-browse t]
   ["Inspect Definition"     xref-interactive-completion-goto t]
   ["Complete"               xref-interactive-completion-select t]
   ["Backward"               xref-interactive-completion-previous t]
   ["Forward"                xref-interactive-completion-next t]
   ["Close and Return"       xref-interactive-completion-escape t]
   ["Close"                  xref-interactive-completion-close t]
	) "Xref button3 popup menu for completions" 
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                   Class (sub)tree result local keymap
;; This is keymap for window resulting from 'View class tree' function, not
;; for the class tree resulting from pushing actions.

(defvar xref-class-tree-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map xref-escape-key-sequence 'xref-interactive-class-tree-escape)
    (define-key map "q" 'xref-interactive-class-tree-escape)
    ;;(define-key map [left] 'xref-scroll-right)
    ;;(define-key map [right] 'xref-scroll-left)
    (define-key map [(shift left)] 'xref-scroll-right)
    (define-key map [(shift right)] 'xref-scroll-left)
    (define-key map [(control left)] 'xref-resize-left)
    (define-key map [(control right)] 'xref-resize-right)
    (define-key map "\C-m" 'xref-interactive-class-tree-inspect)
    (define-key map " " 'xref-interactive-class-tree-inspect)
    (define-key map "?" 'xref-interactive-class-tree-help)
	(xref-bind-default-button map 'xref-interactive-class-tree-mouse-inspect)
    map)
  "Keymap for xref class tree."
)
(if (eq xref-running-under 'xemacs)
	(progn
	  ;; XEmacs
	  (define-key xref-class-tree-mode-map 'button3 'xref-popup-xemacs-class-tree-menu)
	  )
  ;; Emacs
  (define-key xref-class-tree-mode-map [mouse-3] 'xref-class-tree-3bmenu)
  )
(xref-add-bindings-to-keymap xref-class-tree-mode-map)

;;;;;;;;;;;;;;;;;;;;; mouse3 menu ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar xref-xemacs-popup-class-tree-menu
  '("Class Tree Menu"
   ["Inspect Class"           xref-interactive-class-tree-inspect t]
   ["Close Window"            xref-interactive-class-tree-escape t]
	) "Xref button3 popup menu for class-trees" 
)

(defvar xref-class-tree-3bmenu (make-sparse-keymap "Class Tree Menu"))
(fset 'xref-class-tree-3bmenu (symbol-value 'xref-class-tree-3bmenu))
(define-key xref-class-tree-3bmenu [xref-ct-3bclose] '("Close Window" . xref-interactive-class-tree-escape))
(define-key xref-class-tree-3bmenu [xref-ct-3binspect] '("Inspect Class" . xref-interactive-class-tree-mouse-inspect))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      tag search results keymap

(defvar xref-tag-search-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\e" 'xref-interactive-tag-search-escape)
    (define-key map "q" 'xref-interactive-tag-search-escape)
    (define-key map "p" 'xref-interactive-tag-search-previous)
    (define-key map "n" 'xref-interactive-tag-search-next)
    ;;(define-key map [left] 'xref-scroll-right)
    ;;(define-key map [right] 'xref-scroll-left)
    (define-key map [(shift left)] 'xref-scroll-right)
    (define-key map [(shift right)] 'xref-scroll-left)
    (define-key map "\C-m" 'xref-interactive-tag-search-select)
    (define-key map " " 'xref-interactive-tag-search-inspect)
    (define-key map "?" 'xref-interactive-class-tag-search-help)
	(xref-bind-default-button map 'xref-interactive-tag-search-mouse-inspect)
    map)
  "Keymap for xref class tree."
)
(if (eq xref-running-under 'xemacs)
	(progn
	  ;; XEmacs
	  (define-key xref-tag-search-mode-map 'button3 'xref-popup-xemacs-tag-search-menu)
	  )
  ;; Emacs
  (define-key xref-tag-search-mode-map [mouse-3] 'xref-tag-search-3bmenu)
  )
(xref-add-bindings-to-keymap xref-tag-search-mode-map)


;;;;;;;;;;;;;;;;;;;;; mouse3 menu ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar xref-xemacs-popup-tag-search-menu
  '("Search Results Menu"
   ["Inspect Symbol"          xref-interactive-tag-search-inspect t]
   ["Select Symbol"           xref-interactive-tag-search-select t]
   ["Backward"                xref-interactive-tag-search-previous t]
   ["Forward"                 xref-interactive-tag-search-next t]
   ["Close Window"            xref-interactive-tag-search-escape t]
	) "Xref button3 popup menu for tag-searchs" 
)

(defun xref-popup-xemacs-tag-search-menu (event)
  (interactive "e")
  (select-window (event-window event))
  (goto-char (event-closest-point event))
  (beginning-of-line)
  (popup-menu xref-xemacs-popup-tag-search-menu)
)


(defvar xref-tag-search-3bmenu (make-sparse-keymap "Search Results Menu"))
(fset 'xref-tag-search-3bmenu (symbol-value 'xref-tag-search-3bmenu))
(define-key xref-tag-search-3bmenu [xref-ts-3bclose] '("Close Window" . xref-interactive-tag-search-escape))
(define-key xref-tag-search-3bmenu [xref-ts-3bnext] '("Forward" . xref-interactive-tag-search-mouse-next))
(define-key xref-tag-search-3bmenu [xref-ts-3bprev] '("Backward" . xref-interactive-tag-search-mouse-previous))
(define-key xref-tag-search-3bmenu [xref-ts-3bselect] '("Select Symbol" . xref-interactive-tag-search-mouse-select))
(define-key xref-tag-search-3bmenu [xref-ts-3binspect] '("Inspect Symbol" . xref-interactive-tag-search-mouse-inspect))


(defun xref-add-basic-modal-keybindings-no-escape (map)
  (define-key map [up] 'xref-modal-dialog-previous-line)
  (define-key map [down] 'xref-modal-dialog-next-line)
  (define-key map [kp-up] 'xref-modal-dialog-previous-line)
  (define-key map [kp-down] 'xref-modal-dialog-next-line)
  (define-key map "\C-p" 'xref-modal-dialog-previous-line)
  (define-key map "\C-n" 'xref-modal-dialog-next-line)
  (define-key map [newline] 'xref-modal-dialog-select)
  (define-key map [return] 'xref-modal-dialog-select)
  (define-key map "\C-m" 'xref-modal-dialog-select)

  (define-key map "1" 'xref-modal-dialog-select-1-line)
  (define-key map "2" 'xref-modal-dialog-select-2-line)
  (define-key map "3" 'xref-modal-dialog-select-3-line)
  (define-key map "4" 'xref-modal-dialog-select-4-line)
  (define-key map "5" 'xref-modal-dialog-select-5-line)
)

(defun xref-add-basic-modal-keybindings (map)
  (xref-add-basic-modal-keybindings-no-escape map)
  (define-key map xref-escape-key-sequence 'xref-modal-dialog-exit)
; No \C-g, it would escape from xref-interrupt-current-process too!
;  (define-key map "\C-g" 'xref-modal-dialog-exit)
  (define-key map "q" 'xref-modal-dialog-exit)
  (define-key map [f7] 'xref-modal-dialog-exit)
)

(defvar xref-modal-dialog-select-functions 
  '(
	(xref-modal-dialog-select)
	(xref-modal-dialog-select-1-line)									   
	(xref-modal-dialog-select-2-line)									   
	(xref-modal-dialog-select-3-line)									   
	(xref-modal-dialog-select-4-line)									   
	(xref-modal-dialog-select-5-line)
	(xref-modal-dialog-continue)
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; HELPS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar xref-context-help-buffer " *help*")
(defvar xref-help-map (make-sparse-keymap "Xref refactorings"))
(define-key xref-help-map [up] 'xref-modal-dialog-previous-line-no-sentinel)
(define-key xref-help-map [down] 'xref-modal-dialog-next-line-no-sentinel)
(define-key xref-help-map [kp-up] 'xref-modal-dialog-previous-line-no-sentinel)
(define-key xref-help-map [kp-down] 'xref-modal-dialog-next-line-no-sentinel)
(define-key xref-help-map xref-escape-key-sequence 'xref-modal-dialog-continue)
(define-key xref-help-map [prior] 'xref-modal-dialog-page-up)
(define-key xref-help-map [next] 'xref-modal-dialog-page-down)
(define-key xref-help-map [kp-prior] 'xref-modal-dialog-page-up)
(define-key xref-help-map [kp-next] 'xref-modal-dialog-page-down)
(define-key xref-help-map "\C-g" 'xref-modal-dialog-exit)
(define-key xref-help-map "q" 'xref-modal-dialog-continue)
(define-key xref-help-map " " 'xref-modal-dialog-continue)
(define-key xref-help-map "?" 'xref-modal-dialog-continue)
(define-key xref-help-map [newline] 'xref-modal-dialog-continue)
(define-key xref-help-map [return] 'xref-modal-dialog-continue)
(define-key xref-help-map "\C-m" 'xref-modal-dialog-continue)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;             BROWSER            ;;;;;;;;;;;;;;;;;;;;


(defvar xref-browser-dialog-key-map (make-sparse-keymap "Xref symbol-resolution"))
(define-key xref-browser-dialog-key-map [up] 'xref-browser-dialog-previous-line)
(define-key xref-browser-dialog-key-map [down] 'xref-browser-dialog-next-line)
(define-key xref-browser-dialog-key-map [kp-up] 'xref-browser-dialog-previous-line)
(define-key xref-browser-dialog-key-map [kp-down] 'xref-browser-dialog-next-line)
(define-key xref-browser-dialog-key-map " " 'xref-browser-dialog-select-one)
(define-key xref-browser-dialog-key-map "t" 'xref-browser-dialog-toggle)
(define-key xref-browser-dialog-key-map [insert] 'xref-browser-dialog-toggle)
(define-key xref-browser-dialog-key-map [kp-insert] 'xref-browser-dialog-toggle)
;; use t if nothing else works
(define-key xref-browser-dialog-key-map "a" 'xref-browser-dialog-select-all)
(define-key xref-browser-dialog-key-map "n" 'xref-browser-dialog-select-none)
(define-key xref-browser-dialog-key-map xref-escape-key-sequence 'xref-browser-dialog-exit)
(define-key xref-browser-dialog-key-map "\C-g" 'xref-cancel-with-error)  				; XEmacs only
(define-key xref-browser-dialog-key-map "q" 'xref-browser-dialog-break)
(define-key xref-browser-dialog-key-map [f3] 'xref-browser-dialog-previous-reference)
(define-key xref-browser-dialog-key-map [f4] 'xref-browser-dialog-next-reference)
(define-key xref-browser-dialog-key-map [f7] 'xref-browser-dialog-exit)
(define-key xref-browser-dialog-key-map "o" 'xref-browser-dialog-other-window)
(define-key xref-browser-dialog-key-map "0" 'xref-interactive-browser-dialog-set-filter)
(define-key xref-browser-dialog-key-map "1" 'xref-interactive-browser-dialog-set-filter)
(define-key xref-browser-dialog-key-map "2" 'xref-interactive-browser-dialog-set-filter)
(define-key xref-browser-dialog-key-map "3" 'xref-interactive-browser-dialog-set-filter)
(define-key xref-browser-dialog-key-map "4" 'xref-interactive-browser-dialog-set-filter)
(define-key xref-browser-dialog-key-map "5" 'xref-interactive-browser-dialog-set-filter)
(define-key xref-browser-dialog-key-map "6" 'xref-interactive-browser-dialog-set-filter)
(define-key xref-browser-dialog-key-map "7" 'xref-interactive-browser-dialog-set-filter)
(define-key xref-browser-dialog-key-map "8" 'xref-interactive-browser-dialog-set-filter)
(define-key xref-browser-dialog-key-map "9" 'xref-interactive-browser-dialog-set-filter)
(define-key xref-browser-dialog-key-map [newline] 'xref-modal-dialog-continue)
(define-key xref-browser-dialog-key-map [return] 'xref-modal-dialog-continue)
(define-key xref-browser-dialog-key-map "\C-m" 'xref-modal-dialog-continue)
(define-key xref-browser-dialog-key-map [(shift left)] 'xref-modal-dialog-shift-left)
(define-key xref-browser-dialog-key-map [(shift right)] 'xref-modal-dialog-shift-right)
(define-key xref-browser-dialog-key-map [(control left)] 'xref-resize-left)
(define-key xref-browser-dialog-key-map [(control right)] 'xref-resize-right)
(define-key xref-browser-dialog-key-map [(control up)] 'xref-resize-up)
(define-key xref-browser-dialog-key-map [(control down)] 'xref-resize-down)
(define-key xref-browser-dialog-key-map "?" 'xref-interactive-browser-dialog-help)
(xref-bind-default-button xref-browser-dialog-key-map 'xref-modal-dialog-mouse-button1)

(if (eq xref-running-under 'xemacs)
	(progn
	  ;; XEmacs
	  (if xref-bind-left-mouse-button
		  (progn
			(define-key xref-browser-dialog-key-map [(control button1)] 'xref-undefined)
			(define-key xref-browser-dialog-key-map [(control button1up)] 'xref-modal-dialog-mouse-button2)
			))
	  (define-key xref-browser-dialog-key-map [(control button2)] 'xref-undefined)
	  (define-key xref-browser-dialog-key-map [(control button2up)] 'xref-modal-dialog-mouse-button2)
	  (define-key xref-browser-dialog-key-map 'button3 'xref-popup-xemacs-browser-menu)
	  (define-key xref-browser-dialog-key-map 'button3up 'xref-undefined)
	  )
  ;; Emacs
  (if xref-bind-left-mouse-button
	  (progn
		(define-key xref-browser-dialog-key-map [(control mouse-1)] 'xref-modal-dialog-mouse-button2)
		))
  (define-key xref-browser-dialog-key-map [(control mouse-2)] 'xref-modal-dialog-mouse-button2)
  (define-key xref-browser-dialog-key-map [mouse-3] 'xref-browser-menu-3bmenu)
  )



(defvar xref-browser-dialog-refs-key-map (copy-keymap xref-browser-dialog-key-map))
(if (eq xref-running-under 'emacs)
	(define-key xref-browser-dialog-refs-key-map [mouse-3] 'xref-browser-refs-3bmenu)
  )


;;;;;;;;;;;;;;;;;;;;; mouse3 menu ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar xref-xemacs-popup-browser-refs-menu
  '("Search Results Menu"
   ["Inspect"                    xref-browser-dialog-select-one t]
   ["Filter 0"                   xref-interactive-browser-dialog-set-filter0 t]
   ["Filter 1"                   xref-interactive-browser-dialog-set-filter1 t]
   ["Filter 2"                   xref-interactive-browser-dialog-set-filter2 t]
   ["Filter 3"                   xref-interactive-browser-dialog-set-filter3 t]
   ["Close"               xref-browser-dialog-exit t]
   ["Continue"                   xref-modal-dialog-continue t]
	) "Xref button3 popup menu for browser-menus" 
)

(defvar xref-browser-refs-3bmenu (make-sparse-keymap "Search Results Menu"))
(fset 'xref-browser-refs-3bmenu (symbol-value 'xref-browser-refs-3bmenu))
(define-key xref-browser-refs-3bmenu [xref-bm-3b-cont] '("Continue" . xref-browser-3b-mouse-selected))
(define-key xref-browser-refs-3bmenu [xref-bm-3b-close] '("Close" . xref-browser-3b-mouse-selected))
(define-key xref-browser-refs-3bmenu [xref-bm-3b-filt3] '("Filter 3" . xref-browser-3b-mouse-selected))
(define-key xref-browser-refs-3bmenu [xref-bm-3b-filt2] '("Filter 2" . xref-browser-3b-mouse-selected))
(define-key xref-browser-refs-3bmenu [xref-bm-3b-filt1] '("Filter 1" . xref-browser-3b-mouse-selected))
(define-key xref-browser-refs-3bmenu [xref-bm-3b-filt0] '("Filter 0" . xref-browser-3b-mouse-selected))
(define-key xref-browser-refs-3bmenu [xref-bm-3b-sel-one] '("Inspect" . xref-browser-3b-mouse-selected))

(defvar xref-xemacs-popup-browser-menu-menu
  '("Search Results Menu"
   ["Select One and Inspect"     xref-browser-dialog-select-one t]
   ["Toggle Selection"           xref-browser-dialog-toggle t]
   ["Select All"                 xref-browser-dialog-select-all t]
   ["Unselect All"               xref-browser-dialog-select-none t]
   ["Filter 0"                   xref-interactive-browser-dialog-set-filter0 t]
   ["Filter 1"                   xref-interactive-browser-dialog-set-filter1 t]
   ["Filter 2"                   xref-interactive-browser-dialog-set-filter2 t]
   ["Close Window"               xref-browser-dialog-exit t]
   ["Continue"                   xref-modal-dialog-continue t]
	) "Xref button3 popup menu for browser-menus" 
)

(defvar xref-browser-menu-3bmenu (make-sparse-keymap "Search Results Menu"))
(fset 'xref-browser-menu-3bmenu (symbol-value 'xref-browser-menu-3bmenu))
(define-key xref-browser-menu-3bmenu [xref-bm-3b-cont] '("Continue" . xref-browser-3b-mouse-selected))
(define-key xref-browser-menu-3bmenu [xref-bm-3b-close] '("Close Window" . xref-browser-3b-mouse-selected))
(define-key xref-browser-menu-3bmenu [xref-bm-3b-filt2] '("Filter 2" . xref-browser-3b-mouse-selected))
(define-key xref-browser-menu-3bmenu [xref-bm-3b-filt1] '("Filter 1" . xref-browser-3b-mouse-selected))
(define-key xref-browser-menu-3bmenu [xref-bm-3b-filt0] '("Filter 0" . xref-browser-3b-mouse-selected))
(define-key xref-browser-menu-3bmenu [xref-bm-3b-sel-none] '("Unselect All" . xref-browser-3b-mouse-selected))
(define-key xref-browser-menu-3bmenu [xref-bm-3b-sel-all] '("Select All" . xref-browser-3b-mouse-selected))
(define-key xref-browser-menu-3bmenu [xref-bm-3b-toggle] '("Toggle Selection" . xref-browser-3b-mouse-selected))
(define-key xref-browser-menu-3bmenu [xref-bm-3b-sel-one] '("Select One and Inspect" . xref-browser-3b-mouse-selected))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          !!! this function is executed at LOADING time !!!

(defun xref-remove-old-temporary-files ()
"Check  if  there are  some  forgotten  temporary  files generated  by
Xrefactory. This  may happen if xref  task was killed, or  a macro was
interrupted by C-g. If there are such files, delete them.
"
  (interactive "")
  (let ((fl) (flist) (ff) (ffl) (modif) (ctime) (loop))
	(setq ctimecar (car (current-time)))
	(setq flist (directory-files 
				 xref-tmp-dir t 
				 (format "xref%s.*\\.tmp" xref-user-identification) 
				 t))
	(setq loop t)
	(setq fl flist)
	(while (and fl loop)
	  (setq ff (car fl))
	  (setq modifcar (car (nth 5 (file-attributes ff))))
	  (if (> (- ctimecar modifcar) 2)
		  (setq loop nil)
		)
	  (setq fl (cdr fl))
	  )
	(if (not loop)
		(progn
		  (if (y-or-n-p "[xref] there are some old temporary files, can I delete them ")
			  (progn
				(setq fl flist)
				(while fl
				  (setq ff (car fl))
				  (setq modifcar (car (nth 5 (file-attributes ff))))
				  (if (> (- ctimecar modifcar) 1)
					  (delete-file ff)
					)
				  (setq fl (cdr fl))
				  )
				))))
	(message "")
))

;; 
(xref-remove-old-temporary-files)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	
(defun xref-popup-xemacs-completion-menu (event)
  (interactive "e")
  (select-window (event-window event))
  (goto-char (event-closest-point event))
  (beginning-of-line)
  (popup-menu xref-xemacs-compl-3bmenu)
)

(defun xref-popup-xemacs-class-tree-menu (event)
  (interactive "e")
  (select-window (event-window event))
  (goto-char (event-closest-point event))
  (beginning-of-line)
  (popup-menu xref-xemacs-popup-class-tree-menu)
)


(defun xref-help-highlight-expr (expr)
  (xref-fontify-region (point-min) (point-max) expr)
;;  (let ((pos) (mb) (me))
;;	(goto-char (point-min))
;;	(while (search-forward-regexp expr nil t)
;;	  (progn
;;		(setq mb (match-beginning 0))
;;		(setq me (match-end 0))
;;		(put-text-property mb me 'face 'bold)
;;		))
;;	)
)

(defun xref-interactive-help (text search highlight)
  (let ((help))
	(save-window-excursion
	  (setq help (substitute-command-keys text))
	  (get-buffer-create xref-context-help-buffer)
	  (set-buffer xref-context-help-buffer)
	  (xref-erase-buffer)
 	  (insert help)
	  (goto-char (point-min))
	  (if highlight (xref-help-highlight-expr highlight))
	  (set-window-buffer (selected-window) (current-buffer))
	  (xref-appropriate-window-height t nil)
	  (xref-appropriate-window-width t nil)
	  (if search
		  (search-forward search nil t)
		)
	  (xref-modal-dialog-loop xref-help-map "")
	  (kill-buffer xref-context-help-buffer)
	  (set-window-buffer (selected-window) (current-buffer))
	  )
))

(defun xref-interactive-completion-help (event)
  (interactive "i")
  (xref-interactive-help
"Special hotkeys available:

\\[xref-interactive-completion-select] \t-- select completion
\\[xref-interactive-completion-goto] \t-- inspect symbol's definition or browse JavaDoc
\\[xref-interactive-completion-browse] \t-- browse JavaDoc or inspect symbol's definition 
\\[xref-interactive-completion-escape] \t-- close completions and return to completion position
\\[xref-interactive-completion-close] \t-- close completions
\\[xref-pop-and-return] \t-- previous completions
\\[xref-re-push] \t-- next completions
`[];',./-=\\~!@#%^&*()+|{}:\"<>\t-- leave completion window, insert character
A-Za-z0-9.\t-- incremental search, insert character
\\[xref-completion-auto-search-w] \t-- incremental search insert word
\\[xref-completion-auto-search-s] \t-- incremental search next match
\\[xref-completion-auto-search-back] \t-- incremental search return to last match
\\[xref-interactive-completion-help] \t-- toggle this help page
" nil nil)
)

(defun xref-interactive-class-tree-help (event)
  (interactive "i")
  (xref-interactive-help
"Special hotkeys available:

\\[xref-interactive-class-tree-inspect] \t-- inspect class
\\[xref-interactive-class-tree-escape] \t-- close window
\\[xref-resize-right] \t-- resize left
\\[xref-resize-left] \t-- resize right
\\[xref-scroll-left] \t-- scroll left
\\[xref-scroll-right] \t-- scroll right
\\[xref-interactive-class-tree-help] \t-- toggle this help page
" nil nil)
)

(defun xref-interactive-class-tag-search-help (event)
  (interactive "i")
  (xref-interactive-help
"Special hotkeys available:

\\[xref-interactive-tag-search-inspect] \t-- inspect symbol
\\[xref-interactive-tag-search-select] \t-- select symbol
\\[xref-interactive-tag-search-escape] \t-- close window
\\[xref-pop-and-return] \t-- previous search results
\\[xref-re-push] \t-- next search results
\\[xref-scroll-left] \t-- scroll left
\\[xref-scroll-right] \t-- scroll right
\\[xref-interactive-class-tag-search-help] \t-- toggle this help page
" nil nil)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; VERSION CONTROL SYSTEM SUPPORT ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; If you wish to define your own version control system, you will
;; need to implement functions for loading a file (and checkout);
;; making the buffer modifiable if readonly; saving all modified
;; buffers (and checkin); writing file under another name and deleting
;; a file. Then add code calling your version control functions to the
;; following function and finally set customizable variable
;; `xref-version-control' to contain your system identification.
;; For example to add version control system "my-vc", add the code:
;;         (
;;			(eq xref-version-control 'my-vc)
;;			(setq assoc-list (list (cons 'find-file 'xref-my-vc-find-file)
;;								   (cons 'make-buffer-writable 'xref-my-vc-make-buffer-writable)
;;								   (cons 'save-some-buffers 'xref-my-vc-save-some-buffers)
;;								   (cons 'write-file 'xref-my-vc-write-file)
;;								   (cons 'delete-file 'xref-my-vc-delete-file)
;;								   ))
;;			)
;; into the cond statement. Then implement all functions: xref-my-vc-find-file, 
;; xref-my-vc-make-buffer-writable, xref-my-vc-save-some-buffers, xref-my-vc-write-file 
;; and xref-my-vc-delete-file and finally set xref-version-control to 'my-vc.


(defun xref-version-control-operation (operation file)
  (let ((assoc-list) (association))
	(if (boundp 'xref-version-control)
		(progn
		  (cond
		   (
			(eq xref-version-control t)
			(require 'vc)
			(require 'vc-hooks)
			(setq assoc-list (list (cons 'find-file 'xref-vc-find-file)
								   (cons 'make-buffer-writable 'xref-vc-make-buffer-writable)
								   (cons 'save-some-buffers 'xref-vc-save-some-buffers)
								   (cons 'write-file 'xref-vc-write-file)
								   (cons 'delete-file 'xref-vc-delete-file)
								   ))
			)
		   (
			(eq xref-version-control nil)
			(setq assoc-list (list (cons 'find-file 'xref-novc-find-file)
								   (cons 'make-buffer-writable 'xref-novc-make-buffer-writable)
								   (cons 'save-some-buffers 'xref-novc-save-some-buffers)
								   (cons 'write-file 'xref-novc-write-file)
								   (cons 'delete-file 'xref-novc-delete-file)
								   ))
			)
		   (
			t
			(error "Unknown version system configured: %S" xref-version-control)
		   )))
	  ;; xref-version-control is unbound
	  (setq assoc-list (list (cons 'find-file 'xref-novc-find-file)
							 (cons 'make-buffer-writable 'xref-novc-make-buffer-writable)
							 (cons 'save-some-buffers 'xref-novc-save-some-buffers)
							 (cons 'write-file 'xref-novc-write-file)
							 (cons 'delete-file 'xref-novc-delete-file)
							 ))
	  )
	(setq association (assoc operation assoc-list))
	(if association
		(apply (cdr association) file nil)
	  (error "operation %s not found in association list %S" operation assoc-list)
	  )
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation of file operations with NO version control system

(defun xref-novc-find-file (file)
  (let ((buff))
	(setq buff (get-file-buffer file))
	(if (and buff (verify-visited-file-modtime buff))
		(switch-to-buffer buff)         ;; be conformant with find-file
	  (if xref-run-find-file-hooks
		  (find-file file)                  ;; full standard find-file
		(setq buff (create-file-buffer file))
		(switch-to-buffer buff)
		(insert-file-contents file t nil nil t)
		(after-find-file nil t t nil t)
		))
))

(defun xref-novc-make-buffer-writable (dummy)
  (setq buffer-read-only nil)
)

(defun xref-novc-save-some-buffers (checkin)
  (save-some-buffers t)
)

(defun xref-novc-write-file (file)
  (let ((dir))
	(setq dir (file-name-directory file))
	(if (not (file-exists-p dir))
		(make-directory dir t)
	  )
	(write-file file)
))

(defun xref-novc-delete-file (file)
  (if (file-exists-p file) (delete-file file))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation of file operations with Emacs version control system

(defun xref-vc-find-file (file)
  (xref-novc-find-file file)
)

(defun xref-vc-make-buffer-writable (dummy)
  (if (vc-backend (buffer-file-name))
	  (if (not (eq buffer-read-only nil))
		  (progn
			(vc-toggle-read-only)
			)
		)
	(xref-novc-make-buffer-writable dummy)
	)
)

(defun xref-vc-log-buffer-string ()
  (let ((res ""))
  (save-excursion
	(if (buffer-live-p (get-buffer xref-vc-log-buffer))
		(progn
		  (set-buffer xref-vc-log-buffer)
		  (setq res (buffer-string))
		  )))
  res
))

(defun xref-checkin-if-buffer-modified (b comment)
  (if (and (buffer-live-p b)
		   (buffer-modified-p b)
		   (not (buffer-base-buffer b))
		   (buffer-file-name b))
	  (progn
		(set-buffer b)
		(save-buffer)
		;; set window buffer to avoid vc-checkin to create new window
		(set-window-buffer (selected-window) b)
		(if (vc-backend (buffer-file-name))
			(progn
			  (vc-checkin (buffer-file-name) nil comment)
			  )
		  ))			
	)
)

(defun xref-vc-save-some-buffers (checkin)
  (let ((bb) (b) (comm))
	(if (and xref-version-control-checkin-on-auto-saved-buffers checkin)
		(progn
		  ;; wait 1 second, it seems that checkin within the same second
		  ;; as checkout cause troubles to Emacs vc.
		  (sleep-for 1)
		  (setq bb (buffer-list))
		  (setq comm (xref-vc-log-buffer-string))
		  (while bb
			(setq b (car bb))
			(xref-checkin-if-buffer-modified b comm)
			(setq bb (cdr bb))
			)
		  )
	  (xref-novc-save-some-buffers checkin)
	  )
))

(defun xref-vc-write-file (file)
  (let ((dir) (dl) (cb) (lb))
	(if (eq (vc-backend (buffer-file-name)) 'CVS)
		(progn
		  ;; CVS special care
		  (setq lb (get-buffer-create xref-cvs-shell-log-buffer))
		  (setq dir (directory-file-name (file-name-directory file)))
		  (setq dl nil)
		  (while (and dir (or (not (file-exists-p dir))
							  (not (file-exists-p (concat dir "/CVS")))))
			(setq dl (cons dir dl))
			(setq dir (directory-file-name (file-name-directory dir)))
			)
		  (while dl
			(setq dir (car dl))
			(if (not (file-exists-p dir)) (make-directory dir))
			(save-excursion (set-buffer lb) (goto-char (point-max)))
			(call-process "cvs" nil lb nil "add" dir)
			(setq dl (cdr dl))
			)
		  (xref-novc-write-file file)
		  (save-excursion (set-buffer lb) (goto-char (point-max)))
		  (call-process "cvs" nil lb nil "add" file)
		  ;; to fix vc bug, clear vc properties
		  (vc-file-clearprops file)
		  (display-buffer lb)
		  )
	  ;; Non CVS general
	  (if (vc-backend (buffer-file-name))
		  (progn
			(setq dir (file-name-directory file))
			(if (not (file-exists-p dir))
				(make-directory dir t)
			  )
			(set-buffer-modified-p t)
			(xref-checkin-if-buffer-modified (current-buffer) (xref-vc-log-buffer-string))
			;; if exists, remove it first (no prompt, user has been prompted before)
			(if (file-exists-p file) (xref-vc-delete-file file))
			(vc-rename-file (buffer-file-name) file)
			)
		(xref-novc-write-file file)
		))
))

(defun xref-vc-delete-file (file)
  (let ((lb))
  (if (eq (vc-backend file) 'CVS)
	  (progn
		;; TODO, better
		(xref-novc-delete-file file)
		(setq lb (get-buffer-create xref-cvs-shell-log-buffer))
		(save-excursion (set-buffer lb) (goto-char (point-max)))
		(call-process "cvs" nil lb nil "remove" file)
		(display-buffer lb)
		)
	(xref-novc-delete-file file)
	)
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions manipulating source files. Those functions are here for
;; backward compatibility only. If possible, do not use or modify them
;; anymore.  They will be removed in some future version of
;; Xrefactory.

(defun xref-find-file (file-name)
  "Xrefactory version of 'find-file'.

With no version control this function simply calls (find-file
FILE-NAME).  Under version control it may checkout the file from
version control.
"
  (xref-version-control-operation 'find-file file-name)
)

(defun xref-make-buffer-writable ()
  "Set `buffer-read-only' to nil.

With no version control this function simply calls (setq
buffer-read-only nil).  Under version control it may checkout the file
from version control.
"
  (xref-version-control-operation 'make-buffer-writable nil)
)

(defun xref-write-file (name confirm)
  "Xrefactory version of 'write-file'.

With no version control this function simply calls (write-file NAME).
Under version control it may add the new file to the version control
system.
"
  (xref-version-control-operation 'write-file name)
)

(defun xref-delete-file (name)
  "Xrefactory version of 'delete-file'.

With no version control this function simply calls (delete-file NAME).
Under version control it may remove the file from the version control
system.
"
  (xref-version-control-operation 'delete-file name)
)

(defun xref-save-some-buffers (flag)
  "Xrefactory version of 'save-some-buffers'.

Without version control this function simply calls (save-some-buffers
t).  If `xref-version-control-checkin-on-auto-saved-buffers' option is
not nil and FLAG is not nil, then under version control it may checkin
all modified files into the version control system.
"
  (xref-version-control-operation 'save-some-buffers flag)  
)

(defun xref-save-buffer ()
  (xref-write-file (buffer-file-name) nil)
)

;; actually this function is not used anymore by Xrefactory 1.6
(defun xref-move-directory (old-name new-name)
  ;; used only for Java rename package
  (if (eq xref-platform 'windows)
	  (progn
		;; MS-Windows
		(shell-command (format "xcopy \"%s\" \"%s\" /E /Y /I /C /Q" old-name new-name))
		(if (yes-or-no-p "Files copied, delete original folder? ")
			(progn
			  (if (string-match "-nt" system-configuration)
				  (shell-command (format "rd /S /Q \"%s\"" old-name))
				(shell-command (format "deltree /Y \"%s\"" old-name))
				)
			  )))
	;; unix
	(shell-command (format "mv \"%s\" \"%s\"" old-name new-name))
	)
)


(defun xref-rename-class-and-package-cvs-warning ()
  (if (eq xref-version-control t)
	  (progn
		(require 'vc-hooks)
		(if (eq (vc-backend (buffer-file-name)) 'CVS)
			(if (not (xref-yes-or-no-window 
"[Warning]  Renaming of  classes  and packages  under  CVS requires  a
successful  commit   after  the  renaming.   Otherwise,   it  will  be
impossible  to undo  changes made  in  the file  system.  In  general,
commits can cause troubles and it may be better to perform refactoring
without  version  control (doing  CVS  operations  manually after  the
renaming).

Do you really wish to continue this refactoring under CVS ?"
				 t nil))
				(error "Refactoring canceled.")
				))))
)

(defun xref-rcs-undo-warning (message)
  (let ((res))
	(setq res nil)
	(if (eq xref-version-control t)
		(progn
		  (require 'vc-hooks)
		  (if (eq (vc-backend (buffer-file-name)) 'RCS)
			  (progn
				(setq res t)
				(if (not (xref-yes-or-no-window (format
"[Warning] Undoing under RCS  will probably not work correctly because
check-outs clear  undo informations.  You should use  RCS snapshots to
memorize and undo changes with this version control system.

Do you really wish to undo %s?" message)
				 t nil))
					(error "Undo canceled.")
				)))))
	res
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun xref-global-options ()
  (interactive "")
  (xref-soft-select-dispach-data-caller-window xref-this-buffer-dispatch-data)
  (customize-group 'xrefactory)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MISC ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; xemacs with mule contains in standard distribution a bug
;; making elt and aref to take time proportional to string
;; size, so convert xref answer to list of chars.
(defvar xref-xemacs-mule-problem (and xref-perform-xemacs-mule-fix
									  (eq xref-running-under 'xemacs)
									  (fboundp 'init-mule)
									  (not xref-debug-mode)
									  ))


(defun xref-sit-for-no-redisplay (delay)
  (if (eq xref-running-under 'xemacs)
	  (sit-for delay t)
	(sit-for delay nil t)
	)
)

(defun xref-html-browsing-options ()
  (let ((res))
	(setq res "")
	(if xref-browse-url-directly
		(setq res " -urldirect")
	  (if xref-browse-url-manual-redirect
		  (setq res " -urlmanualredirect")
		))
	res
))

(defun xref-cut-too-long-questions (qq offset space)
  (let ((ww) (ll) (res))
	(setq res qq)
	(setq ww (- (frame-width) space))
	(setq ll (length qq))
	(if (> (+ ll 2) ww)
		(setq res (concat
				   (substring qq 0 offset)
				   "..."
				   (substring qq (+ (- ll ww ) offset 2))))
	  )
	res
))

(defun xref-browse-url (url)
  (let ((cc) (ff) (sw) (confirm))
	(setq confirm 'answer-yes)
	(if xref-ask-before-browse-javadoc
		(progn
		  (setq confirm (xref-get-single-yes-no-event t (xref-cut-too-long-questions (format "browse URL: %s " url) 11 10)))
		  (if (eq confirm 'answer-all)
			  (progn
				(setq xref-ask-before-browse-javadoc nil)
				(setq confirm 'answer-yes)
				))
		  (message "")
		  ))
	(if (eq confirm 'answer-yes)
		(progn
		  (setq ff (selected-frame))
		  (setq sw (selected-window))
		  (xref-soft-select-dispach-data-caller-window xref-this-buffer-dispatch-data)
		  (browse-url url)
		  ;; try to get focus as soon as possible
		  (if (> xref-browse-url-focus-delay 0) 
			  (progn
				;; first sleep and discard windows generated events
				(sleep-for .05)
				(discard-input)
				(xref-sit-for-no-redisplay xref-browse-url-focus-delay)
				(select-window sw)
				(raise-frame ff)
				))
		  ))
))

(defun xref-upcase-first-letter (name)
  (let ((res))
	(setq res name)
	(if (equal (length res) 1)
		(setq res (downcase res))
	  (if (> (length res) 1)
		  (setq res (concat (upcase (substring res 0 1)) (substring res 1)))
		))
	res
))

(defun xref-downcase-first-letter (name)
  (let ((res))
	(setq res name)
	(if (equal (length res) 1)
		(setq res (downcase res))
	  (if (> (length res) 1)
		  (setq res (concat (downcase (substring res 0 1)) (substring res 1)))
		))
	res
))

(defun xref-param-from-class-name (class)
  (let ((res) (p) (i))
	(setq i (- (length class) 1))
	(setq p 0)
	(while (and (>= i 0) (not (eq (elt class i) ?\.)))
	  (setq i (- i 1))
	  )
	(setq res (xref-downcase-first-letter (substring class (+ i 1))))
))

(defun xref-get-single-yes-no-event (cursor-in-echo prompt) 
  (let ((key) (res nil) (ce))
	(if (y-or-n-p prompt)
		(setq res 'answer-yes)
	  (setq res 'answer-no)
	  )
;;	(setq ce cursor-in-echo-area)
;;	(if cursor-in-echo
;;		(setq cursor-in-echo-area t)
;;	  (setq cursor-in-echo-area nil)
;;	  )
;;	(while (eq res nil)
;;	  (setq key (xref-read-key-sequence nil))
;;;;	  (setq key (read-key-sequence-vector "")
;;	  (setq res (lookup-key xref-query-replace-map key))
;;	  (if (eq res nil)
;;		  (message "Invalid key, please answer [y/n].")
;;		)
;;	  )
;;	(setq cursor-in-echo-area ce)
;;	(if (eq res 'answer-abort)
;;		(error "quit")
;;	  )
	res
))

(defun xref-current-column ()
  (let ((poin) (res))
	(setq poin (point))
	(beginning-of-line)
	(setq res (- poin (point)))
	(goto-char poin)
	res
))

(defun xref-is-this-buffer-created-by-xrefactory (name)
  (let ((res))
	(setq res nil)
	(if (or
		 (equal name xref-log-view-buffer)
		 (equal name xref-server-answer-buffer)
		 (equal name xref-completions-buffer)
		 (equal name xref-tag-results-buffer)
		 (equal name xref-browser-info-buffer)
		 (equal name xref-project-list-buffer)
		 (equal name xref-class-tree-buffer)
		 (equal name xref-info-buffer)
		 (equal name xref-run-buffer)
		 (equal name xref-compilation-buffer)
		 (equal name xref-cvs-shell-log-buffer)
		 (equal name xref-info-modal-buffer)
		 (equal name xref-error-modal-buffer)
		 (equal name xref-confirmation-modal-buffer)
		 (equal name xref-selection-modal-buffer)
		 (equal name xref-extraction-buffer)
		 (xref-string-has-prefix name xref-symbol-resolution-buffer nil)
		 (xref-string-has-prefix name xref-references-buffer nil)
		 ;; delete also help buffers
		 (xref-string-has-prefix name "*Help" nil)
		 )
		(setq res t)
	  )
;;(message "checking %S --> %S" name res)
	res
))

(defun xref-delete-window ()
"Remove an (Xrefactory) window.

Remove a window. This function is like `delete-window' (C-x 0), but it
tries to delete Xrefactory windows first.
"
  (interactive "")
  (let ((osw) (sw) (i) (loop) (deleted))
	(setq loop t)
	(setq deleted nil)
	(setq i 0)
	(setq osw (selected-window))
	(setq sw osw)
	;; dangerous loop, securize it
	(while loop
	  (if (xref-is-this-buffer-created-by-xrefactory (buffer-name))
		  (progn
			(if (equal sw (selected-window))
				(progn
				  (bury-buffer)
				  (delete-window)
				  (other-window -1)
				  )
			  (bury-buffer)
			  (delete-window)
			  (select-window sw)
			  )
			(setq loop nil) ; (not (equal sw (selected-window))))
			(setq deleted t)
			)
		(other-window 1)
		(setq loop (not (equal sw (selected-window))))
		)
	  (setq i (+ i 1))
	  (if (and loop (> i 50)) 
		  (progn
			setq loop nil
			(message "[PROBLEM] A WINDOW DELETING LOOP?")
			))
	  )
	(if (and (not deleted) (not (one-window-p)))
		(progn
		  (other-window -1)
		  (delete-window)
		  ))
))

(defun xref-exact-string-match (regexp str)
  (let ((res) (si) (ei) (rr))
	(setq rr (string-match (concat ">>>>>" regexp "<<<<<")
						   (concat ">>>>>" str    "<<<<<")))
	(setq res (eq rr 0))
	res
))

(defun xref-write-tmp-buff (tmpFile from-p to-p coding-system)
  (if (eq xref-running-under 'xemacs)
	  (write-region from-p to-p tmpFile nil 'no-message nil coding-system)
	(write-region from-p to-p tmpFile nil 'no-message)
	)
)

(defun xref-get-identifier-after (poin)
  (let ((opoin) (res))
	(setq opoin (point))
	(goto-char poin)
	(search-forward-regexp xref-forward-pass-identifier-regexp)
	(if (not (eq (point) (point-min))) (backward-char))
	(setq res (buffer-substring poin (point)))
	(goto-char opoin)
	res
))

(defun xref-is-letter (cc)
  (let ((res))
	(setq res (or (and (>= cc ?a) (<= cc ?z))
				  (and (>= cc ?A) (<= cc ?Z))
				  (eq cc ?_) (eq cc ?\$)))
	res
))

(defun xref-is-number (cc)
  (let ((res))
	(setq res (or (and (>= cc ?0) (<= cc ?9))))
	res
))

(defun xref-get-identifier-on-point ()
  (let ((sym) (poin) (cc))
	(setq poin (point))
	(search-backward-regexp xref-backward-pass-identifier-regexp)
	(if (not (xref-is-letter (char-after (point)))) (forward-char 1))
	(setq sym (xref-get-identifier-after (point)))
	(goto-char poin)
	sym
))

(defun xref-get-identifier-before-point ()
  (let ((sym) (poin))
	(setq poin (point))
	(search-backward-regexp xref-backward-pass-identifier-regexp)
	(forward-char 1)
	(setq sym (buffer-substring (point) poin))
	(goto-char poin)
	sym
))

(defun xref-delete-pending-ident-after-completion ()
  (let ((p (point)))
	(search-forward-regexp xref-forward-pass-identifier-regexp)
	(if (not (eq (point) (+ (buffer-size) 1)))
		(progn
		  (backward-char)
;; finally this does not seem very natural
;		  (if (and xref-completion-inserts-parenthesis (eq (char-after (point)) ?\())
;			  (progn
;				(forward-char 1)
;				(if (eq (char-after (point)) ?\))
;					(forward-char 1)
;				  )))
		  ))
	(if (eq p (point))
		()
	  (delete-char (- p (point)) t)
	  (if xref-completion-delete-pending-identifier
		  (message "** Pending identifier killed (C-y restores it) **")
		))
))

(defun xref-buffer-has-one-of-suffixes (bname suffixes)
  (let ((suf) (sl) (res))
	(setq suf (concat "." (xref-file-name-extension bname)))
	(setq sl suffixes)
	(while (and sl (not (equal suf (car sl))))
	  (setq sl (cdr sl))
	  )
	(if (not sl)
		(setq res nil)
	  (setq res t)
	  )
	res
))

(defun xref-string-has-prefix (str prefix case-unsens)
  (let ((res) (len) (pf))
	(setq res nil)
	(setq len (length prefix))
	(if (>= (length str) len)
		(progn
		  (setq pf (substring str 0 len))
		  (if (or (equal pf prefix)
				  (and case-unsens  (equal (downcase pf)
										   (downcase prefix))))
		(setq res t)
	  )))
	res
))

(defun xref-file-directory-name (fname)
  (let ((spos) (res) (len))
	(setq res nil)
	(setq len (length fname))
	(if (eq len 0)
		(setq res nil)
	  (setq spos (- len 1))
	  (while (and (> spos 0) 
				  (not (eq (elt fname spos) ?/))
				  (not (eq (elt fname spos) ?\\)))
		(setq spos (- spos 1))
		)
	  (if (> spos 0)
		  (setq res (substring fname 0 spos))
		(setq res nil)
		)
	  (if (and (eq xref-platform 'windows)
			   (eq spos 3)
			   (eq (elt fname (- spos 1)) ?\:))
		  (setq res nil)
		))
	res
))

(defun xref-file-name-extension (fname)
  (let ((spos) (res) (len))
	(setq res nil)
	(setq len (length fname))
	(if (eq len 0)
		(setq res nil)
	  (setq spos (- len 1))
	  (while (and (> spos 0) 
				  (not (eq (elt fname spos) ?.)))
		(setq spos (- spos 1))
		)
	  (if (> spos 0)
		  (setq res (substring fname (+ spos 1) len))
		(setq res fname)
		))
	res
))

(defun xref-file-last-name (fname)
  (let ((spos) (res) (len))
	(setq res nil)
	(setq len (length fname))
	(if (eq len 0)
		(setq res nil)
	  (setq spos (- len 1))
	  (while (and (> spos 0) 
				  (not (eq (elt fname spos) ?/))
				  (not (eq (elt fname spos) ?\\)))
		(setq spos (- spos 1))
		)
	  (if (> spos 0)
		  (setq res (substring fname (+ spos 1) len))
		(setq res fname)
		))
	res
))

(defun xref-file-name-without-suffix (fname)
  (let ((spos) (res) (len))
	(setq res nil)
	(setq len (length fname))
	(if (eq len 0)
		(setq res nil)
	  (setq spos (- len 1))
	  (while (and (> spos 0) 
				  (not (eq (elt fname spos) ?.)))
		(setq spos (- spos 1))
		)
	  (if (> spos 0)
		  (setq res (substring fname 0 spos))
		(setq res fname)
		))
	res
))

(defun xref-backslashify-name (fname) 
  (let ((spos) (len) (res))
	(if (or (eq xref-platform 'unix) (eq (length fname) 0))
		(setq res fname)
	  (setq res (substring fname 0 nil))
	  (setq len (length res))
	  (setq spos 0)
	  (while (< spos len) 
		(if (or (eq (elt res spos) ?/) (eq (elt res spos) ?\\))
			(store-substring res spos xref-slash)
		  )
		(setq spos (+ spos 1))
		)
	  )
	res
))

(defun xref-optionify-string (ss qt)
  (let ((res))
	(if (string-match " " ss)
		(setq res (format "%s%s%s" qt ss qt))
	  (setq res ss)
	  )
	res
))

(defun xref-cut-string-prefix (ss prefix case-unsens)
  (let ((res) (len))
	(setq res ss)
	(setq len (length prefix))
	(if (>= (length ss) len)
		(if (or (equal (substring ss 0 len) prefix)
				(and case-unsens (equal (downcase (substring ss 0 len)) 
										(downcase prefix))))
			(setq res (substring ss len))
		  )
	  )
	res
))


(defun xref-cut-string-suffix (ss suffix case-unsens)
  (let ((res) (len) (sslen))
	(setq res ss)
	(setq len (length suffix))
	(setq sslen (length ss))
	(if (>= sslen len)
		(if (or (equal (substring ss (- sslen len)) suffix)
				(and case-unsens (equal (downcase (substring ss (- sslen len))) 
										(downcase suffix))))
			(setq res (substring ss 0 (- sslen len)))
		  )
	  )
	res
))


(defun xref-soft-delete-window (buffername)
  (let ((displayed))
	(setq displayed (get-buffer-window buffername nil))
    (if displayed (delete-window displayed))
))

(defun xref-switch-to-marker (marker)
  (switch-to-buffer (marker-buffer marker))
  (goto-char (marker-position marker))
)

(defun xref-set-to-marker (marker)
  (set-buffer (marker-buffer marker))
  (goto-char (marker-position marker))
)

(defun xref-delete-window-in-any-frame (buffername setwidth killbuffer)
  (let ((displayed (get-buffer-window buffername t)))
    (if displayed
        (progn
		  (if setwidth (set setwidth (window-width displayed)))
          (delete-window displayed)
		  (if killbuffer (kill-buffer buffername))
        )
    )
  )
)


(defun xref-erase-buffer ()
  "Secure buffer erase. Do not erase anything from .c .java .y"
  (let ((bname) (suf))
	(setq bname (buffer-file-name nil))
	(if bname
		(setq suf (xref-file-name-extension bname))
	  (setq suf "")
	  )
	(if (or (equal suf "java") (equal suf "c") (equal suf "y"))
		(progn
		  (error "[xref] internal error: Attempt to erase '%s'!" bname)
		  (if (yes-or-no-p (format "![warning] Attempt to erase '%s'! Really erase? " (xref-file-last-name bname)))
			  (erase-buffer)
			)
		  )
	  (erase-buffer)
	  )
))

(defun xref-use-local-map (map)
  (if (xref-is-this-buffer-created-by-xrefactory (buffer-name))
	  (use-local-map map)
	(error "Attempt to redefine local keymap for non-Xrefactory buffer %s" (buffer-name))
	)
)


(defun xref-char-search (pos step char-code)
  (let ((res pos))
	(while (and (>= res 1) (< res (point-max)) 
				(not (eq (char-after res) char-code)))
	  (setq res (+ res step))
	  )
	res
))

(defun xref-char-count (char str)
  (let ((res) (i) (len))
	(setq res 0)
	(setq i 0)
	(setq len (length str))
	(while (< i len)
	  (if (eq (elt str i) char)
		  (setq res (1+ res))
		)
	  (setq i (1+ i))
	  )
	res
))

(defun xref-references-counts (drefn refn)
  (let ((res) (dr) (r))
	(if (equal drefn 0) 
		(setq dr "  -")
	  (setq dr (format "%3d" drefn))
	  )
	(if (equal refn 0) 
		(setq r "-  ")
	  (setq r (format "%-3d" refn))
	  )
	(setq res (format "%s/%s" dr r))
	res
))

(defun xref-show-file-line-in-caller-window (file line)
  (let ((sw))
	(setq sw (selected-window))
	(if (not (xref-soft-select-dispach-data-caller-window xref-this-buffer-dispatch-data))
		(other-window -1)
	  )
	(find-file file)
	(if (not (equal line 0))
		(goto-line line)
	  )
	(sit-for 1)
	(select-window sw)
))

(defun xref-find-file-on-point ()
  (let ((b) (e) (p) (file) (line 0) (ne) (c1) (c2))
	(setq p (point))
	(search-backward-regexp xref-find-file-on-mouse-delimit (point-min) 0)
	(forward-char 1)
	(setq b (point))
	(if (and (eq xref-platform 'windows) (>= (point) 3))
		(if (xref-exact-string-match "[^A-Za-z][A-Za-z]:" (buffer-substring (- (point) 3) (point)))
			;; add windows drive letter to file
			(setq b (- (point) 2))
		  ))
	(search-forward-regexp xref-find-file-on-mouse-delimit (point-max) 0)
	(setq e (point))
	(if (<= (- e 1) b)
		(error "No file name on mouse")
	  )
	(setq file (buffer-substring b (- e 1)))
	(if (or (eq (char-after (- e 1)) ?:)
			(eq (char-after (- e 1)) ?#)
			)
		(progn
		  ;; probably a line number
		  (forward-char 1)
		  (search-forward-regexp "[^0-9]" (point-max) 0)
		  (setq ne (point))
		  (setq line (string-to-int (buffer-substring e (- ne 1))))
		  )
	  )
	(goto-char p)
	(if (not (or (equal (substring file 0 1) "/") 
				 (and (> (length file) 3) (equal (substring file 1 3) ":\\"))
				 (equal (substring file 0 1) "\\")))
		(progn
		  ;; a relative path, concatenate with working dir
		  (setq file (concat default-directory file))
		  ))
	(if (not (file-attributes file))
	  (error "Can't open %s" file)
	)
	(xref-show-file-line-in-caller-window file line)
	)
  t
)

(defun xref-find-file-on-mouse (event)
  (interactive "e")
  (let ((b) (e) (p) (file) (line 0) (ne) (c1) (c2))
	(mouse-set-point event)
	(xref-find-file-on-point)
))

(defun xref-scroll-left ()
  (interactive "")
  (scroll-left 1)
  (if (not (eq (char-after (point)) ?\n))
	  (forward-char 1)
	)
)

(defun xref-scroll-right ()
  (interactive "")
  (scroll-right 1)
  (if (and (> (point) 1) (not (eq (char-after (- (point) 1)) ?\n)))
	  (backward-char 1)
	)
)

(defun xref-set-window-width (width)
  (let ((sw) (rightmost-win) (enlarge) (res) (abort))
	(setq sw (selected-window))
	(setq res t)
	(setq enlarge (- width (window-width)))
	(if (>= width window-min-width)
		(progn
		  (setq rightmost-win nil)
		  (if (eq xref-running-under 'emacs)
			  (setq rightmost-win (>= (elt (window-edges (selected-window)) 2) (frame-width)))
			;; xemacs
			(setq rightmost-win (>= (+ (elt (xref-window-edges) 2) 20) (frame-pixel-width)))
			)
		  (if rightmost-win
			  (progn
				(if (> enlarge 0)
					(progn
					  ;; (set-frame-width (selected-frame) (+ (frame-width) enlarge))
					  (setq res nil)
					  ))
				)
			(setq abort nil)
			(if (and (> enlarge 0) (xref-select-righter-window))
				(progn
				  (if (< (- (window-width) enlarge) window-min-width)
					  (setq abort (not (xref-set-window-width (+ window-min-width enlarge))))
					)))
			(if abort
				(setq res nil)
			  (select-window sw)
			  (enlarge-window-horizontally (- width (window-width)))
			  ))))
	res
))

;; appropriate window height by resizing upper window(s)
;;
(defun xref-set-window-height (height)
  (let ((wh) (sw) (nh) (diff))
	(setq sw (selected-window))
	(setq diff (- height (window-height)))
	(if (xref-select-upper-window)
		(progn
		  (setq nh (- (window-height) diff))
		  (if (<= nh window-min-height)
			  (xref-set-window-height (+ window-min-height diff))
			)
		  (enlarge-window (- 0 diff))
		  )
	  (enlarge-window diff)
	  )
	(select-window sw)
))

;; take care to not delete window on the right, do not resize in this case
(defun xref-resize-window-width (val)
  (let ((rightmost-win))
	(setq rightmost-win nil)
	(if (eq xref-running-under 'emacs)
		(setq rightmost-win (>= (elt (window-edges (selected-window)) 2) (frame-width)))
	  ;; xemacs
	  (setq rightmost-win (>= (+ (elt (xref-window-edges) 2) 20) (frame-pixel-width)))
	  )
	(if rightmost-win
		(progn
;;;				(set-frame-width (selected-frame) (+ (frame-width) val))
		  (if (>= (- (window-width) val) window-min-width)
			  (enlarge-window-horizontally (- 0 val))
			))
	  (if (>= (+ (window-width) val) window-min-width)
		  (enlarge-window-horizontally val)
		))
))

;; take care to not delete window on the right, do not resize in this case
(defun xref-resize-window-height (val)
  (if (< val 0)
	  (if (> (window-height) window-min-height)
		  (enlarge-window val)
		)
	;; todo check the bottom window size!
	(enlarge-window val)
	)
)

(defun xref-resize-left (event)
  (interactive "i")
  (xref-resize-window-width -1)
)

(defun xref-resize-right (event)
  (interactive "i")
  (xref-resize-window-width 1)
)

(defun xref-resize-up (event)
  (interactive "i")
  (xref-resize-window-height -1)
)

(defun xref-resize-down (event)
  (interactive "i")
  (xref-resize-window-height 1)
)

(defun xref-window-edges ()
  (let ((res))
	(if (eq xref-running-under 'emacs)
		(setq res (window-edges (selected-window)))
	  (setq res (window-pixel-edges (selected-window)))
	  )
	res
))

(defun xref-select-righter-window ()
  (let ((owe) (cwe) (loop) (sw) (res))
	(setq owe (xref-window-edges))
	(setq sw (selected-window))
	(setq loop t)
	(while loop
	  (other-window 1)
	  (setq cwe (xref-window-edges))
;;(message "testing %S %S" cwe owe)
	  (if (or 
		   (and (> (elt cwe 2) (elt owe 2))
				(not
				 (or (< (elt cwe 3) (elt owe 1))
					 (> (elt cwe 1) (elt owe 3))
					 )))
		   (equal (selected-window) sw))
		  (setq loop nil)
		)
	  )
	(if (equal (selected-window) sw)
		(setq res nil)
	  (setq res t)
	  )
	res
))

(defun xref-select-upper-window ()
  (let ((owe) (cwe) (loop) (sw) (res))
	(setq owe (xref-window-edges))
	(setq sw (selected-window))
	(setq loop t)
	(while loop
	  (other-window -1)
	  (setq cwe (xref-window-edges))
	  (if (or 
		   (and (< (elt cwe 1) (elt owe 1))
				(not
				 (or (< (elt cwe 2) (elt owe 0))
					 (> (elt cwe 0) (elt owe 2))
					 )))
		   (equal (selected-window) sw))
		  (setq loop nil)
		)
	  )
	(if (equal (selected-window) sw)
		(setq res nil)
	  (setq res t)
	  )
	res
))

(defun xref-appropriate-window-height (aplus aminus)
  (let ((pp) (lnnum) (wsize) (sw))
	(setq sw (selected-window))
	(setq pp (point))
	(setq lnnum (count-lines 1 (point-max)))
	(setq wsize (window-height (selected-window)))
	(if (< lnnum xref-window-minimal-size)
		(setq lnnum xref-window-minimal-size)
	  )
	(if (> lnnum (- (frame-height) window-min-height))
		(setq lnnum (- (frame-height) window-min-height))
	  )
	(if (or 
		 (and aplus (> lnnum wsize))
		 (and aminus (< lnnum wsize)))
		(progn
		  ;; the constant contains scrollbar and information line
		  (xref-set-window-height (+ lnnum 3))
		  ))
	(goto-char pp)
))

(defun xref-appropriate-window-width (aplus aminus)
  (let ((pp) (i) (wsize) (wwidth) (width) (w))
	(setq pp (point))
	(setq wsize (window-height))
	(setq width 10)
	(setq i 1)
	(goto-char (point-min))
	(end-of-line)
	(while (and (<= i wsize) (< (point) (point-max)))
	  (next-line 1)
	  (end-of-line)
	  (setq w (xref-current-column))
	  (if (> w width) (setq width w))
	  (setq i (+ i 1))	  
	  )
	(setq wwidth (window-width))
	(if (or
		 (and aplus (> width wwidth))
		 (and aminus (< width wwidth)))
		(xref-set-window-width (+ width 2))
	  )
	(goto-char pp)
))

(defun xref-appropriate-browser-windows-sizes (oldwins)
  (if (not oldwins)
	  (progn
		(if xref-browser-windows-auto-resizing
			(progn 
			  (xref-appropriate-window-height nil t)
			  (xref-appropriate-window-width t t)
			  )
		  (xref-set-window-height xref-symbol-selection-window-height)
		  (xref-set-window-width xref-symbol-selection-window-width)
		  )
		))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  highlighting ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun xref-font-lock-list-keywords (sym)
    (cons (cons (xref-keywords-regexp) '(xref-keyword-face))
		  (cons (cons "^.\\([^:]*:[0-9]*\\):" '(xref-list-pilot-face))
				(if (eq sym nil)
					nil
				  (cons (cons (concat "[^A-Za-z0-9_$]" sym "[^A-Za-z0-9_$]") '(xref-list-symbol-face))
						nil
						))
				))
)


(defun xref-create-escaped-cased-string (sstr)
  (let ((ind) (maxi) (ress) (cc))
	(setq ress "")
	(setq ind 0)
	(setq maxi (length sstr))
	(while (< ind maxi)
	  (setq cc (substring sstr ind (+ ind 1)))
	  (if (string-match "[A-Za-z]" cc)
		  (setq ress (concat ress "[" (downcase cc) (upcase cc) "]" ))
		(if (string-match "[][.*+?^$ \\]" cc)
			(setq ress (concat ress "\\" cc))
		  (setq ress (concat ress cc))
		))
	  (setq ind (+ ind 1))
	  )
	ress
))

(defun xref-create-tag-search-fontif (sstr)
  (let ((res) (ress) (loop) (ind) (ind0) (prefix))
	(setq prefix "\\(")
	(setq loop t)
	(setq ind 0)
	(while loop
	  (setq ind0 (string-match "[^ \n\t]" sstr ind)) 
	  (if (not ind0)
		  (setq loop nil)
		(setq ind (string-match "[ \t\n]" sstr ind0))
		(if (not ind)
			(setq loop nil)
		  (setq ress (concat ress prefix (xref-create-escaped-cased-string (substring sstr ind0 ind))))
		  (setq prefix "\\|")
		  ))
	)
	(if ind0
		(setq ress (concat ress prefix (xref-create-escaped-cased-string (substring sstr ind0))))
	  )
	(setq ress (concat ress "\\)"))
	(setq res 
		  (cons (cons ress '(xref-list-symbol-face))
		  (cons (cons ":.*$" '(xref-list-default-face))
				nil))
		  )
;;(message "fontif == %S" res)
	res
))

(defun xref-completion-symbol-highlight (begpos endpos)
  (let ((line) (sym) (spos) (mbeg) (mend))
	(setq line (buffer-substring begpos endpos))
	(setq spos (string-match "[ \t\n]" line 0))
	(if (and spos (> spos 0))
		(progn
		  (setq sym (xref-create-escaped-cased-string (substring line 0 spos)))
		  (setq mbeg (string-match (concat "[^A-Za-z0-9_$][a-zA-Z0-9_\\.]*" sym "(") line spos))
		  (if mbeg
			  (progn
				(setq mend (match-end 0))
				(put-text-property (+ mbeg begpos) 
								   (- (+ mend begpos) 1)
								   'face 'xref-list-symbol-face)
				)
			(setq mbeg (string-match (concat "[^A-Za-z0-9_$][a-zA-Z0-9_\\.]*" sym "\\[") line spos))
			(if mbeg
				(progn
				  (setq mend (match-end 0))
				  (put-text-property (+ mbeg begpos) 
									 (- (+ mend begpos) 1)
									 'face 'xref-list-symbol-face)
				  )
			  (setq mbeg (string-match (concat "define " sym " ") line spos))
			  (if mbeg
				  (progn
					(setq mend (match-end 0))
					(put-text-property (+ mbeg begpos 7) 
									   (- (+ mend begpos) 1)
									   'face 'xref-list-symbol-face)
					)
				(setq mbeg (string-match (concat "[^A-Za-z0-9_$][a-zA-Z0-9_\\.]*" sym "$") line spos))
				(if mbeg
					(progn
					  (setq mend (match-end 0))
					  (put-text-property (+ mbeg begpos) 
										 (+ mend begpos) 
										 'face 'xref-list-symbol-face)
					)))))))
))

(defun xref-hightlight-keywords (begpos endpos keywords-regexp keyword-face)
  (let ((buff) (bpos) (loop-flag) (mbeg) (mend)
		(ocfs))
;;(message "highlighting keywords %d %d" begpos endpos)
	(if (< begpos endpos)
		(progn
		  (setq buff (buffer-substring begpos endpos))
		  (setq bpos 0)
		  (setq loop-flag t)
		  (setq ocfs case-fold-search)
		  (setq case-fold-search nil)
		  (while loop-flag
			(setq mbeg (string-match keywords-regexp buff bpos))
			(if (eq mbeg nil)
				(setq loop-flag nil)
			  (setq mend (match-end 0))
			  (put-text-property (+ mbeg begpos) (+ mend begpos) 'face keyword-face)
			  (setq bpos mend)
			  ))
		  (setq case-fold-search ocfs)
		  ))
))

(defun xref-fontify-region (begpos endpos kw-font-list)
  (let ((kwl) (kw))
;;(message "xref-fontify-region %d %d %S" begpos endpos kw-font-list)
	(setq kwl kw-font-list)
	(while kwl
	  (setq kw (car kwl))
	  (xref-hightlight-keywords begpos endpos (car kw) (car (cdr kw)))
	  (setq kwl (cdr kwl))
	  )
))

(defun xref-line-hightlight (begpos endpos multiple-lines-flag hbeg-offset 
									fontification completion-highlighting) 
  (let ((bp) (loop-flag) (cpos) (prev-line) (dp-pos)
		)
;;(message "xref-line-hightlight [ %d-%d ]" begpos endpos)
	(setq cpos (xref-char-search begpos -1 ?\n))
	(if (< cpos 0) (setq cpos 0))
	(if (and xref-coloring (< (+ cpos 1) endpos))
		(xref-fontify-region (+ cpos 1) endpos fontification)
	  )
	(if (or xref-mouse-highlight xref-coloring)
		(progn
		  (while (< (+ cpos 1) endpos)
			(setq bp cpos)
			(setq loop-flag t)
			(while (and loop-flag 
						multiple-lines-flag
						(eq (char-after (+ bp 1)) ?\ )
						(eq (char-after (+ bp 2)) ?\ )
						)
			  (setq prev-line (xref-char-search (- bp 1) -1 ?\n))
			  (if (< prev-line 1)
				  (setq loop-flag nil)
				(setq bp prev-line)
				)
			  )
			(setq cpos (xref-char-search (+ cpos 1) 1 ?\n))
			(if (and xref-mouse-highlight (< (+ bp hbeg-offset) cpos))
				(put-text-property (+ bp hbeg-offset) cpos 'mouse-face 'highlight)
			  )
			(if (and xref-coloring completion-highlighting (< (+ bp 1) cpos))
				(xref-completion-symbol-highlight (+ bp 1) cpos)
			  )
			)))
))



(defun xref-entry-point-make-initialisations-no-project-required ()
  (xref-soft-select-dispach-data-caller-window xref-this-buffer-dispatch-data)
)

(defun xref-entry-point-make-initialisations ()
  (xref-entry-point-make-initialisations-no-project-required)
  ;; hack in order to permit asking for version without src project
  (setq xref-active-project (xref-compute-active-project))
  (if xref-display-active-project-in-minibuffer
	  (message "Project: %s" xref-active-project)
	)
  ;; probably useless here
  (setq xref-full-auto-update-allowed nil)
  (setq xref-full-auto-update-perform nil)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; XREF-TASK ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar xref-buffers-counter 1)
(defun xref-new-symbol-resolution-buffer ()
  (let ((res))
	(setq res (format "%s (%d)" xref-symbol-resolution-buffer xref-buffers-counter))
	(setq xref-buffers-counter (+ xref-buffers-counter 1))
	res
))
(defun xref-new-references-buffer ()
  (let ((res))
	(setq res (format "%s (%d)" xref-references-buffer xref-buffers-counter))
	(setq xref-buffers-counter (+ xref-buffers-counter 1))
	res
))

(defun xref-start-server-process (initopts ofile proc filter)
  (let ((oldpct) (opts))
	(setq oldpct process-connection-type)
	(setq process-connection-type nil)
	(setq opts (append (list
						 "-xrefactory-II" 
						 ;;"-xrefrc" xref-options-file
						 ;;"-urldirect"
						 ;; do not comment following options without testing
						 ;; on all platform - files combinations!
						 ;; I have moved them into the main task
						 ;; "-crlfautoconversion" 
						 ;; "-crconversion"
						 ;; despite all I comment this out at [16 Jan 2007]
						 ;; (if xref-allow-multibyte "-encoding=default" "-encoding=european")
						 "-o" ofile
						 )
					   initopts))
	(if xref-debug-mode (message "calling: %S" opts))
	(set proc (cons 
			   (apply 'start-process 
					  (concat xref-home-directory "xref") 
					  nil 
					  (concat xref-home-directory "xref")
					  opts
					  )
			   (cons "" nil)))
	(set-process-filter (car (eval proc)) filter)
	(setq process-connection-type oldpct)
	(process-kill-without-query (car (eval proc)))
))

(defvar xref-interrupt-dialog-map (make-sparse-keymap "Xref interrupt dialog"))
(xref-add-basic-modal-keybindings-no-escape xref-interrupt-dialog-map)


(defun xref-interrupt-current-process (process tmp-files)
  (let ((sel))
	(setq sel (xref-modal-dialog xref-selection-modal-buffer
"Interrupt current process?
----
 1.) Yes
 2.) No, just interrupt Emacs macro
 3.) No
----
"
					   3 0 t xref-interrupt-dialog-map nil))
	(if (eq sel 3)
		(progn
		  ;; execution interrupted by C-g, kill process, clean tmp files
		  (delete-process (car process))
		  (xref-server-remove-tmp-files tmp-files)
		  (if (file-exists-p xref-server-tasks-ofile) (delete-file xref-server-tasks-ofile))
		  (setq inhibit-quit nil)
		  (error "Process killed")
		  )
	  (if (eq sel 4)
		  (progn
			(setq inhibit-quit nil)
			(error "Quit")
			)
		(setq quit-flag nil)
		))
	(sit-for .1)
))

(defun xref-wait-until-task-sync (proc tmp-files)
  (let ((process))
	;; I will check quit-flag manually
	(setq inhibit-quit t)
	(setq process (eval proc))
	(while (and 
			(not (cdr (cdr process)))
			(eq (process-status (car process)) 'run))
	  (accept-process-output (car process) 0 100)
	  (if quit-flag (xref-interrupt-current-process process tmp-files))
	  )
	(setq inhibit-quit nil)
))

(defun xref-encoding-option ()
  (let ((res))
	(setq res "")
	(if (or (eq xref-files-encoding 'ascii)
			(eq xref-files-encoding 'generic)
			(eq xref-files-encoding 'european))
		(setq res "-encoding=european")
	  (if (eq xref-files-encoding 'euc)
		  (setq res "-encoding=euc")
		(if (eq xref-files-encoding 'sjis)
			(setq res "-encoding=sjis")
		  (if (eq xref-files-encoding 'utf)
			  (setq res "-encoding=utf")
			(if (eq xref-files-encoding 'project)
				(setq res "")			  
			  )))))
	res
))

(defun xref-send-data-to-running-process (data proc)
  (let ((cbuffer))
	(setq cbuffer (current-buffer))
	(if (eq (eval proc) nil)
		(xref-start-server-process '("-task_regime_server") xref-server-tasks-ofile proc 'xref-server-filter)
	  (if (not (eq (process-status (car (eval proc))) 'run))
		  (progn
			(delete-process (car (eval proc)))
			(xref-start-server-process '("-task_regime_server") xref-server-tasks-ofile proc 'xref-server-filter)
			)))
	(if xref-debug-mode (message "sending: %s" data))
	(get-buffer-create " *xref-server-options*")
	(set-buffer " *xref-server-options*")
	(xref-erase-buffer)
	(insert data)
	(insert "\nend-of-options\n\n")
	(xref-init-server-filter proc)
	(process-send-region (car (eval proc)) 1 (point))
	(kill-buffer nil)
	(set-buffer cbuffer)
))

(defun xref-init-server-filter (proc)
  (setcdr (cdr (eval proc)) nil)
)

(defun xref-processes-filter (process output proc)
  (let ((pn) (ss) (i) (j) (len))
	(if xref-debug-mode (message "got: %s" output))
	(setq ss (format "%s%s" (car (cdr (eval proc))) output))
	(setq len (length ss))
	(setq i 0)
	(setq i (xref-server-dispatch-skip-blank ss i len))
	(while (and (< (+ i xref-ppc-progress-len) len)
				(equal (substring ss i (+ i xref-ppc-progress-len)) xref-ppc-progress)
				)
	  (progn
		(setq i (+ i xref-ppc-progress-len))
		(setq j i)
		(while (and (< i len)
					(>= (elt ss i) ?0)
					(<= (elt ss i) ?9)
					)
		  (setq i (+ i 1))
		  )
		(setq pn (string-to-int (substring ss j i)))
		(message "progress %s%%" pn)
		(setq i (xref-server-dispatch-skip-blank ss i len))
		))
	(if (< i len)
		(progn
		  (if (and (< (+ i xref-ppc-synchro-record-len) len)
				   (equal (substring ss i (+ i xref-ppc-synchro-record-len)) xref-ppc-synchro-record))
			  (progn
				(setq i (+ i xref-ppc-synchro-record-len))
				(setcdr (cdr (eval proc)) t)
				)
			(message "SYNCHRO PROBLEM: %s" ss)
			(setq i len)
			)))
	(setcar (cdr (eval proc)) (substring ss i))
))

(defun xref-server-filter (process output)
  (xref-processes-filter process output 'xref-server-process)
)

(defun xref-refactorer-filter (process output)
  (xref-processes-filter process output 'xref-refactorer-process)
)

(defun xref-tags-filter (process output)
  (xref-processes-filter process output 'xref-tags-process)
)

(defun xref-server-add-buffer-to-tmp-files-list (buffer lst)
  (let ((res))
	(setq res (cons (list buffer (xref-server-get-new-tmp-file-name)) lst))
	res
))

(defun xref-server-save-buffers-to-tmp-files (lst)
  (let ((bb) (tt) (bbt) (cb) (res) (coding))
	(setq cb (current-buffer))
	(setq res nil)
	(while lst
	  (setq bbt (car lst))
	  (setq bb (car bbt))
	  (setq tt (car (cdr bbt)))
	  (set-buffer bb)
	  (if (boundp 'buffer-file-coding-system)
		  (setq coding buffer-file-coding-system)
		(setq coding nil)
		)
	  (xref-write-tmp-buff tt 1 (+ (buffer-size) 1) coding)
	  (setq res (append (list "-preload" (buffer-file-name bb) tt) res))
	  (setq lst (cdr lst))
	  )
	(set-buffer cb)
	res
))

(defun xref-server-remove-tmp-files (lst)
  (let ((bb) (tt) (bbt))
	(while lst
	  (setq bbt (car lst))
	  (setq bb (car bbt))
	  (setq tt (car (cdr bbt)))
	  (if (and xref-debug-mode xref-debug-preserve-tmp-files)
		  (message "keeping tmp file %s" tt)
		(delete-file tt)
		)
	  (setq lst (cdr lst))
	  )
))

(defun xref-server-get-list-of-buffers-to-save-to-tmp-files (can-add-current-buffer)
  (let ((bl) (bb) (res) (cb))
	(setq res nil)
	(setq bl (buffer-list))
	(setq cb (current-buffer))
	(while bl
	  (setq bb (car bl))
	  (if (and (buffer-file-name bb)
			   (buffer-modified-p bb)
			   )
		  (progn
			(if (or can-add-current-buffer
					(not (equal bb cb)))
				(setq res (xref-server-add-buffer-to-tmp-files-list bb res))
		  )))
	  (setq bl (cdr bl))
	  )
	(set-buffer cb)
	res
))

(defun xref-server-get-point-and-mark-options-via-tmp-file ()
  (let ((olcursor) (olmark) (res) (tmpFile) (coding) (eolc))
	(if (boundp 'buffer-file-coding-system)
		(setq coding buffer-file-coding-system)
	  (setq coding nil)
	  )
	;; try to inhibit crlf conversion
	(if (boundp 'inhibit-eol-conversion)
		(progn
		  (setq eolc inhibit-eol-conversion)
		  (setq inhibit-eol-conversion t)
		  ))
	(if (and (eq xref-running-under 'xemacs)
			 (fboundp 'coding-system-eol-type)
			 (boundp 'buffer-file-coding-system)
			 (fboundp 'coding-system-base))
		(progn	 
		  (if (eq (coding-system-eol-type buffer-file-coding-system) 'crlf)
			  (setq coding (coding-system-base buffer-file-coding-system))
			)))
	(setq tmpFile (format "%s/xref%s.tmp" xref-tmp-dir xref-user-identification))
	(setq res nil)
	(if (>= (point) 1)
		(progn
		  (xref-write-tmp-buff tmpFile 1 (point) coding)
		  (setq olcursor (nth 7 (file-attributes tmpFile)))
		  (setq res (cons (format "-olcursor=%d" olcursor) res))
		  ))
	(if (and (mark t) (>= (mark t) 1))
		(progn
		  (xref-write-tmp-buff tmpFile 1 (mark t) coding)
		  (setq olmark (nth 7 (file-attributes tmpFile)))
		  (setq res (cons (format "-olmark=%d" olmark) res))
		  ))
	(if (boundp 'inhibit-eol-conversion)
		(setq inhibit-eol-conversion eolc)
	  )
	(if res (delete-file tmpFile))
	res
))

(defun xref-server-get-point-and-mark-options ()
  (let ((res))
	(if (eq xref-files-encoding 'generic)
		(setq res (xref-server-get-point-and-mark-options-via-tmp-file))
	  (setq res nil)
	  (if (>= (point) 1)
		  (setq res (cons (format "-olcursor=%d" (- (point) 1)) res))
		)
	  (if (and (mark t) (>= (mark t) 1))
		  (setq res (cons (format "-olmark=%d" (- (mark t) 1)) res))
		)
	  )
	res
))

(defun xref-buffer-char-list ()
  (let ((res) (ll) (i) (max))
	(setq res (make-list (1- (point-max)) ?\n))
	(setq ll res)
	(setq max (point-max))
	(setq i 1)
	(while (< i max)
	  (setcar ll (char-after i))
	  (setq ll (cdr ll))
	  (setq i (1+ i))
	  )
	res
))

(defun xref-char-list-substring (list from to)
  (let ((res) (i) (ll) (len) (max))
	(if (stringp list)
		(setq res (substring list from to))
	  (setq ll (nthcdr from list))
	  (if to 
		  (setq len (- to from))
		(setq len (length ll))
		)
	  (setq res (make-string len ?\n))
	  (setq i 0)
	  (while (< i len)
		(aset res i (car ll))
		(setq ll (cdr ll))
		(setq i (1+ i))
		)
	  )
;;(message "returning substring(%S %S %S) --> %S" list from to res)
	res
))

(defun xref-server-read-answer-file-and-dispatch (dispatch-data tmp-files)
  (let ((res) (cb) (i) (coding-system-for-read 'raw-text))
	(setq cb (current-buffer))
	(get-buffer-create xref-server-answer-buffer)
	(set-buffer xref-server-answer-buffer)
	;;(xref-erase-buffer)
	(insert-file-contents xref-server-tasks-ofile nil nil nil t)
	(if xref-xemacs-mule-problem 
		(setq res (xref-buffer-char-list))
	  (if (fboundp 'buffer-substring-no-properties)
		  (setq res (buffer-substring-no-properties 1 (point-max)))
		(setq res (buffer-substring 1 (point-max)))
		))
	(setq i 0)
	(kill-buffer xref-server-answer-buffer)
	(delete-file xref-server-tasks-ofile)
	(xref-server-remove-tmp-files tmp-files)
	(set-buffer cb)
	(setq i (xref-server-dispatch res i (length res) dispatch-data))
	res
))

(defun xref-send-data-to-process-and-dispatch (data dispatch-data tmp-files)
  (let ((proc) (frame-id) (opts))
	(setq proc (cdr (assoc 'process dispatch-data)))
	(setq frame-id (cdr (assoc 'frame-id dispatch-data)))
	(setq opts (format "%s%s %s -xrefrc \"%s\" -p \"%s\" -user %d" 
					   (if xref-browser-lists-source-lines "" "-rlistwithoutsrc ")
					   (xref-encoding-option)
					   data 
					   xref-options-file 
					   xref-active-project 
					   frame-id
					   ))

	(xref-send-data-to-running-process opts proc)
	(xref-wait-until-task-sync proc tmp-files)
	(xref-server-read-answer-file-and-dispatch dispatch-data tmp-files)
))

(defun xref-server-call-on-current-buffer (options dispatch-data obl)
  (let ((res) (bl) (data) (topt) (blo))
	(setq bl (xref-server-add-buffer-to-tmp-files-list 
			  (current-buffer) obl))
	(setq blo (xref-server-save-buffers-to-tmp-files bl))
	(setq blo (append blo (xref-server-get-point-and-mark-options )))
	(setq topt "")
	(while blo
	  (setq topt (format "%s \"%s\"" topt (car blo)))
	  (setq blo (cdr blo))
	  )
	(setq data (format "%s %s \"%s\"" options topt (buffer-file-name nil)))
	(xref-send-data-to-process-and-dispatch data dispatch-data bl)
))

(defun xref-server-call-on-current-buffer-no-saves (options dispatch-data)
  (xref-server-call-on-current-buffer options dispatch-data nil)
)

(defun xref-server-call-on-current-buffer-all-saves (options dispatch-data)
  (let ((bl))
	(setq bl (xref-server-get-list-of-buffers-to-save-to-tmp-files nil))
	(xref-server-call-on-current-buffer options dispatch-data bl)
))

(defun xref-kill-refactorer-process-if-any ()
  (if (not (eq xref-refactorer-process nil))
	  (delete-process (car xref-refactorer-process))
	)
  (setq xref-refactorer-process nil)
)

(defun xref-server-call-refactoring-task (opts)
  (let ((bl) (frame-id) (enc))
	(if (and (not (eq xref-refactorer-process nil))
			 (eq (process-status (car xref-refactorer-process)) 'run))
		;;(if (xref-yes-or-no-window "A refactoring process is running, can I kill it? " t nil)
			(progn
			  (xref-kill-refactorer-process-if-any)
			  )
		;;  (error "Cannot run two refactoring processes")
		;;  )
	  )
	(setq xref-refactorer-process nil)
	(setq xref-refactorer-dispatch-data (xref-get-basic-server-dispatch-data 'xref-refactorer-process))
	(setq bl (xref-server-get-list-of-buffers-to-save-to-tmp-files nil))
	(setq bl (xref-server-add-buffer-to-tmp-files-list 
			  (current-buffer) bl))
	(setq frame-id (cdr (assoc 'frame-id xref-refactorer-dispatch-data)))
	(setq opts (append opts (xref-server-save-buffers-to-tmp-files bl)))
	(setq opts (append opts (xref-server-get-point-and-mark-options )))
	(setq opts (append (list "-refactory" 
							 "-xrefrc" xref-options-file
							 "-p" xref-active-project
							 "-user" (int-to-string frame-id)
							)
					  opts))
	(setq enc (xref-encoding-option))
	(if (not (equal enc ""))
		(setq opts (append (list enc) opts))
	  )
	(if xref-prefer-import-on-demand
		(setq opts (append opts '("-addimportdefault=0")))
	  (setq opts (append opts '("-addimportdefault=1")))
	  )
	(if (eq xref-refactoring-security-level 'low)
		(setq opts (append opts '("-rsl=low")))
	  (if (eq xref-refactoring-security-level 'high)
		  (setq opts (append opts '("-rsl=high")))
		(error "Wrong value %S in  xref-refactoring-security-level" xref-refactoring-security-level)
		))
	(if xref-manual-symbol-selection-within-refactoring
		  (setq opts (append opts '("-rf-manual-selection")))
	  )
	(setq opts (append opts (cons 
							 (format "%s" (buffer-file-name))
							 nil)))

	(xref-start-server-process opts xref-server-tasks-ofile 'xref-refactorer-process 'xref-refactorer-filter)
	(xref-wait-until-task-sync 'xref-refactorer-process bl)
	(xref-server-read-answer-file-and-dispatch xref-refactorer-dispatch-data bl)
	(xref-kill-refactorer-process-if-any)
))

(defun xref-server-tags-process (opts)
  (let ((bl) (enc))
	(if (and (not (eq xref-tags-process nil))
			 (eq (process-status (car xref-tags-process)) 'run))
		(if (xref-yes-or-no-window "tags maintenance process is running, can I kill it? " t nil)
			(progn
			  (delete-process (car xref-tags-process))
			  (setq xref-tags-process nil)
			  )
		  (error "Cannot run two tag maintenance processes.")
		  ))
	(setq bl (xref-server-get-list-of-buffers-to-save-to-tmp-files t))
	(setq opts (append opts (list "-errors" 
								  "-xrefrc" xref-options-file
								  "-p" xref-active-project
								  "-user" (int-to-string (xref-get-this-frame-id))
								  )))
	(setq enc (xref-encoding-option))
	(if (not (equal enc ""))
		(setq opts (append (list enc) opts))
	  )
	(setq opts (append opts (xref-server-save-buffers-to-tmp-files bl)))
;;	(setq xref-tags-dispatch-data (xref-get-basic-server-dispatch-data 'xref-tags-process))
	(xref-start-server-process opts xref-tags-tasks-ofile 'xref-tags-process 'xref-tags-filter)
	(xref-wait-until-task-sync 'xref-tags-process bl)
;;	(xref-server-read-answer-file-and-dispatch xref-tags-dispatch-data nil)
	(delete-process (car xref-tags-process))
	(setq xref-tags-process nil)
	(xref-server-remove-tmp-files bl)
))

(defun xref-get-basic-server-dispatch-data (proc)
  (let ((res))
	(setq res (cons (cons 'dummy-parameter-to-make-cdr-in-memory nil)
					(cons (cons 'caller-window (selected-window))
						  (cons (cons 'process proc) 
								(cons (cons 'frame-id (xref-get-this-frame-id))
									  nil)))))
	res
))

(defun xref-call-process-with-basic-file-data-no-saves (option)
  (setq xref-global-dispatch-data (xref-get-basic-server-dispatch-data 
								   'xref-server-process))
  (xref-server-call-on-current-buffer-no-saves option 
											   xref-global-dispatch-data)  
)

(defun xref-call-process-with-smart-browsing-sym-all-saves (option)
  (let ((line) (col) (sym) (poin) (spoin) (sym-end-offset))
  (if xref-smart-browsing-mode
	  (if (buffer-modified-p (current-buffer))
		  (error "** Buffer modified (smart mode requires all buffers saved) **")
		(setq line (count-lines (point-min) (+ (point) 1)))
		(setq col (xref-current-column))
		(setq sym nil)
		(if (bobp)
			(progn
			  (setq sym "include")
			  (setq sym-end-offset 6)
			  )
		  (if (or (xref-is-letter (char-after (point)))
				  (xref-is-number (char-after (point))))
			  (progn
				(setq poin (point))
				(search-backward-regexp xref-backward-pass-identifier-regexp)
				(if (not (xref-is-letter (char-after (point)))) (forward-char 1))
				(setq spoin (point))
				(search-forward-regexp xref-forward-pass-identifier-regexp)
				(if (not (eq (point) (point-min))) (backward-char))
				(setq sym (buffer-substring spoin (point)))
				;; get position at the end of the symbol to avoid interference in ## symbols
				(if (not (eq (point) (point-min))) (backward-char))
				(setq sym-end-offset (- (point) poin))
				(goto-char poin)
				)
			(setq sym ";;")             ;; shouldn't this be "_Z"????  magic.
			(setq sym-end-offset 0)
			))
		(if sym
			(progn
			  (setq option (format "%s -olcxlccursor=%d:%d -browsedsym=%s -browsedseo=%d" option line col sym sym-end-offset))
			  )
		  (error "** No symbol to browse **")
		  )
		(if (not xref-smart-browse-check-name)
			(setq option (format "%s -browseintern" option))
		  )
		))
  (xref-server-call-on-current-buffer-all-saves option 
											   xref-global-dispatch-data)  
))

(defun xref-call-process-with-basic-file-data-all-saves (option)
  (setq xref-global-dispatch-data (xref-get-basic-server-dispatch-data 
								   'xref-server-process))
  (xref-call-process-with-smart-browsing-sym-all-saves option)
)

(defun xref-softly-preset-project (pname)
  (let ((actp))
	(setq actp xref-active-project)
	(setq xref-active-project pname)
	;; this is just a hack, it may cause problems, because current buffer
	;; is passed to xref (be careful on which buffer you call it)
	;; but xref needs to parse something to softsetup project, maybe
	;; I should implement special option '-softprojectset'?
	(xref-call-process-with-basic-file-data-no-saves "-olcxgetprojectname")
	;; Hmm. this is also dispatching, hope it is no problem.
	(setq xref-active-project actp)
))

(defun xref-compute-simple-information (option)
  (let ((res))
	(xref-call-process-with-basic-file-data-no-saves option)
	(setq res (cdr (assoc 'info xref-global-dispatch-data)))
	res
))

(defun xref-compute-active-project ()
  (let ((res) (proc))
	(if xref-current-project
		(setq xref-active-project xref-current-project)
	  (setq proc 'xref-server-process)
	  (setq xref-global-dispatch-data (xref-get-basic-server-dispatch-data 
									   proc))
	  ;; keep here the -noerrors options, otherwise I can not edit project options
	  ;; when there is some wrong option
	  (xref-send-data-to-running-process 
	   (format "-olcxgetprojectname -xrefrc \"%s\" \"%s\"" xref-options-file (buffer-file-name))
	   proc)
	  (xref-wait-until-task-sync proc nil)
	  (xref-server-read-answer-file-and-dispatch xref-global-dispatch-data nil)
	  (setq res (cdr (assoc 'info xref-global-dispatch-data)))
	  res
	  )
))

(defun xref-get-env (name) 
"Get value of an Xrefactory environment variable.

This  function  gets a  value  associated  with  the Xref  environment
variable set by the -set option in the .xrefrc file (its value depends
on active project selection).
"
  (let ((res))
	(setq xref-global-dispatch-data (xref-get-basic-server-dispatch-data 'xref-server-process))
	(xref-server-call-on-current-buffer-no-saves (format "-get \"%s\"" name) 
												 xref-global-dispatch-data)  
	(setq res (cdr (assoc 'info xref-global-dispatch-data)))
	res
))

(defvar xref-frame-id-counter 0)
(defun xref-get-this-frame-id ()
  (let ((res) (dd))
	(setq dd (xref-get-this-frame-dispatch-data))
	(if dd
		(setq res (cdr (assoc 'frame-id dd)))
	  (setq xref-frame-id-counter (+ xref-frame-id-counter 1))
	  (xref-set-this-frame-dispatch-data (cons (cons 'frame-id xref-frame-id-counter) nil))
	  (setq res xref-frame-id-counter)
	  )
	res
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DISPATCH ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar xref-server-ctag "")
(defvar xref-server-ctag-attributes nil)
(defvar xref-server-cstring-value "")

(defun xref-server-dispatch-skip-blank (ss i len)
  (while (and (< i len) 
			  (or (eq (elt ss i) ?\ )
				  (eq (elt ss i) ?\t)
				  (eq (elt ss i) ?\n)))
	(setq i (1+ i))
	)
  i
)

(defun xref-server-parse-xml-tag (ss i len)
  (let ((b) (e) (att) (attval))
	(setq xref-server-ctag PPC_NO_TAG)
	(setq i (xref-server-dispatch-skip-blank ss i len))
	(if (< i len)
		(progn
		  (if (not (eq (elt ss i) ?<))
			  (error "tag starts by %c instead of '<'" (elt ss i))
			)
		  (setq i (1+ i))
		  (setq b i)
		  (while (and (< i len) 
					  (not (eq (elt ss i) ? ))
					  (not (eq (elt ss i) ?>)))
			(setq i (1+ i))
			)
		  (setq xref-server-ctag (xref-char-list-substring ss b i))
;;(message "xref-server-ctag == %s" xref-server-ctag)
		  (setq i (xref-server-dispatch-skip-blank ss i len))
		  (setq xref-server-ctag-attributes nil)
		  (while (and (< i len) (not (eq (elt ss i) ?>)))
			(setq b i)
			(while (and (< i len) 
						(not (eq (elt ss i) ? ))
						(not (eq (elt ss i) ?=))
						(not (eq (elt ss i) ?>)))
			  (setq i (1+ i))
			  )
			(setq att (xref-char-list-substring ss b i))
;;(message "att %s" att)
			(if (eq (elt ss i) ?=)
				(progn
				  (setq i (1+ i))
				  (if (eq (elt ss i) ?\")
					  (progn
						(setq i (1+ i))
						(setq b i)
						(while (and (< i len) (not (eq (elt ss i) ?\")))
						  (setq i (1+ i))
						  )
						(setq attval (xref-char-list-substring ss b i))
						(setq i (1+ i))
						)
					(setq b i)
					(while (and (< i len) 
								(not (eq (elt ss i) ? ))
								(not (eq (elt ss i) ?>)))
					  (setq i (1+ i))
					  )
					(setq attval (xref-char-list-substring ss b i))
					)
;;(message "attval %s == %s" att attval)
				  (setq xref-server-ctag-attributes
						(cons (cons att attval) 
							  xref-server-ctag-attributes))
				  ))
			(setq i (xref-server-dispatch-skip-blank ss i len))
			)
		  (if (< i len) (setq i (1+ i)))
		  ))
	i
))

(defun xref-server-dispatch-get-int-attr (attr)
  (let ((as) (res))
	(setq res 0)
	(setq as (assoc attr xref-server-ctag-attributes))
	(if as
		(setq res (string-to-int (cdr as)))
	  )
	res
))


(defun xref-server-dispatch-require-ctag (tag)
  (if (not (equal xref-server-ctag tag))
	  (error "[xref answer parsing] <%s> expected instead of <%s>" tag xref-server-ctag)
	)
)

(defun xref-server-dispatch-require-end-ctag (tag)
  (xref-server-dispatch-require-ctag (format "/%s" tag))
)


(defun xref-soft-select-dispach-data-caller-window (dispatch-data)
  (let ((winassoc) (win) (res))
	(setq res nil)
	(if dispatch-data
		(progn
		  (setq winassoc (assoc 'caller-window dispatch-data))
		  (if winassoc
			  (progn
				(setq win (cdr winassoc))
				(if (and win (windowp win) (window-live-p win))
					(progn
					  (select-window  win)
					  (set-buffer (window-buffer win))
					  (setq res t)
					  ))))))
	res
))

(defun xref-select-dispach-data-caller-window (dispatch-data)
  (let ((ww))
	(setq ww (cdr (assoc 'caller-window dispatch-data)))
	(if ww
		(progn
		  (if (not (window-live-p ww))
			  (error "The associated window no longer exists.")
			(select-window ww)
			(set-buffer (window-buffer (selected-window)))
			)))
))

(defun xref-select-dispach-data-refs-window (dispatch-data)
  (select-window  (cdr (assoc 'linked-refs-window dispatch-data)))
  (set-buffer (window-buffer (selected-window)))
)

(defun xref-select-dispach-data-resolution-window (dispatch-data)
  (select-window  (cdr (assoc 'linked-resolution-window dispatch-data)))
  (set-buffer (window-buffer (selected-window)))
)

(defun xref-display-and-set-new-dialog-window (buff horizontal in-new-win)
  (let ((res))
	(if horizontal
		(progn
		  (if (< (window-width) (* 2 window-min-width))
			  (xref-set-window-width (* 2 window-min-width))
			)
		  (split-window-horizontally)
		  )
	  (if (< (window-height) (* 2 window-min-height))
		  (xref-set-window-height (* 2 window-min-height))
		)
	  (split-window-vertically)
	  )
	(other-window 1)
	(setq res (selected-window))
	(other-window -1)
	(if in-new-win (other-window 1))
	(set-buffer (get-buffer-create buff))
	(setq buffer-read-only nil)
	(xref-erase-buffer)
	(set-window-buffer (selected-window) (current-buffer))
	(setq truncate-lines t)
	res
))

(defun xref-display-and-set-maybe-existing-window (buff horizontal in-new-win)
  (let ((ww))
	(setq ww (get-buffer-window buff))
	(if ww
		(select-window ww)
	  (xref-display-and-set-new-dialog-window buff  horizontal in-new-win)
	  )
))

(defun xref-server-dispatch-error (ss i len dispatch-data tag)
  (let ((tlen) (cc) (cw))
	(setq cw (selected-window))
	(setq tlen (xref-server-dispatch-get-int-attr PPCA_LEN))
	(setq cc (xref-char-list-substring ss i (+ i tlen)))
	(setq i (+ i tlen))
	(setq i (xref-server-parse-xml-tag ss i len))
	(xref-server-dispatch-require-end-ctag tag)
	(xref-select-dispach-data-caller-window dispatch-data)
	(xref-display-and-set-new-dialog-window xref-error-modal-buffer nil t)
	(setq truncate-lines nil)
	(insert-string "[error] : ")
	(insert-string cc)
	(goto-char (point-min))
	(xref-appropriate-window-height nil t)
	(beep t)
	(xref-read-key-sequence "Press a key to continue")
	(xref-delete-window-in-any-frame (current-buffer) nil nil)
	(select-window cw)
	(xref-kill-refactorer-process-if-any)
	(error "exiting")
	i
))

(defun xref-server-dispatch-fupdate-or-error (ss i len dispatch-data tag)
  (let ((tlen) (cc) (cw) (action))
	(setq cw (selected-window))
	(if (not xref-full-auto-update-allowed)
		(xref-server-dispatch-error ss i len dispatch-data tag)
	  )
	(setq tlen (xref-server-dispatch-get-int-attr PPCA_LEN))
	(setq cc (xref-char-list-substring ss i (+ i tlen)))
	(setq i (+ i tlen))
	(setq i (xref-server-parse-xml-tag ss i len))
	(xref-server-dispatch-require-end-ctag tag)
	(setq action xref-full-auto-update-allowed)
	(setq xref-full-auto-update-allowed nil)
	(setq xref-full-auto-update-perform t)
	i
))

(defun xref-server-dispatch-information (ss i len dispatch-data tag)
  (let ((tlen) (cc) (cw) (dw))
	(setq cw (selected-window))
	(setq tlen (xref-server-dispatch-get-int-attr PPCA_LEN))
	(setq cc (xref-char-list-substring ss i (+ i tlen)))
	(setq i (+ i tlen))
	(setq i (xref-server-parse-xml-tag ss i len))
	(xref-server-dispatch-require-end-ctag tag)
	(if (and (equal tag PPC_DEBUG_INFORMATION)
			 (eq xref-debug-mode nil))
		() ;; do nothing if debug information and mode is off
	  (xref-select-dispach-data-caller-window dispatch-data)
	  (setq dw (xref-display-and-set-new-dialog-window xref-info-modal-buffer nil t))
	  ;;(insert-string "[info] : ")
	  (insert-string cc)
	  (goto-char (point-min))
	  (xref-appropriate-window-height nil t)
	  (xref-read-key-sequence "Press a key to continue")
	  (delete-window dw)
	  (select-window cw)
	  )
	i
))

(defvar xref-license-error-dialog-map (make-sparse-keymap "Xref license error"))
(xref-add-basic-modal-keybindings xref-license-error-dialog-map)

(defun xref-server-dispatch-license-error (ss i len dispatch-data)
  (let ((tlen) (cc) (cw) (sel) (ln))
	(setq cw (selected-window))
	(setq tlen (xref-server-dispatch-get-int-attr PPCA_LEN))
	(setq cc (xref-char-list-substring ss i (+ i tlen)))
	(setq i (+ i tlen))
	(setq i (xref-server-parse-xml-tag ss i len))
	(xref-server-dispatch-require-end-ctag PPC_LICENSE_ERROR)
	(setq ln (1+ (xref-char-count ?\n cc)))
	(xref-select-dispach-data-caller-window dispatch-data)
	(setq sel (xref-modal-dialog xref-selection-modal-buffer (format
"%s
----
 1.) Exit
 2.) Browse URL
----
" cc)
					   (+ ln 2) 0 t xref-license-error-dialog-map dispatch-data))
	(if (eq sel (+ ln 3))
		(xref-browse-url xref-registration-url)
		)
	i
))

(defvar xref-add-to-imports-dialog-map (make-sparse-keymap "Xref fqt completion"))
(xref-add-basic-modal-keybindings xref-add-to-imports-dialog-map)

(defun xref-server-dispatch-add-to-imports-dialog (ss i len dispatch-data)
  (let ((tlen) (cc) (cw) (dw) (cl) (pack) (copt) (default) (sel))
	(setq cw (selected-window))
	(setq default (xref-server-dispatch-get-int-attr PPCA_VALUE))
	(setq tlen (xref-server-dispatch-get-int-attr PPCA_LEN))
	(setq cc (xref-char-list-substring ss i (+ i tlen)))
	(setq i (+ i tlen))
	(setq i (xref-server-parse-xml-tag ss i len))
	(xref-server-dispatch-require-end-ctag PPC_ADD_TO_IMPORTS_DIALOG)
	(xref-select-dispach-data-caller-window dispatch-data)
	(setq cl (xref-file-name-extension cc))
	(setq pack (xref-file-name-without-suffix cc))
	(setq sel (xref-modal-dialog xref-selection-modal-buffer (format
"Class %s.%s is used but not imported, should I:
----
 1.) Import %s.*
 2.) Import %s.%s
 3.) Keep it as is
----
" pack cl pack pack cl cl)
					   (+ 3 default) 0 t xref-add-to-imports-dialog-map dispatch-data))
	(cond 
	 (
	  (eq sel 3)
	  (setq copt "-continuerefactoring=importOnDemand")
	  )
	 (
	  (eq sel 4)
	  (setq copt "-continuerefactoring=importSingle")
	  )
	 (
	  t
	  (setq copt "-continuerefactoring")
	  )
	 )
	(xref-send-data-to-process-and-dispatch copt dispatch-data nil)
	i
))

(defun xref-server-dispatch-call-macro (ss i len dispatch-data)
  (let ((tlen) (cc))
	(setq tlen (xref-server-dispatch-get-int-attr PPCA_LEN))
	(setq cc (xref-char-list-substring ss i (+ i tlen)))
	(setq i (+ i tlen))
	(setq i (xref-server-parse-xml-tag ss i len))
	(xref-server-dispatch-require-end-ctag PPC_CALL_MACRO)
	(call-last-kbd-macro 1)
	i
))

(defun xref-server-dispatch-kill-buffer-remove-file (ss i len dispatch-data)
  (let ((tlen) (cc))
	(setq tlen (xref-server-dispatch-get-int-attr PPCA_LEN))
	(setq cc (xref-char-list-substring ss i (+ i tlen)))
	(setq i (+ i tlen))
	(setq i (xref-server-parse-xml-tag ss i len))
	(xref-server-dispatch-require-end-ctag PPC_KILL_BUFFER_REMOVE_FILE)
	(if (xref-yes-or-no-window cc t dispatch-data)
		(progn
		  ;; do not kill buffer really, this would make undo impossible
		  ;; rather clear it
		  (erase-buffer)
		  (set-buffer-modified-p nil)
		  ;; and really delete the file
		  (xref-delete-file (buffer-file-name))
		  (bury-buffer)
		  ))
	i
))

(defun xref-server-dispatch-no-project (ss i len dispatch-data)
  (let ((tlen) (cc))
	(setq tlen (xref-server-dispatch-get-int-attr PPCA_LEN))
	(setq cc (xref-char-list-substring ss i (+ i tlen)))
	(setq i (+ i tlen))
	(setq i (xref-server-parse-xml-tag ss i len))
	(xref-server-dispatch-require-end-ctag PPC_NO_PROJECT)
	(if (xref-yes-or-no-window 
		 (format "No project covers %s,\ncreate new project?" cc)
		 t dispatch-data)
		(progn
		  (xref-project-new)
		  ;; a non local exit
		  (error "Done.")
		  )
	  (error "Exiting")
	  )
	i
))

(defvar xref-confirmation-dialog-map (make-sparse-keymap "Xref confirmation"))
(define-key xref-confirmation-dialog-map [up] 'xref-modal-dialog-previous-line)
(define-key xref-confirmation-dialog-map [down] 'xref-modal-dialog-next-line)
(define-key xref-confirmation-dialog-map [kp-up] 'xref-modal-dialog-previous-line)
(define-key xref-confirmation-dialog-map [kp-down] 'xref-modal-dialog-next-line)
(define-key xref-confirmation-dialog-map "\C-p" 'xref-modal-dialog-previous-line)
(define-key xref-confirmation-dialog-map "\C-n" 'xref-modal-dialog-next-line)
(define-key xref-confirmation-dialog-map xref-escape-key-sequence 'xref-modal-dialog-exit)
(define-key xref-confirmation-dialog-map "\C-g" 'xref-modal-dialog-exit)
(define-key xref-confirmation-dialog-map "q" 'xref-modal-dialog-exit)
(define-key xref-confirmation-dialog-map [f7] 'xref-modal-dialog-exit)
(define-key xref-confirmation-dialog-map [newline] 'xref-modal-dialog-continue)
(define-key xref-confirmation-dialog-map [return] 'xref-modal-dialog-continue)
(define-key xref-confirmation-dialog-map "\C-m" 'xref-modal-dialog-continue)
(define-key xref-confirmation-dialog-map "?" 'xref-confirmation-dialog-help)

(defun xref-confirmation-dialog-help (event)
  (interactive "i")
  (xref-interactive-help
"Special hotkeys available:

\\[xref-modal-dialog-continue] \t-- select
\\[xref-modal-dialog-exit] \t-- cancel
\\[xref-confirmation-dialog-help] \t-- this help
" nil nil)
)

(defun xref-yes-or-no-window (message default dispatch-data)
  (let ((sw) (sel) (yes-line) (default-line) (res))
	(setq sw (selected-window))
	(xref-select-dispach-data-caller-window dispatch-data)
	(setq yes-line (+ (xref-char-count ?\n message) 4))
	(if default
		(setq default-line yes-line)
	  (setq default-line (1+  yes-line))
	  )
	(setq sel (xref-modal-dialog xref-confirmation-modal-buffer (format
"%s

---
Yes.
No.
---
" message)
    default-line 0 nil xref-confirmation-dialog-map dispatch-data))
	(select-window sw)
	(setq res (eq sel yes-line))
	res
))

(defun xref-server-dispatch-ask-confirmation (ss i len dispatch-data tag)
  (let ((tlen) (cc) (conf))
	(setq tlen (xref-server-dispatch-get-int-attr PPCA_LEN))
	(setq cc (xref-char-list-substring ss i (+ i tlen)))
	(setq i (+ i tlen))
	(setq i (xref-server-parse-xml-tag ss i len))
	(xref-server-dispatch-require-end-ctag tag)
	(setq conf (xref-yes-or-no-window cc t dispatch-data))
	(if (not conf)
		(error "exiting")
	  )
	i
))

(defun xref-server-dispatch-bottom-information (ss i len dispatch-data tag)
  (let ((tlen) (cc) (beep))
	(setq beep (xref-server-dispatch-get-int-attr PPCA_BEEP))
	(setq tlen (xref-server-dispatch-get-int-attr PPCA_LEN))
	(setq cc (xref-char-list-substring ss i (+ i tlen)))
	(setq i (+ i tlen))
	(setq i (xref-server-parse-xml-tag ss i len))
	(xref-server-dispatch-require-end-ctag tag)
	(message "%s" cc)
	(if (not (equal beep 0)) (beep t))
	i
))

(defun xref-server-dispatch-insert-completion (cc )
  (let ((cb) (i) (len) (id) (ccc))
	(setq cb (current-buffer))
   	(xref-select-dispach-data-caller-window xref-completions-dispatch-data)
	(xref-insert-completion cc)
	(set-buffer cb)
))

(defun xref-server-dispatch-single-completion (ss i len dispatch-data)
  (let ((tlen) (cc))
	(setq tlen (xref-server-dispatch-get-int-attr PPCA_LEN))
	(setq cc (xref-char-list-substring ss i (+ i tlen)))
	(setq i (+ i tlen))
	(xref-select-dispach-data-caller-window dispatch-data)
	(xref-server-dispatch-insert-completion cc)
	(setq i (xref-server-parse-xml-tag ss i len))
	(xref-server-dispatch-require-end-ctag PPC_SINGLE_COMPLETION)
	i
))

(defun xref-hard-prepend-to-dispatch-data (dispatch-data new-data)
  (let ((p) (dd))
	(if (not dispatch-data)
		(error "[xref] appending to empty dispatch-data, internal error")
	  )
;;(message "hard append: %S to %S" new-data dispatch-data)
	(setq dd (cdr dispatch-data))
	(if new-data
		(progn
		  (setcdr dispatch-data new-data)
		  (setq p dispatch-data)
		  (while (cdr p) (setq p (cdr p)))
		  (setcdr p dd)
		  ))
))

(defun xref-server-dispatch-set-info (ss i len dispatch-data)
  (let ((tlen) (cc))
	(setq tlen (xref-server-dispatch-get-int-attr PPCA_LEN))
	(setq cc (xref-char-list-substring ss i (+ i tlen)))
	(setq i (+ i tlen))
	(setq i (xref-server-parse-xml-tag ss i len))
	(xref-server-dispatch-require-end-ctag PPC_SET_INFO)
	(xref-hard-prepend-to-dispatch-data dispatch-data (cons (cons 'info cc) nil))
	i
))

(defun xref-server-dispatch-browse-url (ss i len dispatch-data)
  (let ((tlen) (url))
	(setq tlen (xref-server-dispatch-get-int-attr PPCA_LEN))
	(setq url (xref-char-list-substring ss i (+ i tlen)))
	(setq i (+ i tlen))
	(setq i (xref-server-parse-xml-tag ss i len))
	(xref-server-dispatch-require-end-ctag PPC_BROWSE_URL)
	(xref-browse-url url)
	i
))

(defun xref-server-dispatch-fqt-complete (import cc dispatch-data)
  (let ((iline-val) (iline))
	(if import
		(progn
		  (xref-select-dispach-data-caller-window dispatch-data)
		  (xref-server-call-on-current-buffer-no-saves 
		   "-olcxtrivialprecheck -getlastimportline" 
		   dispatch-data)
		  (setq iline (cdr (assoc 'info dispatch-data)))
		  (setq iline-val (string-to-int iline))
		  (save-excursion
			(goto-line (+ iline-val 1))
			(beginning-of-line)
			;; check if after package, insert on blank line
			(if (not (bobp))
				(progn
				  (forward-line -1)
				  (if (equal (xref-get-identifier-on-point) "package")
					  (progn
						(forward-line 1)
						(newline)
						)
					(forward-line 1)
					)))
			(insert (format "import %s;\n" import))
			(sit-for 1)
			)))
	(xref-server-dispatch-insert-completion cc)
))

(defvar xref-fqt-completion-dialog-map (make-sparse-keymap "Xref fqt completion"))
(xref-add-basic-modal-keybindings xref-fqt-completion-dialog-map)

(defun xref-server-dispatch-fqt-completion (ss i len dispatch-data)
  (let ((tlen) (cc) (pack) (cl) (sel))
	(setq tlen (xref-server-dispatch-get-int-attr PPCA_LEN))
	(setq cc (xref-char-list-substring ss i (+ i tlen)))
	(setq i (+ i tlen))
	(setq i (xref-server-parse-xml-tag ss i len))
	(xref-server-dispatch-require-end-ctag PPC_FQT_COMPLETION)
	(setq cl (xref-file-name-extension cc))
	(setq pack (xref-file-name-without-suffix cc))
	(setq sel (xref-modal-dialog xref-selection-modal-buffer (format
"Fully qualified type completion will:
----
 1.) Import %s.* and complete %s
 2.) Import %s.%s and complete %s
 3.) Complete %s.%s
----
" pack cl pack cl cl pack cl)
					   3 0 nil xref-fqt-completion-dialog-map dispatch-data))
	(cond 
	 (
	  (eq sel 3)
	  (xref-server-dispatch-fqt-complete (format "%s.*" pack) cl dispatch-data))
	 (
	  (eq sel 4)
	  (xref-server-dispatch-fqt-complete (format "%s.%s" pack cl) cl dispatch-data))
	 (
	  (eq sel 5)
	  (xref-server-dispatch-fqt-complete nil (format "%s.%s" pack cl) dispatch-data))
	  )
	i
))

(defvar xref-refactorings-dialog-map (make-sparse-keymap "Xref refactorings"))
(define-key xref-refactorings-dialog-map [up] 'xref-modal-dialog-previous-line)
(define-key xref-refactorings-dialog-map [down] 'xref-modal-dialog-next-line)
(define-key xref-refactorings-dialog-map [kp-up] 'xref-modal-dialog-previous-line)
(define-key xref-refactorings-dialog-map [kp-down] 'xref-modal-dialog-next-line)
(define-key xref-refactorings-dialog-map xref-escape-key-sequence 'xref-modal-dialog-exit)
(define-key xref-refactorings-dialog-map "\C-g" 'xref-modal-dialog-exit)
(define-key xref-refactorings-dialog-map "q" 'xref-modal-dialog-exit)
(define-key xref-refactorings-dialog-map [f7] 'xref-modal-dialog-exit)
(define-key xref-refactorings-dialog-map [newline] 'xref-modal-dialog-continue)
(define-key xref-refactorings-dialog-map [return] 'xref-modal-dialog-continue)
(define-key xref-refactorings-dialog-map "\C-m" 'xref-modal-dialog-continue)
(define-key xref-refactorings-dialog-map "?" 'xref-refactorings-dialog-help)

(defvar xref-info-dialog-map (make-sparse-keymap "Xref refactorings"))
(define-key xref-info-dialog-map [newline] 'xref-modal-dialog-continue)
(define-key xref-info-dialog-map [return] 'xref-modal-dialog-continue)
(define-key xref-info-dialog-map "\C-m" 'xref-modal-dialog-continue)

(defun xref-get-line-content ()
  (let ((res) (bb) (pos))
	(setq pos (point))
	(beginning-of-line)
	(setq bb (point))
	(end-of-line)
	(setq res (buffer-substring bb (point)))
	(goto-char pos)
	res
))

(defun xref-refactorings-dialog-help (event)
  (interactive "i")
  (let ((crf) (rr) (r) (doc))
	(setq crf (xref-get-line-content))
;;(message "looking for %s" crf)
	(setq rr xref-refactorings-assoc-list)
	(setq r nil)
	(while rr
	  (setq r (car rr))
	  (if (equal (car (cdr r)) crf)
		  (progn
			(setq doc r)
			))
	  (setq rr (cdr rr))
	  )
	(xref-interactive-help (format
"Select a refactoring to perform.
Special hotkeys available:

\\[xref-modal-dialog-continue] \t-- select
\\[xref-modal-dialog-exit] \t-- cancel
\\[xref-refactorings-dialog-help] \t-- this help
\n\n\n%s
" (xref-refactoring-documentation))
						   (format "%s:" crf)
						   (list 
							(list "^\\*[a-zA-Z0-9 ]*:" 'bold)
							(list "^[a-zA-Z0-9 ]*:" 'bold-italic)
							(list "--*-" 'xref-list-symbol-face)
							;;(list (xref-keywords-regexp) 'xref-keyword-face)
							)
						   )
))


(defun xref-server-dispatch-available-refactorings (ss i len dispatch-data)
  (let ((code) (selectedref) (srefn) (selectedline) (refs) (menu) 
		(tlen) (cc) (rfirst-line 3) (dd2))
	(setq refs nil)
	;; unique completion window in all frames
	(setq menu "Select action to perform:\n---")
	(setq i (xref-server-parse-xml-tag ss i len))
	(while (equal xref-server-ctag PPC_INT_VALUE)	  
	  (setq code (xref-server-dispatch-get-int-attr PPCA_VALUE))
	  (setq tlen (xref-server-dispatch-get-int-attr PPCA_LEN))
	  (setq menu (format "%s\n%s" menu (car (cdr (assoc code xref-refactorings-assoc-list)))))
	  (setq cc (xref-char-list-substring ss i (+ i tlen)))
	  (setq i (+ i tlen))
	  (setq refs (append refs (list (list code cc))))
	  (setq i (xref-server-parse-xml-tag ss i len))
	  (xref-server-dispatch-require-end-ctag PPC_INT_VALUE)
	  (setq i (xref-server-parse-xml-tag ss i len))
	  )
	(xref-server-dispatch-require-end-ctag PPC_AVAILABLE_REFACTORINGS)
	(setq menu (format "%s\n---\n" menu))
	(setq selectedline (xref-modal-dialog xref-selection-modal-buffer menu rfirst-line 0 nil xref-refactorings-dialog-map dispatch-data))
	(setq srefn (elt refs (- selectedline rfirst-line)))
	(setq selectedref (assoc (car srefn) xref-refactorings-assoc-list))
	(apply (elt selectedref 2) srefn nil)
	i
))

(defun xref-server-dispatch-create-and-set-completions-buffer (dispatch-data)
  (let ((cid))
	(xref-select-dispach-data-caller-window dispatch-data)
	(setq cid (xref-get-identifier-before-point))
	(setq xref-completion-id-before-point cid)
	(setq xref-completion-id-after-point (xref-get-identifier-after (point)))
	(setq xref-completion-auto-search-list (list cid (+ (length cid) 1)))
	(set-marker xref-completion-marker (- (point) (length cid)))
	(xref-display-and-set-new-dialog-window xref-completions-buffer nil t)
	(buffer-disable-undo xref-completions-buffer)
	(setq xref-this-buffer-type 'completion)
))

(defun xref-server-dispatch-show-completions-buffer (focus dispatch-data)
  (xref-auto-search-search (car xref-completion-auto-search-list))
  (xref-use-local-map xref-completion-mode-map)
  (setq buffer-read-only t)
  (xref-appropriate-window-height nil t)
  (if (and focus xref-completion-auto-focus)
	  (message xref-standard-help-message)
	(xref-select-dispach-data-caller-window dispatch-data)
	)
)

(defun xref-reset-or-increment-completion-windows-counter ()
  (let ((win))
	(setq win (get-buffer-window xref-completions-buffer))
	(if (and win (windowp win) (window-live-p win))
		(setq xref-completions-windows-counter (1+ xref-completions-windows-counter))
	  (setq xref-completions-windows-counter 0)
	  )
))

(defun xref-server-dispatch-completions (ss i len dispatch-data)
  ;; this is old way of passing compeltions requiring parsing of each
  ;; completion and hence too slow
  (let ((tlen) (cc) (nofocus))
	  ;; unique completion window in all frames
	(setq nofocus (xref-server-dispatch-get-int-attr PPCA_NO_FOCUS))
	(xref-reset-or-increment-completion-windows-counter)
	(xref-delete-window-in-any-frame xref-completions-buffer nil t)
	(xref-server-dispatch-create-and-set-completions-buffer dispatch-data)
	(setq xref-this-buffer-dispatch-data dispatch-data)
	(setq i (xref-server-parse-xml-tag ss i len))
	(while (equal xref-server-ctag PPC_MULTIPLE_COMPLETION_LINE)	  
	  (setq tlen (xref-server-dispatch-get-int-attr PPCA_LEN))
	  (setq cc (xref-char-list-substring ss i (+ i tlen)))
	  (setq i (+ i tlen))
	  (insert-string cc)
	  (newline)
	  (setq i (xref-server-parse-xml-tag ss i len))
	  (xref-server-dispatch-require-end-ctag PPC_MULTIPLE_COMPLETION_LINE)
	  (setq i (xref-server-parse-xml-tag ss i len))
	  )
	(xref-server-dispatch-require-end-ctag PPC_MULTIPLE_COMPLETIONS)
	(xref-line-hightlight 0 (point-max) nil 1 xref-font-lock-compl-keywords t)
	(goto-char (point-min))
	(xref-server-dispatch-show-completions-buffer (eq nofocus 0) dispatch-data)
	i
))

(defun xref-server-dispatch-all-completions (ss i len dispatch-data)
  (let ((tlen) (cc) (nofocus))
	  ;; unique completion window in all frames
	(xref-reset-or-increment-completion-windows-counter)
	(xref-delete-window-in-any-frame xref-completions-buffer nil t)
	(xref-server-dispatch-create-and-set-completions-buffer dispatch-data)
	(setq xref-this-buffer-dispatch-data dispatch-data)
	(setq nofocus (xref-server-dispatch-get-int-attr PPCA_NO_FOCUS))
	(setq tlen (xref-server-dispatch-get-int-attr PPCA_LEN))
	(setq cc (xref-char-list-substring ss i (+ i tlen)))
	(setq i (+ i tlen))
	(insert-string cc)
	(setq i (xref-server-parse-xml-tag ss i len))
	(xref-server-dispatch-require-end-ctag PPC_ALL_COMPLETIONS)
	(xref-line-hightlight 0 (point-max) nil 1 xref-font-lock-compl-keywords t)
	(goto-char (point-min))
	(xref-server-dispatch-show-completions-buffer (eq nofocus 0) dispatch-data)
	i
))

(defun xref-server-dispatch-goto (ss i len dispatch-data)
  (let ((tlen) (path) (offset) (line) (col) (rpos))
	(setq i (xref-server-parse-xml-tag ss i len))
	(setq tlen (xref-server-dispatch-get-int-attr PPCA_LEN))
	(setq path (xref-char-list-substring ss i (+ i tlen)))
	(setq i (+ i tlen))
	(if (equal xref-server-ctag PPC_OFFSET_POSITION)
		(progn
		  (setq offset (xref-server-dispatch-get-int-attr PPCA_OFFSET))
		  (xref-select-dispach-data-caller-window dispatch-data)
		  (xref-find-file path)
		  (goto-char (+ offset 1))
		  (setq i (xref-server-parse-xml-tag ss i len))
		  (xref-server-dispatch-require-end-ctag PPC_OFFSET_POSITION)
		  )
	  (xref-server-dispatch-require-ctag PPC_LC_POSITION)
	  (setq line (xref-server-dispatch-get-int-attr PPCA_LINE))
	  (setq col (xref-server-dispatch-get-int-attr PPCA_COL))
	  (xref-select-dispach-data-caller-window dispatch-data)
	  (xref-find-file path)
	  (goto-line line)
	  (beginning-of-line)
	  (setq rpos (+ (point) col))
	  (end-of-line)
	  (if (> rpos (point))
		  (setq rpos (point))
		)
	  (goto-char rpos)
	  (setq i (xref-server-parse-xml-tag ss i len))
	  (xref-server-dispatch-require-end-ctag PPC_LC_POSITION)
	  )
	(setq i (xref-server-parse-xml-tag ss i len))
	(xref-server-dispatch-require-end-ctag PPC_GOTO)
	i
))

(defun xref-server-dispatch-update-report (ss i len dispatch-data)
  (let ((tlen) (cc))
	(setq i (xref-server-parse-xml-tag ss i len))
	(while (and (< i len) (not (equal xref-server-ctag (format "/%s" PPC_UPDATE_REPORT))))
	  (setq tlen (xref-server-dispatch-get-int-attr PPCA_LEN))
	  (if (equal xref-server-ctag PPC_FATAL_ERROR)
		  (xref-server-dispatch-error ss i len dispatch-data PPC_FATAL_ERROR)
		)
	  (setq i (+ i tlen))
	  (setq i (xref-server-parse-xml-tag ss i len))
	  )
	(xref-server-dispatch-require-end-ctag PPC_UPDATE_REPORT)
	i
))

(defun xref-server-dispatch-parse-string-value (ss i len dispatch-data)
  (let ((tlen))
	(setq i (xref-server-parse-xml-tag ss i len))
	(xref-server-dispatch-require-ctag PPC_STRING_VALUE)
	(setq tlen (xref-server-dispatch-get-int-attr PPCA_LEN))
	(setq xref-server-cstring-value (xref-char-list-substring ss i (+ i tlen)))
	(setq i (+ i tlen))
	(setq i (xref-server-parse-xml-tag ss i len))
	(xref-server-dispatch-require-end-ctag PPC_STRING_VALUE)
	i
))

(defun xref-server-dispatch-replacement (ss i len dispatch-data)
  (let ((tlen) (str) (with) (slen) (rcc))
	(setq i (xref-server-dispatch-parse-string-value ss i len dispatch-data))
	(setq str xref-server-cstring-value)
	(setq i (xref-server-dispatch-parse-string-value ss i len dispatch-data))
	(setq with xref-server-cstring-value)
	(setq i (xref-server-parse-xml-tag ss i len))
	(xref-server-dispatch-require-end-ctag PPC_REFACTORING_REPLACEMENT)
	(setq slen (length str))
	(xref-select-dispach-data-caller-window dispatch-data)
	(setq rcc (buffer-substring (point) (+ (point) slen)))
	(if (not (equal str rcc))
		(error "[internal error] '%s' expected here" str)
	  )
	(xref-make-buffer-writable)
	(delete-char slen)
	(insert with)
	i
))

(defun xref-server-dispatch-refactoring-cut-block (ss i len dispatch-data)
  (let ((blen))
	(setq blen (xref-server-dispatch-get-int-attr PPCA_VALUE))
	(setq xref-refactoring-block (buffer-substring (point) (+ (point) blen)))
	(delete-char blen t)
	(setq i (xref-server-parse-xml-tag ss i len))
	(xref-server-dispatch-require-end-ctag PPC_REFACTORING_CUT_BLOCK)
	i
))

(defun xref-server-dispatch-refactoring-paste-block (ss i len dispatch-data)
  (insert-string xref-refactoring-block)
  (setq i (xref-server-parse-xml-tag ss i len))
  (xref-server-dispatch-require-end-ctag PPC_REFACTORING_PASTE_BLOCK)
  i
)

(defun xref-server-dispatch-indent (ss i len dispatch-data)
  (let ((blen) (ii) (bb))
	(setq blen (xref-server-dispatch-get-int-attr PPCA_VALUE))
	(if (fboundp 'indent-region)
		(progn
		  (save-excursion
			(setq bb (point))
			(forward-line blen)
			(indent-region bb (point) nil)
			)))
	(setq i (xref-server-parse-xml-tag ss i len))
	(xref-server-dispatch-require-end-ctag PPC_INDENT)
	i
))

(defun xref-server-dispatch-precheck (ss i len dispatch-data)
  (let ((tlen) (cc) (rcc))
	(setq tlen (xref-server-dispatch-get-int-attr PPCA_LEN))
	(setq cc (xref-char-list-substring ss i (+ i tlen)))
	(setq i (+ i tlen))
	(xref-select-dispach-data-caller-window dispatch-data)
	(setq rcc (buffer-substring (point) (+ (point) tlen)))
	;; (message "prechecking '%s' <-> '%s'" cc rcc)
	(if (not (equal cc rcc))
		(error "[error] '%s' expected here" cc)
	  )
	(setq i (xref-server-parse-xml-tag ss i len))
	(xref-server-dispatch-require-end-ctag PPC_REFACTORING_PRECHECK)
	i
))

(defun xref-create-browser-main-window (buff split-horizontal docking-top-left dispatch-data)
  (let ((sw) (nw) (res))
	(if docking-top-left
		(progn
		  (setq sw (selected-window))
		  (setq nw (xref-display-and-set-new-dialog-window buff split-horizontal nil))
		  ;; exchange caller window in dispatch-data
		  (xref-reset-cdr 'caller-window sw nw dispatch-data)
		  (xref-reset-all-windows-caller-window sw nw)
		  (xref-set-window-width xref-class-tree-window-width)
		  (setq res sw)
		  )
	  (setq nw (xref-display-and-set-new-dialog-window buff split-horizontal t))
	  (setq res nw)
	  )
	res
))

(defun xref-cr-new-symbol-resolution-window (force-bottom-position dispatch-data)
  (let ((res))
	(if force-bottom-position
		(setq res (xref-create-browser-main-window (xref-new-symbol-resolution-buffer) 
												   nil nil
												   dispatch-data))
	  (setq res (xref-create-browser-main-window (xref-new-symbol-resolution-buffer) 
												 xref-browser-splits-window-horizontally
												 xref-browser-position-left-or-top
												 dispatch-data))
	  )
	res
))

(defun xref-cr-new-references-window (force-bottom-position)
  (let ((res) (sw))
	(if force-bottom-position
		(setq res (xref-display-and-set-new-dialog-window (xref-new-references-buffer) 
														  t t))
	  (setq res (xref-display-and-set-new-dialog-window (xref-new-references-buffer) 
														(not xref-browser-splits-window-horizontally)
														t))
	  )
	;; scrolling in Emacs/XEmacs is mysterious. Set up some values
	; (setq sw (selected-window))
	; (select-window res)
	; (setq scroll-margin 2)
	; (setq scroll-conservatively 1)
	; (setq scroll-step 1)
	; (select-window sw)
	;;
	res
))

(defun xref-valid-window (win bnameprefix)
  (let ((res))
	(setq res nil)
	(if (and win (windowp win) (window-live-p win))
	  (progn
		(if (xref-string-has-prefix (buffer-name (window-buffer win)) 
									 bnameprefix nil)
			(setq res t)
		  )))
	res
))


(defun xref-clean-unused-browser-buffers ()
  (let ((bl) (bb) (bn) (win))
	(setq bl (buffer-list))
	(while bl
	  (setq bb (car bl))
	  (setq bn (buffer-name bb))
	  (if (or (xref-string-has-prefix bn xref-symbol-resolution-buffer nil)
			  (xref-string-has-prefix bn xref-references-buffer nil))
		  (progn
			(setq win (get-buffer-window bb t))
			(if (or (not win) (not (windowp win)) (not (window-live-p win)))
				(kill-buffer bb)
			  )))
	  
	  (setq bl (cdr bl))
	  )
))

(defun xref-create-browser-windows (refactoring-resolution-flag dispatch-data)
  (let ((resolvewin) (listwin) (oldwins) (frame-id) (new-dispatch-data) (frw))
	(if (xref-get-this-frame-dispatch-data)
		(progn
		  (setq resolvewin (cdr (assoc 'linked-resolution-window (xref-get-this-frame-dispatch-data))))
		  (setq listwin (cdr (assoc 'linked-refs-window (xref-get-this-frame-dispatch-data))))
		  (cond
		   (
			(and (xref-valid-window resolvewin xref-symbol-resolution-buffer)
				 (xref-valid-window listwin xref-references-buffer))
			;; do nothing as both windows exists
			nil
			)
		   (
			(xref-valid-window resolvewin xref-symbol-resolution-buffer)
			;; only symbol window exists
			(select-window resolvewin)
			(setq listwin (xref-cr-new-references-window refactoring-resolution-flag))
			)
		   (
			(xref-valid-window listwin xref-references-buffer)
			;; only reference window exists, hmmm.
			(setq resolvewin listwin)
			(select-window resolvewin)
			(rename-buffer (xref-new-symbol-resolution-buffer))
			(setq listwin (xref-cr-new-references-window refactoring-resolution-flag))
			)
		   (t
			(setq resolvewin (xref-cr-new-symbol-resolution-window refactoring-resolution-flag dispatch-data))
			(setq listwin (xref-cr-new-references-window refactoring-resolution-flag))
			)))
	  (setq resolvewin (xref-cr-new-symbol-resolution-window refactoring-resolution-flag dispatch-data))
	  (setq listwin (xref-cr-new-references-window refactoring-resolution-flag))
	  )
	;; maybe I should erase them here, but check before that 
	;; buffer is the one created by xref, (user may load his source here)!!
	(select-window listwin)
	(setq xref-this-buffer-type 'reference-list)
	(setq xref-this-buffer-dispatch-data dispatch-data)
	(xref-use-local-map xref-browser-dialog-refs-key-map)
	(select-window resolvewin)
	(setq xref-this-buffer-type 'symbol-list)
	(setq xref-this-buffer-dispatch-data dispatch-data)
	(xref-use-local-map xref-browser-dialog-key-map)
	;; add information about windows to dispatch-data
	;; big hack, if there is some problem with browser/refactoring
	;; windows, the problem is probably somewhere here!
	(setq frame-id (xref-get-this-frame-id))
	(if refactoring-resolution-flag
		(progn
		  (xref-set-this-frame-dispatch-data
		   (cons (cons 'linked-refactoring-window listwin)
				 (xref-get-this-frame-dispatch-data)))
		  ))
	(xref-hard-prepend-to-dispatch-data 
	 dispatch-data 
	 (cons (cons 'linked-refs-window listwin)
		   (cons (cons 'linked-resolution-window resolvewin)
				 (cons (cons 'frame-id frame-id)
					   nil))))
	(if (not refactoring-resolution-flag)
		(progn
		  (setq frw (xref-is-failed-refactoring-window-displayed))
		  (if frw
			  (xref-hard-prepend-to-dispatch-data 
			   dispatch-data
			   (cons (cons 'linked-refactoring-window frw) nil)
			   ))
		  (xref-set-this-frame-dispatch-data dispatch-data)
	  ))
	oldwins
))

(defun xref-appropriate-other-window-height (win aplus aminus)
  (let ((sw))
	(setq sw (selected-window))
	(select-window win)
	(xref-appropriate-window-height nil t)
	(select-window sw)
))

(defun xref-server-dispatch-display-resolution (ss i len dispatch-data)
  (let ((tlen) (mess) (messagewin) (winconfig) (fdd))
	(setq tlen (xref-server-dispatch-get-int-attr PPCA_LEN))
	(setq mess (xref-char-list-substring ss i (+ i tlen)))
	(setq i (+ i tlen))
	(setq i (xref-server-parse-xml-tag ss i len))
	(xref-server-dispatch-require-end-ctag PPC_DISPLAY_RESOLUTION)
	(xref-select-dispach-data-caller-window dispatch-data)
	;; this window excursion causes that after reporting missinterpreted references
	;; the windows are lost for further browsing, try it differently
	(setq fdd (xref-get-this-frame-dispatch-data))
	(setq winconfig (current-window-configuration))
	  (delete-other-windows)
	  (setq messagewin (xref-display-and-set-new-dialog-window xref-browser-info-buffer nil t))
	  (insert-string mess)
	  (insert-string xref-resolution-dialog-explication)
	  (goto-char (point-min))
	  (xref-create-browser-windows t dispatch-data)
	  (xref-browser-dialog-set-new-filter dispatch-data)
	  (xref-appropriate-browser-windows-sizes nil)
	  (xref-appropriate-other-window-height messagewin t t)
	  (xref-modal-dialog-loop xref-browser-dialog-key-map "<enter> - continue; q - quit modal dialog; ? - help")
	  (xref-close-resolution-dialog-windows dispatch-data)
	  (sit-for 0.1)
	(set-window-configuration winconfig)
	(xref-set-this-frame-dispatch-data fdd)
	;; send nothing to continue
	(xref-send-data-to-process-and-dispatch "-continuerefactoring" dispatch-data nil)
	i
))

(defun xref-server-dispatch-display-or-update-browser (ss i len dispatch-data)
  (let ((tlen) (mess) (messagewin))
	(setq tlen (xref-server-dispatch-get-int-attr PPCA_LEN))
	(setq mess (xref-char-list-substring ss i (+ i tlen)))
	(setq i (+ i tlen))
	(setq i (xref-server-parse-xml-tag ss i len))
	(xref-server-dispatch-require-end-ctag PPC_DISPLAY_OR_UPDATE_BROWSER)
	(xref-select-dispach-data-caller-window dispatch-data)
	(xref-create-browser-windows nil dispatch-data)
	;; do not update content, it is done automatically after
	;;(xref-browser-dialog-set-new-filter dispatch-data)
	(message mess)
	i
))

(defun xref-server-dispatch-classh-lines (ss i len dispatch-data ct-flag)
  (let ((tlen) (clas) (deps) (selected) (rd) (ru) (prefix) (sch) 
		(defc) (interfc) (bb) (fc) (upp))
	(setq i (xref-server-parse-xml-tag ss i len))
	(while (equal xref-server-ctag PPC_CLASS)
	  (setq tlen (xref-server-dispatch-get-int-attr PPCA_LEN))
	  (setq clas (xref-char-list-substring ss i (+ i tlen)))
	  (setq i (+ i tlen))
	  (setq deps (cdr (assoc PPCA_TREE_DEPS xref-server-ctag-attributes))) 
	  (setq selected (xref-server-dispatch-get-int-attr PPCA_SELECTED))
	  (setq rd (xref-server-dispatch-get-int-attr PPCA_DEF_REFN))
	  (setq ru (xref-server-dispatch-get-int-attr PPCA_REFN))
	  (setq defc (xref-server-dispatch-get-int-attr PPCA_DEFINITION))
	  (setq interfc (xref-server-dispatch-get-int-attr PPCA_INTERFACE))
	  (setq upp (xref-server-dispatch-get-int-attr PPCA_TREE_UP))
	  (if (eq selected 1) (setq sch "+") (setq sch " "))
	  (if ct-flag
		  (setq prefix "")
		(setq prefix (format "%s  %s  " sch (xref-references-counts rd ru)))
		)
	  (insert (format "\n%s%s" prefix deps))
	  (if (eq upp 1) (insert "("))
	  (setq bb (point))
	  (insert clas)
	  (setq fc nil)
	  (if (eq defc 1) 
		   (if (eq interfc 1) 
			   (setq fc 'bold-italic)
			 (setq fc 'bold)
			 )
		(if (eq interfc 1) 
			(setq fc 'italic)
		  ))
	  (if fc (put-text-property bb (point) 'face fc))
	  (if (eq upp 1) (insert ")"))
	  (setq i (xref-server-parse-xml-tag ss i len))
	  (xref-server-dispatch-require-end-ctag PPC_CLASS)
	  (setq i (xref-server-parse-xml-tag ss i len))
	  )
	i
))

(defun xref-server-dispatch-symbol-resolution (ss i len dispatch-data)
  (let ((tlen) (symbol) (clas) (indent) (deps) (ru) (rd) 
		(selected) (sch) (divnewline) (firstline))
	(setq divnewline "")
	(setq firstline 0)
	(xref-select-dispach-data-resolution-window dispatch-data)
	(setq buffer-read-only nil)
	(xref-erase-buffer)
	(setq i (xref-server-parse-xml-tag ss i len))
	(while (or (equal xref-server-ctag PPC_VIRTUAL_SYMBOL)
			   (equal xref-server-ctag PPC_SYMBOL))
	  (setq tlen (xref-server-dispatch-get-int-attr PPCA_LEN))
	  (setq symbol (xref-char-list-substring ss i (+ i tlen)))
	  (setq i (+ i tlen))
	  (if (equal xref-server-ctag PPC_SYMBOL)
		  (progn
			(setq divnewline "\n")
			(setq selected (xref-server-dispatch-get-int-attr PPCA_SELECTED))
			(setq rd (xref-server-dispatch-get-int-attr PPCA_DEF_REFN))
			(setq ru (xref-server-dispatch-get-int-attr PPCA_REFN))
			(if (equal selected 1) (setq sch "+") (setq sch " "))
			(insert (format "%s%s  %s  %s" divnewline sch (xref-references-counts rd ru) symbol))
			(if (<= firstline 0) (setq firstline (count-lines 1 (point))))
			(setq i (xref-server-parse-xml-tag ss i len))
			(xref-server-dispatch-require-end-ctag PPC_SYMBOL)
			(setq i (xref-server-parse-xml-tag ss i len))
			)
		(insert (format "%s\n== %s" divnewline symbol))
		(if (<= firstline 0) (setq firstline (+ (count-lines 1 (point)) 1)))
		(setq i (xref-server-parse-xml-tag ss i len))
		(xref-server-dispatch-require-end-ctag PPC_VIRTUAL_SYMBOL)
		(setq i (xref-server-dispatch-classh-lines ss i len dispatch-data nil))
		)
	  (setq divnewline "\n")
	  )
	(xref-server-dispatch-require-end-ctag PPC_SYMBOL_RESOLUTION)
	(xref-line-hightlight 0 (point-max) nil 1 nil nil)
	(goto-char (point-min))
	(if (> firstline 0) (goto-line firstline))
	(setq buffer-read-only t)
	i
))

(defun xref-reset-cdr (attr oldval newval dispatch-data)
  (let ((ss))
	(setq ss (assoc attr dispatch-data))
	(if (and ss (equal (cdr ss) oldval))
		(setcdr ss newval)
	  )
))

(defun xref-reset-all-windows-caller-window (old-caller-window new-caller-window)
  (let ((sw) (loop))
	(setq sw (selected-window))
	(setq loop t)
	(while loop
	  (if xref-this-buffer-dispatch-data
		  (xref-reset-cdr 'caller-window 
						  old-caller-window 
						  new-caller-window 
						  xref-this-buffer-dispatch-data)
		)
	  (other-window 1)
	  (setq loop (not (equal sw (selected-window))))
	  )
	(if (xref-get-this-frame-dispatch-data)
		(xref-reset-cdr 'caller-window 
						old-caller-window 
						new-caller-window 
						(xref-get-this-frame-dispatch-data))
	  )
))

(defun xref-server-dispatch-display-class-tree (ss i len dispatch-data)
  (let ((nw) (sw))
	(xref-delete-class-tree-window)
	(xref-select-dispach-data-caller-window dispatch-data)
	(xref-create-browser-main-window xref-class-tree-buffer 
									 xref-class-tree-splits-window-horizontally
									 xref-class-tree-position-left-or-top
									 dispatch-data)
	(setq buffer-read-only nil)
	(xref-erase-buffer)
	(setq i (xref-server-dispatch-classh-lines ss i len dispatch-data t))
	(xref-server-dispatch-require-end-ctag PPC_DISPLAY_CLASS_TREE)
	(xref-line-hightlight 0 (point-max) nil 3 nil nil)
	(goto-line 1)
	(setq buffer-read-only t)
	(if (not xref-class-tree-splits-window-horizontally)
		(xref-appropriate-window-height nil t)
	  )
	(setq xref-this-buffer-dispatch-data dispatch-data)
	(xref-use-local-map xref-class-tree-mode-map)
	i
))

(defun xref-server-dispatch-reference-list (ss i len dispatch-data)
  (let ((tlen) (srcline) (n) (divnewline) (aline) (line) (j) (pointer)
		(listed-symbol))
	(setq j 0)
	(setq divnewline "")
	(setq aline (xref-server-dispatch-get-int-attr PPCA_VALUE))
	(setq listed-symbol (cdr (assoc PPCA_SYMBOL xref-server-ctag-attributes)))
	(if (equal listed-symbol "")
		(setq listed-symbol nil)
	  )
	(setq line 0)
	(xref-select-dispach-data-refs-window dispatch-data)
	(setq buffer-read-only nil)
	(xref-erase-buffer)
	(setq i (xref-server-parse-xml-tag ss i len))
	(while (equal xref-server-ctag PPC_SRC_LINE)
	  (setq tlen (xref-server-dispatch-get-int-attr PPCA_LEN))
	  (setq srcline (xref-char-list-substring ss i (+ i tlen)))
	  (setq i (+ i tlen))
	  (setq n (xref-server-dispatch-get-int-attr PPCA_REFN))
	  (while (> n 0)
		(progn
		  (if (eq line aline) 
			  (setq pointer ">")
			(setq pointer " ")
			)
		  (insert (format "%s%s%s" divnewline pointer srcline))
		  (setq divnewline "\n")
		  (setq line (+ line 1))
		  (setq n (- n 1))
		  ))
	  (setq i (xref-server-parse-xml-tag ss i len))
	  (xref-server-dispatch-require-end-ctag PPC_SRC_LINE)
	  (if xref-xemacs-mule-problem (progn (setq len (- len i)) (setq ss (nthcdr i ss)) (setq j (+ j i)) (setq i 0)))
	  (setq i (xref-server-parse-xml-tag ss i len))
	  )
	(xref-server-dispatch-require-end-ctag PPC_REFERENCE_LIST)
	(xref-line-hightlight 0 (point-max) nil 1 (xref-font-lock-list-keywords listed-symbol) nil)
	(goto-char (point-min))
	(setq buffer-read-only t)
	i
	(+ i j)
))

(defun xref-server-dispatch-symbol-list (ss i len dispatch-data)
  (let ((tlen) (sym) (sl) (ll) (j))
	(setq j 0)
	(setq sl (cons 'begin nil))
	(setq ll sl)
	(setq i (xref-server-parse-xml-tag ss i len))
;;(message "start %S len %d" (current-time) len)
	(while (equal xref-server-ctag PPC_STRING_VALUE)
	  (setq tlen (xref-server-dispatch-get-int-attr PPCA_LEN))
	  (setq sym (xref-char-list-substring ss i (+ i tlen)))
	  (setq i (+ i tlen))
	  (setcdr ll (cons sym nil))
	  (setq ll (cdr ll))
	  (setq i (xref-server-parse-xml-tag ss i len))
	  (xref-server-dispatch-require-end-ctag PPC_STRING_VALUE)
	  (setq i (xref-server-parse-xml-tag ss i len))
	  (if xref-xemacs-mule-problem (progn (setq len (- len i)) (setq ss (nthcdr i ss)) (setq j (+ j i)) (setq i 0)))
	  )
;;(message "stop %S" (current-time))
	(xref-server-dispatch-require-end-ctag PPC_SYMBOL_LIST)
	(xref-hard-prepend-to-dispatch-data 
	 dispatch-data 
	 (cons (cons 'symbol-list (cdr sl)) nil))
	(+ i j)
))


(defun xref-server-dispatch-move-file-as (ss i len dispatch-data)
  (let ((tlen) (cc) (rcc))
	(setq tlen (xref-server-dispatch-get-int-attr PPCA_LEN))
	(setq cc (xref-char-list-substring ss i (+ i tlen)))
	(setq i (+ i tlen))
	(xref-select-dispach-data-caller-window dispatch-data)
	(xref-undoable-move-file cc)
	(setq i (xref-server-parse-xml-tag ss i len))
	(xref-server-dispatch-require-end-ctag PPC_MOVE_FILE_AS)
	i
))

(defun xref-server-dispatch-extraction-dialog (ss i len dispatch-data)
  (let ((tlen) (minvocation) (mhead) (mtail) (mline) (dname))
	(setq dname (cdr (assoc PPCA_TYPE xref-server-ctag-attributes)))

	(setq i (xref-server-parse-xml-tag ss i len))
	(xref-server-dispatch-require-ctag PPC_STRING_VALUE)
	(setq tlen (xref-server-dispatch-get-int-attr PPCA_LEN))
	(setq minvocation (xref-char-list-substring ss i (+ i tlen)))
	(setq i (+ i tlen))
	(setq i (xref-server-parse-xml-tag ss i len))
	(xref-server-dispatch-require-end-ctag PPC_STRING_VALUE)

	(setq i (xref-server-parse-xml-tag ss i len))
	(xref-server-dispatch-require-ctag PPC_STRING_VALUE)
	(setq tlen (xref-server-dispatch-get-int-attr PPCA_LEN))
	(setq mhead (xref-char-list-substring ss i (+ i tlen)))
	(setq i (+ i tlen))
	(setq i (xref-server-parse-xml-tag ss i len))
	(xref-server-dispatch-require-end-ctag PPC_STRING_VALUE)

	(setq i (xref-server-parse-xml-tag ss i len))
	(xref-server-dispatch-require-ctag PPC_STRING_VALUE)
	(setq tlen (xref-server-dispatch-get-int-attr PPCA_LEN))
	(setq mtail (xref-char-list-substring ss i (+ i tlen)))
	(setq i (+ i tlen))
	(setq i (xref-server-parse-xml-tag ss i len))
	(xref-server-dispatch-require-end-ctag PPC_STRING_VALUE)

	(setq i (xref-server-parse-xml-tag ss i len))
	(xref-server-dispatch-require-end-ctag PPC_EXTRACTION_DIALOG)

	(xref-extraction-dialog minvocation mhead mtail dname)

	i
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DISPATCH ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun xref-server-dispatch (ss i len dispatch-data)
  (let ((x) (j) (cloop))
	(setq j 0)
	(if xref-debug-mode (message "dispatching: %s" ss))
	(setq cloop t)
	(setq i (xref-server-dispatch-skip-blank ss i len))
	(while (and cloop (< i len))  ;; (eq (elt ss i) ?<))
	  (setq i (xref-server-parse-xml-tag ss i len))
	  ;;(message "tag == %s" xref-server-ctag)
	  (cond
	   (	
		(equal xref-server-ctag PPC_SET_INFO)
		(setq i (xref-server-dispatch-set-info ss i len dispatch-data)))
	   (
		(equal xref-server-ctag PPC_ALL_COMPLETIONS)
		(setq i (xref-server-dispatch-all-completions ss i len dispatch-data)))
	   (	
		(equal xref-server-ctag PPC_SINGLE_COMPLETION)
		(setq i (xref-server-dispatch-single-completion ss i len dispatch-data)))
	   (	
		(equal xref-server-ctag PPC_FQT_COMPLETION)
		(setq i (xref-server-dispatch-fqt-completion ss i len dispatch-data)))
	   (	
		(equal xref-server-ctag PPC_GOTO)
		(setq i (xref-server-dispatch-goto ss i len dispatch-data)))
	   (	
		(equal xref-server-ctag PPC_BROWSE_URL)
		(setq i (xref-server-dispatch-browse-url ss i len dispatch-data)))
	   (	
		(equal xref-server-ctag PPC_AVAILABLE_REFACTORINGS)
		(setq i (xref-server-dispatch-available-refactorings ss i len dispatch-data)))
	   (	
		(equal xref-server-ctag PPC_UPDATE_REPORT)
		(setq i (xref-server-dispatch-update-report ss i len dispatch-data)))
	   (	
		(equal xref-server-ctag PPC_REFACTORING_PRECHECK)
		(setq i (xref-server-dispatch-precheck ss i len dispatch-data)))
	   (	
		(equal xref-server-ctag PPC_REFACTORING_REPLACEMENT)
		(setq i (xref-server-dispatch-replacement ss i len dispatch-data)))
   	   (	
		(equal xref-server-ctag PPC_REFACTORING_CUT_BLOCK)
		(setq i (xref-server-dispatch-refactoring-cut-block ss i len dispatch-data)))
   	   (	
		(equal xref-server-ctag PPC_REFACTORING_PASTE_BLOCK)
		(setq i (xref-server-dispatch-refactoring-paste-block ss i len dispatch-data)))
   	   (	
		(equal xref-server-ctag PPC_INDENT)
		(setq i (xref-server-dispatch-indent ss i len dispatch-data)))
	   (	
		(equal xref-server-ctag PPC_DISPLAY_RESOLUTION)
		(setq i (xref-server-dispatch-display-resolution ss i len dispatch-data)))
	   (	
		(equal xref-server-ctag PPC_SYMBOL_RESOLUTION)
		(setq i (xref-server-dispatch-symbol-resolution ss i len dispatch-data)))
	   (	
		(equal xref-server-ctag PPC_DISPLAY_OR_UPDATE_BROWSER)
		(setq i (xref-server-dispatch-display-or-update-browser ss i len dispatch-data)))
	   (	
		(equal xref-server-ctag PPC_REFERENCE_LIST)
		(setq i (xref-server-dispatch-reference-list ss i len dispatch-data)))
	   (	
		(equal xref-server-ctag PPC_SYMBOL_LIST)
		(setq i (xref-server-dispatch-symbol-list ss i len dispatch-data)))
	   (	
		(equal xref-server-ctag PPC_DISPLAY_CLASS_TREE)
		(setq i (xref-server-dispatch-display-class-tree ss i len dispatch-data)))
   	   (	
		(equal xref-server-ctag PPC_MOVE_FILE_AS)
		(setq i (xref-server-dispatch-move-file-as ss i len dispatch-data)))
   	   (	
		(equal xref-server-ctag PPC_EXTRACTION_DIALOG)
		(setq i (xref-server-dispatch-extraction-dialog ss i len dispatch-data)))
   	   (	
		(equal xref-server-ctag PPC_ADD_TO_IMPORTS_DIALOG)
		(setq i (xref-server-dispatch-add-to-imports-dialog ss i len dispatch-data)))
   	   (	
		(equal xref-server-ctag PPC_CALL_MACRO)
		(setq i (xref-server-dispatch-call-macro ss i len dispatch-data)))
   	   (	
		(equal xref-server-ctag PPC_KILL_BUFFER_REMOVE_FILE)
		(setq i (xref-server-dispatch-kill-buffer-remove-file ss i len dispatch-data)))
   	   (	
		(equal xref-server-ctag PPC_NO_PROJECT)
		(setq i (xref-server-dispatch-no-project ss i len dispatch-data)))
	   
	   (
		(equal xref-server-ctag PPC_ASK_CONFIRMATION)
		(setq i (xref-server-dispatch-ask-confirmation ss i len dispatch-data xref-server-ctag)))
	   (
		(equal xref-server-ctag PPC_INFORMATION)
		(setq i (xref-server-dispatch-information ss i len dispatch-data xref-server-ctag)))
	   (
		(equal xref-server-ctag PPC_BOTTOM_INFORMATION)
		(setq i (xref-server-dispatch-bottom-information ss i len dispatch-data xref-server-ctag)))
	   (
		(equal xref-server-ctag PPC_WARNING)
		(setq i (xref-server-dispatch-information ss i len dispatch-data xref-server-ctag)))
	   (
		(equal xref-server-ctag PPC_BOTTOM_WARNING)
		(setq i (xref-server-dispatch-bottom-information ss i len dispatch-data xref-server-ctag)))
	   (
		(equal xref-server-ctag PPC_ERROR)
		(setq cloop nil)
		(setq i (xref-server-dispatch-error ss i len dispatch-data xref-server-ctag)))
	   (
		(equal xref-server-ctag PPC_FATAL_ERROR)
		(setq cloop nil)
		(setq i (xref-server-dispatch-error ss i len dispatch-data xref-server-ctag)))
	   (
		(equal xref-server-ctag PPC_LICENSE_ERROR)
		(setq cloop nil)
		(setq i (xref-server-dispatch-license-error ss i len dispatch-data)))
	   (
		(equal xref-server-ctag PPC_FULL_UPDATE_OR_ERROR)
		(setq cloop nil)
		(setq i (xref-server-dispatch-fupdate-or-error ss i len dispatch-data xref-server-ctag)))
	   (
		(equal xref-server-ctag PPC_DEBUG_INFORMATION)
		(setq i (xref-server-dispatch-information ss i len dispatch-data xref-server-ctag)))
	   (
		;; unused tag (backward compatibility)
		(equal xref-server-ctag PPC_MULTIPLE_COMPLETIONS)
		(setq i (xref-server-dispatch-completions ss i len dispatch-data)))
	   (	
		t
		(error "unknown tag: %s" xref-server-ctag))
		;;(message "unknown tag: %s" xref-server-ctag))
	   )
	  (if xref-xemacs-mule-problem (progn (setq len (- len i)) (setq ss (nthcdr i ss)) (setq j (+ j i)) (setq i 0)))
	  (setq i (xref-server-dispatch-skip-blank ss i len))
	  )
	(+ i j)
))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;; MODAL DIALOG ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun xref-modal-dialog-sentinel () 
  (let ((pp) (res))
	(setq pp (point))
	(setq res nil)
	(if (and (eq (char-after pp) ?-)
			 (eq (char-after (+ pp 1)) ?-)
			 (eq (char-after (+ pp 2)) ?-))
		(setq res t)
	  )
	res
))

(defun xref-modal-dialog-page-up (event)
  (interactive "i")
  (condition-case nil
	  (scroll-down nil)
	(error nil)
	)
)

(defun xref-modal-dialog-page-down (event)
  (interactive "i")
  (condition-case nil
	  (scroll-up nil)
	(error nil)
	)
)

(defun xref-modal-dialog-previous-line-no-sentinel (event)
  (interactive "i")
  (condition-case nil
	  (forward-line -1)
	(error nil)
	)
)

(defun xref-modal-dialog-next-line-no-sentinel (event)
  (interactive "i")
  (condition-case nil
	  (forward-line 1)
	(error nil)
	)
)

(defun xref-modal-dialog-previous-line (event)
  (interactive "i")
  (forward-line -1)
  (if (xref-modal-dialog-sentinel) (forward-line 1))
  (beginning-of-line)
)

(defun xref-modal-dialog-next-line (event)
  (interactive "i")
  ;; TODO swap here over end of references (if in references window)
  (forward-line 1)
  (if (xref-modal-dialog-sentinel) (forward-line -1))
  (beginning-of-line)
)

(defun xref-modal-dialog-goto-line (line)
  (let ((i))
	;; hmm, there is no way to move directly to the first line?
	(setq i 20)
	(while (> i 0)
	  (progn
		(xref-modal-dialog-previous-line nil)
		(setq i (- i 1))
		))
	(setq i line)
	(while (> i 1)
	  (progn
		(xref-modal-dialog-next-line nil)
		(setq i (- i 1))
		))
))

(defun xref-modal-dialog-select (event)
  (interactive "i")
)
(defun xref-modal-dialog-select-1-line (event)
  (interactive "i")
  (xref-modal-dialog-goto-line 1)
)
(defun xref-modal-dialog-select-2-line (event)
  (interactive "i")
  (xref-modal-dialog-goto-line 2)
)
(defun xref-modal-dialog-select-3-line (event)
  (interactive "i")
  (xref-modal-dialog-goto-line 3)
)
(defun xref-modal-dialog-select-4-line (event)
  (interactive "i")
  (xref-modal-dialog-goto-line 4)
)
(defun xref-modal-dialog-select-5-line (event)
  (interactive "i")
  (xref-modal-dialog-goto-line 5)
)

(defun xref-modal-dialog-exit (event)
  (interactive "i")
  (let ((winassoc) (dispatch-data) (win))
	(setq dispatch-data xref-this-buffer-dispatch-data)
	(delete-window (selected-window))  
	(setq winassoc (assoc 'caller-window dispatch-data))
	(if winassoc 
		(progn
		  (setq win (cdr winassoc))
		  (if (window-live-p win) (select-window win))
	  ))
	(message "Cancel")
	(keyboard-quit)
))

(defun xref-modal-dialog-loop (keymap mess)
  (let ((event) (ev) (key))
	(setq key nil)
	(while (not (assoc key xref-modal-dialog-select-functions))
	  (condition-case nil
		  (progn
			(message mess)
			(setq ev (xref-read-key-sequence mess))
			(setq event (elt ev 00))
			(setq key (lookup-key keymap ev))
			;; TODO fix this special case, this is due to a mismatch in function names
			;; xref-modal-dialog-continue is used here simply to exit dialog and is not 
			;; expected to be called ( a bug from previous versions)
			(if (not (eq key 'xref-modal-dialog-continue))
				(if key
					(apply key event nil)
				  (message "Invalid key")
				  ))
			)
		(error nil)
		)
	  )
))

(defun xref-modal-dialog (title text line col blick keymap dispatch-data)
  (let ((key) (res) (win) (owin))
	(setq owin (selected-window))
	(setq win (xref-display-and-set-new-dialog-window title nil t))
	(insert-string text)
	(xref-use-local-map keymap)
	(goto-line line)
	(xref-appropriate-window-height nil t)
	(goto-char (+ (point) col))
	(setq xref-this-buffer-dispatch-data dispatch-data)
	(if blick 
		(progn
		  (other-window -1)
		  (sit-for 1)
		  (other-window 1)
		  ))
	(xref-modal-dialog-loop keymap "? - help")
	(setq res (count-lines 1 (+ (point) 1)))
	(delete-window win)
	(select-window owin)
	res
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;    Projects    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun xref-split-path-on-path-list (pname cut-slash)
  (let ((lplist) (i) (path) (len) (lchar))
	;; cut project path
	(setq lplist nil)
	(setq i (length pname))
	(while (> i 0) 
	  (setq i (- i 1))
	  (if (eq (elt pname i) xref-path-separator)
		  (progn
			(setq path (substring pname (+ i 1)))
			(if cut-slash (setq path (xref-remove-pending-slash path)))
			(setq lplist (append lplist (cons path nil)))
			(setq pname (substring pname 0 i))
			)))
	(setq lplist (append lplist (cons pname nil)))
	lplist
))

	
(defun xref-get-project-list ()
  (let ((new-name) (loop) (mbeg) (mend) (pname "") (project-list) (i) (len) (lplist))
	(save-excursion
	  (set-buffer (get-buffer-create " xref-project-list"))
	  ;;(xref-erase-buffer)
	  (insert-file-contents xref-options-file  nil nil nil t)
	  (goto-char (point-min))
	  (setq project-list nil)
	  (setq loop t)
	  (while loop
		(setq loop (search-forward-regexp "^\\[\\([^\]]*\\)\\]" 
										  (buffer-size) 1))
		(if loop
			(progn
			  (setq mbeg (match-beginning  1))
			  (setq mend (match-end  1))
			  (setq pname (buffer-substring mbeg mend))
			  (setq lplist (xref-split-path-on-path-list pname nil))
			  (setq project-list (append lplist project-list))
			  )))
	  (kill-buffer nil)
	  )
	project-list
))

(defun xref-prj-list-get-prj-on-line ()
  (let ((res) (ppp) (bl) (el))
	(setq ppp (point))
	(beginning-of-line)
	(setq bl (point))
	(end-of-line)
	(setq el (point))
	(setq res (buffer-substring (+ bl 2) el))
	(goto-char ppp)
	res
))

(defun xref-interactive-project-select (&optional argp)
  "Go to the reference corresponding to this line."
  (interactive "P")
  (let ((bl) (el) (prj))
	(setq prj (xref-prj-list-get-prj-on-line))
	(if (string-equal prj xref-directory-dep-prj-name)
		(progn
		  (setq xref-current-project nil)
		  ;; and reseting of softly selected project
		  (xref-softly-preset-project "")
		  )
	  (setq xref-current-project prj)
	  )
	(xref-delete-window-in-any-frame  xref-project-list-buffer nil t)
	(message "Project '%s' is active." prj)
  )
  t
)

(defun xref-interactive-m-project-select (event)
  (interactive "e")
  (mouse-set-point event)
  (xref-interactive-project-select)
  (other-window -1)
  t
)

(defun xref-interactive-project-escape (&optional argp)
  "Escape from the project selection window."
  (interactive "P")
  (xref-delete-window-in-any-frame  xref-project-list-buffer nil t)
)

(defvar xref-project-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\e" 'xref-interactive-project-escape)
    (define-key map "q" 'xref-interactive-project-escape)
    (define-key map "\C-m" 'xref-interactive-project-select)
;;    (define-key map " " 'xref-interactive-project-select)
    (define-key map "?" 'xref-interactive-project-selection-help)
	(xref-bind-default-button map 'xref-interactive-m-project-select)
    map)
  "Keymap for `xref-project-list-mode'."
)
(xref-add-bindings-to-keymap xref-project-list-mode-map)

(defun xref-interactive-project-selection-help ()
  (interactive "")
  (xref-interactive-help
"Special hotkeys available:

\\[xref-interactive-project-select] \t-- select project
\\[xref-interactive-project-escape] \t-- close
\\[xref-interactive-project-selection-help] \t-- toggle this help page
" nil nil)
)


(defun xref-project-set-active-new ()
"Set  active  project.   

This function  is meaningful  only if your  '.xrefrc' file  contains a
section defining options for your  project. After setting a project to
be active  all Xrefactory functions will proceed  according to options
corresponding to this project name.
"
  (interactive "")
  (xref-entry-point-make-initialisations-no-project-required)
  (let ((prj) (cpl))
	(setq cpl (append 
			   (cons (cons xref-directory-dep-prj-name nil) nil)
			   (xref-get-project-list-completions) 
			   ))
	(setq prj (completing-read "Activate project: " cpl))
	(if prj
		(progn
		  (if (string-equal prj xref-directory-dep-prj-name)
			  (setq xref-current-project nil)
			(setq xref-current-project prj)
			)))
	(xref-project-active)
))

(defun xref-display-project-list (last-project local-keymap)
  (let ((xref-project-list) (dd) (cpp))
	(xref-delete-window-in-any-frame xref-project-list-buffer nil t)
	(setq dd (xref-get-basic-server-dispatch-data 'xref-server-process))
	(setq xref-this-buffer-dispatch-data dd)
	(setq xref-project-list (xref-get-project-list))
	(xref-display-and-set-new-dialog-window xref-project-list-buffer nil t)
	(setq xref-this-buffer-dispatch-data dd)
	(insert "  ")
	(insert last-project)
	(put-text-property 3 (point) 'mouse-face 'highlight)
	(newline)
	(while xref-project-list
	  (goto-char (point-min))
	  (if (string-equal xref-current-project (car xref-project-list))
		  (insert "> ")
		(insert "  ")
		)
	  (insert (car xref-project-list))
	  (put-text-property 3 (point) 'mouse-face 'highlight)
	  (newline)
	  (setq xref-project-list (cdr xref-project-list))
	  )
	(setq buffer-read-only t)
	(xref-use-local-map local-keymap)
	(message xref-standard-help-message)
))

(defun xref-project-set-active ()
"Set  active  project.   

This function  is meningfull  only if your  '.xrefrc' file  contains a
section defining options for your  project. After setting a project to
be active  all Xrefactory functions will proceed  according to options
corresponding to this project name.
"
  (interactive "")
  (xref-entry-point-make-initialisations-no-project-required)
  (xref-display-project-list xref-directory-dep-prj-name 
							 xref-project-list-mode-map)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun xref-interactive-project-delete (&optional argp)
  (interactive "P")
  (let ((prj) (eprj) (ppp) (eppp))
	(setq prj (xref-prj-list-get-prj-on-line))
	(if (string-equal prj xref-abandon-deletion)
		(progn
		  (xref-delete-window-in-any-frame  xref-project-list-buffer nil t)
		  (message "Deletion canceled.")
		  )
	  (next-line 1)
	  (setq eprj (xref-prj-list-get-prj-on-line))
	  (xref-delete-window-in-any-frame  xref-project-list-buffer nil t)
	  (find-file xref-options-file)
	  (goto-char (point-min))
	  (setq ppp (search-forward (format "[%s]" prj) nil t))
	  (if (not ppp)
		  (error "Project section not found, it's probably sharing options, delete it manually.")
		(setq ppp (- ppp (length (format "[%s]" prj))))
		)
	  (if (equal eprj xref-abandon-deletion)
		  (setq eppp (point-max))
		(setq eppp (search-forward (format "[%s]" eprj) nil t))
		(if (not eppp)
			(setq eppp (search-forward (format "[%s:" eprj) nil t))
		  )
		(if (not eppp)
			(error "Can't find end of project section, internal error, sorry.")
		  (setq eppp (- eppp (length (format "[%s]" eprj))))
		  ))
	  (if (> ppp eppp)
		  (error "[ppp>eppp] internal check failed, sorry")
		(goto-char ppp)
		(if (yes-or-no-p (format "Really delete %s? " prj))
			(progn
			  (delete-char (- eppp ppp))
			  (save-buffer)
			  (kill-buffer (current-buffer))
			  (message "Project %s has been deleted." prj)
			  )
		  (message "No deletion.")
		  )
		))
  t
))

(defun xref-interactive-m-project-delete (event)
  (interactive "e")
  (mouse-set-point event)
  (xref-interactive-project-delete)
  (other-window -1)
  t
)

(defvar xref-project-delete-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\e" 'xref-interactive-project-escape)
    (define-key map "q" 'xref-interactive-project-escape)
    (define-key map "\C-m" 'xref-interactive-project-delete)
    ;; (define-key map " " 'xref-interactive-project-delete)
    (define-key map "?" 'xref-interactive-project-delete-help)
	(xref-bind-default-button map 'xref-interactive-m-project-delete)
    map)
  "Keymap for `xref-project-delete-mode'."
)
(xref-add-bindings-to-keymap xref-project-delete-mode-map)


(defun xref-interactive-project-delete-help ()
  (interactive "")
  (xref-interactive-help
"Special hotkeys available:

\\[xref-interactive-project-delete] \t-- delete project
\\[xref-interactive-project-escape] \t-- close
\\[xref-interactive-project-delete-help] \t-- toggle this help page
" nil nil)
)


(defun xref-project-delete ()
"Delete a project.   

This function asks you to select  the project you wish to delete. Then
the part of .xrefrc file describing this project will be deleted.
"
  (interactive "")
  (xref-entry-point-make-initialisations-no-project-required)
  (xref-display-project-list xref-abandon-deletion
							 xref-project-delete-mode-map)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun xref-remove-pending-slash (pfiles)
  (let ((dlen))
	(setq dlen (- (length pfiles) 1))
	(if (> dlen 0)
		(progn
		  (if (or (eq (elt pfiles dlen) ?/) (eq (elt pfiles dlen) ?\\))
			  (setq pfiles (substring pfiles 0 dlen))
			)))
	pfiles
))

(defun xref-char-replace (ss schars dchar)
  (let ((res) (i) (j) (len) (slen) (cc) (sc) (loop))
	(setq res "")
	(setq len (length ss))
	(setq slen (length schars))
	(setq i 0)
	(while (< i len) 
	  (setq cc (elt ss i))
	  (setq j 0)
	  (setq loop t)
	  (while (and (< j slen) loop)
		(setq sc (elt schars j))
		(if (eq cc sc)
			(setq loop nil)
		  )
		(setq j (+ j 1))
		)
	  (if loop
		  (setq res (format "%s%c" res cc))
		(setq res (format "%s%c" res dchar))
		)
	  (setq i (1+ i))
	  )
	res
))


(defun xref-get-full-executable-name (name)
  (let ((p) (cp) (res))
	(setq res name)
	(setq p exec-path)
	(while p
	  (progn
		(setq cp (car p))
		(if (eq xref-platform 'windows)
			(setq cp (format "%s%c%s.exe" cp xref-slash name))
		  (setq cp (format "%s%c%s" cp xref-slash name))
		  )
		(if (file-attributes cp)
			(progn
			  (setq res (xref-backslashify-name cp))
			  (setq p nil)
			  )
		  (setq p (cdr p))
		  )))
	res
))

(defun xref-append-new-project-section (pname planguage pcomments 
											  pfiles mclass classpath 
											  sourcepath 
											  javadocpath classdir
											  apfiles javahome ifiles
											  rest refs htmlopt exactp
											  recipe 
											  ccompiler msc-version gnu-version
											  templ-instantiation
											  prbdirectory prbcommand prccommand
											  )
  (let ((comment) (dlen) (cldopt) (tdq))
	(setq comment (or (equal pcomments "y") (equal pcomments "Y")))
	(goto-char (point-max))
	(insert (concat "\n\n[" pname "]\n"))
	(if (or (equal planguage "j") (equal planguage "J"))
		(insert "  -javafilesonly\t// pure Java project\n")
	  ;; C/C++
	  (if recipe
		  (progn
			(if msc-version (insert "  -cpp--microsoft_version " msc-version "\n"))
			(if gnu-version (insert "  -cpp--gnu_version " gnu-version "\n"))
			)
		;; no recipe
		(if (and (> (length ccompiler) 0) (eq (elt ccompiler 0) ?m))
			(progn
			  (insert "  -cpp--microsoft\n")
			  (if msc-version (insert "  -cpp--microsoft_version " msc-version "\n"))
			  )
		  (if (and (> (length ccompiler) 0) (eq (elt ccompiler 0) ?s))
			  (insert "  -cpp--sun\t\n")
			(insert "  -cpp--gcc\n")
			(if gnu-version (insert "  -cpp--gnu_version " gnu-version "\n"))
			)))
	  (if templ-instantiation (insert "  -cpp-t" templ-instantiation "\n"))
	  )
	(insert "  -encoding=european\t// default encoding\n")
	(if recipe
		(progn
		  (if comment
			  (insert "  //  the recipe (generated by xref-recipe-make command)\n")
			)
		  (insert (concat "  -recipe " (xref-optionify-string recipe "\"")))
		  (newline)
		  )
	  (if comment
		  (insert "  //  input files and directories (processed recursively)\n")
		)
	  (insert (concat "  " apfiles))
	  (newline)
	  )
	(setq pfiles (xref-remove-pending-slash pfiles))
	(if comment
		(insert "  //  directory where tag files are stored\n")
	  )
	(insert (format "  -refs %s\n" (xref-optionify-string refs "\"")))
	(if (equal htmlopt "")
		(progn
		  (insert "  //  number of tag files\n")
		  (if (or (equal exactp "y") (equal exactp "Y"))
			  (insert "  -refnum=100\n")
			(insert "  -refnum=10\n")
			)
		  )
	  (if (or (equal exactp "y") (equal exactp "Y"))
		  (progn
			(if comment (insert "  //  split tag files using first two letters\n"))
			(insert "  -refalpha2hash\n")
			)
		(if comment (insert "  //  split tag files using first letter\n"))
		(insert "  -refalphahash\n")
		)
	  )
	(if (not recipe)
		(progn
		  (if (or (equal planguage "c") (equal planguage "C")
				  (equal planguage "b") (equal planguage "B"))
			  (progn
				(if (eq xref-platform 'windows)
					(insert "  -DWIN32 -D_WINDOWS -D_DEBUG\t// few common macros\n")
				  )
				(if (not (equal ifiles ""))
					(insert (format "%s\n" ifiles))
				  )
				))
		  ))
	(if (or (equal planguage "c") (equal planguage "C"))
		(progn
		  (if (or (equal exactp "y") (equal exactp "Y"))
			  (insert "  // resolve symbols using definition place\n  -exactpositionresolve\n")
			)
		  (if recipe
			  (progn
				(if comment (insert "  //  setting for Emacs build of recipe\n"))
				(insert (format "  -set buildrecipe \"\n\tcd %s\n\t%s\n\t%s %s %s\n\t\"\n" (xref-optionify-string prbdirectory "${dq}") prccommand (xref-get-full-executable-name "xref-recipe-build") (xref-optionify-string recipe  "${dq}") prbcommand))
				))
		  (if comment (insert "  //  setting for Emacs compile and run\n"))
		  (insert "  -set compilefile \"cc %s\"\n")
		  (insert "  -set compiledir \"cc *.c\"\n")
		  (insert (format "  -set compileproject \"\n\tcd %s\n\t%s\n\t\"\n" prbdirectory prbcommand))
		  (if (eq xref-platform 'windows)
			  (insert "  -set run1 \"a.exe\"\n")
			(insert "  -set run1 \"a.out\"\n")
			)
		  (insert "  -set run5 \"\"  // an empty run; C-F8 will only compile\n")
		  (insert "  //  set default to run1\n")
		  (insert "  -set run ${run1}\n")
		))
	(if (or (equal planguage "j") (equal planguage "J")
			(equal planguage "b") (equal planguage "B"))
		(progn
		  (insert (format "  -set cp %s\n" (xref-optionify-string classpath "\"")))
		  (insert (format "  -set qcp \"${dq}${cp}${dq}\" \t// quoted classpath (if it contains spaces)\n"))
		  (if (not (equal javahome ""))
			  (progn
				(insert (format "  -set jhome \"%s\"\n" javahome))
				(insert (format "  -set jbin \"${jhome}%s\"\n" (xref-backslashify-name "bin/")))
				(insert (format "  -jdkclasspath \"${jhome}%s\"" 
									   (xref-backslashify-name "jre/lib/rt.jar")))
				(if comment
					(insert "\t// Java run time for xrefactory\n")
				  )
			))
		  (insert (format "  -classpath ${cp}"))
		  (if comment
			  (insert "\t// classpath for xrefactory\n")
			)
		  (insert (format "  -sourcepath %s" (xref-optionify-string sourcepath "\"")))
		  (if comment
			  (insert "\t// sourcepath for xrefactory\n")
			)
		  (if javadocpath
			  (progn
				(insert (format "  -javadocpath %s\n" (xref-optionify-string javadocpath "\"")))
				))

		  (if (or (equal planguage "b") (equal planguage "B"))
			  (progn
				(insert "  //  include directories for JNI\n")
				(insert "  -I ${jhome}include -I ${jhome}include/linux -I ${jhome}include/win32\n")
				))

		  (if comment
			  (insert "  //  setting for Emacs compile and run\n")
			)
		  (if classdir
			  (progn
				(setq cldopt (format "-d %s" (xref-optionify-string classdir "${dq}")))
				)
			(setq cldopt "")
			)
		  (if (string-match " " javahome)
			  (progn
				(setq tdq "${dq}")
				)
			(setq tdq "")
			)
		  (insert (format "  -set compilefile \"%s${jbin}javac%s -classpath ${qcp} %s ${dq}${__file}${dq}\"\n" tdq tdq cldopt))
		  (insert (format "  -set compiledir \"%s${jbin}javac%s -classpath ${qcp} %s *.java\"\n" tdq tdq cldopt))
		  (insert (format "  -set compileproject \"\n\tcd %s\n\tant\n\t\"\n" prbdirectory))
		  (insert (format "  -set runthis \"%s${jbin}java%s -classpath ${qcp} %%s\"\n" tdq tdq))
		  (if (not (equal mclass ""))
			  (insert (format "  -set run1 \"%s${jbin}java%s -classpath ${qcp} %s\"\n" tdq tdq mclass))
			)
		  (insert "  -set run5 \"\"  // an empty run; C-F8 will only compile\n")
		  (if (not (equal mclass ""))
			  (progn
				(if comment (insert "  //  set default to run1\n"))
				(insert "  -set run ${run1}\n")
				)
			(if comment (insert "  //  set default to runthis\n"))
			(insert "  -set run ${runthis}\n")
		  )))
	(if (not (equal htmlopt ""))
		(insert (format "%s\n" htmlopt))
	  )
	(if (not (equal rest ""))
		(insert (format "%s\n" rest))
	  )
))


(defun xref-java-path-completionfun (cstr filter type)
  (let ((res) (cc) (fname) (dir) (sep) (str) (prefix))
	(setq str cstr)
	(setq sep (string-match (format "\\%c" xref-path-separator) str))
	(setq prefix "")
	(while sep
	  (progn 
		(setq prefix (concat prefix (substring str  0 (+ sep 1))))
		(setq str (substring str (+ sep 1) nil))
		(setq sep (string-match (format "\\%c" xref-path-separator) str))
	  ))
;;	(setq dir (concat (xref-file-directory-name str) "/"))
	(setq dir (file-name-directory str))
	(setq fname (file-name-nondirectory str))
	(if (eq type t)
		(setq res (file-name-all-completions fname dir))
	  (setq res (file-name-completion fname dir))
	  (if (eq xref-platform 'windows)
		  (setq res (xref-backslashify-name res))
		)
	  )
	(if (stringp res)
		(setq res (concat cstr (substring res (length fname))))
	  )
	res
))

(defun xref-read-jpath-from-minibuffer (prompt default)
  (let ((res))
	(setq res (completing-read prompt 'xref-java-path-completionfun nil nil default))
	res
))

(defun xref-read-jpath-from-minibuffer-xx (prompt)
  (let ((res) (ini))
	(if (eq xref-platform 'windows)
		(setq ini "c:\\")
	  (setq ini "/")
	  )
	(setq res (completing-read prompt 'xref-java-path-completionfun nil nil ini))
	res
))

(defvar xref-foo-macros-counter 1)
(defun xref-collect-macros-for-new-project (prefix mess1 mess2)
  (let ((ifloop) (rrr) (rest) (aaa) (deflt))
	(setq rest "")
	(setq ifloop t)
	(while ifloop
	  (if (eq xref-foo-macros-counter 1)
		  (setq deflt "FOO")
		(setq deflt (format "FOO%d" xref-foo-macros-counter))
		)
	  (setq xref-foo-macros-counter
			(+ xref-foo-macros-counter 1))
	  (setq rrr (read-from-minibuffer mess1 deflt))
	  (setq rest (format "%s\n  %s-D%s" rest prefix rrr))
	  (setq aaa (read-from-minibuffer 
				 (format "%s [yn]? " mess2) "n"))
	  (if (not (or (equal aaa "y") (equal aaa "Y"))) (setq ifloop nil))
	  )
	rest
))

(defun xref-remove-dangerous-fname-chars (ss)
  (let ((res) (i) (len))
	(setq res ss)
	(if (eq xref-platform 'windows)
		(progn
		  (setq i 0)
		  (setq len (length ss))
		  (setq res "")
		  (while (< i len)
			(if (and (> i 2) (eq (elt ss i) ?\:))
				(setq res res)
			  (setq res (concat res (char-to-string (elt ss i))))
			  )
			(setq i (1+ i))
			)
		  )
	  )
	res
))

(defun xref-get-dir-from-path-and-package (dir package)
  (let ((res) (i) (len))
	(setq res (format "%s/" dir))
	(setq len (length package))
	(setq i 0)
	(while (< i len)
	  (if (eq (elt package i) ?.)
		  (setq res (format "%s/" res))
		(setq res (format "%s%c" res (elt package i)))
		)
	  (setq i (+ i 1))
	  )
	res
))


(defun xref-infer-package-proposal ()
  (let ((package))
	(save-excursion
	  (goto-char (point-min))
	  (setq ff (search-forward-regexp "package[ \t]+\\([a-zA-Z0-9$.]*\\)" nil t))
	  (if ff
		  (setq package (buffer-substring (match-beginning 1) (match-end 1)))
		(setq package "")
		)
	  )
	package
))

(defun xref-infer-main-class-proposal ()
  (let ((package) (res) (cl))
	(setq package (xref-infer-package-proposal))
	(setq cl (xref-file-name-without-suffix (buffer-name)))
	(if (equal package "")
		(setq res cl)
	  (setq res (format "%s.%s" package cl))
	  )
	res
))

(defun xref-infer-classpath-proposal ()
  (let ((package) (ff) (bcp) (dn) (res))
	(setq package (xref-infer-package-proposal))
	(setq bcp (xref-get-dir-from-path-and-package "" package))
	(setq dn (xref-file-directory-name (buffer-file-name)))
	(setq res (xref-cut-string-suffix dn bcp (eq xref-platform 'windows)))
	res
))


(defun xref-project-mk-xrefs-dir-name (hom pname)
  (let ((refs) (fname ".xrefs"))
	(if hom
		(progn
		  (setq refs (format "%s%c%s%c%s" hom xref-slash fname xref-slash 
							 (xref-cut-string-prefix 
							  (xref-cut-string-prefix (xref-backslashify-name pname) 
													  (format "%s%c" hom xref-slash)
													  (eq xref-platform 'windows))
							  (format "%c" xref-slash)
							  nil)))
		  )
	  (setq refs (format "%s%c%s" pname xref-slash fname))
	  )
	(setq refs (xref-remove-dangerous-fname-chars refs))
	refs
))

(defun xref-project-infer-gcc-version ()
  (let ((gccv) (gccout) (dotp) (dot2p) (vp) (gccvid) (vn) (svn))
	(setq gccvid "gcc version ")
	(setq gccout (shell-command-to-string "gcc -v"))
	(setq vp (string-match gccvid gccout))
	(if (not vp)
		(progn
		  (message "Can't exec: gcc")
		  (beep t)
		  (sit-for 1)
		  (setq gccv "40100")       ;; some default value
		  )
	  ;; I hope that gcc will not goes over version 10 
	  (setq dotp (string-match "\\." gccout vp))
	  (setq dot2p (string-match "\\." gccout (+ dotp 1)))
	  (setq vn (string-to-int (substring gccout (+ vp (length gccvid)) dotp)))
	  (setq svn (string-to-int (substring gccout (+ dotp 1) dot2p)))
	  (setq gccv (format "%d%02d00" vn svn))
      )
	gccv
))

(defun xref-project-manual-edit-help ()
  (xref-interactive-help "


Verify the generated options of  your new project and then answer this
question.  If you  don't need to adjust options  manually, the project
setup will continue by automatic  creation of recipe and tags.  If you
select   manual  adjustement,   the  project   setup   will  terminate
immediately  and you  will need  to create  recipe and  tags  later by
invoking:

Xrefactory -> Create Recipe

followed by

Xrefactory -> Create Tags

from Emacs menu bar. 

Manual  adjustement is  needed, in  particular,  if the  value of  the
variable  buildrecipe  does  not  correctly  clean  and  rebuild  your
project.


Press any key to continue.

" nil nil)
)

(defun xref-project-recipe-help ()
  (xref-interactive-help  "


This is the  basic choice how Xrefactory will get  options (such as -I
and  -D) and  the list  of project  files. You  can either  enter them
manually (and answer 'n'), or say Xrefactory to automatically generate
a so-called 'recipe' file (and  answer 'y').  A recipe file is storing
informations  about all  files  and  options of  the  project.  It  is
generated by cleaning  and rebuilding the project by  a user definable
script.  If you do not use  make or a similar command line tool (bjam,
ant,  etc.)  for  building your  project  (or you  have troubles  with
recipes), then answer 'n' to  this question, otherwise it may be wiser
to answer 'y'.


Press any key to continue.

" nil nil)
)



(defun xref-project-new ()
"Create new project in  the Xrefactory option file (.xrefrc).

This  function will  ask few  questions  about your  project and  then
create a  project description in Xrefactory  configuration file.  This
function creates  a simple description which can  cover most projects.
It  is mostly useful  for Xrefactory  beginners.  Advanced  users will
probably prefer to  create and edit their .xrefrc  file manually.  See
also 'xrefrc' and  'xref' manual pages for more info  on the format of
the Xrefactory configuration file and available options.

"
  (interactive "")
  (let ((pname) (checked) (tname) (breakcheck) (planguage) (pcomments)
		(pact) (pedit) (sfiles) (pfiles) (mclass) (system-class-path) (refs)
		(classpath) (sourcepath) (classdir nil) (spcp) (ljd) (javadocpath) 
		(inidir) (mif) (miff) (mifloop) (apfiles) (crtag) (stat) (jdkcp nil) 
		(javahome) (ptdef) (if) (ifiles "") (ifloop) (iff) (ifmess) (rest "")
		(aaa) (rrr) (pasi) (pasn) (hom) (htmlopt) (exactp) (recipe nil) (rcp)
		(hasrecipe nil) (ccompiler nil) (msc-version nil) (gnu-version nil)
		(templ-instantiation nil) (templ-inst-yn nil) (recprj) (recipe-project)
		(crrecipe) (recipe-created)
		(prbdirectory) (prbcommand) (prccommand)
		)
	(xref-soft-select-dispach-data-caller-window xref-this-buffer-dispatch-data)
	;; no entry point initialisations, it calls get-active-project
	(xref-entry-point-make-initialisations-no-project-required)
	(xref-interactive-help  "



                           NEW PROJECT



This function creates  a skeleton of new project.   In case of complex
projects, you will need to edit it later.  Read 'xref' manual page for
all  available options  and 'xrefrc'  manual  page for  syntax of  the
configuration  file.  During  creation of  the project,  this function
will ask  you several questions, if  you do not know  the answer, just
enter the proposed (default) value.  In several cases you can get help
by answering 'h'.

In  autodetection mode  (which is  the default),  'active'  project is
inferred from source directory and  project name.  For this reason, it
is  desirable to  have  project names  in  form of  absolute paths  to
project depositories.


Now press any key to continue.

" nil nil)
	(if (eq xref-platform 'windows)
		(setq inidir "c:\\")
	  (setq inidir "/")
	  )
	(setq hom (getenv "HOME"))
	(if hom (setq hom (xref-remove-pending-slash (xref-backslashify-name hom))))
	;; this is a hack, just to create initial file (if not yet done)
	(xref-softly-preset-project "")
	;;
	(setq pname (xref-backslashify-name (xref-file-directory-name (buffer-file-name))))
	(setq sfiles pname)
	(setq pname (xref-read-jpath-from-minibuffer "Enter new project name: " pname))
	(find-file xref-options-file)
	(setq checked nil)
	(while (not checked)
	  (setq pname (xref-remove-pending-slash pname))
	  (goto-char (point-min))
	  (if (search-forward (concat "[" pname "]") nil t)
		  (setq pname (xref-read-jpath-from-minibuffer
					   "** Name already used, new name: " pname))
		(goto-char (point-min))
		(if (or (search-forward (concat "[" pname "/") nil t)
				(search-forward (concat "[" pname "\\") nil t))
			(setq pname (xref-read-jpath-from-minibuffer
						 "** Name overlaps an existing project, enter new name: " pname))
		  (setq tname pname)
		  (setq breakcheck nil)
		  (while (and tname (not breakcheck))
			(setq tname (xref-file-directory-name tname))
			(goto-char (point-min))
			(if (and tname 
					 (search-forward (concat "[" tname "]") nil t))
				(progn
				  (setq pname (xref-read-jpath-from-minibuffer
						 "** This name is covered, enter new name: " pname))
				  (setq breakcheck t)
				  )))
		  (if (not breakcheck)
			  (setq checked t)
			)
		)))
	;; The name is checked 
	(save-buffer) 
	(kill-buffer nil)

	(setq recprj "h")
	(while (equal recprj "h")
	  (progn
		(setq recprj (read-from-minibuffer 
							  "Will you use a 'recipe file' to store project options (h for help) [ynh]? " (if (eq xref-platform 'windows) "n" "y")))
		(if (equal recprj "h")
			(xref-project-recipe-help)
		  (if (or (equal recprj "y") (equal recprj "Y"))
			  (progn
				(setq recipe (xref-backslashify-name (format "%s-Recipe" 
									 (xref-project-mk-xrefs-dir-name hom pname)
									 )))
				(setq recipe (xref-read-jpath-from-minibuffer
							  "Enter a name for the recipe: " recipe))
				(setq hasrecipe t)
				)))
		))

	(setq recipe-project nil)
	(if (or (equal recprj "y") (equal recprj "Y"))
		(setq recipe-project t)
	  )

	(if (xref-buffer-has-one-of-suffixes (buffer-file-name) xref-java-suffixes)
		(setq ptdef "j")
	  (if (xref-buffer-has-one-of-suffixes (buffer-file-name) xref-c-suffixes)
		  (setq ptdef "c")
		(if (xref-buffer-has-one-of-suffixes (buffer-file-name) xref-cpp-suffixes)
			(setq ptdef "c")
		  (setq ptdef "b")
		)))
	(setq planguage "c")
;;	(setq planguage (read-from-minibuffer 
;;				 "Will this be a C/C++ or Java project (or Both) [cjb]? " ptdef))
;;   	(setq pcomments (read-from-minibuffer 
;;				 "Do you wish the project description to contain commentaries [yn]? " "y"))

;;	(if (or (equal planguage "c") (equal planguage "C")
;;			(equal planguage "b") (equal planguage "B"))
;;		(progn
;;		  (if (not hasrecipe)
;;			  (progn
;;				(setq rcp (read-from-minibuffer 
;;						   "Have you generated a recipe file for this project [yn]? " "n"))
;;				(if (or (equal rcp "y") (equal rcp "Y"))
;;					(progn
;;					  (setq ifiles "")
;;					  (setq rest "")
;;					  (setq recipe (format "%s%cRecipe" 
;;										   (xref-project-mk-xrefs-dir-name hom pname) xref-slash
;;										   ))
;;					  (setq hasrecipe t)
;;					  )
;;				  )))))
;;
;;
	(setq prbdirectory (xref-remove-starting-tilda pname))
	(setq prbcommand "make all")
	(setq prccommand "make clean")
	(if hasrecipe
		(progn
		  (setq prbcommand (read-from-minibuffer 
							"Command building your project: " prbcommand))
		  (setq prccommand (read-from-minibuffer 
							"Command cleaning your project: " prccommand ))
		  (setq prbdirectory (xref-read-jpath-from-minibuffer
					"Directory where to run above commands: " prbdirectory))
		  (setq pfiles "")
		  (setq apfiles "")
		  )
	  (setq pfiles (xref-read-jpath-from-minibuffer
					"Directory containing project sources: " sfiles))
	  (setq mifloop t)
	  (setq apfiles (xref-optionify-string pfiles "\""))
	  (while mifloop
		(progn
		  (setq mif (read-from-minibuffer 
					 "Add another source directory, file or jar [yn]? " "n"))
		  (if (or (equal mif "y") (equal mif "Y"))
			  (progn
				(setq miff (xref-read-jpath-from-minibuffer
							"Additional source:  " sfiles))
				(setq apfiles (format "%s\n  %s" apfiles (xref-optionify-string miff "\"")))
				)
			(setq mifloop nil)
			)))
	  )


	;;
	(setq refs (xref-project-mk-xrefs-dir-name hom pname))
	(setq refs (xref-read-jpath-from-minibuffer
				"Directory to store tags:  " refs))


	;;
	(if (or (equal planguage "j") (equal planguage "J")
			(equal planguage "b") (equal planguage "B"))
		(progn
		  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Java project specifics
		  (setq javahome (xref-compute-simple-information "-olcxgetjavahome -noerrors"))
		  (setq javahome (xref-read-jpath-from-minibuffer
						  "Java home directory (containing jre, lib, bin...): " javahome))
		  (if (not (equal javahome ""))
			  (setq javahome (xref-backslashify-name (concat (xref-remove-pending-slash javahome) "/")))
			)

		  (setq mclass (xref-infer-main-class-proposal))
		  (setq mclass (xref-read-jpath-from-minibuffer
						"Main class for 'run' commands? " mclass))
		  (setq classpath (getenv "CLASSPATH"))
		  (if classpath 
			  (progn
				(setq system-class-path (read-from-minibuffer 
						   "Your system CLASSPATH is set, use it for this project [yn]? " "n"))
				(if (or (equal system-class-path "n") (equal system-class-path "N"))
					(setq classpath (xref-read-jpath-from-minibuffer "Enter classpath for this project: " (xref-infer-classpath-proposal)))
				  (setq classpath "${CLASSPATH}")
				  ))
			(setq classpath (xref-read-jpath-from-minibuffer "Enter classpath for this project: " (xref-infer-classpath-proposal)))
		  )
		  (setq classpath (xref-remove-pending-slash classpath))
		  (setq spcp (read-from-minibuffer 
					  "Are your source files stored in the same directories as the classes [yn]? " "y"))
		  (if (or (equal spcp "n") (equal spcp "N"))
			  (progn
				(setq sourcepath (xref-remove-pending-slash (xref-read-jpath-from-minibuffer "Enter sourcepath for this project: " pfiles)))
				(setq classdir (xref-remove-pending-slash (xref-read-jpath-from-minibuffer "Directory to generate .class files (-d javac option): " classpath)))
				)
			(setq sourcepath "${cp}")
			)
		  (setq ljd (read-from-minibuffer 
					  "Have you a local copy of JavaDoc documentation on your computer [yn]? " "y"))
		  (if (or (equal ljd "n") (equal ljd "N"))
			  (setq javadocpath nil)
			(setq javadocpath (xref-read-jpath-from-minibuffer "Enter javadocpath: " (concat javahome (xref-backslashify-name "docs/api"))))
			)
		  )
	  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; C project specifics
	  ;;
	  (setq exactp (read-from-minibuffer 
				 "Is this a large project with numerous name conflicts [yn]? " "n"))

	  (setq ccompiler (read-from-minibuffer 
					   "Which compiler do you use: Gnu gcc/g++, Sun CC or Microsoft [gsm]? " "g"))
	  (if (equal ccompiler "g")
		  (progn
			(setq gnu-version (read-from-minibuffer 
							   "Compiler version (for ex. 30200 for gcc 3.2, 30300 for gcc 3.3)? " (xref-project-infer-gcc-version)))
			)
		(if (equal ccompiler "m")
			(progn
			  (setq msc-version (read-from-minibuffer 
								 "Compiler version (enter 1200 for MSVC++ 6., 1300 for MSVC++ 7.)? " "1300"))
			  )))
	  (setq templ-inst-yn (read-from-minibuffer 
						   "Should Xrefactory parse unused template entities [yn]? " "n"))
	  (if (or (equal templ-inst-yn "y") (equal templ-inst-yn "Y"))
		  (setq templ-instantiation "all");
		(setq templ-instantiation "none");
		)
	  (if hasrecipe
		  ()
		;; no recipe
		(setq ifiles "")
		(setq ifmess "Do you use some ")
; 		(if (eq xref-platform 'windows)
; 			(progn
; 			  (setq iff (xref-read-jpath-from-minibuffer "Directory containing standard headers (stdio.h, stdlib.h, ...): " inidir))
; 			  (setq ifiles (format "%s\n  -I %s" ifiles 
; 								   (xref-optionify-string (xref-remove-pending-slash iff) "\"")))
; 			  (setq ifmess "Add any ")
; 			  ))
		(setq ifloop t)
		(while ifloop
		  (progn
			(setq if (read-from-minibuffer 
					  (concat ifmess "nonstandard include directory (-I option) [yn]? ") "n"))
			(if (or (equal if "y") (equal if "Y"))
				(progn
				  (setq iff (xref-read-jpath-from-minibuffer
							 "Additional include directory:  " inidir))
				  (setq ifiles (format "%s\n  -I %s" ifiles 
									   (xref-optionify-string (xref-remove-pending-slash iff) "\"")))
				  )
			  (setq ifloop nil)
			  )
			(setq ifmess "Add another ")
			))
		(if (not (equal ifiles ""))
			(setq ifiles (concat "  //  include directories" ifiles))
		  )
		;;
		(setq rest "")
		(setq xref-foo-macros-counter 1)
		(setq aaa (read-from-minibuffer 
				   "Do you compile sources with command line macro definitions (-D option) [yn]? " "n"))
		(if (or (equal aaa "y") (equal aaa "Y"))
			(progn
			  (setq aaa (read-from-minibuffer 
						 "Do you compile sources several times with different macro settings [yn]? " "n"))
			  (if (or (equal aaa "y") (equal aaa "Y"))
				  (progn
					(setq pasn (string-to-int 
								(read-from-minibuffer 
								 "How many compilations with different initial macro settings: " "2")))
					(setq aaa (read-from-minibuffer 
							   "Are there macros common to all compilations [yn]? " "n"))
					(if (or (equal aaa "y") (equal aaa "Y"))
						(progn
						  (setq rest (concat rest 
											 (xref-collect-macros-for-new-project ""
																				  "A common macro to be defined in all compilations: "
																				  "Add another common macro definition")))
						  ))
					(setq pasi 1)
					(while (<= pasi pasn)
					  (progn
						(setq rest (format "%s\n  -pass%d" rest pasi))
						(setq rest (concat rest 
										   (xref-collect-macros-for-new-project "  "
																				(format "A macro specific to compilation #%d: " pasi)
																				(format "Add another macro definition for compilation #%d"pasi))))
						(setq pasi (+ pasi 1))
						))
					)
				;; a single pass macros
				(setq rest (concat rest 
								   (xref-collect-macros-for-new-project ""
																		"A macro defined during compilation: "
																		"Add another macro definition")))
				)
			  ))
		(if (not (equal rest ""))
			(setq rest (concat "  //  pre-processor macros and passes specification" 
							   rest))
		  )
		)
	  ) ;; recipe

	;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; HTML common questions
	(setq htmlopt "")
	(setq aaa (read-from-minibuffer 
			   "Will you generate HTML documentation for this project [yn]? " "n"))
	(if (or (equal aaa "y") (equal aaa "Y"))
		(progn	
		  (if hom
			  (setq rrr (format "%s%cHTML" hom xref-slash))
			(setq rrr (format "%sHTML" inidir))
			)
		  (setq rrr (xref-read-jpath-from-minibuffer
					 "Output directory for generated HTML documentation: " rrr))
		  (setq rrr (xref-remove-pending-slash (xref-backslashify-name rrr)))
		  (setq htmlopt "  //  HTML configuration")
		  (setq htmlopt (format "%s\n  %s" htmlopt 
								(xref-optionify-string (concat "-htmlroot=" rrr) "\"")))
		  (setq htmlopt (format "%s\n  -htmldirectx -htmltab=%d -htmllinenumcolor=000000" htmlopt tab-width))
		  (if (or (equal planguage "j") (equal planguage "J")
				  (equal planguage "b") (equal planguage "B"))
			  (progn
				(setq htmlopt (format "%s\n  -htmlgenjavadoclinks -htmlcutsuffix -htmllinenumlabel=line" htmlopt))
				))
		  ))

	;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; all questions done

	(find-file xref-options-file)
	(xref-append-new-project-section pname planguage "y" ;; pcomments 
									 pfiles mclass
									 classpath sourcepath javadocpath
									 classdir apfiles javahome ifiles 
									 rest refs htmlopt exactp recipe 
									 ccompiler msc-version gnu-version
									 templ-instantiation
									 prbdirectory prbcommand prccommand
									 )
	;;
	(save-buffer)
	(search-backward (concat "[" pname "]"))
	(recenter)

	;; just ask here and make it later
	(setq xref-current-project nil)
	(xref-softly-preset-project pname)
	(setq pedit "n")
	(if hasrecipe
		(progn
		  (setq pedit "h")
		  (while (equal pedit "h")
			(progn
			  (setq pedit (read-from-minibuffer 
						   "Do you wish to adjust options manually (h for help) [ynh]? " "n"))
			  (if (equal pedit "h") (xref-project-manual-edit-help))
			  )))
	  )
	(if (or (equal pedit "y") (equal pedit "Y"))
		(progn
		  ;; set the project active (for case he does not quit the buffer)
		  (setq xref-current-project pname)
		  ;; Quiting here !!!!
		  (error (format "All done. Do not forget to Create %stags after editing." (if hasrecipe "recipe and " "")))
		  ))

	(setq pact "n")
	(if (or (equal pedit "n") (equal pedit "N"))
		(setq pact (read-from-minibuffer 
					"Make the new project active [yn]? " "n"))
	  )

	(if (or (equal pedit "n") (equal pedit "N"))
		(progn
		  (kill-buffer nil)
		  (setq recipe-created nil)
		  (if hasrecipe
			  (progn
				(setq crrecipe "y")
				(setq crrecipe (read-from-minibuffer 
								 "Can I create recipe file for this project [yn]? " crrecipe))
				(if (or (equal crrecipe "y") (equal crrecipe "Y"))
					(progn
					  (setq xref-current-project pname)
					  (xref-compile-and-busy-wait 'buildrecipe)
					  (setq xref-current-project nil)
					  (setq recipe-created t)
					  ))))
		  (if (and hasrecipe (not recipe-created))
			  (setq crtag "n")
			(setq crtag "y")
			)
		  (setq crtag (read-from-minibuffer 
					   "Can I create tags for this project [yn]? " crtag))
		  (if recipe-created (xref-delete-window))
		  (if (or (equal crtag "y") (equal crtag "Y"))
			  (progn
				(setq xref-current-project pname)
				(xref-create-refs)
				(setq xref-current-project nil)
;;				(xref-project-exit-message-on-nonzero stat)

	  ))))

	(if (or (equal pact "y") (equal pact "Y"))
		(progn
		  (setq xref-current-project pname)
		  (message "All done. Project '%s' is now active." pname)
		  )
	  (setq xref-current-project nil)
	  (message "All done. Setting project auto-detection.")
	  )
))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun xref-project-active () 
"Show currently  active project name. 

This  function is useful  mainly if  you are  not sure  which .xrefrc
section applies to the currently edited file.
"
  (interactive "")
  (xref-entry-point-make-initialisations)
  (if xref-current-project
	  (message "Active project (manual): %s" xref-active-project)
	(message "Active project (auto): %s" xref-active-project)
))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun xref-project-edit-options ()
"Edit manually .xrefrc file. 

This  function just  loads the  .xrefrc file  and goes  to  the active
project section.  You  need to edit this text  file manually. For more
info about  the .xrefrc  file format, read  'xrefrc' manual  page. For
more info about available options, read the 'xref' manual page.
"
  (interactive "")
  (xref-entry-point-make-initialisations)
  (find-file xref-options-file)
  (goto-char (point-min))
  (if xref-active-project
	  (progn
		(if (not (search-forward (concat "[" xref-active-project "]") nil t))
			(if (not (search-forward (concat "[" xref-active-project ":") nil t))
				(if (not (search-forward (concat ":" xref-active-project "]") nil t))
					(if (not (search-forward (concat ":" xref-active-project ":") nil t))
						  (message "Options for %s found" xref-active-project)
					  ))))
		  (beginning-of-line)
		  (next-line 1)
		  ))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;; (X)EMACS - IDE  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun xref-ide-get-last-command (project commands default)
  (let ((as) (res))
	(setq as (assoc project (eval commands)))
	(if as 
		(setq res (cdr as))
	  (setq res default)
	  )
	res
))

(defun xref-ide-set-last-command (project command commands)
  (let ((as))
	(setq as (assoc project (eval commands)))
	(if as 
		(setcdr as command)
	  (set commands (cons (cons project command) (eval commands)))
	  )
))

;; this function can be used to run a user-defined non-interactive
;; command; the value of 'name' argument has to be set in .xrefrc file.

(defun xref-compile-function (action) 
  (let ((comm) (name) (arg) (bfile) (shcomm) (cwin) (sdir) (dispatch-data))
	(require 'compile)
	(xref-entry-point-make-initialisations)
	(setq dispatch-data (xref-get-basic-server-dispatch-data nil))
	(if (not (featurep 'compile))
		(error "Package 'compile.el' not found; please install it first.")
	  )
	(cond
	 (
	  (eq action 'compilefile)
	  (setq name "compilefile")
	  (setq arg (xref-file-last-name (buffer-file-name))))
	 (
	  (eq action 'compiledir)
	  (setq name "compiledir")
	  (setq arg (xref-backslashify-name (xref-file-directory-name (buffer-file-name)))))
	 (
	  (eq action 'compileproject)
	  (setq name "compileproject")
	  (setq arg xref-active-project))
	 (
	  (eq action 'buildrecipe)
	  (setq name "buildrecipe")
	  (setq arg xref-active-project))
	 )
	(setq sdir default-directory)
	(setq comm (format (xref-get-env name) (xref-optionify-string arg "\"")))
	(if (and (not xref-always-batch-file) (not (string-match "\n" comm)))
		(setq shcomm comm)
	  (setq bfile (xref-create-batch-file default-directory comm))
	  (if (eq xref-platform 'windows)
		  (setq shcomm (format "%s" (xref-optionify-string bfile "\"")))
		(setq shcomm (format "%s %s" xref-shell (xref-optionify-string bfile "\"")))
		))
	(if (not (eq action 'buildrecipe))
		(xref-ide-set-last-command xref-active-project action 'xref-ide-last-compile-commands)
	  )
	(xref-display-and-set-maybe-existing-window xref-compilation-buffer nil t)
	(compile shcomm)
	(sleep-for 0.3)
	(setq cwin (get-buffer-window xref-compilation-buffer))
	(if cwin
		(progn
		  (select-window cwin)
		  (goto-char (point-max))
		  (setq default-directory sdir)
		  (setq xref-this-buffer-dispatch-data dispatch-data)
		  (setq tab-width 8) 	;; as usual on terminals
		  (bury-buffer (current-buffer))
		  ))
))

(defun xref-process-status (proc)
  (let ((res))
	(condition-case nil
		(setq res (process-status proc))
	  (error (setq res nil))
	  )
	res
))

(defun xref-compile-and-busy-wait (action)
  (let ((cproc) (owin) (cwin) (opc) (sdir))
	(setq owin (get-buffer-window (current-buffer)))
	(setq sdir default-directory)
	(setq opc process-connection-type)
	(setq  process-connection-type nil)
	(xref-compile-function action)
	(setq cproc (get-buffer-process xref-compilation-buffer))
	(while (eq (xref-process-status cproc) 'run)
	  (setq cwin (get-buffer-window xref-compilation-buffer))
	  (if cwin
		  (progn
			(select-window (get-buffer-window xref-compilation-buffer))
			(end-of-buffer)
			(setq default-directory sdir)
			(setq tab-width 8)      ;; as usual on terminals
			))
	  (sleep-for 0.1) ;; sleep at least some time
	  (discard-input) ;; discard input, so sit-for will refresh and wait
	  (sit-for 0.1)
	  )
	(sleep-for 0.1)
	(discard-input)
	(sit-for 0.1)
	(select-window owin)
	(setq process-connection-type opc)
))


(defun xref-ide-compile () 
"Repeat last  compilation command.

This  is  whichever  of  \"Compile file\",  \"Compile  Directory\"  or
\"Compile Project\" was last executed for this project.
"
  (interactive "")
  (xref-entry-point-make-initialisations)
  (xref-compile-function (xref-ide-get-last-command xref-active-project 
													'xref-ide-last-compile-commands
													xref-ide-last-compile-command))
)

(defun xref-ide-compile-file () 
"Compile file. 

The compile  command must  be specified in  your .xrefrc file  via the
'-set compilefile <command>' option.  If the <command> string contains
the %s  character sequence,  it will  be replaced by  the name  of the
currently edited  file. Read the 'xref'  man page for  more info about
the -set option.  This function  actually just calls the Emacs compile
function with  the appropriate  command string. You  need to  have the
'compile' package installed.
"
  (interactive "")
  (xref-compile-function 'compilefile)
)

(defun xref-ide-compile-dir () 
"Compile  directory. 

The compile  command must  be specified in  your .xrefrc file  via the
'-set compiledir  <command>' option. If the  <command> string contains
the  %s  character  sequence,  it  will be  replaced  by  the  current
directory name.  Read the 'xref' man page for more info about the -set
option.  This function actually  just calls the Emacs compile function
with the  appropriate command string.  You need to have  the 'compile'
package installed.
"
  (interactive "")
  (xref-compile-function 'compiledir)
)

(defun xref-ide-compile-project () 
"Compile  project. 

The compile  command must  be specified in  your .xrefrc file  via the
'-set  compileproject  <command>'  option.    You  need  to  have  the
'compile' package installed.
"
  (interactive "")
  (xref-compile-function 'compileproject)
)

(defun xref-build-recipe () 
"Build recipe file. 

The  command for  rebuilding recipe  file (and  your project)  must be
specified in your .xrefrc file via the '-set buildrecipe <command>'
option.  You need to have the 'compile' package installed.
"
  (interactive "")
  (xref-compile-function 'buildrecipe)  
)


(defun xref-ide-previous-error ()
"Move to  the previous  error of the  last compilation.  

This function actually calls `next-error' with argument -1. See also
`xref-alternative-previous-reference'.

"
  (interactive "")
  (next-error -1)
)

(defun xref-ide-next-error ()
"Move  to the  next  error  of the  last  compilation.  

This  function actually calls `next-error' with argument 1. See also
`xref-alternative-next-reference'.

"
  (interactive "")
  (next-error 1)
)

;; kill the process associated with this buffer (if any)
(defun xref-kill-this-buffers-process ()
  (let ((pl) (pp))
	(setq pl (process-list))
	(while pl
	  (setq pp (car pl))
	  (setq pl (cdr pl))
	  (if (equal (process-buffer pp) (current-buffer))
		  (progn
			(kill-process pp)
			(sleep-for 0.1)
			(discard-input)
			(while (eq (process-status pp) 'run)
			  (message "Waiting until process dies.")
			  (sleep-for 0.1)
			  (discard-input)
			  )
			(message "Done.")
			(setq pl nil)
			)))
))

(defun xref-escape-dq (ss)
  (let ((res) (mp) (lmp))
	(setq res ss)
	(setq lmp 0)
	(setq mp (string-match "\"" res))
	(while mp
	  (setq res (format "%s\\%s" (substring res 0 mp) (substring res mp)))
	  (setq mp (string-match "\"" res (+ mp 2)))
	  )
	res
))

(defun xref-cr-echos-commands (command)
  (let ((res) (mp) (lmp) (line))
	(setq res "")
	(setq lmp 0)
	(setq mp (string-match "\n" command))
	(while mp
	  (setq line (substring command lmp mp))
	  ;; do not echo exit and empty lines so as not to damage the return value
	  (if (or (equal line "exit") (xref-exact-string-match "[ \t]*" line))
		  (setq res (format "%s\n%s" res line))
		(setq res (format "%s\necho \">%s\"\n%s" res (xref-escape-dq line) line))
		)
	  (setq lmp (+ mp 1))
	  (setq mp (string-match "\n" command lmp))
	  )
	;; don't do this, command ends with \n (last line is empty)
	;; following lines damages return value
	;;(setq line (substring command lmp))
	;;(setq res (format "%s\necho %s\n%s" res line line))
	res
))


(defun xref-remove-starting-tilda (fname)
  (let ((res))
	(if (equal (elt fname 0) ?~)
		(setq res (format "%s%s" (getenv "HOME") (substring fname 1)))
	  (setq res fname)
	  )
	res
))

(defun xref-create-batch-file (bdir command)
  (let ((res) (cc))
	(setq cc "")
	(if (and (eq xref-platform 'windows)
			 (> (length bdir) 1)
			 (equal (elt bdir 1) ?:))
		(setq cc (format "%s%s:\n" cc (substring bdir 0 1)))
	  )
	(setq cc (format "%scd %s\n%s\n" cc (xref-optionify-string (xref-backslashify-name (xref-remove-starting-tilda bdir)) "\"") command))
	(if (not (eq xref-platform 'windows))
		(setq cc (xref-cr-echos-commands cc))
	  )
	(get-buffer-create " *xref-run-command*")
	(set-buffer " *xref-run-command*")
	(xref-erase-buffer)
	(insert cc)
	(write-region 1 (+ (buffer-size) 1) xref-run-batch-file)
	(kill-buffer nil)
	(setq res xref-run-batch-file)
	res
))
  

;; this function can be used to run a Xref user-defined interactive
;; command; the value of 'name' argument has to be set in .xrefrc file.
(defun xref-run-function (name skip-one-win)
  (let ((bb) (rc) (args) (ww) (bdir) (cc)
		(bfile) (command) (dispatch-data) (owin) (cwin))
	(setq dispatch-data (xref-get-basic-server-dispatch-data nil))
	(setq bdir default-directory)
	(require 'comint)
	(if (not (featurep 'comint))
		(error "Package 'comint.el' not found; please install it first")
	  )
	(setq rc (xref-get-env name))
	(if rc
		(progn
		  (if (and (xref-buffer-has-one-of-suffixes (buffer-file-name) xref-java-suffixes)
			   (or (equal name xref-run-this-option) (equal name xref-run-option)))
			  (setq command (format rc (xref-compute-simple-information 
										"-olcxcurrentclass -noerrors")))
			(setq command rc)
			)
		  (setq bfile (xref-create-batch-file bdir command))
		  (if (eq xref-platform 'windows)
			  (setq args (cons bfile nil))
			(setq args (cons xref-shell (cons bfile nil)))
			)
		  (setq owin (get-buffer-window xref-run-buffer))
		  (if owin
			  (select-window owin)
			(setq cwin (get-buffer-window xref-compilation-buffer))
			(if cwin (select-window cwin))
			(xref-display-and-set-new-dialog-window xref-run-buffer nil t)
			(bury-buffer (current-buffer))
			)
		  (xref-kill-this-buffers-process)
		  (xref-erase-buffer)
		  (xref-ide-set-last-command xref-active-project name 'xref-ide-last-run-commands)
		  (apply 'make-comint xref-run-buffer-no-stars (car args) nil (cdr args))
		  (setq default-directory bdir)
		  (setq xref-this-buffer-dispatch-data dispatch-data)
		  )
	  )
))

(defun xref-ide-run ()
"Repeat last run command (i.e.  one of run1, run2, ...). 

If no run  command was previously executed, run  the command specified
vith  the '-set  run <command>'  option  in your  .xrefrc file.   This
function actually  just calls the `make-comint'  function with command
string taken  from .xrefrc configuration  file.  You need to  have the
'comint' package installed.
"
  (interactive "")
  (xref-entry-point-make-initialisations)
  (xref-run-function (xref-ide-get-last-command 
                      xref-active-project 
                      'xref-ide-last-run-commands "run") nil)
)

(defun xref-ide-run-this ()
"Run 'runthis' command.

Run the command  specified by '-set runthis <command>'  option in your
.xrefrc file.  If '%s' occurs in  <command> it will be replaced by the
name of the  currently edited class.  It also sets  this command to be
executed by the default run function 'xref-ide-run'.
"
  (interactive "")
  (xref-entry-point-make-initialisations)
  (xref-run-function xref-run-this-option nil)
)

(defun xref-ide-run1 ()
"Run 'run1' command.

Run  the command  specified by  '-set run1  <command>' option  in your
.xrefrc file.  It also sets this command to be executed by the default
run function 'xref-ide-run'.
"
  (interactive "")
  (xref-entry-point-make-initialisations)
  (xref-run-function "run1" nil)
)


(defun xref-ide-run2 ()
"Run 'run2' command.

Run  the command  specified by  '-set run2  <command>' option  in your
.xrefrc file.  It also sets this command to be executed by the default
run function 'xref-ide-run'.
"
  (interactive "")
  (xref-entry-point-make-initialisations)
  (xref-run-function "run2" nil)
)


(defun xref-ide-run3 ()
"Run 'run3' command.

Run  the command  specified by  '-set run3  <command>' option  in your
.xrefrc file.  It also sets this command to be executed by the default
run function 'xref-ide-run'.
"
  (interactive "")
  (xref-entry-point-make-initialisations)
  (xref-run-function "run3" nil)
)


(defun xref-ide-run4 ()
"Run 'run4' command.

Run  the command  specified by  '-set run4  <command>' option  in your
.xrefrc file.  It also sets this command to be executed by the default
run function 'xref-ide-run'.
"
  (interactive "")
  (xref-entry-point-make-initialisations)
  (xref-run-function "run4" nil)
)


(defun xref-ide-run5 ()
"Run 'run5' command.

Run  the command  specified by  '-set run5  <command>' option  in your
.xrefrc file.  It also sets this command to be executed by the default
run function 'xref-ide-run'.
"
  (interactive "")
  (xref-entry-point-make-initialisations)
  (xref-run-function "run5" nil)
)

(defun xref-ide-compile-run () 
"Invoke (last)  compile command followed  by (last) run  command.  

The run command is invoked only if it is non-empty and the compilation
is successful.  See also `xref-ide-compile' and `xref-ide-run'.
"
  (interactive "")
  (let ((cproc) (owin) (cwin) (rc) (opc) (sdir) (dispatch-data))
	(xref-soft-delete-window xref-run-buffer)
	(xref-entry-point-make-initialisations)
	(setq owin (get-buffer-window (current-buffer)))
	(setq dispatch-data (xref-get-basic-server-dispatch-data nil))
	(setq sdir default-directory)
	(setq opc process-connection-type)
	(setq  process-connection-type nil)
	(xref-ide-compile)
	(setq cproc (get-buffer-process xref-compilation-buffer))
	(if cproc
		(set-process-sentinel cproc (list 'lambda '(cc message)
		   (list 'xref-ide-compile-run-sentinel 'cc 'message owin opc)))
	  )
	(sit-for .1)
	(setq cwin (get-buffer-window xref-compilation-buffer))
	(if cwin
		(progn
		  (select-window (get-buffer-window xref-compilation-buffer))
		  (goto-char (point-max))
		  (setq default-directory sdir)
		  (setq xref-this-buffer-dispatch-data dispatch-data)
		  (setq tab-width 8) 	;; as usual on terminals
		  (bury-buffer (current-buffer))
		  ))
	(setq process-connection-type opc)
	(if (not cproc) ;; process ahs finished during execution
		(xref-ide-compile-run-sentinel cproc "finished" owin opc)
	  )
))

(defun xref-ide-compile-run-sentinel (cc message owin opc)
  (let ((rc))
	(select-window owin)
	;;(setq process-connection-type opc)
	(if (and (>= (length message) 8)
			 (equal (substring message 0 8) "finished"))
		(progn
		  (setq rc (xref-get-env (xref-ide-get-last-command 
								  xref-active-project 
								  'xref-ide-last-run-commands
								  "run")))
		  (if (and rc (not (equal rc "")))
			  (progn
				(xref-run-function (xref-ide-get-last-command 
									xref-active-project 
									'xref-ide-last-run-commands
									"run") t)
		  ))))
))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;; TAGS maintenance ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun xref-tags-dispatch-href-in-error (ss i len)
  (let ((tlen) (cc) (link) (ccc) (bp) (tlink))
	(setq tlen (xref-server-dispatch-get-int-attr PPCA_LEN))
	(setq tlink (cdr (assoc "HREF" xref-server-ctag-attributes)))
	;; tag
	(setq bp (point))
	(insert "HREF=\"" tlink "\"")
	(put-text-property bp
					   (point)
					   'invisible t)
	(setq bp (point))
	(insert (xref-char-list-substring ss i (+ i tlen)))
	(setq i (+ i tlen))
	(put-text-property bp (point) 'face 'xref-error-face)
	(put-text-property bp (point) 'mouse-face 'highlight)
	;; after tag text
	(setq i (xref-server-parse-xml-tag ss i len))
	(xref-server-dispatch-require-end-ctag "A")
	i
))

(defun xref-tags-dispatch-single-error-if-any (ss i len)
  (if (or (equal xref-server-ctag PPC_WARNING)
		  (equal xref-server-ctag PPC_ERROR)
		  (equal xref-server-ctag PPC_COMPOSED_ERROR)
		  (equal xref-server-ctag PPC_FATAL_ERROR)
		  (equal xref-server-ctag PPC_LICENSE_ERROR)
		  )
	  (progn
		(setq i (xref-tags-dispatch-single-item ss i len))
		(setq i (xref-server-parse-xml-tag ss i len))
		)
	)
  i
)

(defun xref-tags-dispatch-error (ss i len tag)
  (let ((tlen) (cc) (cw) (link) (j) (cclen) (ccc) (bp) (tlink))
	(setq cw (selected-window))
	(setq tlen (xref-server-dispatch-get-int-attr PPCA_LEN))
	(setq cc (xref-char-list-substring ss i (+ i tlen)))
	(setq i (+ i tlen))
	(if (and (>= tlen 16) 
			 (equal (substring cc 0 16) "<A HREF=\"file://"))
		(progn
		  ;; hypertext link
		  (setq cclen (length cc))
		  (setq bp (point))
		  (setq j (xref-server-parse-xml-tag cc 0 cclen))
		  (setq j (xref-tags-dispatch-href-in-error cc j cclen))
		  (insert (substring cc j))
		  )
	  (setq bp (point))
	  (insert-string cc)
	  (put-text-property bp (point) 'face 'xref-error-face)
	  )
	(setq i (xref-server-parse-xml-tag ss i len))
	(setq i (xref-tags-dispatch-single-error-if-any ss i len)) ;; hack error inside composed error
	(if (equal xref-server-ctag "A")
		(progn
		  (setq i (xref-tags-dispatch-href-in-error ss i len))
		  (setq i (xref-server-parse-xml-tag ss i len))
		  ))
	(if (equal tag PPC_COMPOSED_ERROR)
		(progn
		  (setq i (xref-tags-dispatch-single-error-if-any ss i len))
		  (while (equal xref-server-ctag PPC_COMPOSED_ERROR_PART)
			(setq tlen (xref-server-dispatch-get-int-attr PPCA_LEN))
			(setq cc (xref-char-list-substring ss i (+ i tlen)))
			(setq i (+ i tlen))
			(insert-string cc)
			(setq i (xref-server-parse-xml-tag ss i len))
			(xref-server-dispatch-require-end-ctag PPC_COMPOSED_ERROR_PART)
			(setq i (xref-server-parse-xml-tag ss i len))
			;; handle degenerated case when another error occurs inside composed error
			(setq i (xref-tags-dispatch-single-error-if-any ss i len))
		  )))
	(xref-server-dispatch-require-end-ctag tag)
	i
))

(defun xref-tags-dispatch-information (ss i len tag)
  (let ((tlen) (cc) (cw) (dw))
	(setq cw (selected-window))
	(setq tlen (xref-server-dispatch-get-int-attr PPCA_LEN))
	(setq cc (xref-char-list-substring ss i (+ i tlen)))
	(setq i (+ i tlen))
	(setq i (xref-server-parse-xml-tag ss i len))
	(xref-server-dispatch-require-end-ctag tag)
	(insert-string cc)
	(newline)
	i
))

(defun xref-tags-dispatch-single-item (ss i len)
  (cond
   (
	(equal xref-server-ctag PPC_INFORMATION)
	(setq i (xref-tags-dispatch-information ss i len xref-server-ctag)))
   (
	(equal xref-server-ctag PPC_WARNING)
	(setq i (xref-tags-dispatch-error ss i len xref-server-ctag)))
   (
	(equal xref-server-ctag PPC_ERROR)
	(setq i (xref-tags-dispatch-error ss i len xref-server-ctag)))
   (
	(equal xref-server-ctag PPC_COMPOSED_ERROR)
	(setq i (xref-tags-dispatch-error ss i len xref-server-ctag)))
   (
	(equal xref-server-ctag PPC_FATAL_ERROR)
	(setq i (xref-tags-dispatch-error ss i len xref-server-ctag)))
   (
	(equal xref-server-ctag PPC_LICENSE_ERROR)
	(setq i (xref-tags-dispatch-error ss i len xref-server-ctag)))
   (	
	t
	;;	  (error "unexpected tag in log file: %s" xref-server-ctag))
	(message "unexpected tag in log file: %s" xref-server-ctag))
   )
  i
)

(defun xref-tags-dispatch (ss i len)
  (let ((x) (j))
	(setq j 0)
	(if xref-debug-mode (message "tag dispatching: %s" ss))
	(setq i (xref-server-dispatch-skip-blank ss i len))
	(while (and (< i len) (eq (elt ss i) ?<))
	  (setq i (xref-server-parse-xml-tag ss i len))
	  (setq i (xref-tags-dispatch-single-item ss i len))
	  (if xref-xemacs-mule-problem (progn (setq len (- len i)) (setq ss (nthcdr i ss)) (setq j (+ j i)) (setq i 0)))
	  (setq i (xref-server-dispatch-skip-blank ss i len))
	  )
	(+ i j)
))

(defun xref-tags-log-exit ()
  (interactive "")
  (xref-soft-delete-window xref-log-view-buffer)
)

(defun xref-tags-log-browse ()
  (interactive "")
  (let ((el) (line) (file) (b) (i) (di) (len) (ln))
	(end-of-line)
	(setq el (point))
	(beginning-of-line)
	(setq line (buffer-substring (point) el))
;;(message "checking %s" (substring line 0 6))
	(if (equal (substring line 0 6) "HREF=\"")
		(progn
		  (setq b 6)
		  (if (equal (substring line b (+ b 5)) "file:")
			  (progn
				(setq b (+ b 5))
				(if (and (eq (elt line b) ?/)
						 (eq (elt line (+ b 1)) ?/))
					(setq b (+ b 2))
				  )))
		  (setq i b)
		  (setq di 0)
		  (setq len (length line))
		  (while (and (< i len)
					  (not (eq (elt line i) ?\"))
					  )
			(if (eq (elt line i) ?#) (setq di i))
			(setq i (1+ i))
			)
		  (if (eq di 0) (setq di i))
		  (setq file (substring line b di))
		  (setq ln (string-to-int (substring line (+ di 1) i)))
		  (xref-show-file-line-in-caller-window file ln)
		  )
	  (xref-find-file-on-point)
	  )
))

(defun xref-tags-log-mouse-button (event)
  (interactive "e")
  (mouse-set-point event)
  (xref-tags-log-browse)
)

(defvar xref-tags-log-key-map (make-sparse-keymap "Xref symbol-resolution"))
(define-key xref-tags-log-key-map "\e" 'xref-tags-log-exit)
(define-key xref-tags-log-key-map "q" 'xref-tags-log-exit)
(define-key xref-tags-log-key-map " " 'xref-tags-log-browse)
(define-key xref-tags-log-key-map [newline] 'xref-tags-log-browse)
(define-key xref-tags-log-key-map [return] 'xref-tags-log-browse)
(define-key xref-tags-log-key-map "\C-m" 'xref-tags-log-browse)
(xref-bind-default-button xref-tags-log-key-map 'xref-tags-log-mouse-button)

(defun xref-tags-process-show-log ()
  (let ((ss) (len) (dispatch-data) (conf))
	(setq dispatch-data (xref-get-basic-server-dispatch-data 'nil))
	(get-buffer-create xref-server-answer-buffer)
	(set-buffer xref-server-answer-buffer)
	(setq buffer-read-only nil)
	;; (xref-erase-buffer)
	(insert-file-contents xref-tags-tasks-ofile  nil nil nil t)
	(if xref-xemacs-mule-problem 
		(setq ss (xref-buffer-char-list))
	  (setq ss (buffer-string))
	  )
	(setq len (length ss))
	(kill-buffer xref-server-answer-buffer)
	(if (> len 0)
		(progn
		  (setq conf (read-from-minibuffer 
					  "View log file [yn]? " "n"))
		  (if (or (equal conf "y") (equal conf "Y"))
			  (progn
				(xref-delete-window-in-any-frame xref-log-view-buffer nil t)
				(xref-display-and-set-new-dialog-window xref-log-view-buffer nil t)
				(setq xref-this-buffer-dispatch-data dispatch-data)
				(xref-tags-dispatch ss 0 len)
				(setq buffer-read-only t)
				(xref-use-local-map xref-tags-log-key-map)
				))
		  ))
))

(defun xref-update-tags (option log)
  (xref-server-tags-process (cons option nil))
  (if log (xref-tags-process-show-log))
)

(defvar xref-save-buffer-dialog-map (make-sparse-keymap "Xref save buffer dialog"))
(xref-add-basic-modal-keybindings xref-save-buffer-dialog-map)

(defun xref-before-push-actions ()
  (let ((sel))
	(if xref-smart-browsing-mode
		(progn
		  (if (not (verify-visited-file-modtime (current-buffer)))
			  (xref-find-file (buffer-file-name))
			)
		  (if (buffer-modified-p (current-buffer))
			  (progn
				(if xref-save-buffers-without-prompt
					(setq sel 3)
				  
				  (setq sel (xref-modal-dialog xref-selection-modal-buffer
"Current buffer not saved in smart browsing mode.
----
 1.) Save current buffer
 2.) Save all buffers (ask about each one)
 3.) Save all buffers (don't ask)
 4.) Cancel
----
"
                       3 0 t xref-save-buffer-dialog-map nil))
				  ;; TODO !!!!!!!! FIX THIS
				  ;; this should go through xref-save-some-buffers, so that 
				  ;; it cooperates with possible version control system
				  ;; unfortunately, this will be hard to implement
				  )
				(if (eq sel 3)
					(xref-save-buffer)
				  (if (eq sel 4)
					  (save-some-buffers nil)
					(if (eq sel 5)
						(xref-save-some-buffers t)
					  (if (eq sel 6)
						  (error "** Current buffer must be saved in smart browsing mode **")
						))))
				))))
	(if (or xref-auto-update-tags-before-push 
			(and xref-smart-browsing-mode xref-smart-browsing-mode-auto-update))
		(progn
		  (xref-update-tags "-fastupdate" nil)
		  (setq xref-full-auto-update-allowed t)
		  ;; clear the 100% message
		  (message "")
		  ))
))

(defun xref-create-refs ()
"Create tags.  

This function  executes 'xref -create'.  The effect  of the invocation
is  that the  Xrefactory tag  files (used  by the  source  browser and
refactorer)  are  created.  The  behavior  of  the  'xref' command  is
controlled by options read from the '.xrefrc' file.
"
  (interactive "")
  (xref-entry-point-make-initialisations)
  (if xref-save-buffers-without-prompt (xref-save-some-buffers t))
  (xref-server-tags-process (cons "-create" nil))
  (xref-tags-process-show-log)
)

(defun xref-fast-update-refs ()
"Fast update of tags.  

This  function  executes  'xref   -fastupdate'.   The  effect  of  the
invocation is  that Xrefactory tag  files (used by the  source browser
and refactorer)  are updated.  The  behavior of the 'xref'  command is
controlled by options read from the '.xrefrc' file.
"
  (interactive "")
  (xref-entry-point-make-initialisations)
  (xref-update-tags "-fastupdate" t)
)

(defun xref-update-refs (log)
"Full update of tags.  

This function  executes 'xref -update'.  The effect  of the invocation
is  that  Xrefactory  tag  files  (used  by  the  source  browser  and
refactorer)  are  updated.  The  behavior  of  the  'xref' command  is
controlled by options read from the '.xrefrc' file.
"
  (interactive "P")
  (xref-entry-point-make-initialisations)
  (if xref-save-buffers-without-prompt (xref-save-some-buffers t))
  (xref-server-tags-process (cons "-update" nil))
  (xref-tags-process-show-log)
)


(defun xref-gen-html-documentation ()
"Generate HTML documentation for active project.  

This  function  executes 'xref  -html'  command.   The  effect of  the
invocation is that Xrefactory  generates files containing html form of
source files.  This function requires  that tag file is created and is
up to date.  It also requires that options controlling HTML generation
for  active project  are  configured in  the Xrefactory  configuration
file.
"
  (interactive "")
  (xref-entry-point-make-initialisations)
  (if xref-save-buffers-without-prompt (xref-save-some-buffers t))
  (xref-server-tags-process (cons "-html" nil))
  (xref-tags-process-show-log)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;; COMPLETION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun xref-delete-completion-window ()
  (xref-delete-window-in-any-frame xref-completions-buffer nil t)
)

(defun xref-interactive-completion-select (&optional argp)
  "Go to the reference corresponding to this line."
  (interactive "P")
  (let ((lineno 1) (replace-flag) (cw) (offset))
	(if (eq (point) (point-max))
		(setq replace-flag nil)
	  (setq replace-flag t)
	  (setq lineno (count-lines 1 (+ (point) 1)))
	  )
	(xref-send-data-to-process-and-dispatch (format "-olcomplselect%d" lineno) xref-completions-dispatch-data nil)
	(if (equal xref-completions-windows-counter  0)
		(xref-delete-completion-window)
	  (setq xref-completions-windows-counter (1- xref-completions-windows-counter))
	  ;; display previous completion, but keeps cursor position and focus
	  (setq cw (selected-window))
	  (xref-soft-select-dispach-data-caller-window xref-completions-dispatch-data)
	  (set-marker xref-completion-pop-marker (point))
	  (select-window cw)
	  (xref-interactive-completion-previous nil)
	  (xref-soft-select-dispach-data-caller-window xref-completions-dispatch-data)
	  (xref-switch-to-marker xref-completion-pop-marker)
	  )
	t
))

(defun xref-interactive-completion-previous (event)
  "Previous completions."
  (interactive "i")
  (xref-send-data-to-process-and-dispatch "-olcomplback" xref-completions-dispatch-data nil)
  t
)

(defun xref-interactive-completion-next (event)
  "Next completions."
  (interactive "i")
  (xref-send-data-to-process-and-dispatch "-olcomplforward" xref-completions-dispatch-data nil)
  t
)

(defun xref-interactive-completion-mouse-select (event)
  (interactive "e")
  (mouse-set-point event)
  (xref-interactive-completion-select)
  t
)

(defun xref-interactive-completion-mouse-previous (event)
  (interactive "e")
  (mouse-set-point event)
  (xref-interactive-completion-previous event)
  t
)

(defun xref-interactive-completion-mouse-next (event)
  (interactive "e")
  (mouse-set-point event)
  (xref-interactive-completion-next event)
  t
)

(defun xref-interactive-completion-escape (event)
  "Close completions; recover original situation with no completin selected."
  (interactive "i")
  (xref-delete-completion-window)
  (xref-select-dispach-data-caller-window xref-completions-dispatch-data)
  ;; following is to reconstitute original text
  ;; TODO what about fqt completions?
  (xref-switch-to-marker xref-completion-marker)
  (xref-insert-completion-and-delete-pending-id 
   (format "%s%s" xref-completion-id-before-point xref-completion-id-after-point))
  (xref-set-to-marker xref-completion-marker)
  (forward-char (length xref-completion-id-before-point))
  ;;(forward-char (length (car xref-completion-auto-search-list)))
  t
)

(defun xref-interactive-completion-q (event)
  "Quit or auto search for q."
  (interactive "i")
  (if (and xref-completion-quit-on-q (xref-completion-auto-search-list-empty))
	  (xref-interactive-completion-escape event)
	(xref-completion-auto-search)
	)
)

(defun xref-interactive-completion-mouse-escape (event)
  (interactive "e")
  (mouse-set-point event)
  (xref-interactive-completion-escape event)
  t
)

(defun xref-interactive-completion-close (event)
  "Close completions window."
  (interactive "i")
  (xref-delete-completion-window)
  (xref-select-dispach-data-caller-window xref-completions-dispatch-data)
  t
)

(defun xref-interactive-completion-mouse-close (event)
  (interactive "e")
  (mouse-set-point event)
  (xref-interactive-completion-close event)
  t
)

(defun xref-interactive-completion-goto (event)
  "Go to the position corresponding to this line."
  (interactive "i")
  (let ((lineno) (cw))
	(setq lineno (count-lines 1 (+ (point) 1)))
	(setq cw (selected-window))
	(xref-send-data-to-process-and-dispatch 
	 (format "-olcxcgoto%d %s" lineno (xref-html-browsing-options)) 
	 xref-completions-dispatch-data
	 nil)
	(sit-for 1)
	(select-window cw)
  t
))

(defun xref-interactive-completion-mouse-goto (event)
  (interactive "e")
  (mouse-set-point event)
  (xref-interactive-completion-goto event)
  t
)

(defun xref-completion-browse ()
  "Browse the reference corresponding to this line."
  (let ((lineno))
	(setq lineno (count-lines 1 (+ (point) 1)))
	(xref-send-data-to-process-and-dispatch 
	 (format "-olcxcbrowse%d %s" lineno (xref-html-browsing-options)) 
	 xref-completions-dispatch-data
	 nil)
  t
))

(defun xref-interactive-completion-browse (event)
  "Go to the reference corresponding to this line."
  (interactive "i")
  (xref-completion-browse)
)

(defun xref-interactive-completion-mouse-browse (event)
  (interactive "e")
  (mouse-set-point event)
  (xref-completion-browse)
  t
)

(defun xref-auto-search-search (str)
  (let ((sss) (ns))
	(setq sss (format "\\(^\\|^[^ ]*\\.\\)\\(%s\\)" str))
	(if (search-forward-regexp sss (point-max) t)
		  (setq ns (buffer-substring (match-beginning 2) (match-end 2)))
      (setq ns str)
	  )
	(set-text-properties 0 (length ns) nil ns)
	(setq xref-completion-auto-search-list 
		  (cons
		   ns
		   (cons (point) xref-completion-auto-search-list)))
))

(defun xref-insert-completion-and-delete-pending-id (cc)
  (let ((i) (len) (id) (ccc))
	(setq id (xref-get-identifier-after (point)))
	(setq len (length id))
	(setq i 1)
	(while (and (<= i len) (xref-string-has-prefix cc (substring id 0 i) nil))
	  (setq i (+ i 1)))
	(setq i (- i 1))
	(forward-char i)
	(xref-delete-pending-ident-after-completion)
	(setq ccc (substring cc i))
	(if (not (equal ccc "")) (insert-string ccc))
))

(defun xref-insert-completion (completion)
  (if xref-completion-delete-pending-identifier
	  (xref-insert-completion-and-delete-pending-id completion)
	(xref-insert-completion-and-delete-pending-id 
	 (concat completion xref-completion-id-after-point))
	(backward-char (length xref-completion-id-after-point))
	)
)

(defun xref-set-completion-src-window ()
  (let ((sb))  
	(setq sb (get-buffer-window (marker-buffer xref-completion-marker)))
	(if sb 
		(select-window sb)
	  (other-window -1)
	  (xref-switch-to-marker xref-completion-marker)
	  )
))


(defun xref-completion-source-mod ()
  (let ((cb))
	(setq cb (get-buffer-window (current-buffer)))
	(xref-set-completion-src-window)
	(xref-set-to-marker xref-completion-marker)
	(xref-insert-completion (car xref-completion-auto-search-list))
	(select-window cb)
	)
)

(defun xref-completion-auto-search-w ()
  (interactive "")
  (let ((pp) (lend) (ns) (addstr))
	(setq pp (point))
	(end-of-line)
	(setq lend (point))
	(goto-char (+ pp 1))
	(search-forward-regexp xref-forward-pass-identifier-regexp lend t)
	(setq addstr (buffer-substring pp (- (point) 1)))
	(setq ns (format "%s%s" (car xref-completion-auto-search-list) addstr))
	(beginning-of-line)
	(xref-auto-search-search ns)
	(xref-completion-source-mod)
))

(defun xref-completion-auto-search-s ()
  (interactive "")
  (xref-auto-search-search (car xref-completion-auto-search-list))
)

(defun xref-completion-auto-search-list-empty ()
  (let ((res))
	(setq res (<= (length xref-completion-auto-search-list) 4))
	res
))

(defun xref-completion-auto-search-back ()
  (interactive "")
  (let ((op) (cstr) (pstr))
  (if (not (xref-completion-auto-search-list-empty))
	  (progn
		(setq pstr (car xref-completion-auto-search-list))
		(setq xref-completion-auto-search-list 
			  (cdr (cdr xref-completion-auto-search-list )))
		(setq cstr (car xref-completion-auto-search-list))
		(setq op (car (cdr xref-completion-auto-search-list)))
		(goto-char op)
		(xref-completion-source-mod)
		))
))

(defun xref-completion-auto-search ()
  (interactive "")
  (let ((ns) (os) (nns))
	(setq os (car xref-completion-auto-search-list))
	(setq ns (format "%s%c" os last-command-char))
	(beginning-of-line)
	(xref-auto-search-search ns)
	(setq nns (car xref-completion-auto-search-list))	
	(xref-completion-source-mod)
))

(defun xref-completion-auto-switch ()
  (interactive "")
  (select-window (get-buffer-window (marker-buffer xref-completion-marker)))
  (xref-set-to-marker xref-completion-marker)
  (forward-char (length (car xref-completion-auto-search-list)))
  (self-insert-command 1)
)

(defun xref-completion ()
"Complete  identifier.   

This  function parses the  current buffer  up to  point.  Then  if the
identifier at point has a  unique completion it completes it. If there
is  more  than one  possible  completion, a  list  is  displayed in  a
separate window.
"
  (interactive "")
  (let ((opt))
	(xref-entry-point-make-initialisations)
	(setq opt (format "-olcxcomplet -maxcompls=%d" xref-max-completions))
	(if xref-completion-displays-internal-type
		(setq opt (format "%s -completeinterntype" opt))
	  )
	(if xref-completion-linkage-check
		(setq opt (format "%s -olchecklinkage" opt))
	  )
	(if xref-completion-access-check
		(setq opt (format "%s -olcheckaccess" opt))
	  )
	(if (> xref-java-fqt-name-completion-level 0)
		(setq opt (format "%s -olfqtcompletionslevel=%d" opt xref-java-fqt-name-completion-level))
	  )
	(if (> xref-completion-overload-wizard-deep 1)
		(setq opt (format "%s -completionoverloadwizdeep=%d" opt xref-completion-overload-wizard-deep))
	  )
	(if xref-completion-case-sensitive
		(setq opt (format "%s -completioncasesensitive" opt))
	  )
	(if xref-completion-inserts-parenthesis
		(setq opt (format "%s -completeparenthesis" opt))
	  )
	(if xref-completion-truncate-lines
		(setq opt (format "%s -olinelen=50000" opt))
	  (setq opt (format "%s -olinelen=%d" opt (window-width)))
	  )
	(setq xref-completion-id-after-point (xref-get-identifier-after (point)))
	(setq xref-completions-dispatch-data (xref-get-basic-server-dispatch-data 'xref-server-process))
	(xref-server-call-on-current-buffer-no-saves opt xref-completions-dispatch-data)
))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BROWSER ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun xref-is-this-regular-process-dispatch-data (dispatch-data)
  (let ((proc) (res))
	(setq proc (cdr (assoc 'process dispatch-data)))
	(setq res (eq proc 'xref-server-process))
	res
))

(defun xref-is-this-refactorer-process-dispatch-data (dispatch-data)
  (let ((proc) (res))
	(setq proc (cdr (assoc 'process dispatch-data)))
	(setq res (eq proc 'xref-refactorer-process))
	res
))

(defun xref-is-browser-window-displayed ()
  (let ((res) (resolvewin))
	(setq res nil)
	(if (xref-get-this-frame-dispatch-data)
		(progn
		  (setq resolvewin (cdr (assoc 'linked-resolution-window (xref-get-this-frame-dispatch-data))))
		  (if (and (windowp resolvewin) (window-live-p resolvewin))
			  (setq res resolvewin)
			)))
	res
))

(defun xref-is-refactory-window-displayed-in-the-frame (winid)
  (let ((res) (listwin))
	(setq res nil)
	(if (xref-get-this-frame-dispatch-data)
		(progn
		  (setq listwin (cdr (assoc winid (xref-get-this-frame-dispatch-data))))
		  (if (and (windowp listwin) (window-live-p listwin))
			  (setq res listwin)
			)))
	res
))

(defun xref-is-reflist-window-displayed ()
  (let ((res))
	(setq res (xref-is-refactory-window-displayed-in-the-frame 'linked-refs-window))
	res
))

(defun xref-is-failed-refactoring-window-displayed ()
  (let ((res))
	(setq res (xref-is-refactory-window-displayed-in-the-frame 'linked-refactoring-window))
	res
))

(defun xref-browser-of-failed-refactoring-is-displayed ()
  (let ((rw) (sw) (res))
	(setq res nil)
	(setq sw (selected-window))
	(setq rw (xref-is-failed-refactoring-window-displayed))
	(if rw
		(progn
		  (select-window rw)
		  (if (xref-is-this-refactorer-process-dispatch-data xref-this-buffer-dispatch-data)
			  (setq res rw)
			)
		  (select-window sw)
		  ))
	res
))

(defun xref-update-browser-if-displayed (oldwins)
  (let ((bw) (rw) (sw) (dispatch-data))
	(setq sw (selected-window))
	(setq bw (xref-is-browser-window-displayed))
	(if bw 
	  (progn
		(select-window bw)
		(setq dispatch-data xref-this-buffer-dispatch-data)
		(select-window sw)
		(xref-create-browser-windows nil dispatch-data)
		(xref-browser-dialog-set-new-filter dispatch-data)
		(xref-appropriate-browser-windows-sizes oldwins)
		(select-window sw)
		)
	  (setq rw (xref-is-reflist-window-displayed))
	  (if rw
		  (progn
			(select-window rw)
			(setq dispatch-data xref-this-buffer-dispatch-data)
			(select-window sw)
			(xref-references-set-filter 0 dispatch-data)
			(select-window sw)
			))
	  )
))

(defun xref-set-current-reference-list-pointer ()
  (let ((poin (point)) (lastref) (frame) (frame-assoc)
		(lineno (count-lines 1 (+ (point) 1))))
	(setq lastref nil)
	(if (not (eq xref-this-buffer-type 'reference-list))
		(error "Not a reference list buffer")
	  )
	(goto-char (point-min))
	(setq lastref (search-forward-regexp "^>" (point-max) t))
	(setq buffer-read-only nil)
	(if lastref
		(progn
		  (delete-backward-char 1)
		  (insert " ")
		  )
	  )
	(goto-line lineno)
	(beginning-of-line)
	(delete-char 1)
	(insert ">")
	(setq buffer-read-only t)
	(goto-char poin)
))

(defun xref-move-current-reference-list-pointer (direction)
  (let ((cline) (nlines) (lastref) (poin))
	(if (not (eq xref-this-buffer-type 'reference-list))
		(error "Not a reference list buffer")
	  )
	(setq poin (point))
	(goto-char (point-min))
	(setq lastref (search-forward-regexp "^>" (point-max) t))
	(if (not lastref)
		(goto-char poin)
	  )
	(beginning-of-line)
	(setq cline (count-lines (point-min) (point)))
	(setq nlines (count-lines (point-min) (point-max)))
	(if (< direction 0)
		(progn
		  (setq cline (- cline 1))
		  (if (>= cline 0)
			  (next-line -1)
			(goto-char (point-max))
			(beginning-of-line)
			;; (message "Moving to the last reference") (beep t)
			)
		  (xref-set-current-reference-list-pointer)
		  )
	  (setq cline (+ cline 1))
	  (if (< cline nlines)
		  (next-line 1)
		(goto-char (point-min))
		;; (message "Moving to the first reference") (beep t)
		)
	  (xref-set-current-reference-list-pointer)
	  )
))

(defun xref-move-current-reference-list-pointer-if-browser-displayed (direction)
  (let ((rw) (sw) (cline) (nlines))
	(setq sw (selected-window))
	(setq rw (xref-is-reflist-window-displayed))
	(if rw
		(progn
		  (select-window rw)
		  (if (xref-is-this-regular-process-dispatch-data xref-this-buffer-dispatch-data)
			  (xref-move-current-reference-list-pointer direction)
			)
		  (select-window sw)		  
		  ))
))

(defun xref-maybe-full-auto-update-in-push-quasiloop (i)
  (if (and (eq i 0) xref-full-auto-update-perform)
	  (progn
		(if xref-save-buffers-without-prompt
			(progn
			  (xref-save-some-buffers t)
			  ;; remove probable "no files ..." message
			  (message "Full update of Tags requested.")
			  )
		  )
		(xref-update-tags "-update" nil)
		)
	(setq i (+ i 1))
	)
  (setq i (+ i 1))
  i
)

(defun xref-push-references ()
"Push references of a symbol on to the browser stack.

This  function resolves  the symbol  under  point and  pushes all  its
references (from active project) on the browser stack.
"
  (interactive "")
  (let ((oldwins) (i))
	(xref-entry-point-make-initialisations)
	(xref-before-push-actions)
	(setq i 0)
	(while (< i 2)
	  (setq oldwins (xref-is-browser-window-displayed))
	  (xref-call-process-with-basic-file-data-all-saves "-olcxpushonly")
	  (setq i (xref-maybe-full-auto-update-in-push-quasiloop i))
	  )
	(xref-update-browser-if-displayed oldwins)
))

(defun xref-push-and-goto-definition ()
"Push references of a symbol on to the browser stack, goto definition.

This  function resolves  the symbol  under  point and  pushes all  its
references (from  the active project)  on the the browser  stack, then
goes to the symbol's definition.
"
  (interactive "")
  (let ((oldwins) (i))
	(xref-entry-point-make-initialisations)
	(xref-before-push-actions)
	(setq i 0)
	(while (< i 2)
	  (setq oldwins (xref-is-browser-window-displayed))
	  (xref-call-process-with-basic-file-data-all-saves 
	   (concat "-olcxpush" (xref-html-browsing-options)))
	  (setq i (xref-maybe-full-auto-update-in-push-quasiloop i))
	  )
	(xref-update-browser-if-displayed oldwins)
))

(defun xref-push-and-browse (push-option)
  (let ((sw) (oldwins) (i))
	(xref-before-push-actions)
	(setq sw (selected-window))
	(setq xref-global-dispatch-data (xref-get-basic-server-dispatch-data 
									 'xref-server-process))
	(setq i 0)
	(while (< i 2)
	  (setq oldwins (xref-create-browser-windows nil xref-global-dispatch-data))
	  ;; (select-window sw)
	  ;; reselect it like this, as caller may be moved (left-horizontal split)
	  (xref-select-dispach-data-caller-window xref-global-dispatch-data)
	  (xref-call-process-with-smart-browsing-sym-all-saves push-option)
	  (setq i (xref-maybe-full-auto-update-in-push-quasiloop i))
	  )
	(xref-update-browser-if-displayed oldwins)
))

(defun xref-browse-symbol ()
"Browse symbol.

This  function  resolves  the  symbol  under  point,  pushes  all  its
references  (from  the  active  project)  on the  browser  stack,  and
launches the Xrefactory interactive browser.
"
  (interactive "")
  (xref-entry-point-make-initialisations)
  (xref-clean-unused-browser-buffers)
  (xref-push-and-browse "-olcxpushonly")
)

(defun xref-push-global-unused-symbols ()
"Push unused global symbols.

This  function pushes  on to  the  reference stack  all references  to
global symbols  defined in  the project but  not used. As  symbols are
unused,  those  references  will  (usually) be  only  definitions  and
declarations.
"
  (interactive "")
  (xref-entry-point-make-initialisations)
  (xref-push-and-browse "-olcxpushglobalunused")
)


(defun xref-toggle-smart-browsing-mode ()
"Toggle between smart and normal browsing mode.

Two browsing modes  differ in the method of  resolution of the current
symbol.  In the normal mode, the currently edited buffer is parsed and
the symbol  is resolved during  the parsing.  In smart  browsing mode,
the position  of the current  symbol is computed  and the Tag  file is
scanned  searching for  this  reference. Both  methods  give the  same
results  for  symbols  stored  in  Tags,  they  differ  only  in  time
performances.

Normal  browsing  is more  advantageous  when  actively  working on  a
smaller  project, so  that Tag  files are  often out  of  date.  

Smart browsing is  advantageous when parsing takes too  much time (for
example in C++)  or when browsing a project which  is not under active
development.  Smart browsing does not  need to parse the file, however
it requires to have Tags always up to date.

Note: if you are using smart  browsing it is highly recommended to put
-storelocalxrefs into your project options.
"
  (interactive "")
  (xref-entry-point-make-initialisations)
  (if xref-smart-browsing-mode
	  (progn
		(setq xref-smart-browsing-mode nil)
		(message "The smart browsing mode is turned off.")
		)
	(setq xref-smart-browsing-mode t)
	(message "The smart browsing mode is turned on.")
	)
)

(defun xref-push-this-file-unused-symbols ()
"Push unused symbols defined in this file.

This function pushes on to  the reference stack all references to file
local symbols defined in the current file and not used anywhere in the
project.  As  symbols are unused,  those references will  (usually) be
only definitions and declarations.
"
  (interactive "")
  (xref-entry-point-make-initialisations)
  (xref-push-and-browse "-olcxpushfileunused")
)

(defun xref-push-name ()
"Push references to a given symbol. 

This  function asks  for  a symbol  name,  then it  inspects the  tags
looking for symbols  of that name.  For each such  symbol it loads all
its references.

If there  are several symbols of the  same name the user  is asked for
manual  selection.   The selection  menu  is  the  same as  for  other
browsing functions, including class hierarchies for inherited symbols.
"
  (interactive "")
  (xref-entry-point-make-initialisations)
  (let ((sym) (sstr) (line) (col) (oldwins))
	(setq oldwins (xref-is-browser-window-displayed))
	(setq line (count-lines (point-min) (+ (point) 1)))
	(setq col (xref-current-column))
	(setq sym (xref-get-identifier-on-point))
	(setq sstr (completing-read "Symbol to browse: " 
								'xref-symbols-completionfun nil nil sym))
	(xref-before-push-actions)
	(xref-call-process-with-basic-file-data-all-saves 
	 (format "\"-olcxpushname=%s\" -olcxlccursor=%d:%d" sstr line col))
	(xref-update-browser-if-displayed oldwins)
))

(defun xref-push-and-apply-macro ()
"Push references, traverse them and apply macro to each one.

This  function resolves  the symbol  under  point and  pushes all  its
references (from the active project)  on to the browser stack. Then it
traverses  all references  starting at  the last,  checks  whether the
reference is still  pointing to the symbol and  invokes a user defined
macro on each reference.

For example if you define an Emacs macro that deletes the old name and
inserts  a  new  name,  then  this function  will  rename  all  symbol
occurences. Unlike a renaming refactoring there are no safety checks.

NOTE: Be careful when using this  function. Be sure that your macro is
not destructive.  Also,  even though it is not  a refactoring, changes
made by this  function can be undone with  the 'undo last refactoring'
function.
"
  (interactive "")
  (let ((oldwins))
	(xref-entry-point-make-initialisations)
	(xref-multifile-undo-set-buffer-switch-point "mapping of a macro on all occurences")
	(xref-before-push-actions)
	(setq oldwins (xref-is-browser-window-displayed))
	(xref-call-process-with-basic-file-data-all-saves "-olcxpushandcallmacro")
	(xref-update-browser-if-displayed oldwins)
))

(defun xref-show-browser ()
"Display browser windows.
"
  (interactive "")
  (xref-entry-point-make-initialisations)
  (setq oldwins (xref-create-browser-windows nil xref-global-dispatch-data))
  (xref-update-browser-if-displayed oldwins)
)

(defun xref-local-motion (opt)
  (xref-entry-point-make-initialisations)
  (xref-call-process-with-basic-file-data-all-saves "-olcxpushforlm -olnodialog")
  (xref-call-process-with-basic-file-data-no-saves opt)
  (xref-call-process-with-basic-file-data-no-saves "-olcxpoponly -olnodialog")
)

(defun xref-previous-next-reference (option direction)
  (let ((sw))
	(setq sw (selected-window))
	(xref-entry-point-make-initialisations-no-project-required)
	(xref-call-process-with-basic-file-data-no-saves option)
	(xref-move-current-reference-list-pointer-if-browser-displayed direction)
	(sit-for .5)
	(select-window sw)
))

(defun xref-next-reference ()
"Move to the next reference stored on the top of the browser stack.
"
  (interactive "")
  (xref-previous-next-reference "-olcxplus" 1)
)


(defun xref-previous-reference ()
"Move to the previous reference stored on the top of the browser stack.
"
  (interactive "")
  (xref-previous-next-reference "-olcxminus" -1)
)

(defun xref-alternative-previous-reference ()
"Move to the previous reference, error or refactoring error.

Move  to  the previous  reference  of the  symbol  under  point. If  a
compilation  buffer is  displayed in  some  window, then  move to  the
previous error instead. If a  browser from an abandoned refactoring is
displayed in some window, then  move to the previous reference in this
browser instead.
"
  (interactive "")
  (let ((sw) (rw))
	(setq rw (xref-browser-of-failed-refactoring-is-displayed))
	(if rw
		(progn
		  (setq sw (selected-window))
		  (select-window rw)
		  (xref-move-current-reference-list-pointer -1)
		  (xref-browser-dialog-select-one nil)
		  (select-window sw)
		  )
	  (if (and xref-inspect-errors-if-compilation-window
			   (get-buffer-window xref-compilation-buffer))
		  (xref-ide-previous-error)
		(xref-local-motion "-olcxminus")
		))
))

(defun xref-alternative-next-reference ()
"Move to the next reference, error or refactoring error.

Move to the next reference of the symbol under point. If a compilation
buffer  is displayed  in  some window,  then  move to  the next  error
instead.  If a  browser from an abandoned refactoring  is displayed in
some window, then move to the next reference in this browser instead.
"
  (interactive "")
  (let ((sw) (rw))
	(setq rw (xref-browser-of-failed-refactoring-is-displayed))
	(if rw
		(progn
		  (setq sw (selected-window))
		  (select-window rw)
		  (xref-move-current-reference-list-pointer 1)
		  (xref-browser-dialog-select-one nil)
		  (select-window sw)
		  )
	  (if (and xref-inspect-errors-if-compilation-window
			   (get-buffer-window xref-compilation-buffer))
		  (xref-ide-next-error)
		(xref-local-motion "-olcxplus")
		))
))

(defun xref-pop-and-return ()
"Pop references from the top of the browser stack.

This function also  moves to the position from  where those references
were pushed.
"
  (interactive "")
  (let ((oldwins))
	;; first check special contexts
	(if (eq xref-this-buffer-type 'completion)
		(xref-interactive-completion-previous nil)
	  (if (eq xref-this-buffer-type 'tag-search-results)
		  (xref-interactive-tag-search-previous nil)
		;; O.K. here we are
		(xref-entry-point-make-initialisations-no-project-required)
		(setq oldwins (xref-is-browser-window-displayed))
		(xref-call-process-with-basic-file-data-no-saves "-olcxpop")
		(xref-update-browser-if-displayed oldwins)
		))
))

(defun xref-pop-only ()
"Pop references from the top of the browser stack. Do not move.
"
  (interactive "")
  (let ((oldwins))
	(xref-entry-point-make-initialisations-no-project-required)
	(setq oldwins (xref-is-browser-window-displayed))
	(xref-call-process-with-basic-file-data-no-saves "-olcxpoponly")
	(xref-update-browser-if-displayed oldwins)
))


(defun xref-re-push ()
"Re-push references removed by the last pop command.

This function also moves to the current reference.
"
  (interactive "")
  (let ((oldwins))
	;; first check special contexts
	(if (eq xref-this-buffer-type 'completion)
		(xref-interactive-completion-next nil)
	  (if (eq xref-this-buffer-type 'tag-search-results)
		  (xref-interactive-tag-search-next nil)
		;; O.K. here we are
		(xref-entry-point-make-initialisations-no-project-required)
		(setq oldwins (xref-is-browser-window-displayed))
		(xref-call-process-with-basic-file-data-no-saves "-olcxrepush")
		(xref-update-browser-if-displayed oldwins)
		))
))


;; backward compatibility functions
(defun xref-pushCxrefs () (xref-push-and-goto-definition))
(defun xref-listCxrefs () (xref-browse-symbol))
(defun xref-popCxrefs () (xref-pop-and-return))
(defun xref-poponly () (xref-pop-only))
(defun xref-list-top-references() (xref-show-browser))
(defun xref-previousref () (xref-previous-reference))
(defun xref-nextref () (xref-next-reference))
(defun xref-this-symbol-next-ref () (xref-alternative-next-reference))
(defun xref-this-symbol-previous-ref () (xref-alternative-previous-reference))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;; FUNCTIONS COMMON FOR VARIOUS DIALOGS  ;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun xref-interactive-selectable-line-inspect (event option offset)
  (interactive "i")
  (let ((sw) (dispatch-data))
	(setq dispatch-data xref-this-buffer-dispatch-data)
	(setq sw (selected-window))	
	(xref-call-task-on-line 
	 (concat (xref-html-browsing-options) " " option)
	 offset)
	(xref-select-dispach-data-caller-window dispatch-data)
	(sit-for .5)
	(select-window sw)
))

(defun xref-interactive-selectable-line-mouse-inspect (event option offset)
  (interactive "i")
  (let ((sw))
	(setq sw (selected-window))
	(mouse-set-point event)
	(beginning-of-line)
	(xref-interactive-selectable-line-inspect event option offset)
	(xref-modal-dialog-maybe-return-to-caller-window sw)
))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;; CLASS TREE VIEW  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar xref-class-tree-line-offset 0)

(defun xref-delete-class-tree-window ()
  (xref-delete-window-in-any-frame xref-class-tree-buffer 'xref-class-tree-window-width t)
)

(defun xref-interactive-class-tree-escape (event)
  (interactive "i")
  (xref-delete-class-tree-window)
  (xref-select-dispach-data-caller-window xref-class-tree-dispatch-data)
)

(defun xref-interactive-class-tree-inspect (event)
  (interactive "i")
  (xref-interactive-selectable-line-inspect event "-olcxctinspectdef" xref-class-tree-line-offset)
)

(defun xref-interactive-class-tree-mouse-inspect (event)
  (interactive "e")
  (xref-interactive-selectable-line-mouse-inspect event "-olcxctinspectdef" xref-class-tree-line-offset)
)

(defun xref-class-tree-show ()
  (interactive "")
  (xref-entry-point-make-initialisations)
  (xref-call-process-with-basic-file-data-no-saves "-olcxclasstree")
  (setq xref-class-tree-dispatch-data xref-global-dispatch-data)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; SYMBOL RETRIEVING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun xref-get-tags (str searchopt)
  (let ((res))
	(xref-call-process-with-basic-file-data-no-saves 
	 (format "\"-olcxtagsearch=%s\" %s" str searchopt))
	(setq res (cdr (assoc 'symbol-list xref-global-dispatch-data)))
	res
))

(defun xref-get-list-for-completions (ppp)
  (let ((res) (prs))
	(setq res nil)
	(while ppp
	  (progn
		(setq res (cons (cons (car ppp) nil) res))
		(setq ppp (cdr ppp))
		))
	res
))

(defun xref-list-to-alist (lst)
  (let ((res) (rres))
	(setq res nil)
	(if lst
		(progn
		  (setq res (cons (cons (car lst) nil) nil))
		  (setq rres res)
		  (setq lst (cdr lst))
		  (while lst
			(setcdr rres (cons (cons (car lst) nil) nil))
			(setq rres (cdr rres))
			(setq lst (cdr lst))
			)))
	res
))

(defun xref-symbols-completionfun (str filter type)
  (let ((res) (lst))
	(if (or (eq type t)
			(eq type nil))
		(progn
		  ;; list of completions
		  (setq lst (xref-list-to-alist 
					 (xref-get-tags 
					  (format "%s*" str) 
					  (format "-searchshortlist -p \"%s\"" xref-active-project))))
		  (if (eq type t)
			  (setq res (all-completions str lst))
			(setq res (try-completion str lst))
		  ))
	  (setq res nil)
	  )
	res
))

(defun xref-get-search-string () 
  (let ((sstr) (sym) (poin) (table))
	(setq sym (xref-get-identifier-on-point))
	(setq sstr (completing-read "Expression to search (-help for help): " 
								'xref-symbols-completionfun nil nil sym))
	(while (equal sstr "-help")
	  (progn
		(xref-interactive-help  "


Xrefactory   search  expressions   are  similar   to   standard  shell
expressions.  They are composed from a sequence of characters possibly
containing  wild characters.  Following  wild cards  can be  used: '*'
expands to  any (possibly  empty) string, '?'   expands to  any single
character and '[...]'  expands  to one of enclosed characters.  Ranges
of characters can be included between [ and ], so for example [a-zA-Z]
matches   any  letter,   [0-9]  matches   any  digit,   as   in  shell
expressions. If  the first character following  the [ is a  ^ then the
sense  of expansion  is inverted,  for example  [^0-9] expands  to any
non-digit character.  A symbol is reported only if its name completely
matches  the searched string.   Java method  profile is  considered as
part of the  name of the method, for  example, the expression *(*int*)
will report  all methods  taking at least  one parameter of  type int.
Letters are matched case  insensitively except when enclosed between [
and ].

For example the expression  '*get*' will report all symbols containing
the string 'get'.  Expression  'get*' will report all symbols starting
with  the string  'get'.  Expression  [A-Z]* will  report  all symbols
starting with an upper case letter.

If you enter an expression which does not contain any of the wild card
characters *,  ? or [  then Xrefactory reports all  symbols containing
the entered  string. For  example, entering get  as the  expression is
equivalent to entering *get*.

" nil nil)
		(setq sstr (completing-read "Expression to search (-help for help): " 
									'xref-symbols-completionfun nil nil sym))
		))
	sstr
))

(defun xref-interactive-tag-search-escape (event)
  (interactive "i")
  (xref-select-dispach-data-caller-window xref-this-buffer-dispatch-data)
  (xref-delete-window-in-any-frame xref-tag-results-buffer nil t)
)

(defun xref-interactive-tag-search-inspect (event)
  (interactive "i")
  (xref-interactive-selectable-line-inspect event "-olcxtaggoto" 0)
)

(defun xref-interactive-tag-search-select (event)
  (interactive "i")
  (xref-interactive-selectable-line-inspect event "-olcxtagselect" 0)
)

(defun xref-interactive-tag-search-previous-next (option)
  (let ((syms) (sw) (asc))
	(setq sw (selected-window))
	(xref-select-dispach-data-caller-window xref-this-buffer-dispatch-data)
	(xref-call-process-with-basic-file-data-no-saves option)
	(setq asc (assoc 'symbol-list xref-global-dispatch-data))
	(if asc
		(progn
		  (setq syms (cdr asc))
		  (xref-display-tag-search-results syms xref-global-dispatch-data nil)
		)
	  (select-window sw)
	  )
))

(defun xref-interactive-tag-search-next (event)
  (interactive "i")
  (xref-interactive-tag-search-previous-next "-olcxtagsearchforward")
)

(defun xref-interactive-tag-search-previous (event)
  (interactive "i")  
  (xref-interactive-tag-search-previous-next "-olcxtagsearchback")
)

(defun xref-interactive-tag-search-mouse-inspect (event)
  (interactive "e")
  (xref-interactive-selectable-line-mouse-inspect event "-olcxtaggoto" 0)
)

(defun xref-interactive-tag-search-mouse-select (event)
  (interactive "e")
  (xref-interactive-selectable-line-mouse-inspect event "-olcxtagselect" 0)
)

(defun xref-interactive-tag-search-mouse-previous (event)
  (interactive "e")
  (mouse-set-point event)
  (xref-interactive-tag-search-previous event)
  t
)

(defun xref-interactive-tag-search-mouse-next (event)
  (interactive "e")
  (mouse-set-point event)
  (xref-interactive-tag-search-next event)
  t
)

(defun xref-display-tag-search-results (syms dispatch-data searched-sym)
  (xref-delete-window-in-any-frame xref-tag-results-buffer nil t)
  (xref-select-dispach-data-caller-window dispatch-data)
  (xref-display-and-set-new-dialog-window xref-tag-results-buffer nil t)
  (buffer-disable-undo xref-tag-results-buffer)
  (setq xref-this-buffer-type 'tag-search-results)
  (xref-erase-buffer)
  (while syms
	(insert (car syms))
	(newline)
	(setq syms (cdr syms))
	)
  (if searched-sym
	  (xref-line-hightlight 0 (point-max) nil 1 (xref-create-tag-search-fontif searched-sym) nil)
	)
  (goto-char (point-min))
  (setq buffer-read-only t)
  (setq xref-this-buffer-dispatch-data dispatch-data)
  (xref-use-local-map xref-tag-search-mode-map)
)

(defun xref-search-in-tag-file ()
"Search for string(s) in tags.  

This function asks for strings to search, then it inspects the tags of
active project looking for symbols containing the given string(s). For
each  such  symbol it  displays  the  symbol  name together  with  its
definition location.  Strings are matched against the full symbol name
(including profile for Java methods).  For Java projects .jar archives
mentioned in classpath are scanned and matching symbols are reported.

"
  (interactive "")
  (let ((sym) (syms) (line) (col))
;;(load "profile")
;;(setq profile-functions-list (list 'xref-server-dispatch-require-end-ctag 'xref-server-dispatch-get-int-attr 'xref-server-parse-xml-tag 'xref-server-dispatch-symbol-list))
;;(profile-functions profile-functions-list)
	(xref-entry-point-make-initialisations)
	(setq sym (xref-get-search-string))
	(setq line (count-lines (point-min) (+ (point) 1)))
	(setq col (xref-current-column))
	(setq syms (xref-get-tags sym (format "-olinelen=%d -olcxlccursor=%d:%d" (window-width) line col)))
	(xref-display-tag-search-results syms xref-global-dispatch-data sym)
;;(profile-results)
))

(defun xref-search-definition-in-tag-file ()
"Search for a symbol defined in the project.  

This function asks for strings to search, then it inspects the tags of
the  active   project  looking   for  symbols  containing   the  given
string(s). For each  such symbol it displays the  symbol name together
with  its definition  location.  When  looking for  a symbol  only the
symbol name (the identifier) is checked against the given string(s).

"
  (interactive "")
  (let ((sym) (syms) (line) (col))
	(xref-entry-point-make-initialisations)
	(setq sym (xref-get-search-string))
	(setq line (count-lines (point-min) (+ (point) 1)))
	(setq col (xref-current-column))
	(setq syms (xref-get-tags sym (format "-searchdef -olinelen=%d -olcxlccursor=%d:%d" (window-width) line col)))
	(xref-display-tag-search-results syms xref-global-dispatch-data sym)
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; SYMBOL RESOLUTION DIALOG ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun xref-call-task-on-line (opt offset)
  (let ((sw) (cline) (dispatch-data))
	(setq dispatch-data xref-this-buffer-dispatch-data)
	(setq sw (selected-window))
	(setq cline (count-lines (point-min) (+ (point) 1)))
	;; put there html browsing options, it is usefull here
	(xref-send-data-to-process-and-dispatch 
	 (format "%s%d %s" opt (+ cline offset) (xref-html-browsing-options)) 
	 dispatch-data
	 nil)
	(select-window sw)
))

(defun xref-browser-dialog-previous-line (event)
  (interactive "i")
  (xref-modal-dialog-previous-line event)
)

(defun xref-browser-dialog-next-line (event)
  (interactive "i")
  (xref-modal-dialog-next-line event)
)

(defun xref-close-resolution-dialog-windows (dispatch-data)
  (let ((winassoc) (win))
	(setq winassoc (assoc 'linked-resolution-window dispatch-data))
	(if winassoc 
		(progn
		  (setq win (cdr winassoc))
		  (if (and win (windowp win) (window-live-p win))
			  (progn
				(setq xref-symbol-selection-window-width (window-width (cdr winassoc)))
				(setq xref-symbol-selection-window-height (window-height (cdr winassoc)))
				(delete-window (cdr winassoc))
				))
	  ))
	(setq winassoc (assoc 'linked-refs-window dispatch-data))
	(if winassoc 
		(progn
		  (setq win (cdr winassoc))
		  (if (and win (windowp win) (window-live-p win))
			  (delete-window (cdr winassoc))
			)))
	(xref-soft-delete-window xref-browser-info-buffer)

	(setq winassoc (assoc 'caller-window dispatch-data))
	(if winassoc (select-window (cdr winassoc)))
))

(defun xref-cancel-with-error (event)
  (interactive "i")
  (error "Cancel.")
)

(defun xref-browser-dialog-exit (event)
  (interactive "i")
  (let ((dispatch-data))
	(setq dispatch-data xref-this-buffer-dispatch-data)
	(xref-close-resolution-dialog-windows dispatch-data)

	;; kill refactorer process if refactoring
	(if (eq (cdr (assoc 'process dispatch-data)) 'xref-refactorer-process)
		(progn
		  (delete-process (car xref-refactorer-process))
		  (setq xref-refactorer-process nil)
		  (keyboard-quit)	
		  ))
))

(defun xref-browser-dialog-break (event)
  (interactive "i")
  (xref-soft-delete-window xref-browser-info-buffer)
  ;; if called from non-modal mode, close the dialog
  (if (eq this-command 'xref-browser-dialog-break)
	  (xref-browser-dialog-exit event)
	;; otherwise quit, TODO make this message appear in another dialog
	(message (substitute-command-keys "Refactoring abandoned but browser dialog is active (on \\[xref-alternative-previous-reference] and \\[xref-alternative-next-reference])."))
	(keyboard-quit)
	)
)

(defun xref-browser-dialog-other-window (event)
  (interactive "i")
  (let ((dispatch-data))
	(setq dispatch-data xref-this-buffer-dispatch-data)
	(if (eq xref-this-buffer-type 'reference-list)
		(xref-select-dispach-data-resolution-window dispatch-data)
	  (xref-select-dispach-data-refs-window dispatch-data)
	  )
))

(defun xref-browser-dialog-set-selection-sign (sel)
  (beginning-of-line)
  (setq buffer-read-only nil)
  (delete-char 1)
  (if (equal sel 1)
	  (insert "+")
	(insert " ")
	)
  (setq buffer-read-only t)
  (beginning-of-line)
)


(defun xref-blink-in-caller-window (dispatch-data)
  (let ((sw))
	(setq sw (selected-window))	
	(xref-select-dispach-data-caller-window dispatch-data)
	(sit-for .5)
	(select-window sw)
))

(defun xref-browser-dialog-set-selection-sign-on-all (sign)
  (let ((str))
  (save-excursion
	(goto-char (point-min))
	(while (not (equal (point) (point-max)))
	  (beginning-of-line)
	  (setq str (buffer-substring (point) (+ (point) 1)))
	  (if (or (string-equal str " ") (string-equal str "+"))
		  (progn
			(xref-browser-dialog-set-selection-sign sign)
			))
	  (forward-line 1)
	  ))
))

(defvar xref-menu-selection-line-offset 0)

(defun xref-browser-dialog-select-all (event)
  (interactive "i")
  (let ((res) (sw) (dispatch-data))
	(setq dispatch-data xref-this-buffer-dispatch-data)
	(setq sw (selected-window))
	(if (eq xref-this-buffer-type 'symbol-list)
		(progn
		  (xref-browser-dialog-set-selection-sign-on-all 1)
		  (xref-send-data-to-process-and-dispatch "-olcxmenuall" dispatch-data nil)
		  ))
	(select-window sw)
))

(defun xref-browser-dialog-select-none (event)
  (interactive "i")
  (let ((res) (sw) (dispatch-data))
	(setq dispatch-data xref-this-buffer-dispatch-data)
	(setq sw (selected-window))
	(if (eq xref-this-buffer-type 'symbol-list)
		(progn
		  (xref-browser-dialog-set-selection-sign-on-all 0)
		  (xref-send-data-to-process-and-dispatch "-olcxmenunone" dispatch-data nil)
		  ))
	(select-window sw)
))

(defun xref-browser-dialog-select-one (event)
  (interactive "i")
  (let ((res) (sw) (str) (dispatch-data))
	(setq dispatch-data xref-this-buffer-dispatch-data)
	(setq sw (selected-window))	
	(if (eq xref-this-buffer-type 'symbol-list)
		(progn
		  (beginning-of-line)
		  (setq str (buffer-substring (point) (+ (point) 1)))
		  (if (or (string-equal str " ") (string-equal str "+"))
			  (progn
				(xref-browser-dialog-set-selection-sign-on-all 0)
				(xref-browser-dialog-set-selection-sign 1)
				(xref-call-task-on-line "-olcxmenusingleselect" xref-menu-selection-line-offset)
				(xref-select-dispach-data-caller-window dispatch-data)
				(sit-for .5)
				(select-window sw)
				)))
	  (if (eq xref-this-buffer-type 'reference-list)
		  (progn
			(xref-call-task-on-line "-olcxgoto" 0)
			(xref-set-current-reference-list-pointer)
			(xref-select-dispach-data-caller-window dispatch-data)
			(sit-for .5)
			(select-window sw)
			)))
))

(defun xref-browser-dialog-toggle (event)
  (interactive "i")
  (let ((res) (str) (ns) (sw) (dispatch-data))
	(setq dispatch-data xref-this-buffer-dispatch-data)
	(if (eq xref-this-buffer-type 'symbol-list)
		(progn
		  (setq sw (selected-window))	
		  (beginning-of-line)
		  (setq str (buffer-substring (point) (+ (point) 1)))
		  (if (or (string-equal str " ") (string-equal str "+"))
			  (progn
				(if (string-equal str " ") (setq ns 1) (setq ns 0))
				(xref-browser-dialog-set-selection-sign ns)
				(xref-call-task-on-line "-olcxmenuselect" xref-menu-selection-line-offset)
				(xref-select-dispach-data-caller-window dispatch-data)
				(sit-for .5)
				(select-window sw)
				))
		  (xref-modal-dialog-next-line event)
		  ))
))

(defun xref-browser-dialog-next-reference (event)
  (interactive "i")
  (let ((other-win))
	(setq other-win nil)
	(if (eq xref-this-buffer-type 'symbol-list)
		(progn
		  (setq other-win t)
		  (xref-browser-dialog-other-window event)
		  ))
	(end-of-line)
	(if (eobp) 
		(progn
		  (goto-char (point-min))
		  (message "Moving to the first reference")
		  )
	  (beginning-of-line)
	  (xref-browser-dialog-next-line event)
	  )
	(xref-browser-dialog-select-one event)
	(if other-win (xref-browser-dialog-other-window event))
))

(defun xref-browser-dialog-previous-reference (event)
  (interactive "i")
  (let ((other-win))
	(setq other-win nil)
	(if (eq xref-this-buffer-type 'symbol-list)
		(progn
		  (setq other-win t)
		  (xref-browser-dialog-other-window event)
		  ))
	(beginning-of-line)
	(if (bobp)
		(progn
		  (goto-char (point-max))
		  (beginning-of-line)
		  (message "Moving to the last reference")
		  )
	  (xref-browser-dialog-previous-line event)
	  )
	(xref-browser-dialog-select-one event)
	(if other-win (xref-browser-dialog-other-window event))
))

(defun xref-modal-dialog-shift-left (event)
  (interactive "i")
  (xref-scroll-right)
)

(defun xref-modal-dialog-shift-right (event)
  (interactive "i")
  (xref-scroll-left)
)

(defun xref-mouse-set-point (event)
  (let ((sw) (ew) (res))
	(setq res nil)
	(setq sw (selected-window))
	(if (eq xref-running-under 'xemacs)
		(setq ew (event-window event))
	  (setq ew (posn-window (event-end event)))
	  )
    (if (windowp ew)
		(progn
		  (select-window ew)
		  (if (or (eq xref-this-buffer-type 'symbol-list)
				  (eq xref-this-buffer-type 'reference-list))
			  (progn
				(mouse-set-point event)
				(setq res t)
				)
			(select-window sw)
		  )))
	res
))

(defun xref-modal-dialog-maybe-return-to-caller-window (sw)
  (let ((cw))
	(setq cw (cdr (assoc 'caller-window xref-this-buffer-dispatch-data)))
	(if (equal cw sw)
		(select-window sw)
	  )
))

(defun xref-undefined (event)
  (interactive "i")
)

(defun xref-scroll-if-on-first-or-last-line ()
  (let ((dir))
	(setq dir nil)
	(save-excursion
	  (if (eq (forward-line -1) 0)
		  (if (< (point) (window-start))
			  (setq dir 'down)
			)
		))
	(save-excursion
	  (if (eq (forward-line 1) 0)
		  (if (and (>= (point) (window-end)) (not (eq (point) (point-max))))
			  (setq dir 'up)
			)
		))
	(if (eq dir 'up)
		(progn
		  ;; XEmacs magic
		  (forward-line -1)
		  (scroll-up 1)
		  (forward-line 1)
		  )
	  (if (eq dir 'down)
		  (scroll-down 1)
		))
))

(defun xref-modal-dialog-mouse-button1 (event)
  (interactive "e")
  (let ((sw))
	(setq sw (selected-window))
	(xref-mouse-set-point event)
	(beginning-of-line)
	;; scrolling in Emacs/XEmacs is mysterious:
	;; sometimes it goes automatically, sometimes not
	(if (eq xref-this-buffer-type 'reference-list)
		(xref-scroll-if-on-first-or-last-line)
	  )
	(xref-browser-dialog-select-one event)
	(xref-modal-dialog-maybe-return-to-caller-window sw)
))

(defun xref-modal-dialog-mouse-button2 (event)
  (interactive "e")
  (xref-mouse-set-point event)
  (beginning-of-line)
  (xref-browser-dialog-toggle event)
)

(defun xref-modal-dialog-continue (event)
  (interactive "i")
  ;; first try to execute some default action
  (if (or (eq xref-this-buffer-type 'reference-list)
		  (eq xref-this-buffer-type 'symbol-list))
	  (xref-browser-dialog-select-one event)
	(error "Not a modal dialog, probably an aborted action, no continuation defined.")
	)
)

(defun xref-references-set-filter (level dispatch-data)
  ;; save all files, so that references refer to buffers rather than files
  (xref-server-call-on-current-buffer-all-saves 
   (format "-olcxfilter=%d" level)
   dispatch-data)
)

(defun xref-browser-dialog-set-filter (level dispatch-data)
  (xref-select-dispach-data-resolution-window dispatch-data)
  (setq xref-this-buffer-filter-level level)
  (xref-send-data-to-process-and-dispatch 
   (format "-olcxmenufilter=%d" level) 
   dispatch-data
   nil)
  (xref-references-set-filter 0 dispatch-data)
  (xref-select-dispach-data-resolution-window dispatch-data)
)

(defun xref-browser-dialog-set-new-filter (dispatch-data)
  (let ((level))
	(xref-select-dispach-data-resolution-window dispatch-data)
	(setq level xref-default-symbols-filter)
	(if xref-keep-last-symbols-filter
		(setq level xref-this-buffer-filter-level)
	  )
	(xref-browser-dialog-set-filter level dispatch-data)
))

(defun xref-browser-or-refs-set-filter (level)
  (let ((dispatch-data))
	(setq dispatch-data xref-this-buffer-dispatch-data)
	(if (eq xref-this-buffer-type 'symbol-list)
		(progn
		  (if (and (>= level 0) (<= level 2))
			  (xref-browser-dialog-set-filter level dispatch-data)
			(error "filter level out of range <0,2>")
		  ))
	  (if (eq xref-this-buffer-type 'reference-list)
		  (progn
			(if (and (>= level 0) (<= level 3))
				(xref-references-set-filter level dispatch-data)
			  (error "filter level out of range <0,3>")
			  ))))
))

(defun xref-interactive-browser-dialog-set-filter (event)
  (interactive "i")
  (let ((level))
	(setq level (string-to-int (char-to-string last-input-char)))
	(xref-browser-or-refs-set-filter level)
))

(defun xref-interactive-browser-dialog-set-filter0 (event)
  (interactive "i")
  (xref-browser-or-refs-set-filter 0)
)

(defun xref-interactive-browser-dialog-set-filter1 (event)
  (interactive "i")
  (xref-browser-or-refs-set-filter 1)
)

(defun xref-interactive-browser-dialog-set-filter2 (event)
  (interactive "i")
  (xref-browser-or-refs-set-filter 2)
)

(defun xref-interactive-browser-dialog-set-filter3 (event)
  (interactive "i")
  (xref-browser-or-refs-set-filter 3)
)

(defun xref-browser-symbols-help ()
  (xref-interactive-help
"Special hotkeys available:

\\[xref-browser-dialog-select-one] \t-- select one symbol
\\[xref-browser-dialog-toggle] \t-- toggle selected/unselected
\\[xref-browser-dialog-select-all] \t-- select all
\\[xref-browser-dialog-select-none] \t-- unselect all
\\[xref-browser-dialog-other-window] \t-- switch to other window
0 \t-- filter 0 (all symbols of given name)
1 \t-- filter 1 (all symbols of given profile)
2 \t-- filter 2 (only related symbols)
\\[xref-modal-dialog-shift-right] \t-- scroll right
\\[xref-modal-dialog-shift-left] \t-- scroll left
\\[xref-resize-right] \t-- resize right
\\[xref-resize-left] \t-- resize left
\\[xref-browser-dialog-exit] \t-- close browser
\\[xref-browser-dialog-break] \t-- abandon refactoring (if any) while keeping browser or quit
\\[xref-modal-dialog-continue] \t-- continue refactoring (if any) or select one symbol
\\[xref-interactive-browser-dialog-help] \t-- toggle this help page
" nil nil)
)

(defun xref-browser-refs-help ()
  (xref-interactive-help
"Special hotkeys available:

\\[xref-browser-dialog-select-one] \t-- inspect reference
\\[xref-browser-dialog-other-window] \t-- switch to other window
\\[xref-browser-dialog-previous-reference] \t-- previous reference
\\[xref-browser-dialog-next-reference] \t-- next reference
\\[xref-browser-dialog-exit] \t-- close browser
0 \t-- filter 0 (definitions/declarations)
1 \t-- filter 1 (+ top level)
2 \t-- filter 2 (+ l-values)
3 \t-- filter 3 (all)
\\[xref-modal-dialog-shift-right] \t-- scroll right
\\[xref-modal-dialog-shift-left] \t-- scroll left
\\[xref-resize-right] \t-- resize right
\\[xref-resize-left] \t-- resize left
\\[xref-modal-dialog-continue] \t-- continue
\\[xref-interactive-browser-dialog-help] \t-- toggle this help page
" nil nil)
)

(defun xref-interactive-browser-dialog-help (event)
  (interactive "i")
  (if (eq xref-this-buffer-type 'symbol-list)
	  (xref-browser-symbols-help)
	(xref-browser-refs-help)
	)
)

;;;;;;;;;;;;;;;;;;;;; mouse3 menu ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun xref-popup-xemacs-browser-menu (event)
  (interactive "e")
  (select-window (event-window event))
  (goto-char (event-closest-point event))
  (beginning-of-line)
  (if (eq xref-this-buffer-type 'reference-list)
	  (popup-menu xref-xemacs-popup-browser-refs-menu)
	(popup-menu xref-xemacs-popup-browser-menu-menu)
	)
)

(defun xref-browser-3b-mouse-selected (event)
  (interactive "e")
  (mouse-set-point event)
  (cond
   (
	(eq last-input-event 'xref-bm-3b-sel-one)
	(xref-browser-dialog-select-one event)
	)
   (
	(eq last-input-event 'xref-bm-3b-toggle)
	(xref-browser-dialog-toggle event)
	)
   (
	(eq last-input-event 'xref-bm-3b-sel-all)
	(xref-browser-dialog-select-all event)
	)
   (
	(eq last-input-event 'xref-bm-3b-sel-none)
	(xref-browser-dialog-select-none event)
	)
   (
	(eq last-input-event 'xref-bm-3b-filt0)
	(xref-interactive-browser-dialog-set-filter0 event)
	)
   (
	(eq last-input-event 'xref-bm-3b-filt1)
	(xref-interactive-browser-dialog-set-filter1 event)
	)
   (
	(eq last-input-event 'xref-bm-3b-filt2)
	(xref-interactive-browser-dialog-set-filter2 event)
	)
   (
	(eq last-input-event 'xref-bm-3b-filt3)
	(xref-interactive-browser-dialog-set-filter3 event)
	)
   (
	(eq last-input-event 'xref-bm-3b-close)
	(xref-browser-dialog-exit event)
	)
   (
	(eq last-input-event 'xref-bm-3b-cont)
	(xref-modal-dialog-continue event)
	)
   )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;; MISC MENU ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun xref-kill-xref-process (coredump)
"Kill currently running  xref process. 

If  there  is an  xref  process creating/updating  a  tag  file it  is
killed.  If there  is no  such process  this function  kills  the xref
server process.  This function can be  used if the xref task enters an
inconsistent state.  The Xref  process is then restarted automatically
at the next invocation of any of its functions.
"
  (interactive "P")
  (if (and (not (eq xref-tags-process nil))
		   (eq (process-status (car xref-tags-process)) 'run))
	  (progn
        (delete-process (car xref-tags-process))
        (setq xref-tags-process nil)
		(message "Extern Xref process killed.")
        )
	(setq xref-tags-process nil)
	(if (not (eq xref-server-process nil))
		(progn
		  (if current-prefix-arg
			  (progn
				(shell-command (format "kill -3 %d && echo Core dumped into this buffer directory." (process-id (car xref-server-process))))
				)
			(delete-process (car xref-server-process))
			(setq xref-server-process nil)
			(message "Emacs Xref server process killed.")
		  ))
	  (message "** No process to kill. **")
	  ))
)

(defun xref-about (exit)
  "Show Xref version."
  (interactive "P")
  (xref-entry-point-make-initialisations-no-project-required)
  ;; if called with prefix argument, exit server task 
  ;; to be used for profiler informations
  (setq xref-global-dispatch-data (xref-get-basic-server-dispatch-data 'xref-server-process))
  (if current-prefix-arg
	  (xref-send-data-to-process-and-dispatch "-about -exit" xref-global-dispatch-data nil)
	(xref-send-data-to-process-and-dispatch "-about" xref-global-dispatch-data nil)
	)
)

(defun  xref-registration ()
  "Enter Xrefactory license string to run registered version."
  (interactive "")
  (let ((lic) (pp) (iw) (cc) (ln))
	(xref-entry-point-make-initialisations-no-project-required)
	(setq  cc "Evolution and  maintenance of Xrefactory is entirely  dependent on its
users.  The  xref task  is proprietary software.   In order to  run it
legally  you need  to purchase  a  license.  You  can obtain  detailed
information    about    available    licenses    and    ordering    at
http://www.xref-tech.com/xrefactory/license.html.    After  successful
registration you will receive a  license string which will change your
evaluation copy into regular one.

If  you have  received  a  valid license  string,  select \"Enter  the
license string\" now.
")
	(setq ln (1+ (xref-char-count ?\n cc)))
	(setq sel (xref-modal-dialog xref-selection-modal-buffer (format
"%s
----
 1.) Enter the license string
 2.) Browse URL
----
" cc)
					   (+ ln 2) 0 t xref-license-error-dialog-map nil))
	(if (eq sel (+ ln 3))
		(xref-browse-url xref-registration-url)
	  (goto-char (point-min))
	  (setq lic (read-from-minibuffer "Enter the license string: "))
	  (if (not (equal lic ""))
		  (progn
			(find-file xref-options-file)
			(goto-char (point-min))
			(setq pp (search-forward-regexp "^[ \t]*-license=" nil t))
			(if pp
				(progn
				  (beginning-of-line)
				  (insert "// ")
				  (end-of-line)
				  (newline)
				  )
			  (newline)
			  (previous-line 1)
			  )
			(insert (format "-license=%s" lic))
			(save-buffer)
			(bury-buffer)
			(xref-kill-xref-process nil)
			(message "Done. The license string is inserted in %s file." xref-options-file)
			)
		(message "No license string. Exiting.")
		)
	  )
))

(defun xref-interactive-help-escape ()
  (interactive "")
  (delete-window (selected-window))
)

(defvar xref-help-mode-map
  (let ((map (make-keymap)))
    (define-key map "\e" 'xref-interactive-help-escape)
    (define-key map "q" 'xref-interactive-help-escape)
    map)
  "Keymap for xref help buffer."
)


(defun  xref-help ()
  "Show basic help informations for use of Xrefactory."
  (interactive "")
  (let ((iw))
	(setq iw (xref-display-and-set-new-dialog-window xref-info-buffer nil t))
	(insert " Xref Help.

Xrefactory is  a refactoring development  environment for C  and  C++.
Its functions  can be  accessed via the  'Xref' menu.   Optionally the
most   frequently  used   functions  are   accessible   via  hot-keys.
Documentation of a particular  function is available through the \"C-h
k\" key combination (i.e.  the 'control' key together with 'h' key and
then the  'k' key)  followed by mouse  selection of  the corresponding
menu item.

PROJECTS:

Xrefactory  is project based:  you will  need to  create or  select an
'active project' before doing anything else. In particular, you should
start your work with Xrefactory  by invoking the 'Project -> New' menu
item.

TAG FILE: 

The main object you will then  work with is the 'tag file'.  This file
stores all  the necessary information  about your project  sources, in
particular informations  about all symbols,  their linking properties,
definition place(s) and  all usages.  The maintenance of  the tag file
is the responsibility of the user.  An out of date tag file will cause
mistakes in  source browsing.   However, the code  Completion function
and Extract  Region refactorings  are independent of  the tag  file as
they  depend  only on  file-local  information.   Three functions  are
available  for maintenance  of  tag files,  trading  off time  against
accuracy.  The  'Full Update' function  should guarantee a  correct of
tag file; however it is recomended to re-create the tag file from time
to time in order to remove garbage.

BROWSING AND REFACTORINGS:  

In general you have to  position the point on the browsed (refactored)
symbol before invoking any  reference pushing or refactoring function.
For parameter manipulations you need to position the point on the name
of the function/method, not  on the parameter itself.  Before invoking
the extract region function you will need to select a region with your
mouse or by specifying begin of the region by C-<space> and the end by
the point.  Xrefactory may open  some dialogs in a  horizontally split
window; in  general in those screens  the middle mouse  button makes a
choice, and the right button gives a menu of available actions.

XREF TASK:  

Emacs Xref  functions cooperate with  an external 'xref' task;  if you
think that the task has entered  an inconsistent state, or if you wish
to interrupt  creation or update of  the tag file, you  can invoke the
'Kill xref task' function.

CUSTOMIZATION: 

Xrefactory can be  customized via the 'Xref ->  Options' menu item and
via  the  ~/.xrefrc  configuration  file.   The  'Options'  menu  item
customizes  project independent  behaviour  which is  mainly the  user
interface.   In the  '.xrefrc'  file you  can  specify your  projects'
settings and  preferences.  There are  many options you  can customize
via  .xrefrc;  for more  information  read  the  xref and  xrefrc  man
pages. Before  using Xrefactory you  should also read the  README file
included in the distribution package.
")
  (goto-char (point-min))
  (xref-use-local-map xref-help-mode-map)
))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; UNDO ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar xref-multifile-undo-state nil)

(defun xref-this-buffer-undo-state ()
  (undo-boundary)
  (cdr buffer-undo-list)
)

(defun xref-is-buffer-undoable (bname)
  (let ((bnamestart) (res))
	(if (not bname)
		(setq res nil)
	  (setq bnamestart (substring bname 0 1))
	  (if (or (equal bnamestart " ") (equal bnamestart "*"))
		  (setq res nil)
		(if (xref-buffer-has-one-of-suffixes bname xref-undo-allowed-suffixes)
			(setq res t)
		  (setq res nil)
		  )))
	res
))

(defun xref-undoable-move-file (new-fname)
  (let ((old))
	(setq old (buffer-file-name))
	(xref-multifile-undo-add-buffer-write)
	(xref-write-file new-fname t)
	(xref-delete-file old)
))

(defun xref-resave-buffers-from-directory (olddir newdir)
  (let ((bb) (cb) (fn) (nfn) (res))
	(setq bb (buffer-list))
	(setq res nil)
	(setq olddir (backslashify-name olddir))
	(while bb
	  (setq cb (car bb))
	  (setq fn (backslashify-name (buffer-file-name cb)))
	  (if (xref-string-has-prefix fn olddir (eq xref-platform 'windows))
		  (progn
			(set-buffer cb)
			(setq nfn (concat newdir 
							  (xref-cut-string-prefix fn olddir (eq xref-platform 'windows))))
			(set-visited-file-name nil)
			(xref-write-file nfn nil)
			))
	  (setq bb (cdr bb))
	  )
	res
))

(defun xref-undoable-move-directory (olddir newdir)
  (let ((pdir) (blinks))
	;; create parent dir first (if it doesn't exist)
	(setq pdir (xref-file-directory-name newdir))
	(if (not (file-attributes pdir)) (make-directory pdir))
;;	(setq blinks (xref-unlink-buffers-from-directory olddir))
	(xref-save-some-buffers nil)
	(xref-move-directory olddir newdir)
	(xref-multifile-undo-add-move-directory olddir newdir)
	(xref-resave-buffers-from-directory olddir newdir)
	(if (file-attributes olddir) (delete-directory olddir))
;;	(xref-resave-unlinked-buffers blinks olddir newdir)
))

(defun xref-editor-undo-state ()
  (let ((state) (buffs) (bst) (buf))
	(save-excursion
	  (setq state nil)
	  (setq buffs (buffer-list))
;;	  (setq buffs (append buffs (cons (current-buffer) nil)))
	  (while buffs
		(setq buf (car buffs))
		(setq buffs (cdr buffs))
		(if (xref-is-buffer-undoable (buffer-file-name buf))
			(progn
			  (set-buffer buf)
			  (setq bst (xref-this-buffer-undo-state))
			  (setq state (cons (cons buf bst) state))
		  ))))
	state
))

(defun get-corresponding-undo (ulist buf)
  (let ((ul) (found) (res))
	(setq ul ulist)
	(setq found nil)
	(setq res nil)
	(while (and ul (not found))
	  (setq found (eq (car (car ul)) buf))
	  (if (not found)
		  (setq ul (cdr ul))
		)
	  )
	(if found
		(setq res (cdr (car ul)))
	  )
	res
))	  

(defun xref-save-modified-files-with-question-and-error (flag message)
  (if (xref-yes-or-no-window (concat message "Save modified buffers? ") t nil)
	  (progn
		(xref-save-some-buffers flag)
		(error "Changes saved.")
		)
	(error "Not saved.")
))

(defun xref-save-modified-files-with-question (flag message)
  (if (xref-yes-or-no-window (concat message "Save modified buffers? ") t nil)
	  (progn
		(xref-save-some-buffers flag)
		(message "Changes saved.")
		)
	(message "Not saved.")
))

(defun undo-changes-until (bundo interact-flag)
  (let ((cc) (aa) (iinteract) (loop))
	(setq iinteract interact-flag)
	(setq loop t)
	(while (and pending-undo-list (not (eq pending-undo-list bundo)))
	  (xref-make-buffer-writable)
	  (undo-more 1)
	  (if iinteract
		  (progn
			(setq aa (xref-get-single-yes-no-event nil "Continue undoing "))
			)
		(setq aa 'answer-yes)
		)
	  (if (eq aa 'answer-no)
		  (xref-save-modified-files-with-question-and-error nil "** Undoing breaked. ")
		)
	  (if (eq aa 'answer-all)
		  (setq iinteract nil)
		)
	  (message "")
	  (undo-boundary)
	  )
	(if (not (eq pending-undo-list bundo))
		(error "Not enough undo information available (check 'undo-limit' and 'undo-strong-limit' variables)!") 
	  )
	iinteract
))

(defun xref-undo-single-buffer (buf ustate cont-ustate interact-flag)
  (let ((bnamestart) (bufundo) (iinteract))
	(setq iinteract interact-flag)
	(if (not (buffer-name buf))
		(progn
		  (if (not (yes-or-no-p (format "A killed buffer can't be undone, continue? ")))
			  (xref-save-modified-files-with-question-and-error nil "Aborted. ")			
			)
		  )
	  (if (xref-is-buffer-undoable (buffer-file-name buf))
		  (progn
			(switch-to-buffer buf)
			(setq bufundo (get-corresponding-undo ustate buf))
			(if cont-ustate
				(setq pending-undo-list (get-corresponding-undo 
										 cont-ustate buf))
			  (xref-make-buffer-writable)
			  (undo-start)
			  (if bufundo (undo-more 1))
			  )
			(setq iinteract (undo-changes-until bufundo iinteract))
			))
	  )
	iinteract
))

(defun xref-undo-until-undo-state (ustate cont-ustate interact)
  (let ((buffs) (buf) (buffs2) (bufundo) (iinteract))
	(setq iinteract interact)
	(setq buffs (buffer-list))
	;; first process bufferes without undo-state
	(while buffs
	  (setq buf (car buffs))
	  (setq buffs (cdr buffs))
	  (setq bufundo (get-corresponding-undo ustate buf))
	  (if (not bufundo)
		  (setq iinteract (xref-undo-single-buffer buf ustate cont-ustate iinteract))
		)
	  )
	;; than those from list
	(setq buffs2 ustate)
	(while buffs2
	  (setq buf (car (car buffs2)))
	  (setq buffs2 (cdr buffs2))
	  (setq bufundo (get-corresponding-undo ustate buf))
	  (if bufundo
		  (setq iinteract (xref-undo-single-buffer buf ustate cont-ustate iinteract))
		)
	  )
))

(defun cut-long-refactoring-undo-list ()
  (let ((ul) (deep))
	(if (> (length xref-multifile-undo-state) xref-multifile-undo-deep)
		(progn
		  (setq ul xref-multifile-undo-state)
		  (setq deep xref-multifile-undo-deep)
		  (while (and ul (> deep 0))
			(setq deep (- deep 1))
			(setq ul (cdr ul))
			)
		  (if ul
			  (setcdr ul nil)
			))
	  )
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun xref-undo-last-refactoring (rd) 
"Undo a series of refactorings.

This  function undos changes  made in  multiple buffers.   Buffers are
switched at the  start of refactorings. The undo  itself is considered
as a refactoring,  so further invocations can be  used to re-do undone
changes.  It  uses the standard Emacs  undo mecanism which  may not be
appropriate for  such a massive undo;  in case of  problems check your
'undo-strong-limit' and 'undo-limit'  variable settings.  Undo can not
be performed correctly if you have killed a modified buffer or saved a
buffer with a different name.

NOTE!   Do not  kill  any buffer  if  you are  using undo!  Xrefactory
memorizes the  state of  all opened buffers  at the beginning  of each
refactoring. If any opened buffer  is killed in between, undo will not
work correctly.
"
  (interactive "i")
  (let ((undolist) (cloop) (cont-undo-state) (aa) (cbuff) (interact) 
		(u-type) (next-ask))
	(setq interact xref-detailed-refactoring-confirmations)
	(setq cbuff (current-buffer))
	(if (eq xref-multifile-undo-state nil)
		(error "** Refactorings undo stack is empty **")
	  )
	(set-marker xref-undo-marker (point))
	(setq cont-undo-state nil)
	(setq cloop t)
	(setq undolist xref-multifile-undo-state)
	(if (not (xref-rcs-undo-warning (car (car undolist))))
		(progn
		  (setq cloop (xref-yes-or-no-window
					   (format "Really undo %s? " (car (car undolist)))
					   t nil
							   ))))
	(while cloop
	  (xref-multifile-undo-set-buffer-switch-point (format "an undone refactoring (redoing %s)" (car (car undolist))))
	  (setq next-ask t)
	  (setq u-type (car (cdr (car undolist))))
	  (if (equal u-type "move-buffer")
		  (progn
			(setq interact (xref-undo-file-moving (cdr (cdr (car undolist))) interact))
			(setq next-ask nil)
			)
		(if (equal u-type "move-dir")
		  (progn
			(setq interact (xref-undo-dir-moving (cdr (cdr (car undolist))) interact))
			(setq next-ask nil)
			)
		  (setq interact (xref-undo-until-undo-state (cdr (cdr (car undolist))) cont-undo-state interact))
		  (setq cont-undo-state (cdr (cdr (car undolist))))
		))
	  (setq undolist (cdr undolist))
	  (if (eq undolist nil)
		  (setq cloop nil)
		(if (not next-ask)
			(setq cloop t)
		  (sit-for 0.01)   ;; refresh screen
		  (setq cloop (xref-yes-or-no-window
					   (format 
						"The refactoring is undone, continue by undoing\n%s? "
						(car (car undolist)))
					   t nil
					  ))))
	  )
	(switch-to-buffer cbuff)
	(sit-for 0)
	(xref-switch-to-marker xref-undo-marker)
	(xref-save-modified-files-with-question t "Undone. ")
))


(defun xref-undo-file-moving (data interact)
  (let ((buf) (fname) (confirmed))
	(setq fname (car data))
	(setq buf (car (cdr data)))
	(set-buffer buf)
	(if interact
		(progn
		  (switch-to-buffer buf)
		  (setq confirmed (xref-yes-or-no-window (format "Move to file %s ? " fname) t nil))
		  )
	  (setq confirmed t)
	  )
	(if confirmed
		(progn
		  (xref-undoable-move-file fname)
		  ))
	)
  interact
)

(defun xref-multifile-undo-add-buffer-write ()
  (let ((comment))
	(if xref-multifile-undo-state
		(setq comment (car (car xref-multifile-undo-state)))
	  (setq comment (format "moving file %s" (xref-file-last-name (buffer-file-name nil))))
	  )
	(setq xref-multifile-undo-state 
		  (cons 
		   (list
			comment
			"move-buffer"
			(buffer-file-name nil)
			(current-buffer)
			)
		   xref-multifile-undo-state
		   ))
	(cut-long-refactoring-undo-list)
;;(insert (format "multifile-undo-state : %S\n\n" xref-multifile-undo-state))
))


(defun xref-undo-dir-moving (data interact)
  (let ((olddir) (newdir) (confirmed))
	(setq olddir (car data))
	(setq newdir (car (cdr data)))
	(if interact
		(progn
		  (setq confirmed (yes-or-no-p (format "Move directory %s to %s ? " 
											   (xref-file-last-name newdir)
											   (xref-file-last-name olddir))
									   t nil))
		  )
	  (setq confirmed t)
	  )
	(if confirmed
		(progn
		  (xref-undoable-move-directory newdir olddir)
		  ))
	)
  interact
)

(defun xref-multifile-undo-add-move-directory (olddir newdir)
  (let ((comment))
;;	(if xref-multifile-undo-state
;;		(setq comment (car (car xref-multifile-undo-state)))
	(setq comment (format "moving directory %s to %s" 
						  (xref-file-last-name olddir)
						  (xref-file-last-name newdir)))
;;	  )
	(setq xref-multifile-undo-state 
		  (cons 
		   (list
			comment
			"move-dir"
			olddir
			newdir
			)
		   xref-multifile-undo-state
		   ))
	(cut-long-refactoring-undo-list)
;;(insert (format "multifile-undo-state : %S\n\n" xref-multifile-undo-state))
))


(defun xref-multifile-undo-set-buffer-switch-point (comment)
"Set a multifile undo 'change-point'.

This  function memorizes  the undo  state of  all opened  buffers.  An
invocation  of  `xref-undo-last-refactoring'  will then  undo  changes
since  that point  in  all those  buffers.   The COMMENT  is a  string
indicating before which action the state is memorized.
"
;;  (interactive "")
  (setq xref-multifile-undo-state 
		(cons 
		 (cons
		  comment
		  (cons
		   "standard"
		   (xref-editor-undo-state)
		  ))
		 xref-multifile-undo-state
		 ))
  (cut-long-refactoring-undo-list)
;;(insert (format "multifile-undo-state : %S\n\n" xref-multifile-undo-state))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;; REFACTORINGS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun xref-refactor ()
"Invoke refactorer.   

This function offers  a list of refactorings available  for the symbol
under point  and for the region  between mark and point.  You can then
select the refactoring to be performed.

Xrefactory  performs refactorings  by precomputing  all changes  on an
internal copy of source files  in the memory.  All checks are computed
while working  with the model and  the refactoring is  applied on real
sources only  if it  can be safely  performed. In consequence,  if the
refactoring  fails  due to the  accessibility of symbols or  something
similar, your source code should not be modified at all.

Refactoring  does  not  affect  other  files than  source  code.   For
example, if you  move a Java class from one  package into another then
the old .class file will stay untouched and can cause problems for the
compilation. Hence, it's a good idea to clean (and recreate) all class
files after each important refactoring.
"
  (interactive "")
  (xref-entry-point-make-initialisations)
  (setq xref-global-dispatch-data (xref-get-basic-server-dispatch-data 'xref-server-process))
  (xref-server-call-on-current-buffer-no-saves 
   "-olcxgetrefactorings -editor=emacs" 
   xref-global-dispatch-data)
)

;;;;;;;;;;;;;;;;;;;;;;;; REFACTORINGS INIT-FINISH ;;;;;;;;;;;;;;;;;;;;;;;

(defun xref-refactoring-init-actions (description)
  (set-marker xref-refactoring-beginning-marker (point))
  (setq xref-refactoring-beginning-offset (point))
  (xref-multifile-undo-set-buffer-switch-point description)
  (if (or xref-save-files-and-update-tags-before-refactoring 
		  xref-save-buffers-without-prompt)
	  (progn
		;; why there is the flag nil?
		(xref-save-some-buffers nil)
		(xref-update-tags "-update" nil)
		))
  (save-excursion
	(get-buffer-create xref-vc-log-buffer)
	(set-buffer xref-vc-log-buffer)
	(goto-char (point-max))
	(insert-string (format "%s: %s" 
						   (current-time-string)
						   description))
	(newline)
	(get-buffer-create xref-cvs-shell-log-buffer)
	(xref-erase-buffer)
	)
)

(defun xref-refactoring-finish-actions ()
  (if xref-save-buffers-without-prompt
	  (xref-save-some-buffers t)
	)
  (if xref-save-files-and-update-tags-after-refactoring 
	  (progn
		(xref-save-some-buffers t)
		(xref-update-tags "-update" nil)
		))
  ;; there is problem in combination with RCS, losing the marker offset
  (if xref-move-point-back-after-refactoring
	  (progn
		(xref-switch-to-marker xref-refactoring-beginning-marker)
		(if (and (eq (point) (point-min))
				 (not (eq xref-refactoring-beginning-offset (point-min))))
			(goto-char xref-refactoring-beginning-offset)
		  )))
  (message "Done.")
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; RENAMING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun xref-non-interactive-renaming (opt old-name new-name)
  (xref-refactoring-init-actions (format "renaming of %s to %s" old-name new-name))	
  (xref-server-call-refactoring-task (list opt (format "-renameto=%s" new-name)))
  (xref-refactoring-finish-actions)
)

(defun xref-renaming (opt default)
  (let ((tcount) (prompt) (new-name) (old-name))
	(if default
		(setq old-name default)
	  (setq old-name (xref-get-identifier-on-point))
	  )
	(setq new-name (read-from-minibuffer 
					(format "Rename '%s' to : " old-name) old-name))
	(xref-non-interactive-renaming opt old-name new-name)
))

(defun xref-rename-symbol (rd)
  (xref-renaming "-rfct-rename" nil)
)

(defun xref-rename-class (rd)
  (xref-rename-class-and-package-cvs-warning)
  (xref-renaming "-rfct-rename-class" nil)
)

(defun xref-rename-package (rd)
  (xref-rename-class-and-package-cvs-warning)
  (xref-renaming "-rfct-rename-package" (elt rd 1))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PARAMETER MANIPULATIONS  ;;;;;;;;;;;;;;;;;;;;

(defun xref-non-interactive-add-parameter (name arg textdef textval)
  (xref-refactoring-init-actions
   (format "insertion of %s's parameter" name))
  (xref-server-call-refactoring-task 
   (list "-rfct-add-param"
		 (format "-olcxparnum=%s" arg)
		 (format "-rfct-param1=%s" textdef);
		 (format "-rfct-param2=%s" textval)))
  (xref-refactoring-finish-actions)
)

(defun xref-add-parameter (rd)
  (let ((name) (argns) (arg) (textdef) (textval) (default))
	(setq name (xref-get-identifier-on-point))
	(setq argns (read-from-minibuffer 
				 (format 
				  "Insert parameter at position [ 1 - arity('%s') ] : " name)
				 "1"
				 ))
	(setq arg (string-to-int argns))
	(if (and rd (equal (car (cdr rd)) "macro"))
		(setq default "ARG")
	  (setq default "int arg")
	  )
	(setq textdef (read-from-minibuffer 
				   "Declaration of the new parameter: "
				   default
				   ))
	(setq textval (read-from-minibuffer 
				   "Actual value of the new parameter: "
				   "0"
		 ))
	(xref-non-interactive-add-parameter name arg textdef textval)
))

(defun xref-non-interactive-del-parameter (name arg)
  (xref-refactoring-init-actions
   (format "deletion of %s's parameter" name))
  (xref-server-call-refactoring-task 
   (list "-rfct-del-param" (format "-olcxparnum=%s" arg)))
  (xref-refactoring-finish-actions)
)

(defun xref-del-parameter (rd)
  (let ((name) (argns) (arg) (textdef) (textval))
	(setq name (xref-get-identifier-on-point))
	(setq argns (read-from-minibuffer 
				 (format 
				  "Delete parameter from position [ 1 - arity('%s') ] : " name) 
				 "1"
				 ))
	(setq arg (string-to-int argns))
	(xref-non-interactive-del-parameter name arg)
))


(defun xref-non-interactive-move-parameter (name arg1 arg2)
  (xref-refactoring-init-actions
   (format "move of %s's parameter" name))
  (xref-server-call-refactoring-task 
   (list "-rfct-move-param" 
		 (format "-olcxparnum=%s" arg1) 
		 (format "-olcxparnum2=%s" arg2)))
  (xref-refactoring-finish-actions)
)

(defun xref-move-parameter (rd)
  (let ((name) (argns) (arg) (textdef) (textval) (arg1) (arg2))
	(setq name (xref-get-identifier-on-point))
	(setq argns (read-from-minibuffer 
				 (format 
				  "Position of parameter to move [ 1 - arity('%s') ] : " name) 
				 "1"
				 ))
	(setq arg1 (string-to-int argns))
	(setq argns (read-from-minibuffer 
				 (format 
				  "Move to position [ 1 - arity('%s') ] : " name) 
				 "2"
				 ))
	(setq arg2 (string-to-int argns))
	(xref-non-interactive-move-parameter name arg1 arg2)
))


;;;;;;;;;;;;;;;;;;;;;;;; EXPAND - REDUCE NAMES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun xref-simplest-refactoring (opt message)
  (xref-refactoring-init-actions message)
  (xref-server-call-refactoring-task (list opt))
  (xref-refactoring-finish-actions)
)

(defun xref-reduce-names (rd)
  (xref-simplest-refactoring "-rfct-reduce" "reduction of long names")
)

(defun xref-expand-names (rd)
  (xref-simplest-refactoring "-rfct-expand" "expansion of short names")
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MOVING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun xref-set-moving-target-position (rd)
  (interactive "i")
  (set-marker xref-moving-refactoring-marker (point))
  (setq xref-moving-refactoring-line (count-lines (point-min) (point)))
  (if (bolp) (setq xref-moving-refactoring-line (1+ xref-moving-refactoring-line)))
  (if rd
	  (progn
		(message "Next moving refactoring will move to %s:%d" 
				 (xref-file-last-name (buffer-file-name)) 
				 xref-moving-refactoring-line)
		))
)

(defun xref-moving (moveopt)
  (let ((name) (tf))
	(if (eq xref-moving-refactoring-line 0)
		(error "No target position. Use 'Set target position' first.")
	  )
	(setq name (xref-get-identifier-on-point))

	;; check target place
	(save-excursion
	  (xref-set-to-marker xref-moving-refactoring-marker)
	  (if (bobp) (forward-line (- xref-moving-refactoring-line 1)))
	  (if (not (bolp))
		  (progn
			(message "moving target marker at the beginning of line")
			(beginning-of-line)
			))
	  (setq tf (buffer-file-name))
	  )

	;; move
	(xref-server-call-refactoring-task 
	 (append moveopt (list
					  (format "-commentmovinglevel=%d" xref-commentary-scope-level) 
					  (format "-movetargetfile=%s" tf) 
					  (format "-rfct-param1=%s" xref-moving-refactoring-line))))

	;; all done
	(save-excursion
	  (xref-set-to-marker xref-moving-refactoring-marker)
	  (xref-set-moving-target-position nil)
	  )
))


(defun xref-move-static-method (rd)
  (let ((name))
	(setq name (xref-get-identifier-on-point))
	(xref-refactoring-init-actions (format "moving %s" name))
	(xref-moving '("-rfct-move-static-method"))
	(xref-refactoring-finish-actions)
))

(defun xref-move-static-field (rd)
  (let ((name))
	(setq name (xref-get-identifier-on-point))
	(xref-refactoring-init-actions (format "moving %s" name))
	(xref-moving '("-rfct-move-static-field"))
	(xref-refactoring-finish-actions)
))

(defun xref-move-field (rd)
  (let ((name) (prefix))
	(setq name (xref-get-identifier-on-point))
	(xref-refactoring-init-actions (format "moving %s" name))
	(setq prefix (read-from-minibuffer 
				  (format "Field (string) to get target object from source object: ") ""))
	(xref-moving (list "-rfct-move-field" (format "-rfct-param2=%s" prefix)))
	(xref-refactoring-finish-actions)
))

(defun xref-move-class (rd)
  (let ((name))
	(setq name (xref-get-identifier-on-point))
	(xref-refactoring-init-actions (format "moving %s" name))
	(xref-moving '("-rfct-move-class"))
	(xref-refactoring-finish-actions)
))

(defun xref-class-to-new-file-moving (option)
  (let ((fname) (cname) (buff) (cb))
	(setq cname (xref-get-identifier-on-point))
	(setq fname (xref-read-jpath-from-minibuffer "Enter new file name: " 
												 (concat default-directory cname ".java")))
	(setq cb (current-buffer))
	(setq buff (get-file-buffer fname))
	(if buff
		(progn
		  (if (not (xref-yes-or-no-window (format "Buffer %s exists, erase it first? " 
											 (file-name-nondirectory fname))
									 t nil))
			  (error "O.K. Use 'Move Class' instead.")
			(set-buffer buff)
			(erase-buffer)
			))
	  ;; else
	  (if (file-attributes fname)
		  (progn
			(if (not (xref-yes-or-no-window (format "File %s exists, erase it first? " 
											   (file-name-nondirectory fname))
											t nil))
				(error "O.K. Use 'Move Class' instead.")
			  (find-file fname)
			  (erase-buffer)
			  ))))
	(set-buffer cb)
	;; move
	(xref-server-call-refactoring-task 
	 (append option (list
					  (format "-commentmovinglevel=%d" xref-commentary-scope-level) 
					  (format "-movetargetfile=%s" fname) 
					  (format "-rfct-param1=%s" 1))))
))

(defun xref-move-class-to-new-file (rd)
  (let ((name))
	(setq name (xref-get-identifier-on-point))
	(xref-refactoring-init-actions (format "moving %s" name))
	(xref-class-to-new-file-moving '("-rfct-move-class-to-new-file"))
	(xref-refactoring-finish-actions)
))

(defun xref-move-file (rd)
  (let ((name))
	(setq name (xref-get-identifier-on-point))
	(xref-refactoring-init-actions (format "moving %s" name))
	(xref-class-to-new-file-moving '("-rfct-move-all-classes-to-new-file"))
	(xref-refactoring-finish-actions)
))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun xref-pull-up-method (rd)
  (let ((name))
	(setq name (xref-get-identifier-on-point))
	(xref-refactoring-init-actions (format "pulling up %s" name))
	(xref-moving '("-rfct-pull-up-method"))
	(xref-refactoring-finish-actions)
))


(defun xref-pull-up-field (rd)
  (let ((name))
	(setq name (xref-get-identifier-on-point))
	(xref-refactoring-init-actions (format "pulling up %s" name))
	(xref-moving '("-rfct-pull-up-field"))
	(xref-refactoring-finish-actions)
))


(defun xref-push-down-method (rd)
  (let ((name))
	(setq name (xref-get-identifier-on-point))
	(xref-refactoring-init-actions (format "pushing down %s" name))
	(xref-moving '("-rfct-push-down-method"))
	(xref-refactoring-finish-actions)
))


(defun xref-push-down-field (rd)
  (let ((name))
	(setq name (xref-get-identifier-on-point))
	(xref-refactoring-init-actions (format "pushing down %s" name))
	(xref-moving '("-rfct-push-down-field"))
	(xref-refactoring-finish-actions)
))

;;;;;;;;;;;;;;;;;;;;;;;;;;; TURN STATIC - VIRTUAL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun xref-turn-static-method-to-dynamic (rd)
  (let ((argns) (arg) (name) (field))
	(setq name (xref-get-identifier-on-point))
	(setq argns (read-from-minibuffer 
				 (format 
				  "Argument to use to determine method's object [ 1 - arity('%s') ]? " name)
				 "1"
				 ))
	(setq field (read-from-minibuffer
				 "Optionally the field getting method's object from the argument [\"\"] "))
	(setq arg (string-to-int argns))
	(xref-refactoring-init-actions (format "making %s virtual" name))
	(xref-server-call-refactoring-task 
	 (list
	  "-rfct-static-to-dynamic"
	  (format "-rfct-param1=%d" arg)
	  (format "-rfct-param2=%s" field)))

	(xref-refactoring-finish-actions)
))

(defun xref-turn-dynamic-method-to-static (rd)
  (let ((class-name) (new-par-name) (name))
	(setq name (xref-get-identifier-on-point))
	(setq class-name (xref-compute-simple-information "-olcxcurrentclass"))
	(setq new-par-name 
		  (read-from-minibuffer "Name of the new paramater: " 
								(xref-param-from-class-name class-name)))
	(xref-refactoring-init-actions (format "makeing %s static" name))

	(xref-server-call-refactoring-task 
	 (list
	  "-rfct-dynamic-to-static"
	  (format "-rfct-param1=%s" new-par-name)))

	(xref-refactoring-finish-actions)
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EXTRACT METHOD ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun xref-add-macro-line-continuations (reg-beg-pos reg-end-pos)
  (let ( (unmoved) (loopfl))
	(goto-char reg-end-pos)
	(setq loopfl t)
	(while loopfl
	  (end-of-line 1)
	  (insert "\\")
	  (setq unmoved (forward-line -1))
	  (setq loopfl (and (eq unmoved 0) (>= (point) reg-beg-pos)))
	  )
))

(defun xref-extraction-dialog (minvocation mhead mtail dname)
  (let ((mbody) (name) (cfs) (mbuff) (sort) (sw)
		(mm) (pp) (bb) (ee) (ilen) (sr) (classextr))
	(setq sw (selected-window))
	(setq cfs case-fold-search)
	(setq mbuff (current-buffer))
	(setq mm (min (mark) (point)))
	(setq pp (max (mark) (point)))
	(setq mbody (buffer-substring mm pp))
	(xref-delete-window-in-any-frame xref-extraction-buffer nil t)

	(setq case-fold-search nil)
	(setq classextr (string-match "class" dname))
	(setq case-fold-search cfs)
	(if (string-match "method" dname)
		(setq sort "method")
	  (if (string-match "class" dname)
		  (setq sort "class")
		(if (string-match "macro" dname)
			(setq sort "macro")
		  (setq sort "function")
		  )))
	(xref-display-and-set-new-dialog-window xref-extraction-buffer nil t)
	(set-buffer xref-extraction-buffer)
	(xref-erase-buffer)
	(setq truncate-lines nil)
	(insert "")
	(insert (format "---------------   Xrefactory suggests following new %s:\n\n" sort))
	(insert mhead)
	(insert "\t// Original code start\n")
	(setq bb (point))
	(insert mbody)
	(if (not (bolp)) 
		(progn
		  (newline)
		  (setq mbody (buffer-substring bb (point)))
		  ))
	(if (equal sort "macro")
		(progn
		  (xref-add-macro-line-continuations bb (- (point) 1))
		  (goto-char (point-max))
		  (setq mbody (buffer-substring bb (point)))
		  ))
	(insert "\t// Original code end\n")
	(insert mtail)
	;;(insert "\n")
	(insert "---------------   which will be invoked by the command:\n\n")
	(insert minvocation)
	(beginning-of-buffer)
	(display-buffer xref-extraction-buffer)
	(if xref-renaming-default-name
		(setq name xref-renaming-default-name)
	  (setq name (read-from-minibuffer (format "Enter name for the new %s (empty string cancels the extraction): " sort)))
	  )
	(if (equal sort "class")
		(progn
		  (while (not (or (and (> (elt name 0) ?A) (< (elt name 0) ?Z))
						  (and (> (elt name 0) ?a) (< (elt name 0) ?z))))
			(setq name (read-from-minibuffer "Name has to start with a letter, enter new name: " name))
			)
		  (setq name (xref-downcase-first-letter name))
		  ))
	(xref-delete-window-in-any-frame xref-extraction-buffer nil t)
	(select-window sw)
	(if (not (equal name ""))
		(progn
		  ;; call xref task for continuation
		  (xref-send-data-to-process-and-dispatch 
		   (format "-renameto=%s -continuerefactoring" name)
		   dispatch-data nil)
		  ))
))

(defun xref-extract-method (rd)
  (xref-refactoring-init-actions (format "extract method"))
  (xref-server-call-refactoring-task (list 
									  "-rfct-extract-method"
									  (format "-commentmovinglevel=%d" xref-commentary-scope-level) 
									  ))
  (xref-refactoring-finish-actions)
)

(defun xref-extract-function (rd)
  (xref-refactoring-init-actions (format "extract function"))
  (xref-server-call-refactoring-task (list 
									  "-rfct-extract-method"
									  (format "-commentmovinglevel=%d" xref-commentary-scope-level) 
									  ))
  (xref-refactoring-finish-actions)
)

(defun xref-extract-macro (rd)
  (xref-refactoring-init-actions (format "extract macro"))
  (xref-server-call-refactoring-task (list 
									  "-rfct-extract-macro"
									  (format "-commentmovinglevel=%d" xref-commentary-scope-level) 
									  ))
  (xref-refactoring-finish-actions)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun xref-reduce-long-names-in-the-file (rd)
  (xref-refactoring-init-actions (format "reduction of long names"))
  (xref-server-call-refactoring-task (list "-rfct-reduce-long-names-in-the-file"))
  (xref-refactoring-finish-actions)
)

(defun xref-add-to-imports (rd)
  (xref-refactoring-init-actions (format "adding import"))
  (xref-server-call-refactoring-task (list "-rfct-add-to-imports"))
  (xref-refactoring-finish-actions)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun xref-self-encapsulate-field (rd)
  (let ((name))
	(setq name (xref-get-identifier-on-point))
	(xref-refactoring-init-actions (format "self encapsulation of %s" name))
	(xref-server-call-refactoring-task (list "-rfct-self-encapsulate-field"))
	(xref-refactoring-finish-actions)
))

(defun xref-encapsulate-field (rd)
  (let ((name))
	(setq name (xref-get-identifier-on-point))
	(xref-refactoring-init-actions (format "encapsulation of %s" name))
	(xref-server-call-refactoring-task (list "-rfct-encapsulate-field"))
	(xref-refactoring-finish-actions)
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

