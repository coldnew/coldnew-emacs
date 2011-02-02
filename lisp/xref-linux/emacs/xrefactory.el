;;; xrefactory.el - (X)Emacs interface to Xrefactory

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

(provide 'xrefactory)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; You can redefine following two functions in your ~/.emacs file in
;; order to change the default key-bindings

(if (not (functionp 'xref-add-bindings-to-keymap))
	(defun xref-add-bindings-to-keymap (keymap)
	  (define-key keymap [(f11)] 'xref-refactor)
	  (define-key keymap [(f8)] 'xref-completion)
	  (define-key keymap [(control f8)] 'xref-ide-compile-run)
	  (define-key keymap [(f7)] 'xref-delete-window)
	  (define-key keymap [(f6)] 'xref-push-and-goto-definition)
	  (define-key keymap [(control f6)] 'xref-browse-symbol)
	  (define-key keymap [(f5)] 'xref-pop-and-return)
	  (define-key keymap [(control f5)] 'xref-re-push)
	  (define-key keymap [(f4)] 'xref-next-reference)
	  (define-key keymap [(control f4)] 'xref-alternative-next-reference)
	  (define-key keymap [(f3)] 'xref-previous-reference)
	  (define-key keymap [(control f3)] 'xref-alternative-previous-reference)
	  )
)

;; this function applies only if 'xref-key-binding is set to 'local;
;; in this case you can bind more functions

(if (not (functionp 'xref-add-key-bindings-to-local-keymap))
	(defun xref-add-key-bindings-to-local-keymap (keymap)
	  ;; mapping available only for local keymaps
	  (define-key keymap [(meta ?.)] 'xref-push-name)
	  (define-key keymap [(meta tab)] 'xref-completion)
	  ;; plus standard mappings
	  (xref-add-bindings-to-keymap keymap)
	  )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; An  invocation  of  the  following  function  adds  Xrefactory  key
;; bindings to  current local keymap.  You can use this  function when
;; defining your own programming modes (for example yacc-mode).

(defun xref-add-key-bindings-to-current-local-keymap ()
  (if (not (current-local-map))
	  (use-local-map (make-sparse-keymap))
	)
  (xref-add-key-bindings-to-local-keymap (current-local-map))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Suffixes  determining  buffers where  information  for undoing  are
;; recorded.  Xrefactory  undo will operate  only on files  with those
;; suffixes

(defvar xref-undo-allowed-suffixes '(".java" 
									 ".c" ".h" 
									 ".C" ".cpp" ".cc" ".CC"
									 ".tc" ".th" ".tcc" ".thh" 
									 ".y"
									 ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; current platform identification (will be used in default settings)

(if (or (string-match "-nt" system-configuration) 
		(string-match "-win" system-configuration))
	(defvar xref-platform 'windows)
  (defvar xref-platform 'unix)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Completion coloring, you can choose your colors here!

(if (fboundp 'make-face)
	(progn
	  (if (not (boundp 'xref-list-default-face)) 
		  (make-face 'xref-list-default-face)
		)
	  (if (not (boundp 'xref-list-pilot-face)) 
		  (progn
			(make-face 'xref-list-pilot-face)
			(set-face-foreground 'xref-list-pilot-face "navy")
			))
	  (if (not (boundp 'xref-list-symbol-face)) 
		  (progn
			(make-face 'xref-list-symbol-face)
			(set-face-foreground 'xref-list-symbol-face "Sienna")
			))
	  (if (not (boundp 'xref-keyword-face)) 
		  (progn
			(make-face 'xref-keyword-face)
			(set-face-foreground 'xref-keyword-face "blue")
			))
	  (if (not (boundp 'xref-list-classname-face)) 
		  (progn
			(make-face 'xref-list-classname-face)
			(set-face-foreground 'xref-list-classname-face "grey")
			))
	  (if (not (boundp 'xref-error-face)) 
		  (progn
			(make-face 'xref-error-face)
			(set-face-foreground 'xref-error-face "red")
			))
	  )
 ;; if do not know how to make faces, set the coloring off
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar xref-preferable-undo-limit 2500000)
(defvar xref-preferable-undo-strong-limit 3000000)

(if (and (boundp 'undo-limit) (> xref-preferable-undo-limit undo-limit))
	(setq undo-limit xref-preferable-undo-limit)
  )
(if (and (boundp 'undo-strong-limit) 
		 (> xref-preferable-undo-strong-limit undo-strong-limit))
	(setq undo-strong-limit xref-preferable-undo-strong-limit)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      NOTHING MORE TO SETUP                          ;;
;;                    CUSTOMIZATION PART IS OVER                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Here  follows  some e-lisp  code  which  can't  be autoloaded  from
;; xref.el file


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; following code adds standard xrefactory keybinding for current session

(if (not (boundp 'xref-key-binding))
	(defvar xref-key-binding 'global)
  )

(if (eq xref-key-binding 'global)
	(progn
	  (xref-add-bindings-to-keymap global-map)
	  ;; following is always set in the GLOBAL keymap !!
	  ;; if you are using some binding for S-mouse-2, put it in comment
	  (global-set-key [(shift mouse-2)] 'xref-find-file-on-mouse)
	  )

  (if (eq xref-key-binding 'local)
	  (progn
		(if (load "cc-mode" t)
			(progn
			  (if (boundp 'c-mode-map)
				  (xref-add-key-bindings-to-local-keymap c-mode-map)
				(message "[Xrefactory] c-mode-map not found, skipping keymap setting")
				)
			  (if (boundp 'c++-mode-map)
				  (xref-add-key-bindings-to-local-keymap c++-mode-map)
				(message "[Xrefactory] c++-mode-map not found, skipping keymap setting")
				)
			  (if (boundp 'java-mode-map)
				  (xref-add-key-bindings-to-local-keymap java-mode-map)
				(message "[Xrefactory] java-mode-map not found, skipping keymap setting")
				)
			  )
		  (message "[Xrefactory] cc-mode file not found, cannot setup local key binding, making it global.")
		  (xref-add-bindings-to-keymap global-map)
		  )

		;; following is always set in the GLOBAL keymap !!
		;; if you are using some binding for S-mouse-2, put it in comment
		(global-set-key [(shift mouse-2)] 'xref-find-file-on-mouse)
		))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Xrefactory configuration  variables setting. Majority  of variables
;; can be set interactively using  'Options' item of 'Xref' main menu.
;; the first one is the exception :)

;; some versions of Xemacs with Mule support contain bug making
;; string operations very slow, this option controls whether
;; Xrefactory can fix it by converting strings to lists

(if (not (boundp 'xref-perform-xemacs-mule-fix))
	(defvar xref-perform-xemacs-mule-fix t)
)

;; this variable specifies how to escape from Xrefactory windows
;; usually on simple Escape, but on alphanumerical consoles, where
;; special keys are coded via Escape prefix, close window on escape-escape
;; sequence
(if (not (boundp 'xref-escape-key-sequence)) 
	(progn
	  (defvar xref-escape-key-sequence "\e")
	  (if (not window-system)
		  (setq xref-escape-key-sequence "\e\e")
		)))

(if (not (boundp 'xref-save-buffers-without-prompt))
	(defvar xref-save-buffers-without-prompt t)
  )

(if (not (boundp 'xref-version-control)) 
	(defvar xref-version-control nil)
  )

(if (not (boundp 'xref-version-control-checkin-on-auto-saved-buffers)) 
	(defvar xref-version-control-checkin-on-auto-saved-buffers nil)
  )

(if (not (boundp 'xref-inspect-errors-if-compilation-window)) 
	(defvar xref-inspect-errors-if-compilation-window t)
  )

;; by default xrefactory does binds left button to its functions
(if (not (boundp 'xref-bind-left-mouse-button)) 
	(defvar xref-bind-left-mouse-button t)
  )

;; when displaying browser, default filter level will be 2
(if (not (boundp 'xref-default-symbols-filter))
	(defvar xref-default-symbols-filter 2)
  )

(if (not (boundp 'xref-default-references-filter))
	(defvar xref-default-references-filter 0)
  )

;; when updating browser, keep lastly set filter level
(if (not (boundp 'xref-keep-last-symbols-filter))
	(defvar xref-keep-last-symbols-filter t)
  )

(if (not (boundp 'xref-commentary-scope-level)) 
	(defvar xref-commentary-scope-level 0)
  )

(if (not (boundp 'xref-manual-symbol-selection-within-refactoring)) 
	(defvar xref-manual-symbol-selection-within-refactoring nil)
  )

(if (not (boundp 'xref-prefer-import-on-demand)) 
	(defvar xref-prefer-import-on-demand t)
  )

(if (not (boundp 'xref-save-files-and-update-tags-after-refactoring)) 
	(defvar xref-save-files-and-update-tags-after-refactoring nil)
  )

(if (not (boundp 'xref-save-files-and-update-tags-before-refactoring)) 
	(defvar xref-save-files-and-update-tags-before-refactoring nil)
  )


(if (not (boundp 'xref-refactoring-security-level)) 
	(defvar xref-refactoring-security-level 'high)
  )

;; by default xref asks before starting to browse a javadoc URL
(if (not (boundp 'xref-ask-before-browse-javadoc)) 
	(defvar xref-ask-before-browse-javadoc t)
  )

(if (not (boundp 'xref-browse-url-directly))
	(if (eq xref-platform 'windows)
		(defvar xref-browse-url-directly nil)
	  (defvar xref-browse-url-directly t)
	  )
  )

;; autoredirect is turn off on windows because of some problems
(if (not (boundp 'xref-browse-url-manual-redirect))
	(if (eq xref-platform 'windows)
		(defvar xref-browse-url-manual-redirect t)
	  (defvar xref-browse-url-manual-redirect nil)
	  )
  )

;; by  default multibyte  buffer representation  is allowed in order
;; NOT USED ANYMORE, it is here for backward compatibility only,
;; use 'xref-files-encoding now.
(if (not (boundp 'xref-allow-multibyte)) 
	(defvar xref-allow-multibyte t)
  )

(if (not (boundp 'xref-files-encoding)) 
	(defvar xref-files-encoding 'generic)
  )

;; by default truncation is disallowed in order to see profiles
(if (not (boundp 'xref-completion-truncate-lines)) 
	(defvar xref-completion-truncate-lines nil)
  )

(if (not (boundp 'xref-completion-displays-internal-type)) 
	(defvar xref-completion-displays-internal-type nil)
  )

(if (not (boundp 'xref-completion-inserts-parenthesis)) 
	(defvar xref-completion-inserts-parenthesis nil)
  )

;; by default truncation is disallowed in order to see profiles
(if (not (boundp 'xref-completion-overload-wizard-deep)) 
	(defvar xref-completion-overload-wizard-deep 1)
  )

;; by default the automatic project selection is on.
(if (not (boundp 'xref-current-project)) 
	(defvar xref-current-project nil)
  )


;; by default  the accessibility and linkage checks  are not performed
;; on proposed completions
(if (not (boundp 'xref-completion-access-check)) 
	(defvar xref-completion-access-check nil)
  )
(if (not (boundp 'xref-completion-linkage-check)) 
	(defvar xref-completion-linkage-check nil)
  )

;; by default  fully qualified type names are proposed when completing
; Java type name
(if (not (boundp 'xref-java-fqt-name-completion-level)) 
	(defvar xref-java-fqt-name-completion-level 2)
  )

(if (not (boundp 'xref-coloring)) 
	(defvar xref-coloring (fboundp 'make-face))
  )

(if (not (boundp 'xref-highlight-java-keywords)) 
	(defvar xref-highlight-java-keywords nil)
  )

(if (not (boundp 'xref-mouse-highlight)) 
	(defvar xref-mouse-highlight t)
  )

(if (not (boundp 'xref-multifile-undo-deep)) 
	(defvar xref-multifile-undo-deep 50)
  )

(if (not (boundp 'xref-ide-last-run-command)) 
	(defvar xref-ide-last-run-command "run")
)

;; the default variable executed by first xref-ide-compile command
(if (not (boundp 'xref-ide-last-compile-command)) 
	(defvar xref-ide-last-compile-command 'compileproject)
)
;; can be 'compilefile 'compiledir or 'compileproject

;; the default limit for number of completions
(if (not (boundp 'xref-max-completions)) 
	(defvar xref-max-completions 500)
)

;; shell to interpret multi-line compile and run commands
(if (not (boundp 'xref-shell)) 
	(defvar xref-shell "sh")
)

;; generate batch file also when compiling with single line command
(if (not (boundp 'xref-always-batch-file))
	(defvar xref-always-batch-file t)
)

(if (not (boundp 'xref-move-point-back-after-refactoring))
	(defvar xref-move-point-back-after-refactoring nil)
)

(if (not (boundp 'xref-detailed-refactoring-confirmations))
	(defvar xref-detailed-refactoring-confirmations nil)
)

(if (not (boundp 'xref-auto-update-tags-before-push))
	(defvar xref-auto-update-tags-before-push nil)
)

(if (not (boundp 'xref-smart-browsing-mode))
	(defvar xref-smart-browsing-mode t)
)

(if (not (boundp 'xref-smart-browsing-mode-auto-update))
	(defvar xref-smart-browsing-mode-auto-update t)
)

(if (not (boundp 'xref-smart-browse-check-name))
	(defvar xref-smart-browse-check-name t)
)

(if (not (boundp 'xref-browser-lists-source-lines))
	(defvar xref-browser-lists-source-lines t)
)

(if (not (boundp 'xref-close-windows-on-pop))
	(defvar xref-close-windows-on-pop nil)
)

(if (not (boundp 'xref-completion-case-sensitive))
	(defvar xref-completion-case-sensitive nil)
)

(if (not (boundp 'xref-completion-quit-on-q))
	(defvar xref-completion-quit-on-q t)
)

(if (not (boundp 'xref-completion-delete-pending-identifier))
	(defvar xref-completion-delete-pending-identifier t)
)

(if (not (boundp 'xref-completion-auto-focus))
	(defvar xref-completion-auto-focus t)
)

(if (not (boundp 'xref-browse-url-focus-delay))
	(defvar xref-browse-url-focus-delay 0)
)

(if (not (boundp 'xref-window-minimal-size))
	(defvar xref-window-minimal-size 4)
)

(if (not (boundp 'xref-browser-splits-window-horizontally))
	(defvar xref-browser-splits-window-horizontally nil)
)

(if (not (boundp 'xref-class-tree-splits-window-horizontally))
	(defvar xref-class-tree-splits-window-horizontally t)
)

(if (not (boundp 'xref-browser-position-left-or-top))
	(defvar xref-browser-position-left-or-top nil)
)

(if (not (boundp 'xref-class-tree-position-left-or-top))
	(defvar xref-class-tree-position-left-or-top t)
)

(if (not (boundp 'xref-class-tree-window-width))
	(defvar xref-class-tree-window-width 30)
)

(if (not (boundp 'xref-symbol-selection-window-width))
	(defvar xref-symbol-selection-window-width 30)
)

(if (not (boundp 'xref-symbol-selection-window-height))
	(defvar xref-symbol-selection-window-height 10)
)

(if (not (boundp 'xref-browser-windows-auto-resizing))
	(defvar xref-browser-windows-auto-resizing t)
)

(if (not (boundp 'xref-display-active-project-in-minibuffer))
	(defvar xref-display-active-project-in-minibuffer t)
)

(if (not (boundp 'xref-run-find-file-hooks))
	(defvar xref-run-find-file-hooks t)
)

(if (not (boundp 'xref-options-file))
	(if (eq xref-platform 'windows)
		(if (getenv "HOME")
			(defvar xref-options-file (concat (getenv "HOME") "/_xrefrc"))
		  (defvar xref-options-file "c:/_xrefrc")
		  )
	  (defvar xref-options-file (concat (getenv "HOME") "/.xrefrc"))
	  )
)

(if (eq xref-platform 'windows)
	(defvar xref-default-tmp-dir (getenv "TEMP"))
  (defvar xref-default-tmp-dir "/tmp")
  )

(if (eq xref-platform 'windows)
	(defvar xref-default-user-identification "user")
  (defvar xref-default-user-identification (concat "x" (getenv "LOGNAME")))
  )

(if (not (boundp 'xref-tmp-dir))
	(defvar xref-tmp-dir xref-default-tmp-dir)
)

(if (not (boundp 'xref-user-identification))
	(defvar xref-user-identification xref-default-user-identification)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; setting Xrefactory customization group

(if (commandp 'customize)
	(progn
	  (defgroup xrefactory nil
"

Here you  can set variables controlling those  behaviors of Xrefactory
which are not project-dependent, and its general appearance.

")


;;;;;;;;;;;;;;;; xrefactory-general ;;;;;;;;;

	  (defgroup xrefactory-general nil
		"

Here  you  can  set  variables  controlling the  general  behavior  of
Xrefactory functions.

"
		:group 'xrefactory
)

	  (defcustom xref-options-file xref-options-file
		"This option determines where the file describing Xrefactory project-specific options (projects and their setting) is stored. You will need to kill (and restart) the xref process if changing this option."
		:type '(string)
		;; :type '(file) ;; this puts there ~, etc making problems
		:group 'xrefactory-general)

	  (defcustom xref-version-control nil
		"This variable stores identification of version control system used by Xrefactory: nil means no VC system; t means the Emacs VC system implemented by the `vc' package; any other value is interpreted as an identification of a user-defined version system."
		:type '(symbol)
		:group 'xrefactory-general)

	  (defcustom xref-version-control-checkin-on-auto-saved-buffers nil
		"If on and if version control is on, then automatic save after refactoring also checks in modified files. Be careful with this option as this clears undo buffers. Too frequent checkins will make undoing impossible."
		:type '(boolean)
		:group 'xrefactory-general)

	  (defcustom xref-save-buffers-without-prompt t
		"This variable determines whether Xrefactory can save open buffers without confirmation, if it considers it good for further processing of current command. Such situation can happen during browsing, when tags are not up to date, or before or after a refactoring. Keeping this option on usualy makes Tag updates more efficient and less frequent."
		:type '(boolean)
		:group 'xrefactory-general)

	  (defcustom xref-bind-left-mouse-button t
		"If on, Xrefactory will bind the left mouse button in its dialog windows. The button will be bound to the same function as the middle button. If you change this value, you will need to restart Emacs in order for that new value take effect."
		:type '(boolean)
		:group 'xrefactory-general)

	  (defcustom xref-files-encoding 'generic
		"This variable specifies Xrefactory multi language file encoding.  Available values are 'generic', 'ascii', 'european', 'euc', 'sjis', 'utf' and 'project'. If you use only 7-bit ascii charset, set this option to 'ascii. If you use 8-bit europeen encoding, set this value to 'european. If you use a kind of EUC encoding (multiple 8-bits Japanese, Korean, ...), set it to 'euc. If you use Japanese SJIS encoding, set it to 'sjis. If you use one of unicode encodings (utf-8 or utf-16) set it to 'utf'. Otherwise, use 'generic settings, which should work fine at least for completions and browsing. The value 'project allows to specify encoding for each project separately. It is highly recommended to set this option to 'project and to specify encodings for each of your projects within project options."
		:type '(symbol)
		:group 'xrefactory-general)

	  (defcustom xref-tmp-dir xref-default-tmp-dir
		"Directory where temporary files used for communication between Emacs and xref task are stored. If you are working under Windows system, it is a good idea to set this directory to a RAM disk. You will need to restart Emacs when changing this option."
		:type '(string)
		:group 'xrefactory-general)

	  (defcustom xref-user-identification xref-default-user-identification
		"A unique string used to generate names of various temporary files. In case of clashes, if you use simultaneously several copies of Xrefactory, you may need to change this string to be unique for each copy.  You will need to restart Emacs when changing this option."
		:type '(string)
		:group 'xrefactory-general)

	  (defcustom xref-window-minimal-size 5
		"Minimal size of window displaying completions and references. If a positive value is given the window's size will be proportional to number of completions, but at least xref-window-minimal-size lines."
		:type '(integer)
		:group 'xrefactory-general)

	  (defcustom xref-display-active-project-in-minibuffer t
		"If on, Xrefactory will display the active project in minibuffer after each invocation of its functions (except completions)."
		:type '(boolean)
		:group 'xrefactory-general)



;;;;;;;;;;;;;;;; IDE ;;;;;;;;;

	  (defgroup xrefactory-compile-run nil
		"

Here you can set variables controlling the Emacs IDE interface.

"
		:group 'xrefactory
)

	  (defcustom xref-ide-last-compile-command 'compileproject
		"Can be either 'compilefile, 'compiledir or 'compileproject. This variable indicates which compilations will be invoked by 'Emacs IDE -> Last Compile' command. You can preset it to your preferred default value."
		:type '(symbol)
		:group 'xrefactory-compile-run)

	  (defcustom xref-always-batch-file t
		"If on, Xrefactory will generate and then execute a batch file when executing an Emacs IDE compile function. If off, Xrefactory will generate a batch file only when the compile command exceeds one line."
		:type '(boolean)
		:group 'xrefactory-compile-run)

	  (defcustom xref-shell "sh"
		"This option determines which shell is used for interpreting batch files generated for Emacs IDE compile and run commands. This variable applies only on Unix like platforms."
		:type '(string)
		:group 'xrefactory-compile-run)


;;;;;;;;;;;;;;;; xrefactory-completion ;;;;;;;;;

	  (defgroup xrefactory-completion nil
		"

Here  you  can  set  variables controlling  behavior  of  Xrefactory's
completions.

"
		:group 'xrefactory
)

	  (defcustom xref-completion-case-sensitive nil
		"If on, then completion is case-sensitive and does not suggest completions differing in cases from the prefix. Keeping this option off is a good idea for lazy users who don't type case correctly."
		:type '(boolean)
		:group 'xrefactory-completion)

	  (defcustom xref-completion-delete-pending-identifier t
		"If on, after inserting a completion Xrefactory deletes the rest of the old identifier after the point."
		:type '(boolean)
		:group 'xrefactory-completion)

	  (defcustom xref-completion-inserts-parenthesis nil
		"If on, Xrefactory will insert an open parenthesis after a method name."
		:type '(boolean)
		:group 'xrefactory-completion)

	  (defcustom xref-completion-quit-on-q t
		"If on, the 'q' key in the completions window will close the window (if there is no interactive search in progress). Users may find this more useful than searching for completions starting with 'q'."
		:type '(boolean)
		:group 'xrefactory-completion)

	  (defcustom xref-completion-truncate-lines nil
		"If on, Xrefactory will truncate lines in buffers containing completions. Also default formatting of completions will be turned off, so exactly one completion will appear per line. Truncated parts of lines can be viewed by scrolling left and right (Shift-Left and Shift-Right)."
		:type '(boolean)
		:group 'xrefactory-completion)

	  (defcustom xref-completion-displays-internal-type nil
		"If on, Xrefactory will display additional informations about completed symbol by reconstructing it from its internal tables. The resulting type will reflect preprocessing, template type expansion, etc. If the value of this option is nil, then the declaration as it appears in the source code is displayed. In majority cases, the pure source text is more informative than its expanded form."
		:type '(boolean)
		:group 'xrefactory-completion)

	  (defcustom xref-completion-access-check nil
		"If on, then when completing a C++/Java attribute, Xrefactory will suggest only accessible symbols checking protected/public/private attributes."
		:type '(boolean)
		:group 'xrefactory-completion)

	  (defcustom xref-completion-linkage-check nil
		"If on, then when completing a C++/Java attribute in a static context, Xrefactory will suggest only static fields/methods."
		:type '(boolean)
		:group 'xrefactory-completion)

	  (defcustom xref-completion-auto-focus t
		"If on, window with multiple completions get the focus automatically after being displayed. Otherwise the window with source code keeps the focus."
		:type '(boolean)
		:group 'xrefactory-completion)

	  (defcustom xref-java-fqt-name-completion-level 2
		"Level of completion for fully qualified type names. 
'0' means that no fully qualified names are suggested. '1' causes classes read from .jar archives to be suggested. For for 'LinkedLi', 'java.util.LinkedList' is suggested. '2' adds classes from the active project. '3' adds classes stored in classpath directories; finally '4' adds classes stored in sourcepath directories."
		:type '(integer)
		:group 'xrefactory-completion)

	  (defcustom xref-completion-overload-wizard-deep 1
		"Level of inheritance for completion of overloaded methods. When completion is invoked on an empty string at a position when a method definition can start, then all methods from the superclasses are suggested. This option specifies how deeply superclasses are scanned."
		:type '(integer)
		:group 'xrefactory-completion)

	  (defcustom xref-max-completions 500
		"Maximum number of suggested completions. If there are more available, an ellipsis (...) is shown."
		:type '(integer)
		:group 'xrefactory-completion)


;;;;;;;;;;;;;;;; class tree ;;;;;;;;;

	  (defgroup xrefactory-class-tree nil
		"

Here you can set variables controlling Xrefactory's class-tree viewer.

"
		:group 'xrefactory
)

	  (defcustom xref-class-tree-splits-window-horizontally t
		"If on, Xrefactory displays the class tree in a horizontally split window. Otherwise it splits the window vertically."
		:type '(boolean)
		:group 'xrefactory-class-tree)

	  (defcustom xref-class-tree-position-left-or-top nil
		"If on, the class tree is displayed at the left or top of the frame (depending on whether the window is split vertically or not). Otherwise, the class tree is displayed at the right or bottom."
		:type '(boolean)
		:group 'xrefactory-class-tree)

	  (defcustom xref-class-tree-window-width 30
		"Default width of class tree window."
		:type '(integer)
		:group 'xrefactory-class-tree)


;;;;;;;;;;;;;;;; xrefactory-browsing-configuration ;;;;;;;;;

	  (defgroup xrefactory-source-browser nil
		"

Here you can set variables controlling the behavior of
Xrefactory's source browsing functions.

"
		:group 'xrefactory
)

	  (defcustom xref-smart-browsing-mode t
		"If on, Xrefactory will browse in smart mode. In this mode symbols are identified by position (and possibly name) and the currently edited buffer is not parsed."
		:type '(boolean)
		:group 'xrefactory-source-browser)

	  (defcustom xref-smart-browse-check-name t
		"If on, Xrefactory in smart mode will check the name of the browsed symbol. This disallow resolution to other symbols invoked at this place such as implicitly invoked C++ constructors, destructors or symbols created with ## preprocessor directive. In usual case it is convenient to keep this option off. If you need to know exactly which hidden functions are invoked at this place, turn it on."
		:type '(boolean)
		:group 'xrefactory-source-browser)

	  (defcustom xref-auto-update-tags-before-push nil
		"If on, Xrefactory will update the tags file before pushing references on to browser stack. If you are working on a small project and you have a fast computer, then it may be convenient to turn this option on."
		:type '(boolean)
		:group 'xrefactory-source-browser)

	  (defcustom xref-smart-browsing-mode-auto-update t
		"If on and if xref-smart-browsing-mode is on, then Xrefactory will automatically perform update of Tags before each pushing action even if xref-auto-update-tags-before-push is off. This option allows to perform auto updates in smart browse mode and to not perform them in normal browse mode. If you are actively working on the project you are browsing, then keep this option on."
		:type '(boolean)
		:group 'xrefactory-source-browser)

	  (defcustom xref-browser-lists-source-lines t
		"If on, the Xrefactory browser will display one line of source code for each reference in the browser window. This may slow down the display when there are large numbers of references."
		:type '(boolean)
		:group 'xrefactory-source-browser)

	  (defcustom xref-browser-splits-window-horizontally nil
		"If on, Xrefactory displays class tree in a horizontally split window. Otherwise it splits the window vertically."
		:type '(boolean)
		:group 'xrefactory-source-browser)

	  (defcustom xref-browser-position-left-or-top nil
		"If on, the browser is displayed at the left or top of the frame (depending on whether the window was split vertically or not). Otherwise, the browser is displayed at the right or bottom."
		:type '(boolean)
		:group 'xrefactory-source-browser)

	  (defcustom xref-default-symbols-filter 2
		"Controls which symbols/classes are shown when displaying a new browser window. '2': only symbols with the same name and profile as the browsed symbol, in a class related (via inheritance) to that of the browsed symbol; '1': all symbols with the same profile; '0': all symbols of the same name."
		:type '(integer)
		:group 'xrefactory-source-browser)

	  (defcustom xref-default-references-filter 0
		"Controls which references are shown. References to a variable work differently from references to other symbols. For variables: '3': only definition and declarations; '2': also l-value usages; Level 1: also usages where the address of the variable is taken; Level 0: all references. For other symbols: Level 3: only definitions and declarations; Level 2: also usages in EXTENDS and IMPLEMENTS clauses (meaningful only for Java); Level 1: also usages in the top level scope (global variable and function definitions; this can be used to see all the functions of a particular type); Level 0: all references."
		:type '(integer)
		:group 'xrefactory-source-browser)

	  (defcustom xref-keep-last-symbols-filter t
		"If on, then Xrefactory remembers the last filter level when pushing and popping in symbols/classes window. Otherwise the default filter level is used."
		:type '(boolean)
		:group 'xrefactory-source-browser)

	  (defcustom xref-browser-windows-auto-resizing t
		"If on, Xrefactory will resize newly created browser windows appropriately."
		:type '(boolean)
		:group 'xrefactory-source-browser)

	  (defcustom xref-symbol-selection-window-height 10
		"Browser window's default height. This value applies only to the first appearance of the browser window and only if xref-browser-windows-auto-resizing is off."
		:type '(integer)
		:group 'xrefactory-source-browser)

	  (defcustom xref-symbol-selection-window-width 30
		"Browser window's default width. This value applies only to the first appearance of the browser window and only if xref-browser-windows-auto-resizing is off."
		:type '(integer)
		:group 'xrefactory-source-browser)

	  (defcustom xref-inspect-errors-if-compilation-window t
		"If on, and if a compilation buffer is displayed in the current frame, then xref-alternative-previous-reference and xref-alternative-next-reference will inspect the previous/next error rather than the previous/next reference. In this way shortcuts (usually C-F3, C-F4) can be used to browse errors after an unsuccessful compilation."
		:type '(boolean)
		:group 'xrefactory-source-browser)




;;;;;;;;;;;;;;;; xrefactory-extern-html-browser ;;;;;;;;;

	  (defgroup xrefactory-extern-html-browser nil
		"

Here you can set variables  controlling the external HTML browser used
by Xrefactory.

"
		:group 'xrefactory
)

	  (defcustom xref-ask-before-browse-javadoc t
		"If on, Xrefactory asks for confirmation before launching the JavaDoc browser when trying to move to a definition of symbol without available source code."
		:type '(boolean)
		:group 'xrefactory-extern-html-browser)

	  (defcustom xref-browse-url-focus-delay 0
		"This variable specifies for how long the web browser window displaying JavaDoc will be raised before Emacs regains the focus. The value is a floating point number of seconds. If it is less than zero, then Emacs will not regain the focus."
		:type '(number)
		:group 'xrefactory-extern-html-browser)

	  (defcustom xref-browse-url-directly xref-browse-url-directly
		"If off, then instead of browsing a URL directly, Xrefactory will browse a temporary file containing an auto-redirection to browsed URL. This option is used to cope with a problem with browse-url under Windows."
		:type '(boolean)
		:group 'xrefactory-extern-html-browser)

	  (defcustom xref-browse-url-manual-redirect xref-browse-url-manual-redirect
		"If both 'xref-browse-url-directly' and 'xref-browse-url-manual-redirect' are on, then Xrefactory will generate and browse temporary files with links to JavaDoc when browsing the JavaDoc for a method having more than one parameter. The user has to click on the link manually in order to move to the JavaDoc documentation. This option is used to cope with a problem with auto-redirection in Internet Explorer."
		:type '(boolean)
		:group 'xrefactory-extern-html-browser)



;;;;;;;;;;;;;;;; xrefactory-refactoring ;;;;;;;;;

	  (defgroup xrefactory-refactoring nil
		"

Here you can set customizable variables controling behavior of
Xrefactory refactoring functions.

"
		:group 'xrefactory
)



	  (defcustom xref-manual-symbol-selection-within-refactoring nil
		"If on, Xrefactory always pops up a symbol resolution dialog inside refactorings. This can be used for renaming several symbols of the same name (different overloaded methods) at once. Do not keep this option on for a long time. Rather turn it on, do the massive refactoring you need and then turn it off."
		:type '(boolean)
		:group 'xrefactory-refactoring)

	  (defcustom xref-refactoring-security-level 'high
		"This variable determines how much of security checks are performed during refactorings. The value 'low means that the refactoring is applied with a minimal number of checks making refactoring potentially unsafe, the value 'high means maximum level of security for refactorings. Currently, the only supported values for this variable are: 'low and 'high."
		:type '(symbol)
		:group 'xrefactory-refactoring)

	  (defcustom xref-save-files-and-update-tags-before-refactoring nil
		"If on, Xrefactory saves all modified files and updates tags before each refactoring. This can speed up refactorings."
		:type '(boolean)
		:group 'xrefactory-refactoring)

	  (defcustom xref-save-files-and-update-tags-after-refactoring nil
		"If on, Xrefactory saves all modified files and updates tags after each refactoring."
		:type '(boolean)
		:group 'xrefactory-refactoring)

	  (defcustom xref-prefer-import-on-demand t
		"If on, Xrefactory will prefer to generate import on demand statements (import com.pack.*;) instead of single type imports (import com.pack.Type;). This applies whenever Xrefactory needs to add an import statement, which may be during moving refactorings or when reducing long type names."
		:type '(boolean)
		:group 'xrefactory-refactoring)

	  (defcustom xref-commentary-scope-level 0
		"The value of this variable determines how much commenting preceding a field, method or class definition is moved together with the field, method or class when a moving refactoring is applied. '0' moves no comments at all. '1' moves one doubleslashed comments (// ...). '2' moves one standard comment (/* ... */). '3' moves one doubleslashed and one standard comment. '4' moves all doubleslashed comments. '5' moves all standard comments. '6' moves all doubleslashed and all standard comments."
		:type '(integer)
		:group 'xrefactory-refactoring)

	  (defcustom xref-move-point-back-after-refactoring nil
		"If on, then after finishing a refactoring the point will be moved back to the position where the refactoring started. This is convenient for moving a suite of fields/methods into another file. In such cases it is best that the point is not moved to target position, because you wish to select the next field/method to be moved. Note that the target position is kept after a refactoring, so it can be reused without needing to set it again."
		:type '(boolean)
		:group 'xrefactory-refactoring)

	  (defcustom xref-run-find-file-hooks t
		"If on, Xrefactory will use the standard function 'find-file' when loading new files. Otherwise it will load buffers with its own function avoiding running of find-file-hooks. This means that new buffers will not be highlighted, etc. This option can be used to speed up big refactorings on larges projects where running file-local hooks can take a long time. However, not running hooks may cause unwanted side effects."
		:type '(boolean)
		:group 'xrefactory-refactoring)

	  (defcustom xref-multifile-undo-deep 50
		"This variable determines how many refactorings are kept in the undo list, i.e. how many refactorings can be undone. Xrefactory's undo mechanism is built on standard Emacs undo; to be able to undo large number of refactorings, you have to set 'undo-limit' and 'undo-strong-limit' to sufficiently large values."
		:type '(integer)
		:group 'xrefactory-refactoring)



;;;;;;;;;;;;;;;; highlighting colors faces ;;;;;;;;;

	  (defgroup xrefactory-highlighting-coloring nil
		"

Here you  can set variables controlling  everything concerning colors,
faces and highlighting in buffers created by Xrefactory.

"
		:group 'xrefactory
)

	  (defcustom xref-coloring xref-coloring
		"If on, Xrefactory will color symbols in completion and reference list buffers. This may be slow for large projects."
		:type '(boolean)
		:group 'xrefactory-highlighting-coloring)

	  (defcustom xref-highlight-java-keywords nil
		"If on, Xrefactory will color Java keywords in completion and reference list buffers. Otherwise it will color C keywords."
		:type '(boolean)
		:group 'xrefactory-highlighting-coloring)

	  (defcustom xref-mouse-highlight t
		"If on, Xrefactory will highlight symbols under the mouse in its buffers."
		:type '(boolean)
		:group 'xrefactory-highlighting-coloring)


))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar xref-home-directory "")

;; Now follows Xref menu definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                      Emacs


(if (not (string-match "XEmacs" emacs-version))
	(progn
	  ;; IDE menu
	  (defvar xref-ide-menu (make-sparse-keymap "Xref interface to Emacs IDE functions"))
	  (fset 'xref-ide-menu (symbol-value 'xref-ide-menu))
	  (define-key xref-ide-menu [xref-ide-compile-run] '("(Last) Compile & Run" . xref-ide-compile-run))
	  (define-key xref-ide-menu [xref-ide-sep1] '("--"))
	  (define-key xref-ide-menu [xref-ide-run] '("(Last) Run" . xref-ide-run))
	  (define-key xref-ide-menu [xref-ide-runthis] '("Run This" . xref-ide-run-this))
	  (define-key xref-ide-menu [xref-ide-run5] '("Run5" . xref-ide-run5))
	  (define-key xref-ide-menu [xref-ide-run4] '("Run4" . xref-ide-run4))
	  (define-key xref-ide-menu [xref-ide-run3] '("Run3" . xref-ide-run3))
	  (define-key xref-ide-menu [xref-ide-run2] '("Run2" . xref-ide-run2))
	  (define-key xref-ide-menu [xref-ide-run1] '("Run1" . xref-ide-run1))
	  (define-key xref-ide-menu [xref-ide-sep2] '("--"))
	  (define-key xref-ide-menu [xref-ide-next-err] '("Next Error (or Alternative Reference)" . xref-alternative-next-reference))
	  (define-key xref-ide-menu [xref-ide-previous-err] '("Previous Error (or Alternative Reference)" . xref-alternative-previous-reference))
	  ;;(define-key xref-ide-menu [xref-ide-preverror] '("Previous Error" . xref-ide-previous-error))
	  ;;(define-key xref-ide-menu [xref-ide-nexterror] '("Next Error" . xref-ide-next-error))
	  (define-key xref-ide-menu [xref-ide-sep3] '("--"))
	  (define-key xref-ide-menu [xref-ide-cmpl] '("(Last) Compile" . xref-ide-compile))
	  (define-key xref-ide-menu [xref-ide-cprj] '("Compile Project" . xref-ide-compile-project))
	  (define-key xref-ide-menu [xref-ide-cdir] '("Compile Directory" . xref-ide-compile-dir))
	  (define-key xref-ide-menu [xref-ide-cfile] '("Compile File" . xref-ide-compile-file))

	  ;; local browse menu
	  (defvar xref-lm-menu (make-sparse-keymap "Local Motion"))
	  (fset 'xref-lm-menu (symbol-value 'xref-lm-menu))
	  (define-key xref-lm-menu [xref-lm-next] '("Next Usage of This Symbol (or Alternatives)" . xref-alternative-next-reference))
	  (define-key xref-lm-menu [xref-lm-previous] '("Previous Usage of This Symbol (or Alternatives)" . xref-alternative-previous-reference))

	  (defvar xref-sb-menu (make-sparse-keymap "Browsing with Symbol Stack"))
	  (fset 'xref-sb-menu (symbol-value 'xref-sb-menu))
	  (define-key xref-sb-menu [xref-sb-repush] '("Re-push Popped Symbol" . xref-re-push))
	  (define-key xref-sb-menu [xref-sb-sep3] '("--"))
	  (define-key xref-sb-menu [xref-sb-pop-and-ret] '("Pop Symbol and Return" . xref-pop-and-return))
	  (define-key xref-sb-menu [xref-sb-pop] '("Pop Symbol" . xref-pop-only))
	  (define-key xref-sb-menu [xref-sb-sep2] '("--"))
	  (define-key xref-sb-menu [xref-sb-current-sym-and-refs] '("Display Browser" . xref-show-browser))
	  (define-key xref-sb-menu [xref-sb-next] '("Next Reference" . xref-next-reference))
	  (define-key xref-sb-menu [xref-sb-previous] '("Previous Reference" . xref-previous-reference))
	  (define-key xref-sb-menu [xref-sb-sep1] '("--"))
	  (define-key xref-sb-menu [xref-sb-push-and-macro] '("Push Symbol and Apply Macro on All References" . xref-push-and-apply-macro))
	  (define-key xref-sb-menu [xref-sb-push-name] '("Push Symbol by Name" . xref-push-name))
	  (define-key xref-sb-menu [xref-sb-push-and-list] '("Push This Symbol and Display Browser" . xref-browse-symbol))
	  (define-key xref-sb-menu [xref-sb-push-and-go] '("Push This Symbol and Goto Definition" . xref-push-and-goto-definition))
	  (define-key xref-sb-menu [xref-sb-push] '("Push This Symbol" . xref-push-references))


	  (defvar xref-project-menu (make-sparse-keymap "Project"))
	  (fset 'xref-project-menu (symbol-value 'xref-project-menu))
	  (define-key xref-project-menu [xref-prj-edit] '("Edit Options" . 
													  xref-project-edit-options))
	  (define-key xref-project-menu [xref-prj-show-active] '("Show Active" . 
															 xref-project-active))
	  (define-key xref-project-menu [xref-prj-set-active] '("Set Active" . 
															xref-project-set-active))
	  (define-key xref-project-menu [xref-prj-del] '("Delete" . 
													 xref-project-delete))
	  (define-key xref-project-menu [xref-prj-new] '("New" . 
													 xref-project-new))

	  (defvar xref-dead-menu (make-sparse-keymap "Dead code detection"))
	  (fset 'xref-dead-menu (symbol-value 'xref-dead-menu))
	  (define-key xref-dead-menu [xref-dm-globals] '("Browse Project Unused Global Symbols" . xref-push-global-unused-symbols))
	  (define-key xref-dead-menu [xref-dm-locals] '("Browse File Unused Local Symbols" . xref-push-this-file-unused-symbols))

	  (defvar xref-misc-menu (make-sparse-keymap "Misc Menu"))
	  (fset 'xref-misc-menu (symbol-value 'xref-misc-menu))
	  (define-key xref-misc-menu [xref-register] '("Register Your Copy" . xref-registration))
	  (define-key xref-misc-menu [xref-about] '("About Xref" . xref-about))
	  (define-key xref-misc-menu [xref-help] '("Xref Help" . xref-help))
	  (define-key xref-misc-menu [xref-kill] '("Kill Xref Process" . xref-kill-xref-process))


	  (defvar xref-menu (make-sparse-keymap "Xrefactory"))
	  (if (commandp 'customize)
		  (define-key xref-menu [xref-global-options] '("Options" . xref-global-options))
		)
	  (define-key xref-menu [xref-misc-menu] '("Xref Misc" . xref-misc-menu))
	  (define-key xref-menu [separator-buffers2] '("--"))
	  (define-key xref-menu [(f7)] '("Delete One Window" . xref-delete-window))
	  (define-key xref-menu [separator-buffers5] '("--"))
	  (define-key xref-menu [xref-undo] '("Undo Last Refactoring" . xref-undo-last-refactoring))
	  ;;(define-key xref-menu [xref-set-target] '("Set Target for Next Move" . xref-set-moving-target-position))
	  (define-key xref-menu [xref-refactor] '("Refactor" . xref-refactor))
	  (define-key xref-menu [separator-buffers4] '("--"))
	  (define-key xref-menu [xref-togglesb] '("Toggle Smart Browsing Mode" . xref-toggle-smart-browsing-mode))
	  (define-key xref-menu [xref-class-tree] '("View Class (Sub)tree" . xref-class-tree-show))
	  (define-key xref-menu [xref-search-tag] '("Search Symbol" . xref-search-in-tag-file))
	  (define-key xref-menu [xref-search-def] '("Search Definition in Tags" . xref-search-definition-in-tag-file))
	  (define-key xref-menu [xref-dm-menu] '("Dead code detection" . xref-dead-menu))
	  (define-key xref-menu [xref-sb-menu] '("Browsing with Symbol Stack" . xref-sb-menu))
	  (define-key xref-menu [xref-lm-menu] '("Local Motion" . xref-lm-menu))
	  (define-key xref-menu [separator-buffers3] '("--"))
	  (define-key xref-menu [xref-gen-html] '("Generate HTML Documentation" . xref-gen-html-documentation))
	  (define-key xref-menu [xref-build-recipe] '("Create Recipe" . xref-build-recipe))
	  (define-key xref-menu [xref-fast-update-refs] '("Fast Update of Tags" . xref-fast-update-refs))
	  (define-key xref-menu [xref-update-refs] '("Full Update of Tags" . xref-update-refs))
	  (define-key xref-menu [xref-create-refs] '("Create Xref Tags" . xref-create-refs))
	  (define-key xref-menu [separator-buffers6] '("--"))
	  (define-key xref-menu [(f8)] '("Complete Identifier" . xref-completion))
	  (define-key xref-menu [xref-ide-menu] '("Emacs IDE" . xref-ide-menu))
	  (define-key xref-menu [xref-project-menu] '("Project" . xref-project-menu))
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                    XEmacs


(if (string-match "XEmacs" emacs-version)
	(progn
	  (defvar xref-xemacs-ide-menu
		'("XEmacs IDE"
		  ["Compile File" xref-ide-compile-file t]
		  ["Compile Directory" xref-ide-compile-dir t]
		  ["Compile Project" xref-ide-compile-project t]
		  ["(Last) Compile" xref-ide-compile t]
		  "-----"
		  ["Previous Error (or Alternative Reference)" xref-alternative-previous-reference t]
		  ["Next Error (or Alternative Reference)"		xref-alternative-next-reference t]
		  ;;["Next Error" xref-ide-next-error t]
		  ;;["Previous Error" xref-ide-previous-error t]
		  "-----"
		  ["Run1" xref-ide-run1 t]
		  ["Run2" xref-ide-run2 t]
		  ["Run3" xref-ide-run3 t]
		  ["Run4" xref-ide-run4 t]
		  ["Run5" xref-ide-run5 t]
		  ["Run This" xref-ide-run-this t]
		  ["(Last) Run" xref-ide-run t]
		  "-----"
		  ["(Last) Compile & Run" xref-ide-compile-run t]
		  ) "Xref interface to XEmacs IDE functions" 
		)

	  (defvar xref-xemacs-lm-menu
		'("Local Motion"
		  ["Previous Usage of This Symbol (or Alternatives)" xref-alternative-previous-reference t]
		  ["Next Usage of This Symbol (or Alternatives)"		xref-alternative-next-reference t]
		  ) "Xref local motion menu for XEmacs" 
		)

	  (defvar xref-xemacs-sb-menu
		'("Browsing with Symbol Stack"
		  ["Push This Symbol" xref-push-references t]
		  ["Push This Symbol and Goto Definition" xref-push-and-goto-definition t]
		  ["Push This Symbol and Display Browser" xref-browse-symbol t]
		  ["Push Symbol by Name" xref-push-name t]
		  ["Push Symbol and Apply Macro on All References" xref-push-and-apply-macro t]
		  "-----"
		  ["Previous Reference" xref-previous-reference t]
		  ["Next Reference" xref-next-reference t]
		  ["Display Browser" xref-show-browser t]
		  "-----"
		  ["Pop Symbol" xref-pop-only t]
		  ["Pop Symbol and Return" xref-pop-and-return t]
		  "-----"
		  ["Re-push Popped Symbol" xref-re-push t]
		  ) "Xref Source Browsing menu for XEmacs" 
		)

	  (defvar xref-xemacs-project-menu
		'("Project"
		  ["New"		        xref-project-new t]
		  ["Delete"	        xref-project-delete t]
		  ["Set Active"		xref-project-set-active t]
		  ["Show Active"		xref-project-active t]
		  ["Edit Options"		xref-project-edit-options t]
		  ) "Xref Project menu for XEmacs" 
		)

	  (defvar xref-dead-code-menu
		'("Dead Code Detection"
		  ["Browse File Unused Local Symbols"	  xref-push-this-file-unused-symbols t]
		  ["Browse Project Unused Global Symbols"      xref-push-global-unused-symbols t]
		  ) "Xref Dead code detection"
		)

	  (defvar xref-xemacs-misc-menu
		'("Xref Misc"
		  ["Kill Xref Process"				xref-kill-xref-process t]
		  ["About Xref"				    xref-about t]
		  ["Xref Help"				    	xref-help t]
		  ["Register Your Copy"	    	xref-registration t]
		  ) "Xref Miscellaneous Functions"
		)

	  (defvar xref-xemacs-menu
		'("Xrefactory"
		  ["Complete identifier"			xref-completion t]
		  "------"
		  ["Create Xref Tags"			xref-create-refs t]
		  ["Full Update of Tags"	    xref-update-refs t]
		  ["Fast Update of Tags"	    xref-fast-update-refs t]
		  ["Create Recipe"	        xref-build-recipe t]
		  ["Generate HTML Documentation"	xref-gen-html-documentation t]
		  "--"
		  ;; browsing menus will come here
		  ["Search Definition in Tags"	xref-search-definition-in-tag-file t]
		  ["Search Symbol"		xref-search-in-tag-file t]
		  ["View Class (Sub)tree"	        xref-class-tree-show t]
		  ["Toggle Smart Browsing Mode"	xref-toggle-smart-browsing-mode t]
		  "-----"
		  ["Refactor"    		            xref-refactor t]
		  ;;   ["Set Target for Next Move"    xref-set-moving-target-position t]
		  ["Undo Last Refactoring"		    xref-undo-last-refactoring t]
		  "----"
		  ["Delete One Window"		        xref-delete-window t]
		  "---"
		  ) "Xref menu for XEmacs" 
		)
))

(if (string-match "XEmacs" emacs-version)
	(progn
	;; XEmacs
		(defun xref-read-key-sequence (prompt) (read-key-sequence nil prompt))
		(if window-system 
			(progn 
			  (set-buffer (get-buffer-create " *dummytogetglobalmap*"))
			  (add-submenu nil xref-xemacs-menu)
			  (add-submenu '("Xrefactory") xref-xemacs-project-menu "Complete identifier")
			  (add-submenu '("Xrefactory") xref-xemacs-ide-menu     "Complete identifier")
			  (add-submenu '("Xrefactory") xref-xemacs-lm-menu      "Search Definition in Tags")
			  (add-submenu '("Xrefactory") xref-xemacs-sb-menu      "Search Definition in Tags")
			  (add-submenu '("Xrefactory") xref-dead-code-menu      "Search Definition in Tags")
			  (add-submenu '("Xrefactory") xref-xemacs-misc-menu nil)
			  (if (commandp 'customize)
				  (add-menu-button '("Xrefactory")
								   ["Options" xref-global-options t]
								   ))
			  (kill-buffer " *dummytogetglobalmap*")
			  )
		  )
		)
  ;; Emacs
  (defun xref-read-key-sequence (prompt) (read-key-sequence prompt))
  (let ((menu (lookup-key global-map [menu-bar])))
	(define-key-after menu [xref] 
	  (cons "Xrefactory" xref-menu)
	  (car (nth (- (length menu) 3) menu))
	  )
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                           Custom menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar xref-custom-menu-symbol-count 1)

(defun xref-emacs-add-menu-item (menu text expression)
  ;; first create a new function with expression as body
  (defalias (intern (format "xref-custom-menu-fun-%d" xref-custom-menu-symbol-count)) 
	(append '(lambda () "Xrefactory menu custom function" (interactive "")) (list expression)))
  
  ;; now put new menu item for this function
  (define-key menu 
	(vector (make-symbol (format "xref-custom-menu-sym-%d" xref-custom-menu-symbol-count)))
	(cons text (intern (format "xref-custom-menu-fun-%d" xref-custom-menu-symbol-count))))

  ;; increment counter
  (setq xref-custom-menu-symbol-count (1+ xref-custom-menu-symbol-count))
)

(defun xref-xemacs-add-menu-item (menu text expression after)
  (add-menu-button menu (vector text expression t) after)
)

(defun xref-add-custom-menu-item (text expression)
"Add an item to Xrefactory's 'Custom' submenu. If the 'Custom' submenu
does not  exist, create it.   TEXT is the  name of the  inserted item.
EXPRESSION is an  expression which will be evaluated  when the item is
selected.  For  example: (xref-add-custom-menu-item \"Call java2html\"
'(shell-command  (xref-get-env  \"java2html\")))  will  add  a   'Call
java2html' menu item, which will execute the shell command specified by
the 'java2html' xref environment variable. This variable should be set
by a '-set java2html <command>' option in your .xrefrc file.
"
  (if (string-match "XEmacs" emacs-version)
	  (progn
		(add-submenu '("Xrefactory") '("Custom") "------")
		(xref-xemacs-add-menu-item '("Xrefactory" "Custom") text expression 
								   "XEmacs IDE")
		)
	(if (not (boundp 'xref-custom-menu))
		(progn
		  (defvar xref-custom-menu (make-sparse-keymap "Xref Custom Menu"))
		  (fset 'xref-custom-menu (symbol-value 'xref-custom-menu))
		  (define-key-after xref-menu [xref-custom-menu] 
			(cons "Custom" xref-custom-menu) 
			(car (nth 3 xref-menu)))
		  ))
	(xref-emacs-add-menu-item xref-custom-menu text expression)
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  autoloads ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar xref-default-documentation-string "Documentation not yet available, please invoke an Xrefactory function first.")


(autoload 'xref-global-options "xref" xref-default-documentation-string t)
(autoload 'xref-project-new "xref" xref-default-documentation-string t)
(autoload 'xref-project-delete "xref" xref-default-documentation-string t)
(autoload 'xref-project-set-active "xref" xref-default-documentation-string t)
(autoload 'xref-project-active "xref" xref-default-documentation-string t)
(autoload 'xref-project-edit-options "xref" xref-default-documentation-string t)

(autoload 'xref-ide-compile-file "xref" xref-default-documentation-string t)
(autoload 'xref-ide-compile-dir "xref" xref-default-documentation-string t)
(autoload 'xref-ide-compile-project "xref" xref-default-documentation-string t)
(autoload 'xref-ide-compile "xref" xref-default-documentation-string t)
(autoload 'xref-ide-previous-error "xref" xref-default-documentation-string t)
(autoload 'xref-ide-run "xref" xref-default-documentation-string t)
(autoload 'xref-ide-run-this "xref" xref-default-documentation-string t)
(autoload 'xref-ide-run1 "xref" xref-default-documentation-string t)
(autoload 'xref-ide-run2 "xref" xref-default-documentation-string t)
(autoload 'xref-ide-run3 "xref" xref-default-documentation-string t)
(autoload 'xref-ide-run4 "xref" xref-default-documentation-string t)
(autoload 'xref-ide-run5 "xref" xref-default-documentation-string t)
(autoload 'xref-ide-compile-run "xref" xref-default-documentation-string t)

(autoload 'xref-build-recipe "xref" xref-default-documentation-string t)
(autoload 'xref-create-refs "xref" xref-default-documentation-string t)
(autoload 'xref-fast-update-refs "xref" xref-default-documentation-string t)
(autoload 'xref-update-refs "xref" xref-default-documentation-string t)
(autoload 'xref-gen-html-documentation "xref" xref-default-documentation-string t)

(autoload 'xref-alternative-previous-reference "xref" xref-default-documentation-string t)
(autoload 'xref-alternative-next-reference "xref" xref-default-documentation-string t)

(autoload 'xref-push-references "xref" xref-default-documentation-string t)
(autoload 'xref-push-and-goto-definition "xref" xref-default-documentation-string t)
(autoload 'xref-browse-symbol "xref" xref-default-documentation-string t)
(autoload 'xref-push-name "xref" xref-default-documentation-string t)
(autoload 'xref-push-and-apply-macro "xref" xref-default-documentation-string t)
(autoload 'xref-next-reference "xref" xref-default-documentation-string t)
(autoload 'xref-previous-reference "xref" xref-default-documentation-string t)
(autoload 'xref-pop-and-return "xref" xref-default-documentation-string t)
(autoload 'xref-pop-only "xref" xref-default-documentation-string t)
(autoload 'xref-show-browser "xref" xref-default-documentation-string t)
(autoload 'xref-re-push "xref" xref-default-documentation-string t)

(autoload 'xref-toggle-smart-browsing-mode "xref" xref-default-documentation-string t)
(autoload 'xref-search-in-tag-file "xref" xref-default-documentation-string t)
(autoload 'xref-search-definition-in-tag-file "xref" xref-default-documentation-string t)

(autoload 'xref-push-this-file-unused-symbols "xref" xref-default-documentation-string t)
(autoload 'xref-push-global-unused-symbols "xref" xref-default-documentation-string t)

(autoload 'xref-delete-window "xref" xref-default-documentation-string t)

(autoload 'xref-set-moving-target-position "xref" xref-default-documentation-string t)
(autoload 'xref-refactor "xref" xref-default-documentation-string t)
(autoload 'xref-undo-last-refactoring "xref" xref-default-documentation-string t)

(autoload 'xref-completion "xref" xref-default-documentation-string t)

(autoload 'xref-class-tree-show "xref" xref-default-documentation-string t)

(autoload 'xref-help "xref" xref-default-documentation-string t)
(autoload 'xref-about "xref" xref-default-documentation-string t)
(autoload 'xref-kill-xref-process "xref" xref-default-documentation-string t)
(autoload 'xref-registration "xref" xref-default-documentation-string t)

(autoload 'xref-find-file-on-mouse "xref" xref-default-documentation-string t)

;; this has to be kept, because of options
(autoload 'xref-soft-select-dispach-data-caller-window "xref" xref-default-documentation-string t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; examples on how to use 'xref-add-custom-menu-item function

;;(xref-add-custom-menu-item 
;; "Call Java2html" 
;; '(shell-command (xref-get-env "java2html"))
;;)
;;(xref-add-custom-menu-item 
;; "Gen HTML Documentation" 
;; '(xref-call-non-interactive-process xref-current-project "-html" 'assynchronized 
;;									 'newwin "Generating HTML. ")
;;)

