;;; .loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "doxymacs/lisp/doxymacs" "doxymacs/lisp/doxymacs.el"
;;;;;;  (19777 4986))
;;; Generated autoloads from doxymacs/lisp/doxymacs.el

(or (assoc 'doxymacs-mode minor-mode-alist) (setq minor-mode-alist (cons '(doxymacs-mode " doxy") minor-mode-alist)))

;;;***

;;;### (autoloads (xml-reformat-tags insert-xml read-xml) "doxymacs/lisp/xml-parse"
;;;;;;  "doxymacs/lisp/xml-parse.el" (19777 4977))
;;; Generated autoloads from doxymacs/lisp/xml-parse.el

(autoload 'read-xml "doxymacs/lisp/xml-parse" "\
Parse XML data at point into a Lisp structure.
See `insert-xml' for a description of the format of this structure.
Point is left at the end of the XML structure read.

\(fn &optional PROGRESS-CALLBACK)" nil nil)

(autoload 'insert-xml "doxymacs/lisp/xml-parse" "\
Insert DATA, a recursive Lisp structure, at point as XML.
DATA has the form:

  ENTRY       ::=  (TAG CHILD*)
  CHILD       ::=  STRING | ENTRY
  TAG         ::=  TAG_NAME | (TAG_NAME ATTR+)
  ATTR        ::=  (ATTR_NAME . ATTR_VALUE)
  TAG_NAME    ::=  STRING
  ATTR_NAME   ::=  STRING
  ATTR_VALUE  ::=  STRING

If ADD-NEWLINES is non-nil, newlines and indentation will be added to
make the data user-friendly.

If PUBLIC and SYSTEM are non-nil, a !DOCTYPE tag will be added at the
top of the document to identify it as an XML document.

DEPTH is normally for internal use only, and controls the depth of the
indentation.

\(fn DATA &optional ADD-NEWLINES PUBLIC SYSTEM DEPTH RET-DEPTH)" nil nil)

(autoload 'xml-reformat-tags "doxymacs/lisp/xml-parse" "\
If point is on the open bracket of an XML tag, reformat that tree.
Note that this only works if the opening tag starts at column 0.

\(fn)" t nil)

;;;***

;;;### (autoloads (highlight-parentheses-mode) "highlight-parentheses/highlight-parentheses"
;;;;;;  "highlight-parentheses/highlight-parentheses.el" (19777 341))
;;; Generated autoloads from highlight-parentheses/highlight-parentheses.el

(autoload 'highlight-parentheses-mode "highlight-parentheses/highlight-parentheses" "\
Minor mode to highlight the surrounding parentheses.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (turn-on-hungry-delete-mode hungry-delete-mode
;;;;;;  hungry-delete-backward hungry-delete-forward) "hungury-delete/hungry-delete"
;;;;;;  "hungury-delete/hungry-delete.el" (19775 63823))
;;; Generated autoloads from hungury-delete/hungry-delete.el

(autoload 'hungry-delete-forward "hungury-delete/hungry-delete" "\
Delete the following character or all following whitespace up
to the next non-whitespace character.  See
\\[c-hungry-delete-backward].

\(fn)" t nil)

(autoload 'hungry-delete-backward "hungury-delete/hungry-delete" "\
Delete the preceding character or all preceding whitespace
back to the previous non-whitespace character.  See also
\\[c-hungry-delete-forward].

\(fn)" t nil)

(autoload 'hungry-delete-mode "hungury-delete/hungry-delete" "\
Minor mode to enable hungry deletion.  This will delete all
whitespace after or before point when the deletion command is
executed.

\(fn &optional ARG)" t nil)

(autoload 'turn-on-hungry-delete-mode "hungury-delete/hungry-delete" "\
Turns on hungry delete mode if the buffer is appropriate.

\(fn)" nil nil)

;;;***

;;;### (autoloads (lusty-launch-dired lusty-select-current-name lusty-select-match
;;;;;;  lusty-open-this lusty-highlight-previous-column lusty-highlight-next-column
;;;;;;  lusty-highlight-previous lusty-highlight-next lusty-buffer-explorer
;;;;;;  lusty-file-explorer) "lusty-explorer/lusty-explorer" "lusty-explorer/lusty-explorer.el"
;;;;;;  (19777 341))
;;; Generated autoloads from lusty-explorer/lusty-explorer.el

(autoload 'lusty-file-explorer "lusty-explorer/lusty-explorer" "\
Launch the file/directory mode of LustyExplorer.

\(fn)" t nil)

(autoload 'lusty-buffer-explorer "lusty-explorer/lusty-explorer" "\
Launch the buffer mode of LustyExplorer.

\(fn)" t nil)

(autoload 'lusty-highlight-next "lusty-explorer/lusty-explorer" "\
Highlight the next match in *Lusty-Matches*.

\(fn)" t nil)

(autoload 'lusty-highlight-previous "lusty-explorer/lusty-explorer" "\
Highlight the previous match in *Lusty-Matches*.

\(fn)" t nil)

(autoload 'lusty-highlight-next-column "lusty-explorer/lusty-explorer" "\
Highlight the next column in *Lusty-Matches*.

\(fn)" t nil)

(autoload 'lusty-highlight-previous-column "lusty-explorer/lusty-explorer" "\
Highlight the previous column in *Lusty-Matches*.

\(fn)" t nil)

(autoload 'lusty-open-this "lusty-explorer/lusty-explorer" "\
Open the given file/directory/buffer, creating it if not already present.

\(fn)" t nil)

(autoload 'lusty-select-match "lusty-explorer/lusty-explorer" "\
Activate the highlighted match in *Lusty-Matches* - recurse if dir, open if file/buffer.

\(fn)" t nil)

(autoload 'lusty-select-current-name "lusty-explorer/lusty-explorer" "\
Open the given file/buffer or create a new buffer with the current name.

\(fn)" t nil)

(autoload 'lusty-launch-dired "lusty-explorer/lusty-explorer" "\
Launch dired at the current directory.

\(fn)" t nil)

;;;***

;;;### (autoloads (multi-term) "multi-term/multi-term" "multi-term/multi-term.el"
;;;;;;  (19777 341))
;;; Generated autoloads from multi-term/multi-term.el

(autoload 'multi-term "multi-term/multi-term" "\
Create new term buffer.
Will prompt you shell name when you type `C-u' before this command.

\(fn)" t nil)

;;;***

;;;### (autoloads (pylookup-update pylookup-lookup) "pylookup/pylookup"
;;;;;;  "pylookup/pylookup.el" (19775 58061))
;;; Generated autoloads from pylookup/pylookup.el

(autoload 'pylookup-lookup "pylookup/pylookup" "\
Lookup SEARCH-TERM in the Python HTML indexes.

\(fn SEARCH-TERM)" t nil)

(autoload 'pylookup-update "pylookup/pylookup" "\
Run pylookup-update and create the database at `pylookup-db-file'.

\(fn SRC)" t nil)

;;;***

;;;### (autoloads (rainbow-mode) "rainbow-mode/rainbow-mode" "rainbow-mode/rainbow-mode.el"
;;;;;;  (19777 341))
;;; Generated autoloads from rainbow-mode/rainbow-mode.el

(autoload 'rainbow-mode "rainbow-mode/rainbow-mode" "\
Colorize strings that represent colors.
This will fontify with colors the string like \"#aabbcc\" or \"blue\".

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("pymacs/pymacs.el" "ssh-config/ssh-config.el")
;;;;;;  (19777 4989 530545))

;;;***

(provide '.loaddefs)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; .loaddefs.el ends here
