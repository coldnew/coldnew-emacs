;;; .loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (highlight-parentheses-mode) "highlight-parentheses/highlight-parentheses"
;;;;;;  "highlight-parentheses/highlight-parentheses.el" (19775 58312))
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

;;;### (autoloads (multi-term) "multi-term/multi-term" "multi-term/multi-term.el"
;;;;;;  (19776 57137))
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
;;;;;;  (19775 57987))
;;; Generated autoloads from rainbow-mode/rainbow-mode.el

(autoload 'rainbow-mode "rainbow-mode/rainbow-mode" "\
Colorize strings that represent colors.
This will fontify with colors the string like \"#aabbcc\" or \"blue\".

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("auto-complete-clang/auto-complete-clang.el"
;;;;;;  "auto-complete-extension/auto-complete-extension.el" "auto-complete/auto-complete-config.el"
;;;;;;  "auto-complete/auto-complete.el" "auto-complete/fuzzy.el"
;;;;;;  "auto-complete/popup.el" "pos-tip/pos-tip.el" "pymacs/pymacs.el")
;;;;;;  (19776 57138 516408))

;;;***

(provide '.loaddefs)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; .loaddefs.el ends here
