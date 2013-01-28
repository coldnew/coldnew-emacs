;;; guess-style-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (guess-style-guess-all guess-style-guess-variable
;;;;;;  guess-style-set-variable) "guess-style" "guess-style.el"
;;;;;;  (20610 11482))
;;; Generated autoloads from guess-style.el

(autoload 'guess-style-set-variable "guess-style" "\
Override VARIABLE's guessed value for future guesses.
If FILE is a directory, the variable will be overridden for the entire
directory, unless single files are later overridden.
If called interactively, the current buffer's file name will be used for FILE.
With a prefix argument a directory name may be entered.

\(fn VARIABLE VALUE FILE)" t nil)

(autoload 'guess-style-guess-variable "guess-style" "\
Guess a value for VARIABLE according to `guess-style-guesser-alist'.
If GUESSER is set, it's used instead of the default.

\(fn VARIABLE &optional GUESSER)" nil nil)

(autoload 'guess-style-guess-all "guess-style" "\
Guess all variables in `guess-style-guesser-alist'.
Special care is taken so no guesser is called twice.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("guess-style-pkg.el") (20610 11482 391240))

;;;***

(provide 'guess-style-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; guess-style-autoloads.el ends here
