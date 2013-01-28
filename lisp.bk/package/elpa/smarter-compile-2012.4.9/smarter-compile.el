;;; smarter-compile.el --- a smarter wrapper for `compile'

;; Copyright (C) 1998-2011  by Seiji Zenitani

;; Author: Dino Chiesa <dpchiesa@hotmail.com, Seiji Zenitani <zenitani@mac.com>
;; $Id$
;; Keywords: tools, unix
;; Created: 1998-12-27
;; Version: 2012.4.9
;; Compatibility: Emacs 23 or later
;; URL: http://cheeso.members.winisp.net/srcview.aspx?dir=emacs&file=smarter-compile.el


;; origin: http://homepage.mac.com/zenitani/comp-e.html
;; URL(jp): http://homepage.mac.com/zenitani/elisp-j.html#smart-compile

;; Contributors: Sakito Hisakura

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package provides `smarter-compile' function.

;; It acts as a wrapper for `compile', providing a way to
;; intelligently set the `compile-command' and then invoke
;; `compile' as normal.

;; It chooses a compile command, and suggests it by invoking `compile'
;; interactively. It makes the choice according to the name of the
;; current buffer, and the items - think of them as rules - in the
;; `smart-compile-alist'. By default, it performs the selection logic
;; just once per buffer. When you run `smarter-compile', it sets a
;; buffer-local flag and does not walk the rules again.  If you'd like
;; to force to walk through the rules, use C-u M-x smarter-compile.

;; The built-in rules are like this:
;;
;;  - If there is a makefile in the current directory, ask if the user
;;    wants to run make, and if so, suggest make.

;;  - If there is a Msbuild project file in the current directory,
;;    suggest msbuild.exe .

;;  - if there is a comment in the header like compile: <...>, and
;;    the file has an extension that is a member of
;;    `smart-compile-compile-command-in-comments-extension-list', then
;;    use that string that as the compile command.

;;  - if editing a makefile, suggest make.

;;  - if the file extension is .cs, .vb, .c, .java, and so on...
;;    use a module-specific build command.

;; To use this package, add this line to your .emacs file:
;;     (require 'smart-compile)

;; To tweak it, set `smart-compile-alist'. This variable holds the rules
;; for guessing the compile command.  It is a list of cons cells, each
;; one like (TEST . COMMAND). In the simplest case, the TEST is a regex
;; that gets applied to the filename, and COMMAND is a string used for
;; the `compile-command' when the TEST succeeds.  You can either setq
;; the list, which would replacing the built-in rules completely, or you
;; can append items to the default list, thereby extending the built-in
;; behavior of `smarter-compile.el'. Keep in mind that the order of
;; evaluation of the rules is significant.

;; The simplest kind of extension of the rules looks like this:

;; (require 'smarter-compile)
;; (add-to-list
;;   'smart-compile-alist
;;   '("\\.css\\'"   .   "/bin/csslint.js --format=compiler %f"))

;;; More Details:
;;
;; That example uses a string for both the TEST and the COMMAND.
;; But there are other options. TEST can be:
;;
;;   - a string. In this case it is used as a regex,
;;     and test against the filename associated to the
;;     buffer. The TEST succeeds when the regex matches.
;;
;;   - a symbol, representing the major-mode. In this case
;;     if the buffer uses that major mode, the TEST
;;     succeeds.
;;
;;   - a symbol, representing any function with a name not
;;     ending in \"-mode\". In this case, the function is
;;     called and if it returns non-nil, the TEST
;;     succeeds.
;;
;;   - a list of forms. In this case the forms are eval'd,
;;     and if the return value is non-nil, the TEST
;;     succeeds.
;;
;; COMMAND can be:
;;
;;   - a string. This is used as a template to generate the
;;     actual shell command. `smarter-compile` will replace these
;;     opcodes in the string.
;;
;;       %M  make program                 ( nmake.exe or make )
;;       %F  absolute pathname            ( /usr/home/dev/Module.c )
;;       %f  file name without directory  ( Module.c )
;;       %n  file name without extension  ( Module )
;;       %e  extension of file name       ( c )
;;       %t  current time in 24-hr format ( 12:44:18 )
;;       %d  current date                 ( 2012-Mar-20 )
;;
;;     `smart-compile.el' sets `compile-command' to the string with
;;     all the replacements.
;;
;;     With no opcodes, the COMMAND string is used directly
;;     as `compile-command'.
;;
;;   - a symbol, representing a function. In this case the
;;     return value of the function should be a string. It is
;;     expanded as described above, and the expansion is then
;;     used as the `compile-command'.
;;
;;   - a list of forms. In this case the list is eval'd, and
;;     the return value should be a string. It is
;;     expanded as described above, and the expansion is then
;;     used as the `compile-command'.
;;
;; Again, when extending, remember that the ordering of the rules is
;; important. For example, the default ruleset in smarter-compile
;; includes a test that returns true if a makefile is present in the
;; current directory.  There is also a rule that returns true if the
;; file extension is .c.  When both conditions are true, the order of
;; these rules in the list determines which of the rules will apply.
;;
;; Rather than adding new rules, you may wish to replace existing rules
;; in `smart-compile-alist'.  There's a convenience function for this
;; purpose. It inserts or replaces as appropriate:
;;
;; (require 'smarter-compile)
;; (smart-compile-insert-simple-rule
;;   "\\.css\\'"  "/bin/csslint.js --format=compiler %f")
;;
;; `smart-compile-insert-simple-rule' works best when the test is a
;; string.
;;


;;; Code:

(require 'files)

(defgroup smarter-compile nil
  "An interface to `compile'."
  :group 'processes
  :prefix "smarter-compile")

(defcustom smart-compile-alist
  '(
    ("[Mm]akefile\\'"   . smart-compile-get-make-program)

    (smart-compile-consider-makefile . nil)
    (smart-compile-consider-projfile . "msbuild.exe ")
    (smart-compile-command-in-header-comments . nil)

    ("\\.cs\\'"         . "csc /t:exe /debug+ %f")
    ("\\.vb\\'"         . "vbc /t:exe /debug+ %f")

    (emacs-lisp-mode    . (emacs-lisp-byte-compile))
    (html-mode          . (browse-url-of-buffer))
    (nxhtml-mode        . (browse-url-of-buffer))
    (html-helper-mode   . (browse-url-of-buffer))
    (octave-mode        . (run-octave))

    ("\\.c\\'"          . "gcc -O2 %f -lm -o %n")
    ;;  ("\\.c\\'"          . "gcc -O2 %f -lm -o %n && ./%n")
    ("\\.[Cc]+[Pp]*\\'" . "g++ -O2 %f -lm -o %n")
    ("\\.m\\'"          . "gcc -O2 %f -lobjc -lpthread -o %n")
    ("\\.java\\'"       . "javac %f")
    ("\\.php\\'"        . "php -l %f")
    ("\\.f90\\'"        . "gfortran %f -o %n")
    ("\\.[Ff]\\'"       . "gfortran %f -o %n")
    ("\\.cron\\(tab\\)?\\'" . "crontab %f")
    ("\\.tex\\'"        . (tex-file))
    ("\\.texi\\'"       . "makeinfo %f")
    ("\\.mp\\'"         . "mptopdf %f")
    ("\\.pl\\'"         . "perl -cw %f")
    ("\\.rb\\'"         . "ruby -cw %f")
    )

  "List of cons cells, each one (TEST . COMMAND).

COMMAND is used for the `compile-command' when
the TEST succeeds.

Test can be:

  - a string. In this case it is used as a regex,
    and matched against the filename associated to the
    buffer. The TEST succeeds when the regex matches.

  - a symbol, representing the major-mode. In this case
    if the buffer uses that major mode, the TEST
    succeeds.

  - a symbol, representing any function with a name not
    ending in \"-mode\". In this case, the function is
    called and if it returns non-nil, the TEST
    succeeds.

  - a list of forms.  In this case the forms are eval'd,
    and if the return value is non-nil, the TEST
    succeeds.

The ordering of tests is important. The first test that
succeeds will be the one used for a particular buffer.

COMMAND can be:

  - a string. This is used as a template to generate the
    actual shell command. These opcodes will be replaced
    in the string.

      %M  make program                 ( nmake.exe or make )
      %F  absolute pathname            ( /usr/local/bin/Module.c )
      %f  file name without directory  ( Module.c )
      %n  file name without extension  ( Module )
      %e  extension of file name       ( c )
      %t  current time in 24-hr format ( 12:44:18 )
      %d  current date                 ( 2012-Mar-20 )

    `smart-compile.el' sets `compile-command' to the string with
    all the replacements.

    With no opcodes, the COMMAND string is used directly
    as `compile-command'.

  - nil. In this case the result of the TEST is used for
    the compile command. This makes sense when the TEST
    is a function or list of forms.

  - a symbol, representing a function. In this case the
    return value of the function should be a string. It is
    expanded as described above, and the expansion is then
    used as the `compile-command'.

  - a list of forms. In this case the list is eval'd, and
    the return value should be a string. It is
    expanded as described above, and the expansion is then
    used as the `compile-command'.
"
   :type '(repeat
           (cons
            (choice
             (regexp :tag "Filename pattern")
             (function :tag "Major-mode"))
            (choice
             (string :tag "Compilation command")
             (sexp :tag "Lisp expression"))))
   :group 'smarter-compile)

(put 'smart-compile-alist 'risky-local-variable t)

(defconst smart-compile-expando-alist '(
  ("%M" . (smart-compile-get-make-program))
  ("%t" . (format-time-string "%H:%M:%S"))
  ("%d" . (format-time-string "%y-%b-%d"))
  ("%F" . (buffer-file-name))
  ("%f" . (file-name-nondirectory (buffer-file-name)))
  ("%n" . (file-name-sans-extension
           (file-name-nondirectory (buffer-file-name))))
  ("%e" . (or (file-name-extension (buffer-file-name)) ""))
  ))
(put 'smart-compile-expando-alist 'risky-local-variable t)


(defvar smart-compile-wisdom-has-been-bestowed nil
  "Indicates whether smart-compile has made a suggestion
for the current buffer.")


(defvar smart-compile-compile-command-in-comments-extension-list
  '(".c" ".cs" ".vb" ".VB" ".js" )

"The list of extensions of files for which `smarter-compile' will
look in the header comments for a specification for `compile-command'.
The specification might look like this:

  compile: gcc -O2 %f -lm -o %n

Evertything after the \"compile:\" is used as the suggested
`compile-command', after replacing the various expando variables
in the string. You can specify a fully-qualified path for the
command, or no qualification to search the system PATH.

These expando variables are available:

      %M  make program                 ( make or nmake.exe )
      %F  absolute pathname            ( /user/fred/proj/Module.c )
      %f  file name without directory  ( Module.c )
      %n  file name without extension  ( Module )
      %e  extension of file name       ( c )
      %t  current time in 24-hr format ( 12:44:18 )
      %d  current date                 ( 2012-Mar-20 )

 ")


(defcustom smart-compile-make-program nil
  "The command by which to invoke the make program."
  :type 'string
  :group 'smarter-compile)

(if (not (fboundp 'string/ends-with))
    (defun string/ends-with (s ending)
      "return non-nil if string S ends with ENDING"
      (let ((elength (length ending)))
        (string= (substring s (- 0 elength)) ending))))


(defcustom smart-compile-comment-cmd-line-limit 64
  "The number of lines in the file to scan, for a comment that
specifies the compile command. See also
`smart-compile-compile-command-in-comments-extension-list'"
  :type 'integer
  :group 'smarter-compile)


(defun smart-compile-consider-makefile ()
  "function to be used as a test in the `smart-compile-alist',
to test presence of a makefile.
"
  (and (or (file-readable-p "Makefile")
           (file-readable-p "makefile"))
       (y-or-n-p "Makefile is found. Try 'make'? ")
       (smart-compile-get-make-program)))


(defun smart-compile-insert-simple-rule (regex new-command)
  "Insert a simple rule into the `smart-compile-alist', or replace
the command in an existing rule with a new one.

This is just a convenience function to make it easier to
manipulate the alist. This fn finds the rule with the given REGEX
string, and sets the command for that rule to the NEW-COMMAND,
which may be a string, a symbol-name referring to a function, or
a list of forms.

The regexi used in tests in `smart-compile-alist' which match on
file extensions generally follow the convention that they begin
with a dot and end in \\'.  Therefore to replace the rule for .c
files, you need to do something like this:

  (smart-compile-insert-simple-rule \"\\.c\\'\"
      \"cl.exe /Od  /Zi /DEBUG /RTC1 /MTd /W3 /c %f\")

"
  (let (entry-of-interest (assoc regex smart-compile-alist)))
    (if entry-of-interest
        (setcdr entry-of-interest new-command)
      (add-to-list
       'smart-compile-alist
       (cons regex new-command))))


(defun smart-compile-get-value-from-comments (marker-string &optional line-limit)
  "Gets a string from the header comments in the current buffer.

This is used to extract the compile command from the comments.

It looks for MARKER-STRING, followed by a colon, in one of the
first LINE-LIMIT lines in te buffer. If found, it returns the
string that follows it, or returns nil if that string is not
found.  The default LINE-LIMIT is
`smart-compile-comment-cmd-line-limit'.


For example, suppose the following string is found at the top of
the buffer:

     flymake: csc.exe /r:Hallo.dll

...then invoking this function with MARKER-STRING as \"flymake\" will
return

     \"csc.exe /r:Hallo.dll\"

It's ok to have whitespace between the marker and the following
colon.

"
  (let ((line-limit (or line-limit smart-compile-comment-cmd-line-limit))
        start search-limit found)
    ;; determine what lines to look in
    (save-excursion
      (save-restriction
        (widen)
        (cond ((> line-limit 0)
               (goto-char (setq start (point-min)))
               (forward-line line-limit)
               (setq search-limit (point)))
              ((< line-limit 0)
               (goto-char (setq search-limit (point-max)))
               (forward-line line-limit)
               (setq start (point)))
              (t                        ;0 => no limit (use with care!)
               (setq start (point-min))
               (setq search-limit (point-max))))))

    ;; look in those lines
    (save-excursion
      (save-restriction
        (widen)
        (let ((re-string
               (concat "\\b" marker-string "[ \t]*:[ \t]*\\(.+\\)$")))
          (if (and start
                   (< (goto-char start) search-limit)
                   (re-search-forward re-string search-limit 'move))

              (buffer-substring-no-properties
               (match-beginning 1)
               (match-end 1))))))))



(defun smart-compile-any (predicate sequence)
  "Return true if PREDICATE is true of any element of SEQUENCE.
Otherwise nil.

If non-nil, the actual value will be a list, the car of which is
the first element in the sequence to return a non-nil result from
PREDICATE.

"
  (while (and sequence (not (funcall predicate (car sequence))))
    (setf sequence (cdr sequence)))
  sequence)



;; (defun smart-compile-buffer-file-ends-with (ext)
;;   "Return non-nil if the filename associated to the buffer
;; ends with EXT.
;; "
;;   (string-equal (substring buffer-file-name (- 0 (length ext))) ext))


(defun smart-compile-command-in-header-comments ()
  "function to be used as a test in the `smart-compile-alist'.
If the file is a c-language-family module, and there's a
comment that specifies the compile command, then it returns non-nil.
"
  (and
   (stringp buffer-file-name)

   (member (file-name-extension buffer-file-name t)
         smart-compile-compile-command-in-comments-extension-list)

   (smart-compile-get-value-from-comments
    "compile")))


(defun smart-compile-consider-projfile ()
  "function to be used as a test in the `smart-compile-alist',
to test presence of a msbuild project file.
"
   (smart-compile-any
    '(lambda (elt) (file-expand-wildcards elt t))
    '("*.csproj" "*.vcproj" "*.vbproj" "*.wixproj" "*.shfbproj" "*.sln")))


(defun smart-compile-get-make-program ()
  "intelligently get make command."
  (or smart-compile-make-program
      (setq smart-compile-make-program
            (if (eq system-type 'windows-nt)
                "nmake.exe " "make "))))


(defun smart-compile-expand-compile-command (cmd)
  "Given a string, CMD, expand the opcodes within it
to produce a command string suitable for `shell-command'.
This expands %F to the fully qualified file name, and so on.
See the doc for `smart-compile-alist' for details.
 "
  (if (and (boundp 'buffer-file-name)
           (stringp buffer-file-name))
      (let ((rlist smart-compile-expando-alist)
            (case-fold-search nil))
        (while rlist
          (while (string-match (caar rlist) cmd)
            (setq cmd
                  (replace-match
                   (eval (cdar rlist)) t nil cmd)))
          (setq rlist (cdr rlist)))))
  cmd)


(defun smart-compile-choose-alist-item (cmd)
  "Use the given CMD for compiling. It could be a
string, which means to use that as a shell command. It
could be a list of forms, which means to eval that list to get the string.
It could also be a symbol name bound to a function, which means
invoke that fn to get the string. In all cases the string is expanded through
`smart-compile-expand-compile-command'
 "
  (set (make-local-variable 'compile-command)
       (smart-compile-expand-compile-command
        (cond
         ((stringp cmd)
          cmd)

         ((listp cmd)
          (eval cmd))

         ((and (symbolp cmd) (fboundp cmd))
          (funcall cmd))

         (t
          (smart-compile-get-make-program))))))


;;;###autoload
(defun smart-compile-select-compile-command ()
  "The function that selects the `compile-command' for
a buffer, given the `smart-compile-alist'.

This is not to be called interactively.

It does not call `compile'.

"
  (let ((name (buffer-file-name))
        (not-done t))

    (if (not name)
        (error "cannot get filename."))

    ;; walk the list
    (let ((alist smart-compile-alist)
          (case-fold-search nil)
          (function nil))

      (while (and alist not-done)
        (let* ((item (car alist))
               (test (car item))
               (cmd (cdr item)))
          (if (or

               (and (symbolp test)
                    (fboundp test)
                    (string/ends-with (symbol-name test) "-mode")
                    (eq test major-mode))

               (and (symbolp test)
                    (fboundp test)
                    (not (string/ends-with (symbol-name test) "-mode"))
                    (funcall test))

               (and (stringp test)
                    (string-match test name))

               (and (not (symbolp test))
                    (not (stringp test))
                    (list test)
                    (eval test)))

              (progn
                (smart-compile-choose-alist-item (or cmd test))
                (setq not-done nil))))

        (setq alist (cdr alist))))

    ;; If compile-command is not yet defined and the contents begins
    ;; with "#!", set compile-command to filename.
    (if (and not-done
             (not (memq system-type '(windows-nt ms-dos)))
             (not (string-match "/\\.[^/]+$" name))
             (not
              (and (local-variable-p 'compile-command)
                   compile-command)))
        (save-restriction
          (widen)
          (if (equal "#!" (buffer-substring 1 (min 3 (point-max))))
              (set (make-local-variable 'compile-command) name)
            )))
    ))



(defun smarter-compile (&optional arg)
  "An interactive function to wrap the `compile' function.
This simply checks to see if `compile-command' has been
previously set by smart-compile, and if not, invokes
`smart-compile-select-compile-command' to set the value.  Then it
invokes the `compile' function, interactively.

A good way to do things is to bind this function to \C-x\C-e.

    (global-set-key \"\C-x\C-e\"  'smarter-compile)

"
  (interactive "p")

  ;; conditionally apply smartness
  (when
      (or
       ;; (C-u M-x smarter-compile) results in a prefix of 4.
       ;; Use that as a convenience to force smarter-compile to
       ;; re-select the compile-command.
       (and (numberp arg)
            (= arg 4)) ; C-u M-x smart-compile

       (not (local-variable-p 'compile-command))

       (not (boundp 'smart-compile-wisdom-has-been-bestowed)))

    (smart-compile-select-compile-command)
    (set (make-local-variable 'smart-compile-wisdom-has-been-bestowed) t))

  ;; local compile command has now been set
  (call-interactively 'compile))


(provide 'smarter-compile)

;;; smarter-compile.el ends here
