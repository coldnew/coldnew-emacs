;;; .loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (anything-other-buffer anything-at-point anything)
;;;;;;  "anything-config/anything" "anything-config/anything.el"
;;;;;;  (19784 16676))
;;; Generated autoloads from anything-config/anything.el

(autoload 'anything "anything-config/anything" "\
Select anything. In Lisp program, some optional arguments can be used.

PLIST is a list like (:key1 val1 :key2 val2 ...) or
 (&optional sources input prompt resume preselect buffer keymap).

Basic keywords are the following:

- :sources

  Temporary value of `anything-sources'.  It also accepts a
  symbol, interpreted as a variable of an anything source.  It
  also accepts an alist representing an anything source, which is
  detected by (assq 'name ANY-SOURCES)

- :input

  Temporary value of `anything-pattern', ie. initial input of minibuffer.

- :prompt

  Prompt other than \"pattern: \".

- :resume

  If t, Resurrect previously instance of `anything'. Skip the initialization.
  If 'noresume, this instance of `anything' cannot be resumed.

- :preselect

  Initially selected candidate. Specified by exact candidate or a regexp.
  Note that it is not working with delayed sources.

- :buffer

  `anything-buffer' instead of *anything*.

- :keymap

  `anything-map' for current `anything' session.


Of course, conventional arguments are supported, the two are same.

 (anything :sources sources :input input :prompt prompt :resume resume
	   :preselect preselect :buffer buffer :keymap keymap)
 (anything sources input prompt resume preselect buffer keymap)


Other keywords are interpreted as local variables of this anything session.
The `anything-' prefix can be omitted. For example,

 (anything :sources 'anything-c-source-buffers
	   :buffer \"*buffers*\" :candidate-number-limit 10)

means starting anything session with `anything-c-source-buffers'
source in *buffers* buffer and set
`anything-candidate-number-limit' to 10 as session local variable.

\(fn &rest PLIST)" t nil)

(autoload 'anything-at-point "anything-config/anything" "\
Same as `anything' except when C-u is pressed, the initial input is the symbol at point.

\(fn &optional ANY-SOURCES ANY-INPUT ANY-PROMPT ANY-RESUME ANY-PRESELECT ANY-BUFFER)" t nil)

(autoload 'anything-other-buffer "anything-config/anything" "\
Simplified interface of `anything' with other `anything-buffer'

\(fn ANY-SOURCES ANY-BUFFER)" nil nil)

;;;***

;;;### (autoloads (anything-c-set-variable anything-c-call-interactively
;;;;;;  w32-shell-execute-open-file anything-ratpoison-commands anything-c-run-external-command
;;;;;;  anything-c-shell-command-if-needed anything-apt anything-world-time
;;;;;;  anything-select-xfont anything-top anything-create anything-execute-anything-command
;;;;;;  anything-call-source anything-surfraw anything-eval-expression-with-eldoc
;;;;;;  anything-eval-expression anything-yaoddmuse-emacswiki-post-library
;;;;;;  anything-yaoddmuse-emacswiki-edit-or-view anything-yaoddmuse-cache-pages
;;;;;;  anything-all-mark-rings anything-global-mark-ring anything-mark-ring
;;;;;;  anything-simple-call-tree anything-bookmark-ext anything-manage-advice
;;;;;;  anything-M-x anything-filelist+ anything-filelist anything-yank-text-at-point
;;;;;;  anything-c-goto-next-file anything-c-goto-precedent-file
;;;;;;  anything-do-grep anything-dired-bindings anything-dired-hardlink-file
;;;;;;  anything-dired-symlink-file anything-dired-copy-file anything-dired-rename-file
;;;;;;  anything-insert-file anything-write-file anything-find-files
;;;;;;  anything-regexp anything-kill-buffers anything-org-headlines
;;;;;;  anything-browse-code anything-occur anything-list-emacs-process
;;;;;;  anything-timers anything-bm-list anything-eev-anchors anything-emms
;;;;;;  anything-org-keywords anything-man-woman anything-register
;;;;;;  anything-c-insert-latex-math anything-c-pp-bookmarks anything-bookmarks
;;;;;;  anything-colors anything-firefox-bookmarks anything-w3m-bookmarks
;;;;;;  anything-locate anything-bbdb anything-buffers+ anything-for-buffers
;;;;;;  anything-yahoo-suggest anything-google-suggest anything-imenu
;;;;;;  anything-gentoo anything-minibuffer-history anything-show-kill-ring
;;;;;;  anything-info-emacs anything-info-at-point anything-recentf
;;;;;;  anything-for-files anything-mini anything-configuration)
;;;;;;  "anything-config/anything-config" "anything-config/anything-config.el"
;;;;;;  (19784 16676))
;;; Generated autoloads from anything-config/anything-config.el

(autoload 'anything-configuration "anything-config/anything-config" "\
Customize `anything'.

\(fn)" t nil)

(defvar anything-command-map)

(autoload 'anything-mini "anything-config/anything-config" "\
Preconfigured `anything' lightweight version (buffer -> recentf).

\(fn)" t nil)

(autoload 'anything-for-files "anything-config/anything-config" "\
Preconfigured `anything' for opening files.
ffap -> recentf -> buffer -> bookmark -> file-cache -> files-in-current-dir -> locate

\(fn)" t nil)

(autoload 'anything-recentf "anything-config/anything-config" "\
Preconfigured `anything' for `recentf'.

\(fn)" t nil)

(autoload 'anything-info-at-point "anything-config/anything-config" "\
Preconfigured `anything' for searching info at point.

\(fn)" t nil)

(autoload 'anything-info-emacs "anything-config/anything-config" "\
Preconfigured anything for Emacs manual index.

\(fn)" t nil)

(autoload 'anything-show-kill-ring "anything-config/anything-config" "\
Preconfigured `anything' for `kill-ring'. It is drop-in replacement of `yank-pop'.
You may bind this command to M-y.

\(fn)" t nil)

(autoload 'anything-minibuffer-history "anything-config/anything-config" "\
Preconfigured `anything' for `minibuffer-history'.

\(fn)" t nil)

(autoload 'anything-gentoo "anything-config/anything-config" "\
Preconfigured `anything' for gentoo linux.

\(fn)" t nil)

(autoload 'anything-imenu "anything-config/anything-config" "\
Preconfigured `anything' for `imenu'.

\(fn)" t nil)

(autoload 'anything-google-suggest "anything-config/anything-config" "\
Preconfigured `anything' for google search with google suggest.

\(fn)" t nil)

(autoload 'anything-yahoo-suggest "anything-config/anything-config" "\
Preconfigured `anything' for Yahoo searching with Yahoo suggest.

\(fn)" t nil)

(autoload 'anything-for-buffers "anything-config/anything-config" "\
Preconfigured `anything' for buffer.

\(fn)" t nil)

(autoload 'anything-buffers+ "anything-config/anything-config" "\
Enhanced preconfigured `anything' for buffer.

\(fn)" t nil)

(autoload 'anything-bbdb "anything-config/anything-config" "\
Preconfigured `anything' for BBDB.

Needs BBDB.

http://bbdb.sourceforge.net/

\(fn)" t nil)

(autoload 'anything-locate "anything-config/anything-config" "\
Preconfigured `anything' for Locate.
Note you can add locate command after entering pattern.
See man locate for more infos.

\(fn)" t nil)

(autoload 'anything-w3m-bookmarks "anything-config/anything-config" "\
Preconfigured `anything' for w3m bookmark.

Needs w3m and emacs-w3m.

http://w3m.sourceforge.net/
http://emacs-w3m.namazu.org/

\(fn)" t nil)

(autoload 'anything-firefox-bookmarks "anything-config/anything-config" "\
Preconfigured `anything' for firefox bookmark.
You will have to enable html bookmarks in firefox:
open about:config in firefox and double click on this line to enable value to true:

user_pref(\"browser.bookmarks.autoExportHTML\", false);

You should have now:

user_pref(\"browser.bookmarks.autoExportHTML\", true);

After closing firefox, you will be able to browse you bookmarks.

\(fn)" t nil)

(autoload 'anything-colors "anything-config/anything-config" "\
Preconfigured `anything' for color.

\(fn)" t nil)

(autoload 'anything-bookmarks "anything-config/anything-config" "\
Preconfigured `anything' for bookmarks.

\(fn)" t nil)

(autoload 'anything-c-pp-bookmarks "anything-config/anything-config" "\
Preconfigured `anything' for bookmarks (pretty-printed).

\(fn)" t nil)

(autoload 'anything-c-insert-latex-math "anything-config/anything-config" "\
Preconfigured anything for latex math symbols completion.

\(fn)" t nil)

(autoload 'anything-register "anything-config/anything-config" "\
Preconfigured `anything' for Emacs registers.

\(fn)" t nil)

(autoload 'anything-man-woman "anything-config/anything-config" "\
Preconfigured `anything' for Man and Woman pages.

\(fn)" t nil)

(autoload 'anything-org-keywords "anything-config/anything-config" "\
Preconfigured `anything' for org keywords.

\(fn)" t nil)

(autoload 'anything-emms "anything-config/anything-config" "\
Preconfigured `anything' for emms sources.

\(fn)" t nil)

(autoload 'anything-eev-anchors "anything-config/anything-config" "\
Preconfigured `anything' for eev anchors.

\(fn)" t nil)

(autoload 'anything-bm-list "anything-config/anything-config" "\
Preconfigured `anything' for visible bookmarks.

Needs bm.el

http://cvs.savannah.gnu.org/viewvc/*checkout*/bm/bm/bm.el

\(fn)" t nil)

(autoload 'anything-timers "anything-config/anything-config" "\
Preconfigured `anything' for timers.

\(fn)" t nil)

(autoload 'anything-list-emacs-process "anything-config/anything-config" "\
Preconfigured `anything' for emacs process.

\(fn)" t nil)

(autoload 'anything-occur "anything-config/anything-config" "\
Preconfigured Anything for Occur source.

\(fn)" t nil)

(autoload 'anything-browse-code "anything-config/anything-config" "\
Preconfigured anything to browse code.

\(fn)" t nil)

(autoload 'anything-org-headlines "anything-config/anything-config" "\
Preconfigured anything to show org headlines.

\(fn)" t nil)

(autoload 'anything-kill-buffers "anything-config/anything-config" "\
Preconfigured `anything' to kill buffer you selected.

\(fn)" t nil)

(autoload 'anything-regexp "anything-config/anything-config" "\
Preconfigured anything to build regexps and run query-replace-regexp against.

\(fn)" t nil)

(autoload 'anything-find-files "anything-config/anything-config" "\
Preconfigured `anything' for anything implementation of `find-file'.
Called with a prefix arg show history if some.
Don't call it from programs, use `anything-find-files1' instead.
This is the starting point for nearly all actions you can do on files.

\(fn)" t nil)

(autoload 'anything-write-file "anything-config/anything-config" "\
Preconfigured `anything' providing completion for `write-file'.

\(fn)" t nil)

(autoload 'anything-insert-file "anything-config/anything-config" "\
Preconfigured `anything' providing completion for `insert-file'.

\(fn)" t nil)

(autoload 'anything-dired-rename-file "anything-config/anything-config" "\
Preconfigured `anything' to rename files from dired.

\(fn)" t nil)

(autoload 'anything-dired-copy-file "anything-config/anything-config" "\
Preconfigured `anything' to copy files from dired.

\(fn)" t nil)

(autoload 'anything-dired-symlink-file "anything-config/anything-config" "\
Preconfigured `anything' to symlink files from dired.

\(fn)" t nil)

(autoload 'anything-dired-hardlink-file "anything-config/anything-config" "\
Preconfigured `anything' to hardlink files from dired.

\(fn)" t nil)

(autoload 'anything-dired-bindings "anything-config/anything-config" "\
Replace usual dired commands `C' and `R' by anything ones.
When call interactively toggle dired bindings and anything bindings.
When call non--interactively with arg > 0, enable anything bindings.
You can put (anything-dired-binding 1) in init file to enable anything bindings.

\(fn &optional ARG)" t nil)

(autoload 'anything-do-grep "anything-config/anything-config" "\
Preconfigured anything for grep.
Contrarily to Emacs `grep' no default directory is given, but
the full path of candidates in ONLY.
That allow to grep different files not only in `default-directory' but anywhere
by marking them (C-<SPACE>). If one or more directory is selected
grep will search in all files of these directories.
You can use also wildcard in the base name of candidate.
If a prefix arg is given use the -r option of grep.
See also `anything-do-grep1'.

\(fn)" t nil)

(autoload 'anything-c-goto-precedent-file "anything-config/anything-config" "\
Go to precedent file in anything grep/etags buffers.

\(fn)" t nil)

(autoload 'anything-c-goto-next-file "anything-config/anything-config" "\
Go to precedent file in anything grep/etags buffers.

\(fn)" t nil)

(autoload 'anything-yank-text-at-point "anything-config/anything-config" "\
Yank text at point in minibuffer.

\(fn)" t nil)

(autoload 'anything-filelist "anything-config/anything-config" "\
Preconfigured `anything' to open files instantly.

\(fn)" t nil)

(autoload 'anything-filelist+ "anything-config/anything-config" "\
Preconfigured `anything' to open files/buffers/bookmarks instantly.

This is a replacement for `anything-for-files'.

\(fn)" t nil)

(autoload 'anything-M-x "anything-config/anything-config" "\
Preconfigured `anything' for Emacs commands.
It is `anything' replacement of regular `M-x' `execute-extended-command'.

\(fn)" t nil)

(autoload 'anything-manage-advice "anything-config/anything-config" "\
Preconfigured `anything' to disable/enable function advices.

\(fn)" t nil)

(autoload 'anything-bookmark-ext "anything-config/anything-config" "\
Preconfigured `anything' for bookmark-extensions sources.
Needs bookmark-ext.el

http://mercurial.intuxication.org/hg/emacs-bookmark-extension

\(fn)" t nil)

(autoload 'anything-simple-call-tree "anything-config/anything-config" "\
Preconfigured `anything' for simple-call-tree. List function relationships.

Needs simple-call-tree.el.
http://www.emacswiki.org/cgi-bin/wiki/download/simple-call-tree.el

\(fn)" t nil)

(autoload 'anything-mark-ring "anything-config/anything-config" "\
Preconfigured `anything' for `anything-c-source-mark-ring'.

\(fn)" t nil)

(autoload 'anything-global-mark-ring "anything-config/anything-config" "\
Preconfigured `anything' for `anything-c-source-global-mark-ring'.

\(fn)" t nil)

(autoload 'anything-all-mark-rings "anything-config/anything-config" "\
Preconfigured `anything' for `anything-c-source-global-mark-ring' and `anything-c-source-mark-ring'.

\(fn)" t nil)

(autoload 'anything-yaoddmuse-cache-pages "anything-config/anything-config" "\
Fetch the list of files on emacswiki and create cache file.
If load is non--nil load the file and feed `yaoddmuse-pages-hash'.

\(fn &optional LOAD)" t nil)

(autoload 'anything-yaoddmuse-emacswiki-edit-or-view "anything-config/anything-config" "\
Preconfigured `anything' to edit or view EmacsWiki page.

Needs yaoddmuse.el.

http://www.emacswiki.org/emacs/download/yaoddmuse.el

\(fn)" t nil)

(autoload 'anything-yaoddmuse-emacswiki-post-library "anything-config/anything-config" "\
Preconfigured `anything' to post library to EmacsWiki.

Needs yaoddmuse.el.

http://www.emacswiki.org/emacs/download/yaoddmuse.el

\(fn)" t nil)

(autoload 'anything-eval-expression "anything-config/anything-config" "\
Preconfigured anything for `anything-c-source-evaluation-result'.

\(fn ARG)" t nil)

(autoload 'anything-eval-expression-with-eldoc "anything-config/anything-config" "\
Preconfigured anything for `anything-c-source-evaluation-result' with `eldoc' support.

\(fn)" t nil)

(autoload 'anything-surfraw "anything-config/anything-config" "\
Preconfigured `anything' to search PATTERN with search ENGINE.

\(fn PATTERN ENGINE)" t nil)

(autoload 'anything-call-source "anything-config/anything-config" "\
Preconfigured `anything' to call anything source.

\(fn)" t nil)

(autoload 'anything-execute-anything-command "anything-config/anything-config" "\
Preconfigured `anything' to execute preconfigured `anything'.

\(fn)" t nil)

(autoload 'anything-create "anything-config/anything-config" "\
Preconfigured `anything' to do many create actions from STRING.
See also `anything-create--actions'.

\(fn &optional STRING INITIAL-INPUT)" t nil)

(autoload 'anything-top "anything-config/anything-config" "\
Preconfigured `anything' for top command.

\(fn)" t nil)

(autoload 'anything-select-xfont "anything-config/anything-config" "\
Preconfigured `anything' to select Xfont.

\(fn)" t nil)

(autoload 'anything-world-time "anything-config/anything-config" "\
Preconfigured `anything' to show world time.

\(fn)" t nil)

(autoload 'anything-apt "anything-config/anything-config" "\
Preconfigured `anything' : frontend of APT package manager.

\(fn QUERY)" t nil)

(autoload 'anything-c-shell-command-if-needed "anything-config/anything-config" "\
Not documented

\(fn COMMAND)" t nil)

(autoload 'anything-c-run-external-command "anything-config/anything-config" "\
Preconfigured `anything' to run External PROGRAM asyncronously from Emacs.
If program is already running exit with error.
You can set your own list of commands with
`anything-c-external-commands-list'.

\(fn PROGRAM)" t nil)

(autoload 'anything-ratpoison-commands "anything-config/anything-config" "\
Preconfigured `anything' to execute ratpoison commands.

\(fn)" t nil)

(autoload 'w32-shell-execute-open-file "anything-config/anything-config" "\
Not documented

\(fn FILE)" t nil)

(autoload 'anything-c-call-interactively "anything-config/anything-config" "\
Execute CMD-OR-NAME as Emacs command.
It is added to `extended-command-history'.
`anything-current-prefix-arg' is used as the command's prefix argument.

\(fn CMD-OR-NAME)" nil nil)

(autoload 'anything-c-set-variable "anything-config/anything-config" "\
Set value to VAR interactively.

\(fn VAR)" t nil)

;;;***

;;;### (autoloads (anything-other-buffer anything-at-point anything)
;;;;;;  "anything/anything" "anything/anything.el" (19789 13048))
;;; Generated autoloads from anything/anything.el

(autoload 'anything "anything/anything" "\
Select anything. In Lisp program, some optional arguments can be used.

PLIST is a list like (:key1 val1 :key2 val2 ...) or
\(&optional sources input prompt resume preselect buffer keymap).

Basic keywords are the following:

- :sources

Temporary value of `anything-sources'.  It also accepts a
symbol, interpreted as a variable of an anything source.  It
also accepts an alist representing an anything source, which is
detected by (assq 'name ANY-SOURCES)

- :input

Temporary value of `anything-pattern', ie. initial input of minibuffer.

- :prompt

Prompt other than \"pattern: \".

- :resume

If t, Resurrect previously instance of `anything'. Skip the initialization.
If 'noresume, this instance of `anything' cannot be resumed.

- :preselect

Initially selected candidate. Specified by exact candidate or a regexp.
Note that it is not working with delayed sources.

- :buffer

`anything-buffer' instead of *anything*.

- :keymap

`anything-map' for current `anything' session.


Of course, conventional arguments are supported, the two are same.

\(anything :sources sources :input input :prompt prompt :resume resume
:preselect preselect :buffer buffer :keymap keymap)
\(anything sources input prompt resume preselect buffer keymap)


Other keywords are interpreted as local variables of this anything session.
The `anything-' prefix can be omitted. For example,

\(anything :sources 'anything-c-source-buffers
:buffer \"*buffers*\" :candidate-number-limit 10)

means starting anything session with `anything-c-source-buffers'
source in *buffers* buffer and set
`anything-candidate-number-limit' to 10 as session local variable.

\(fn &rest PLIST)" t nil)

(autoload 'anything-at-point "anything/anything" "\
Same as `anything' except when C-u is pressed, the initial input is the symbol at point.

\(fn &optional ANY-SOURCES ANY-INPUT ANY-PROMPT ANY-RESUME ANY-PRESELECT ANY-BUFFER)" t nil)

(autoload 'anything-other-buffer "anything/anything" "\
Simplified interface of `anything' with other `anything-buffer'

\(fn ANY-SOURCES ANY-BUFFER)" nil nil)

;;;***

;;;### (autoloads (vassoc set-modified-alist modify-alist remove-alist
;;;;;;  set-alist del-alist put-alist) "apel/site-lisp/apel/alist"
;;;;;;  "apel/site-lisp/apel/alist.el" (19785 303))
;;; Generated autoloads from apel/site-lisp/apel/alist.el

(autoload 'put-alist "apel/site-lisp/apel/alist" "\
Set cdr of an element (KEY . ...) in ALIST to VALUE and return ALIST.
If there is no such element, create a new pair (KEY . VALUE) and
return a new alist whose car is the new pair and cdr is ALIST.

\(fn KEY VALUE ALIST)" nil nil)

(autoload 'del-alist "apel/site-lisp/apel/alist" "\
Delete an element whose car equals KEY from ALIST.
Return the modified ALIST.

\(fn KEY ALIST)" nil nil)

(autoload 'set-alist "apel/site-lisp/apel/alist" "\
Set cdr of an element (KEY . ...) in the alist bound to SYMBOL to VALUE.

\(fn SYMBOL KEY VALUE)" nil nil)

(autoload 'remove-alist "apel/site-lisp/apel/alist" "\
Delete an element whose car equals KEY from the alist bound to SYMBOL.

\(fn SYMBOL KEY)" nil nil)

(autoload 'modify-alist "apel/site-lisp/apel/alist" "\
Store elements in the alist MODIFIER in the alist DEFAULT.
Return the modified alist.

\(fn MODIFIER DEFAULT)" nil nil)

(autoload 'set-modified-alist "apel/site-lisp/apel/alist" "\
Store elements in the alist MODIFIER in an alist bound to SYMBOL.
If SYMBOL is not bound, set it to nil at first.

\(fn SYMBOL MODIFIER)" nil nil)

(autoload 'vassoc "apel/site-lisp/apel/alist" "\
Search AVLIST for an element whose first element equals KEY.
AVLIST is a list of vectors.
See also `assoc'.

\(fn KEY AVLIST)" nil nil)

;;;***

;;;### (autoloads (module-installed-p exec-installed-p file-installed-p
;;;;;;  get-latest-path add-latest-path add-path) "apel/site-lisp/apel/path-util"
;;;;;;  "apel/site-lisp/apel/path-util.el" (19785 303))
;;; Generated autoloads from apel/site-lisp/apel/path-util.el

(autoload 'add-path "apel/site-lisp/apel/path-util" "\
Add PATH to `load-path' if it exists under `default-load-path'
directories and it does not exist in `load-path'.

You can use following PATH styles:
	load-path relative: \"PATH/\"
			(it is searched from `default-load-path')
	home directory relative: \"~/PATH/\" \"~USER/PATH/\"
	absolute path: \"/HOO/BAR/BAZ/\"

You can specify following OPTIONS:
	'all-paths	search from `load-path'
			instead of `default-load-path'
	'append		add PATH to the last of `load-path'

\(fn PATH &rest OPTIONS)" nil nil)

(autoload 'add-latest-path "apel/site-lisp/apel/path-util" "\
Add latest path matched by PATTERN to `load-path'
if it exists under `default-load-path' directories
and it does not exist in `load-path'.

If optional argument ALL-PATHS is specified, it is searched from all
of load-path instead of default-load-path.

\(fn PATTERN &optional ALL-PATHS)" nil nil)

(autoload 'get-latest-path "apel/site-lisp/apel/path-util" "\
Return latest directory in default-load-path
which is matched to regexp PATTERN.
If optional argument ALL-PATHS is specified,
it is searched from all of load-path instead of default-load-path.

\(fn PATTERN &optional ALL-PATHS)" nil nil)

(autoload 'file-installed-p "apel/site-lisp/apel/path-util" "\
Return absolute-path of FILE if FILE exists in PATHS.
If PATHS is omitted, `load-path' is used.

\(fn FILE &optional PATHS)" nil nil)

(defvar exec-suffix-list '("") "\
*List of suffixes for executable.")

(autoload 'exec-installed-p "apel/site-lisp/apel/path-util" "\
Return absolute-path of FILE if FILE exists in PATHS.
If PATHS is omitted, `exec-path' is used.
If suffixes is omitted, `exec-suffix-list' is used.

\(fn FILE &optional PATHS SUFFIXES)" nil nil)

(autoload 'module-installed-p "apel/site-lisp/apel/path-util" "\
Return t if module is provided or exists in PATHS.
If PATHS is omitted, `load-path' is used.

\(fn MODULE &optional PATHS)" nil nil)

;;;***

;;;### (autoloads (richtext-decode richtext-encode) "apel/site-lisp/emu/richtext"
;;;;;;  "apel/site-lisp/emu/richtext.el" (19785 304))
;;; Generated autoloads from apel/site-lisp/emu/richtext.el

(autoload 'richtext-encode "apel/site-lisp/emu/richtext" "\
Not documented

\(fn FROM TO)" nil nil)

(autoload 'richtext-decode "apel/site-lisp/emu/richtext" "\
Not documented

\(fn FROM TO)" nil nil)

;;;***

;;;### (autoloads (adict-change-dictionary adict-guess-dictionary
;;;;;;  auto-dictionary-mode) "auto-dictionary/auto-dictionary" "auto-dictionary/auto-dictionary.el"
;;;;;;  (19790 25986))
;;; Generated autoloads from auto-dictionary/auto-dictionary.el

(autoload 'auto-dictionary-mode "auto-dictionary/auto-dictionary" "\
A minor mode that automatically sets `ispell-dictionary`.

\(fn &optional ARG)" t nil)

(autoload 'adict-guess-dictionary "auto-dictionary/auto-dictionary" "\
Automatically change ispell dictionary based on buffer language.
Calls `ispell-change-dictionary' and runs `adict-change-dictionary-hook'.  If
BUFFER is nil, the current buffer is used.  If IDLE-ONLY is set, abort
when an input event occurs.

\(fn &optional IDLE-ONLY)" t nil)

(autoload 'adict-change-dictionary "auto-dictionary/auto-dictionary" "\
Set buffer language to LANG and stop detecting it automatically.

\(fn &optional LANG)" t nil)

;;;***

;;;### (autoloads (csharp-mode csharp-mode-hook) "csharp-mode/csharp-mode"
;;;;;;  "csharp-mode/csharp-mode.el" (19789 14584))
;;; Generated autoloads from csharp-mode/csharp-mode.el

(add-to-list 'auto-mode-alist '("\\.cs$" . csharp-mode))

(defvar csharp-mode-hook nil "\
*Hook called by `csharp-mode'.")

(custom-autoload 'csharp-mode-hook "csharp-mode/csharp-mode" t)

(autoload 'csharp-mode "csharp-mode/csharp-mode" "\
Major mode for editing C# code. This mode is derived from CC Mode to
support C#.

The hook `c-mode-common-hook' is run with no args at mode
initialization, then `csharp-mode-hook'.

This mode will automatically add a regexp for Csc.exe error and warning
messages to the `compilation-error-regexp-alist'.

Key bindings:
\\{csharp-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads nil "doxymacs/lisp/doxymacs" "doxymacs/lisp/doxymacs.el"
;;;;;;  (19789 9868))
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

;;;### (autoloads (bookmark-w3m-bookmark-jump) "emacs-w3m/bookmark-w3m"
;;;;;;  "emacs-w3m/bookmark-w3m.el" (19790 35946))
;;; Generated autoloads from emacs-w3m/bookmark-w3m.el

(autoload 'bookmark-w3m-bookmark-jump "emacs-w3m/bookmark-w3m" "\
Default bookmark handler for w3m buffers.

\(fn BOOKMARK)" nil nil)

;;;***

;;;### (autoloads (mime-w3m-preview-text/html) "emacs-w3m/mime-w3m"
;;;;;;  "emacs-w3m/mime-w3m.el" (19790 42257))
;;; Generated autoloads from emacs-w3m/mime-w3m.el

(autoload 'mime-w3m-preview-text/html "emacs-w3m/mime-w3m" "\
Not documented

\(fn ENTITY SITUATION)" nil nil)

;;;***

;;;### (autoloads (octet-mime-setup mime-view-octet mime-preview-octet
;;;;;;  octet-find-file octet-buffer) "emacs-w3m/octet" "emacs-w3m/octet.el"
;;;;;;  (19790 35946))
;;; Generated autoloads from emacs-w3m/octet.el

(autoload 'octet-buffer "emacs-w3m/octet" "\
View octet-stream content according to `octet-type-filter-alist'.
Optional NAME is the filename.
If optional CONTENT-TYPE is specified, it is used for type guess.

\(fn &optional NAME CONTENT-TYPE)" t nil)

(autoload 'octet-find-file "emacs-w3m/octet" "\
Find FILE with octet-stream decoding.

\(fn FILE)" t nil)

(autoload 'mime-preview-octet "emacs-w3m/octet" "\
A method for mime-view to preview octet message.

\(fn ENTITY SITUATION)" nil nil)

(autoload 'mime-view-octet "emacs-w3m/octet" "\
A method for mime-view to display octet message.

\(fn ENTITY SITUATION)" nil nil)

(autoload 'octet-mime-setup "emacs-w3m/octet" "\
Octet setting for MIME module.

\(fn)" nil nil)

;;;***

;;;### (autoloads (w3m-buffer w3m-region w3m-find-file w3m-browse-url
;;;;;;  w3m w3m-gohome w3m-goto-url-new-session w3m-goto-url w3m-download
;;;;;;  w3m-retrieve) "emacs-w3m/w3m" "emacs-w3m/w3m.el" (19790 42257))
;;; Generated autoloads from emacs-w3m/w3m.el

(autoload 'w3m-retrieve "emacs-w3m/w3m" "\
Retrieve web contents pointed to by URL.
It will put the retrieved contents into the current buffer.

If HANDLER is nil, this function will retrieve web contents, return
the content type of the retrieved data, and then come to an end.  This
behavior is what is called a synchronous operation.  You have to
specify HANDLER in order to make this function show its real ability,
which is called an asynchronous operation.

If HANDLER is a function, this function will come to an end in no time.
In this case, contents will be retrieved by the asynchronous process
after a while.  And after finishing retrieving contents successfully,
HANDLER will be called on the buffer where this function starts.  The
content type of the retrieved data will be passed to HANDLER as a
string argument.

NO-UNCOMPRESS specifies whether this function should not uncompress contents.
NO-CACHE specifies whether this function should not use cached contents.
POST-DATA and REFERER will be sent to the web server with a request.

\(fn URL &optional NO-UNCOMPRESS NO-CACHE POST-DATA REFERER HANDLER)" nil nil)

(autoload 'w3m-download "emacs-w3m/w3m" "\
Download contents of URL to a file named FILENAME.
NO-CHACHE (which the prefix argument gives when called interactively)
specifies not using the cached data.

\(fn URL &optional FILENAME NO-CACHE HANDLER POST-DATA)" t nil)

(autoload 'w3m-goto-url "emacs-w3m/w3m" "\
Visit World Wide Web pages.  This is the primitive function of `w3m'.
If the second argument RELOAD is non-nil, reload a content of URL.
Except that if it is 'redisplay, re-display the page without reloading.
The third argument CHARSET specifies a charset to be used for decoding
a content.
The fourth argument POST-DATA should be a string or a cons cell.  If
it is a string, it makes this function request a body as if the
content-type is \"x-www-form-urlencoded\".  If it is a cons cell, the
car of a cell is used as the content-type and the cdr of a cell is
used as the body.
If the fifth argument REFERER is specified, it is used for a Referer:
field for this request.
The remaining HANDLER, ELEMENT[1], and NO-POPUP are for the
internal operations of emacs-w3m.
You can also use \"quicksearch\" url schemes such as \"gg:emacs\" which
would search for the term \"emacs\" with the Google search engine.  See
the `w3m-search' function and the variable `w3m-uri-replace-alist'.

\[1] A note for the developers: ELEMENT is a history element which has
already been registered in the `w3m-history-flat' variable.  It is
corresponding to URL to be retrieved at this time, not for the url of
the current page.

\(fn URL &optional RELOAD CHARSET POST-DATA REFERER HANDLER ELEMENT NO-POPUP)" t nil)

(autoload 'w3m-goto-url-new-session "emacs-w3m/w3m" "\
Visit World Wide Web pages in a new session.
If you invoke this command in the emacs-w3m buffer, the new session
will be created by copying the current session.  Otherwise, the new
session will start afresh.

\(fn URL &optional RELOAD CHARSET POST-DATA REFERER)" t nil)

(autoload 'w3m-gohome "emacs-w3m/w3m" "\
Go to the Home page.

\(fn)" t nil)

(autoload 'w3m "emacs-w3m/w3m" "\
Visit World Wide Web pages using the external w3m command.

When you invoke this command interactively for the first time, it will
visit a page which is pointed to by a string like url around the
cursor position or the home page specified by the `w3m-home-page'
variable, but you will be prompted for a URL if `w3m-quick-start' is
nil (default t) or `w3m-home-page' is nil.

The variables `w3m-pop-up-windows' and `w3m-pop-up-frames' control
whether this command should pop to a window or a frame up for the
session.

When emacs-w3m sessions have already been opened, this command will
pop to the existing window or frame up, but if `w3m-quick-start' is
nil, (default t), you will be prompted for a URL (which defaults to
`popup' meaning to pop to an existing emacs-w3m buffer up).

In addition, if the prefix argument is given or you enter the empty
string for the prompt, it will visit the home page specified by the
`w3m-home-page' variable or the \"about:\" page.

You can also run this command in the batch mode as follows:

  emacs -f w3m http://emacs-w3m.namazu.org/ &

In that case, or if this command is called non-interactively, the
variables `w3m-pop-up-windows' and `w3m-pop-up-frames' will be ignored
\(treated as nil) and it will run emacs-w3m at the current (or the
initial) window.

If the optional NEW-SESSION is non-nil, this function makes a new
emacs-w3m buffer.  Besides that, it also makes a new emacs-w3m buffer
if `w3m-make-new-session' is non-nil and a user specifies a url string.

The optional INTERACTIVE-P is for the internal use; it is mainly used
to check whether Emacs 22 or later calls this function as an
interactive command in the batch mode.

\(fn &optional URL NEW-SESSION INTERACTIVE-P)" t nil)

(autoload 'w3m-browse-url "emacs-w3m/w3m" "\
Ask emacs-w3m to browse URL.
NEW-SESSION specifies whether to create a new emacs-w3m session.  URL
defaults to the string looking like a url around the cursor position.
Pop to a window or a frame up according to `w3m-pop-up-windows' and
`w3m-pop-up-frames'.

\(fn URL &optional NEW-SESSION)" t nil)

(autoload 'w3m-find-file "emacs-w3m/w3m" "\
Function used to open FILE whose name is expressed in ordinary format.
The file name will be converted into the file: scheme.

\(fn FILE)" t nil)

(autoload 'w3m-region "emacs-w3m/w3m" "\
Render the region of the current buffer between START and END.
URL specifies the address where the contents come from.  It can be
omitted or nil when the address is not identified.  CHARSET is used
for decoding the contents.  If it is nil, this function attempts to
parse the meta tag to extract the charset.

\(fn START END &optional URL CHARSET)" t nil)

(autoload 'w3m-buffer "emacs-w3m/w3m" "\
Render the current buffer.
See `w3m-region' for the optional arguments.

\(fn &optional URL CHARSET)" t nil)

;;;***

;;;### (autoloads (w3m-antenna w3m-about-antenna) "emacs-w3m/w3m-antenna"
;;;;;;  "emacs-w3m/w3m-antenna.el" (19790 35946))
;;; Generated autoloads from emacs-w3m/w3m-antenna.el

(autoload 'w3m-about-antenna "emacs-w3m/w3m-antenna" "\
Not documented

\(fn URL &optional NO-DECODE NO-CACHE POST-DATA REFERER HANDLER)" nil nil)

(autoload 'w3m-antenna "emacs-w3m/w3m-antenna" "\
Report changes of WEB sites, which is specified in `w3m-antenna-sites'.

\(fn &optional NO-CACHE)" t nil)

;;;***

;;;### (autoloads (w3m-setup-bookmark-menu w3m-about-bookmark w3m-bookmark-view-new-session
;;;;;;  w3m-bookmark-view w3m-bookmark-add-current-url-group w3m-bookmark-add-all-urls
;;;;;;  w3m-bookmark-add-current-url w3m-bookmark-add-this-url) "emacs-w3m/w3m-bookmark"
;;;;;;  "emacs-w3m/w3m-bookmark.el" (19790 35946))
;;; Generated autoloads from emacs-w3m/w3m-bookmark.el

(autoload 'w3m-bookmark-add-this-url "emacs-w3m/w3m-bookmark" "\
Add link under cursor to bookmark.

\(fn)" t nil)

(autoload 'w3m-bookmark-add-current-url "emacs-w3m/w3m-bookmark" "\
Add a url of the current page to the bookmark.
With prefix, ask for a new url instead of the present one.

\(fn &optional ARG)" t nil)

(autoload 'w3m-bookmark-add-all-urls "emacs-w3m/w3m-bookmark" "\
Add urls of all pages being visited to the bookmark.

\(fn)" t nil)

(autoload 'w3m-bookmark-add-current-url-group "emacs-w3m/w3m-bookmark" "\
Add link of the group of current urls to the bookmark.

\(fn)" t nil)

(autoload 'w3m-bookmark-view "emacs-w3m/w3m-bookmark" "\
Display the bookmark.

\(fn &optional RELOAD)" t nil)

(autoload 'w3m-bookmark-view-new-session "emacs-w3m/w3m-bookmark" "\
Display the bookmark on a new session.

\(fn &optional RELOAD)" t nil)

(autoload 'w3m-about-bookmark "emacs-w3m/w3m-bookmark" "\
Not documented

\(fn &rest ARGS)" nil nil)

(autoload 'w3m-setup-bookmark-menu "emacs-w3m/w3m-bookmark" "\
Setup w3m bookmark items in menubar.

\(fn)" nil nil)

;;;***

;;;### (autoloads (w3m-about-cookie w3m-cookie w3m-cookie-get w3m-cookie-set
;;;;;;  w3m-cookie-shutdown) "emacs-w3m/w3m-cookie" "emacs-w3m/w3m-cookie.el"
;;;;;;  (19790 35946))
;;; Generated autoloads from emacs-w3m/w3m-cookie.el

(autoload 'w3m-cookie-shutdown "emacs-w3m/w3m-cookie" "\
Save cookies, and reset cookies' data.

\(fn)" t nil)

(autoload 'w3m-cookie-set "emacs-w3m/w3m-cookie" "\
Register cookies which correspond to URL.
BEG and END should be an HTTP response header region on current buffer.

\(fn URL BEG END)" nil nil)

(autoload 'w3m-cookie-get "emacs-w3m/w3m-cookie" "\
Get a cookie field string which corresponds to the URL.

\(fn URL)" nil nil)

(autoload 'w3m-cookie "emacs-w3m/w3m-cookie" "\
Display cookies and enable you to manage them.

\(fn &optional NO-CACHE)" t nil)

(autoload 'w3m-about-cookie "emacs-w3m/w3m-cookie" "\
Make the html contents to display and to enable you to manage cookies.

\(fn URL &optional NO-DECODE NO-CACHE POST-DATA &rest ARGS)" nil nil)

;;;***

;;;### (autoloads (w3m-dtree w3m-about-dtree) "emacs-w3m/w3m-dtree"
;;;;;;  "emacs-w3m/w3m-dtree.el" (19790 35946))
;;; Generated autoloads from emacs-w3m/w3m-dtree.el

(autoload 'w3m-about-dtree "emacs-w3m/w3m-dtree" "\
Not documented

\(fn URL &optional NODECODE ALLFILES &rest ARGS)" nil nil)

(autoload 'w3m-dtree "emacs-w3m/w3m-dtree" "\
Display directory tree on local file system.
If called with 'prefix argument', display all directorys and files.

\(fn ALLFILES PATH)" t nil)

;;;***

;;;### (autoloads (w3m-fb-mode) "emacs-w3m/w3m-fb" "emacs-w3m/w3m-fb.el"
;;;;;;  (19790 35946))
;;; Generated autoloads from emacs-w3m/w3m-fb.el

(defvar w3m-fb-mode nil "\
Non-nil if W3m-Fb mode is enabled.
See the command `w3m-fb-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `w3m-fb-mode'.")

(custom-autoload 'w3m-fb-mode "emacs-w3m/w3m-fb" nil)

(autoload 'w3m-fb-mode "emacs-w3m/w3m-fb" "\
Toggle W3M Frame Buffer mode.
This allows frame-local lists of buffers (tabs).

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (w3m-filter) "emacs-w3m/w3m-filter" "emacs-w3m/w3m-filter.el"
;;;;;;  (19790 35946))
;;; Generated autoloads from emacs-w3m/w3m-filter.el

(autoload 'w3m-filter "emacs-w3m/w3m-filter" "\
Apply filtering rule of URL against a content in this buffer.

\(fn URL)" nil nil)

;;;***

;;;### (autoloads (w3m-fontify-forms) "emacs-w3m/w3m-form" "emacs-w3m/w3m-form.el"
;;;;;;  (19790 35946))
;;; Generated autoloads from emacs-w3m/w3m-form.el

(autoload 'w3m-fontify-forms "emacs-w3m/w3m-form" "\
Process half-dumped data and fontify forms in this buffer.

\(fn)" nil nil)

;;;***

;;;### (autoloads (w3m-linknum-bookmark-add-this-url w3m-linknum-download-this-url
;;;;;;  w3m-linknum-print-this-url w3m-linknum-edit-this-url w3m-linknum-external-view-this-url
;;;;;;  w3m-linknum-save-image w3m-linknum-view-image w3m-linknum-toggle-inline-image
;;;;;;  w3m-linknum-follow w3m-go-to-linknum w3m-link-numbering-mode)
;;;;;;  "emacs-w3m/w3m-lnum" "emacs-w3m/w3m-lnum.el" (19790 42257))
;;; Generated autoloads from emacs-w3m/w3m-lnum.el

(autoload 'w3m-link-numbering-mode "emacs-w3m/w3m-lnum" "\
Minor mode to extend point commands by using Conkeror style number selection.
With prefix ARG 0 disable battery included point functions, otherwise
enable them.  With no prefix ARG - toggle.

\(fn &optional ARG)" t nil)

(autoload 'w3m-go-to-linknum "emacs-w3m/w3m-lnum" "\
Turn on link, image and form numbers and ask for one to go to.
With prefix ARG don't highlight current link.
0 corresponds to location url.

\(fn ARG)" t nil)

(autoload 'w3m-linknum-follow "emacs-w3m/w3m-lnum" "\
Turn on link numbers, ask for one and execute appropriate action on it.
When link - visit it, when button - press, when input - activate it,
when image - toggle it.
With prefix ARG visit link in new session or don't move over
field/button/image on activation/push/toggle.

\(fn ARG)" t nil)

(autoload 'w3m-linknum-toggle-inline-image "emacs-w3m/w3m-lnum" "\
If image at point, toggle it.
Otherwise turn on link numbers and toggle selected image.
With prefix ARG open url under image in new session.
If no such url, move over image and toggle it.

\(fn &optional ARG)" t nil)

(autoload 'w3m-linknum-view-image "emacs-w3m/w3m-lnum" "\
Display the image under point in the external viewer.
If no image at poing, turn on image numbers and display selected.
The viewer is defined in `w3m-content-type-alist' for every type of an
image.

\(fn)" t nil)

(autoload 'w3m-linknum-save-image "emacs-w3m/w3m-lnum" "\
Save the image under point to a file.
If no image at poing, turn on image numbers and save selected.
The default name will be the original name of the image.

\(fn)" t nil)

(autoload 'w3m-linknum-external-view-this-url "emacs-w3m/w3m-lnum" "\
Launch the external browser and display the link at point.
If no link at point, turn on link numbers and open selected externally.

\(fn)" t nil)

(autoload 'w3m-linknum-edit-this-url "emacs-w3m/w3m-lnum" "\
Edit the page linked from the anchor under the cursor.
If no such, turn on link numbers and edit selected.

\(fn)" t nil)

(autoload 'w3m-linknum-print-this-url "emacs-w3m/w3m-lnum" "\
Display the url under point in the echo area and put it into `kill-ring'.
If no url under point, activate numbering and select one.

\(fn)" t nil)

(autoload 'w3m-linknum-download-this-url "emacs-w3m/w3m-lnum" "\
Download the file or the page pointed to by the link under point.
If no point, activate numbering and select andchor to download.

\(fn)" t nil)

(autoload 'w3m-linknum-bookmark-add-this-url "emacs-w3m/w3m-lnum" "\
Add link under cursor to bookmark.
If no link under point, activate numbering and ask for one.

\(fn)" t nil)

;;;***

;;;### (autoloads (w3m-namazu w3m-about-namazu) "emacs-w3m/w3m-namazu"
;;;;;;  "emacs-w3m/w3m-namazu.el" (19790 35946))
;;; Generated autoloads from emacs-w3m/w3m-namazu.el

(autoload 'w3m-about-namazu "emacs-w3m/w3m-namazu" "\
Not documented

\(fn URL &optional NO-DECODE NO-CACHE &rest ARGS)" nil nil)

(autoload 'w3m-namazu "emacs-w3m/w3m-namazu" "\
Search indexed files with Namazu.

\(fn INDEX QUERY &optional RELOAD)" t nil)

;;;***

;;;### (autoloads (w3m-perldoc w3m-about-perldoc) "emacs-w3m/w3m-perldoc"
;;;;;;  "emacs-w3m/w3m-perldoc.el" (19790 35946))
;;; Generated autoloads from emacs-w3m/w3m-perldoc.el

(autoload 'w3m-about-perldoc "emacs-w3m/w3m-perldoc" "\
Not documented

\(fn URL &optional NO-DECODE NO-CACHE &rest ARGS)" nil nil)

(autoload 'w3m-perldoc "emacs-w3m/w3m-perldoc" "\
View Perl documents.

\(fn DOCNAME)" t nil)

;;;***

;;;### (autoloads (w3m-search-uri-replace w3m-search-new-session
;;;;;;  w3m-search) "emacs-w3m/w3m-search" "emacs-w3m/w3m-search.el"
;;;;;;  (19790 35946))
;;; Generated autoloads from emacs-w3m/w3m-search.el

(autoload 'w3m-search "emacs-w3m/w3m-search" "\
Search QUERY using SEARCH-ENGINE.
When called interactively with a prefix argument, you can choose one of
the search engines defined in `w3m-search-engine-alist'.  Otherwise use
`w3m-search-default-engine'.
If Transient Mark mode, use the region as an initial string of query
and deactivate the mark.

\(fn SEARCH-ENGINE QUERY)" t nil)

(autoload 'w3m-search-new-session "emacs-w3m/w3m-search" "\
Like `w3m-search', but do the search in a new session.

\(fn SEARCH-ENGINE QUERY)" t nil)

(autoload 'w3m-search-uri-replace "emacs-w3m/w3m-search" "\
Generate query string for ENGINE from URI matched by last search.

\(fn URI ENGINE)" nil nil)

;;;***

;;;### (autoloads (w3m-session-last-crashed-session w3m-session-last-autosave-session
;;;;;;  w3m-setup-session-menu w3m-session-select w3m-session-save)
;;;;;;  "emacs-w3m/w3m-session" "emacs-w3m/w3m-session.el" (19790
;;;;;;  42257))
;;; Generated autoloads from emacs-w3m/w3m-session.el

(autoload 'w3m-session-save "emacs-w3m/w3m-session" "\
Save list of displayed session.

\(fn)" t nil)

(autoload 'w3m-session-select "emacs-w3m/w3m-session" "\
Select session from session list.

\(fn)" t nil)

(autoload 'w3m-setup-session-menu "emacs-w3m/w3m-session" "\
Setup w3m session items in menubar.

\(fn)" nil nil)

(autoload 'w3m-session-last-autosave-session "emacs-w3m/w3m-session" "\
Not documented

\(fn)" nil nil)

(autoload 'w3m-session-last-crashed-session "emacs-w3m/w3m-session" "\
Not documented

\(fn)" nil nil)

;;;***

;;;### (autoloads (w3m-replace-symbol) "emacs-w3m/w3m-symbol" "emacs-w3m/w3m-symbol.el"
;;;;;;  (19790 35946))
;;; Generated autoloads from emacs-w3m/w3m-symbol.el

(autoload 'w3m-replace-symbol "emacs-w3m/w3m-symbol" "\
Not documented

\(fn)" nil nil)

;;;***

;;;### (autoloads (w3m-about-weather w3m-weather) "emacs-w3m/w3m-weather"
;;;;;;  "emacs-w3m/w3m-weather.el" (19790 35946))
;;; Generated autoloads from emacs-w3m/w3m-weather.el

(autoload 'w3m-weather "emacs-w3m/w3m-weather" "\
Display weather report.

\(fn AREA)" t nil)

(autoload 'w3m-about-weather "emacs-w3m/w3m-weather" "\
Not documented

\(fn URL NO-DECODE NO-CACHE POST-DATA REFERER HANDLER)" nil nil)

;;;***

;;;### (autoloads (fic-mode) "fic-mode/fic-mode" "fic-mode/fic-mode.el"
;;;;;;  (19790 46427))
;;; Generated autoloads from fic-mode/fic-mode.el

(autoload 'fic-mode "fic-mode/fic-mode" "\
highlight FIXMEs in comments and strings (as well as TODO BUG and KLUDGE

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (framemove-default-keybindings fm-right-frame fm-left-frame
;;;;;;  fm-up-frame fm-down-frame) "framemove/framemove" "framemove/framemove.el"
;;;;;;  (19790 47712))
;;; Generated autoloads from framemove/framemove.el

(autoload 'fm-down-frame "framemove/framemove" "\
Not documented

\(fn)" t nil)

(autoload 'fm-up-frame "framemove/framemove" "\
Not documented

\(fn)" t nil)

(autoload 'fm-left-frame "framemove/framemove" "\
Not documented

\(fn)" t nil)

(autoload 'fm-right-frame "framemove/framemove" "\
Not documented

\(fn)" t nil)

(autoload 'framemove-default-keybindings "framemove/framemove" "\
Set up keybindings for `framemove'.
Keybindings are of the form MODIFIER-{left,right,up,down}.
Default MODIFIER is 'meta.

\(fn &optional MODIFIER)" t nil)

;;;***

;;;### (autoloads (gnugo) "gnugo/gnugo" "gnugo/gnugo.el" (19789 18872))
;;; Generated autoloads from gnugo/gnugo.el

(autoload 'gnugo "gnugo/gnugo" "\
Run gnugo in a buffer, or resume a game in progress.
Prefix arg means skip the game-in-progress check and start a new
game straight away.

You are queried for additional command-line options (Emacs supplies
\"--mode gtp --quiet\" automatically).  Here is a list of options
that gnugo.el understands and handles specially:

--boardsize num   Set the board size to use (5--19)
--color <color>   Choose your color ('black' or 'white')
--handicap <num>  Set the number of handicap stones (0--9)

If there is already a game in progress you may resume it instead of
starting a new one.  See `gnugo-board-mode' documentation for more info.

\(fn &optional NEW-GAME)" t nil)

;;;***

;;;### (autoloads (htmlize-many-files-dired htmlize-many-files htmlize-file
;;;;;;  htmlize-region htmlize-buffer) "htmlize/htmlize" "htmlize/htmlize.el"
;;;;;;  (19789 19046))
;;; Generated autoloads from htmlize/htmlize.el

(autoload 'htmlize-buffer "htmlize/htmlize" "\
Convert BUFFER to HTML, preserving colors and decorations.

The generated HTML is available in a new buffer, which is returned.
When invoked interactively, the new buffer is selected in the current
window.  The title of the generated document will be set to the buffer's
file name or, if that's not available, to the buffer's name.

Note that htmlize doesn't fontify your buffers, it only uses the
decorations that are already present.  If you don't set up font-lock or
something else to fontify your buffers, the resulting HTML will be
plain.  Likewise, if you don't like the choice of colors, fix the mode
that created them, or simply alter the faces it uses.

\(fn &optional BUFFER)" t nil)

(autoload 'htmlize-region "htmlize/htmlize" "\
Convert the region to HTML, preserving colors and decorations.
See `htmlize-buffer' for details.

\(fn BEG END)" t nil)

(autoload 'htmlize-file "htmlize/htmlize" "\
Load FILE, fontify it, convert it to HTML, and save the result.

Contents of FILE are inserted into a temporary buffer, whose major mode
is set with `normal-mode' as appropriate for the file type.  The buffer
is subsequently fontified with `font-lock' and converted to HTML.  Note
that, unlike `htmlize-buffer', this function explicitly turns on
font-lock.  If a form of highlighting other than font-lock is desired,
please use `htmlize-buffer' directly on buffers so highlighted.

Buffers currently visiting FILE are unaffected by this function.  The
function does not change current buffer or move the point.

If TARGET is specified and names a directory, the resulting file will be
saved there instead of to FILE's directory.  If TARGET is specified and
does not name a directory, it will be used as output file name.

\(fn FILE &optional TARGET)" t nil)

(autoload 'htmlize-many-files "htmlize/htmlize" "\
Convert FILES to HTML and save the corresponding HTML versions.

FILES should be a list of file names to convert.  This function calls
`htmlize-file' on each file; see that function for details.  When
invoked interactively, you are prompted for a list of files to convert,
terminated with RET.

If TARGET-DIRECTORY is specified, the HTML files will be saved to that
directory.  Normally, each HTML file is saved to the directory of the
corresponding source file.

\(fn FILES &optional TARGET-DIRECTORY)" t nil)

(autoload 'htmlize-many-files-dired "htmlize/htmlize" "\
HTMLize dired-marked files.

\(fn ARG &optional TARGET-DIRECTORY)" t nil)

;;;***

;;;### (autoloads (turn-on-hungry-delete-mode hungry-delete-mode
;;;;;;  hungry-delete-backward hungry-delete-forward) "hungury-delete/hungry-delete"
;;;;;;  "hungury-delete/hungry-delete.el" (19789 16857))
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

;;;### (autoloads (magit-status) "magit/magit" "magit/magit.el" (19789
;;;;;;  31922))
;;; Generated autoloads from magit/magit.el

(autoload 'magit-status "magit/magit" "\
Not documented

\(fn DIR)" t nil)

;;;***

;;;### (autoloads (multi-term) "multi-term/multi-term" "multi-term/multi-term.el"
;;;;;;  (19789 18514))
;;; Generated autoloads from multi-term/multi-term.el

(autoload 'multi-term "multi-term/multi-term" "\
Create new term buffer.
Will prompt you shell name when you type `C-u' before this command.

\(fn)" t nil)

;;;***

;;;### (autoloads (notify) "notify/notify" "notify/notify.el" (19791
;;;;;;  37678))
;;; Generated autoloads from notify/notify.el

(autoload 'notify "notify/notify" "\
Notify TITLE, BODY via `notify-method'.
ARGS may be amongst :timeout, :icon, :urgency, :app and :category.

\(fn TITLE BODY &rest ARGS)" nil nil)

;;;***

;;;### (autoloads (pylookup-update-all pylookup-update pylookup-lookup)
;;;;;;  "pylookup/pylookup" "pylookup/pylookup.el" (19784 14506))
;;; Generated autoloads from pylookup/pylookup.el

(autoload 'pylookup-lookup "pylookup/pylookup" "\
Lookup SEARCH-TERM in the Python HTML indexes.

\(fn SEARCH-TERM)" t nil)

(autoload 'pylookup-update "pylookup/pylookup" "\
Run pylookup-update and create the database at `pylookup-db-file'.

\(fn SRC &optional APPEND)" t nil)

(autoload 'pylookup-update-all "pylookup/pylookup" "\
Run pylookup-update for all sources and create the database at `pylookup-db-file'.

\(fn)" t nil)

;;;***

;;;### (autoloads (rainbow-mode) "rainbow-mode/rainbow-mode" "rainbow-mode/rainbow-mode.el"
;;;;;;  (19784 17751))
;;; Generated autoloads from rainbow-mode/rainbow-mode.el

(autoload 'rainbow-mode "rainbow-mode/rainbow-mode" "\
Colorize strings that represent colors.
This will fontify with colors the string like \"#aabbcc\" or \"blue\".

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (rcirc-groups-mode) "rcirc-groups/rcirc-groups"
;;;;;;  "rcirc-groups/rcirc-groups.el" (19791 37678))
;;; Generated autoloads from rcirc-groups/rcirc-groups.el

(autoload 'rcirc-groups-mode "rcirc-groups/rcirc-groups" "\
A major mode for handling rcirc notifications

\(fn)" t nil)

;;;***

;;;### (autoloads (smex-initialize) "smex/smex" "smex/smex.el" (19791
;;;;;;  63090))
;;; Generated autoloads from smex/smex.el

(autoload 'smex-initialize "smex/smex" "\
Not documented

\(fn)" t nil)

;;;***

;;;### (autoloads (speck-multi-read speck-buffer speck-mode) "speck/speck"
;;;;;;  "speck/speck.el" (19790 25986))
;;; Generated autoloads from speck/speck.el

(autoload 'speck-mode "speck/speck" "\
Toggle `speck-mode'.
With prefix ARG, turn speck-mode on if and only if ARG is
positive.  Turning on speck-mode will spell-check (\"speck\") all
windows showing the current buffer.

Global bindings (customizable via `speck-mode-keys').

\\{speck-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'speck-buffer "speck/speck" "\
Toggle `speck-mode' for current buffer.
With non-numeric prefix argument ARG prompt for (new)
dictionary.  With prefix argument ARG zero use the default
dictionary.  With ARG any other number use the corresponding
entry from `speck-dictionary-names-alist'.

\(fn &optional ARG)" t nil)

(autoload 'speck-multi-read "speck/speck" "\
Convert annotations to properties.
BEGIN and END denote the region to convert.

\(fn BEGIN END)" nil nil)

;;;***

;;;### (autoloads (sr-dired sunrise-cd sunrise) "sunrise-commander/sunrise-commander"
;;;;;;  "sunrise-commander/sunrise-commander.el" (19789 18859))
;;; Generated autoloads from sunrise-commander/sunrise-commander.el

(autoload 'sunrise "sunrise-commander/sunrise-commander" "\
Starts the Sunrise Commander. If the param `left-directory' is given the left
window  will  display  this  directory  (the  same   for   `right-directory').
Specifying nil for any of these values uses the default, ie. home.

\(fn &optional LEFT-DIRECTORY RIGHT-DIRECTORY FILENAME)" t nil)

(autoload 'sunrise-cd "sunrise-commander/sunrise-commander" "\
Run Sunrise but give it the current directory to use.

\(fn)" t nil)

(autoload 'sr-dired "sunrise-commander/sunrise-commander" "\
Visits the given directory in sr-mode.

\(fn DIRECTORY &optional SWITCHES)" t nil)

;;;***

;;;### (autoloads (resume-windows see-you-again win-load-all-configurations
;;;;;;  wipe-windows win-save-all-configurations win:startup-with-window
;;;;;;  win:set-wc win-switch-to-window) "windows/windows" "windows/windows.el"
;;;;;;  (19789 9857))
;;; Generated autoloads from windows/windows.el

(autoload 'win-switch-to-window "windows/windows" "\
Switch window configurations to a buffer specified by keyboard.
If calling from program, optional second argument WINDOW can specify
the window number.

\(fn ARG &optional WINDOW)" t nil)

(autoload 'win:set-wc "windows/windows" "\
(Windows low level internal) Set the NUM-th windows configuration.
If Windows uses frame(Emacs 19), Select the NUM-th window frame.

\(fn NUM)" nil nil)

(autoload 'win:startup-with-window "windows/windows" "\
Start up Emacs with window[1] selected.

\(fn)" nil nil)

(autoload 'win-save-all-configurations "windows/windows" "\
Save all window configurations into the configuration file.

\(fn)" t nil)

(autoload 'wipe-windows "windows/windows" "\
Kill all buffers.  Optional argument NO-ASK non-nil skips query.

\(fn &optional NO-ASK)" t nil)

(autoload 'win-load-all-configurations "windows/windows" "\
Load all window configurations from the configuration file.
Non-nil for optional argument PRESERVE keeps all current buffers.

\(fn &optional PRESERVE)" t nil)

(autoload 'see-you-again "windows/windows" "\
Save all of the window configurations and kill-emacs.

\(fn)" t nil)

(autoload 'resume-windows "windows/windows" "\
Restore all window configurations reading configurations from a file.
Non-nil for optional argument PRESERVE keeps current buffers.

\(fn &optional PRESERVE)" t nil)

;;;***

;;;### (autoloads (zencoding-preview zencoding-expand-yas zencoding-mode
;;;;;;  zencoding-expand-line) "zencoding-mode/zencoding-mode" "zencoding-mode/zencoding-mode.el"
;;;;;;  (19790 26166))
;;; Generated autoloads from zencoding-mode/zencoding-mode.el

(autoload 'zencoding-expand-line "zencoding-mode/zencoding-mode" "\
Replace the current line's zencode expression with the corresponding expansion.
If prefix ARG is given or region is visible call `zencoding-preview' to start an
interactive preview.

Otherwise expand line directly.

For more information see `zencoding-mode'.

\(fn ARG)" t nil)

(autoload 'zencoding-mode "zencoding-mode/zencoding-mode" "\
Minor mode for writing HTML and CSS markup.
With zen coding for HTML and CSS you can write a line like

  ul#name>li.item*2

and have it expanded to

  <ul id=\"name\">
    <li class=\"item\"></li>
    <li class=\"item\"></li>
  </ul>

This minor mode defines keys for quick access:

\\{zencoding-mode-keymap}

Home page URL `http://www.emacswiki.org/emacs/ZenCoding'.

See also `zencoding-expand-line'.

\(fn &optional ARG)" t nil)

(autoload 'zencoding-expand-yas "zencoding-mode/zencoding-mode" "\
Not documented

\(fn)" t nil)

(autoload 'zencoding-preview "zencoding-mode/zencoding-mode" "\
Expand zencode between BEG and END interactively.
This will show a preview of the expanded zen code and you can
accept it or skip it.

\(fn BEG END)" t nil)

;;;***

;;;### (autoloads nil nil ("ac-anything/ac-anything.el" "anything-c-shell-history/anything-c-shell-history.el"
;;;;;;  "anything-complete/anything-complete.el" "anything-config/anything-match-plugin.el"
;;;;;;  "anything-config/anything-startup.el" "anything-dabbrev-expand/anything-dabbrev-expand.el"
;;;;;;  "anything-grep/anything-grep.el" "anything-gtags/anything-gtags.el"
;;;;;;  "anything-include/anything-include.el" "anything-ipa/anything-ipa.el"
;;;;;;  "anything-kyr/anything-kyr.el" "anything-menu/anything-menu.el"
;;;;;;  "anything-migemo/anything-migemo.el" "anything-slime/anything-slime.el"
;;;;;;  "anything-yaetags/anything-yaetags.el" "apel/site-lisp/apel/calist.el"
;;;;;;  "apel/site-lisp/apel/filename.el" "apel/site-lisp/apel/install.el"
;;;;;;  "apel/site-lisp/emu/apel-ver.el" "apel/site-lisp/emu/broken.el"
;;;;;;  "apel/site-lisp/emu/emu.el" "apel/site-lisp/emu/inv-23.el"
;;;;;;  "apel/site-lisp/emu/invisible.el" "apel/site-lisp/emu/mcharset.el"
;;;;;;  "apel/site-lisp/emu/mcs-20.el" "apel/site-lisp/emu/mcs-e20.el"
;;;;;;  "apel/site-lisp/emu/mule-caesar.el" "apel/site-lisp/emu/pccl-20.el"
;;;;;;  "apel/site-lisp/emu/pccl.el" "apel/site-lisp/emu/pces-20.el"
;;;;;;  "apel/site-lisp/emu/pces-e20.el" "apel/site-lisp/emu/pces.el"
;;;;;;  "apel/site-lisp/emu/pcustom.el" "apel/site-lisp/emu/poe.el"
;;;;;;  "apel/site-lisp/emu/poem-e20.el" "apel/site-lisp/emu/poem-e20_3.el"
;;;;;;  "apel/site-lisp/emu/poem.el" "apel/site-lisp/emu/product.el"
;;;;;;  "apel/site-lisp/emu/pym.el" "apel/site-lisp/emu/static.el"
;;;;;;  "autocomplete/auto-complete-config.el" "autocomplete/auto-complete.el"
;;;;;;  "autocomplete/fuzzy.el" "autocomplete/popup.el" "eclim/company-emacs-eclim.el"
;;;;;;  "eclim/eclim-ant.el" "eclim/eclim-java.el" "eclim/eclim-maven.el"
;;;;;;  "eclim/eclim-problems.el" "eclim/eclim-project.el" "eclim/eclim.el"
;;;;;;  "elscreen/elscreen.el" "highlight-cl/highlight-cl.el" "ibuffer-git/ibuffer-git.el"
;;;;;;  "ipa/ipa.el" "ppindent/ppindent.el" "pymacs/pymacs.el" "qmake-mode/qmake.el"
;;;;;;  "rcirc-color/rcirc-color.el" "rcirc-controls/rcirc-controls.el"
;;;;;;  "redo+/redo+.el" "rw-ispell/rw-ispell.el" "rw-language-and-country-codes/rw-language-and-country-codes.el"
;;;;;;  "ssh-config/ssh-config.el" "unicad/unicad.el" "vim-mode/vim-defs.el"
;;;;;;  "vim-mode/vim-ex-commands.el" "vim-mode/vim-macs.el" "vim-mode/vim-scroll.el"
;;;;;;  "vim-mode/vim-undo.el" "vim-mode/vim-window.el" "xcscope+/xcscope+.el"
;;;;;;  "zencoding-mode/zencoding-trie.el") (19791 63091 374021))

;;;***

(provide '.loaddefs)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; .loaddefs.el ends here
