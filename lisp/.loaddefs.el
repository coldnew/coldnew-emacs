;;; .loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (anything-other-buffer anything-at-point anything)
;;;;;;  "anything-config/anything" "anything-config/anything.el"
;;;;;;  (19882 36530))
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

;;;### (autoloads (anything-c-reset-adaptative-history anything-c-set-variable
;;;;;;  anything-c-call-interactively w32-shell-execute-open-file
;;;;;;  anything-ratpoison-commands anything-c-run-external-command
;;;;;;  anything-c-shell-command-if-needed anything-apt anything-world-time
;;;;;;  anything-select-xfont anything-top anything-create anything-execute-anything-command
;;;;;;  anything-call-source anything-surfraw anything-calcul-expression
;;;;;;  anything-eval-expression-with-eldoc anything-eval-expression
;;;;;;  anything-yaoddmuse-emacswiki-post-library anything-yaoddmuse-emacswiki-edit-or-view
;;;;;;  anything-yaoddmuse-cache-pages anything-all-mark-rings anything-global-mark-ring
;;;;;;  anything-mark-ring anything-simple-call-tree anything-bookmark-ext
;;;;;;  anything-manage-advice anything-M-x anything-filelist+ anything-filelist
;;;;;;  anything-yank-text-at-point anything-c-goto-next-file anything-c-goto-precedent-file
;;;;;;  anything-do-grep anything-dired-bindings anything-dired-hardlink-file
;;;;;;  anything-dired-symlink-file anything-dired-copy-file anything-dired-rename-file
;;;;;;  anything-insert-file anything-write-file anything-find-files
;;;;;;  anything-ff-run-gnus-attach-files anything-ff-run-open-file-externally
;;;;;;  anything-ff-run-switch-other-frame anything-ff-run-switch-other-window
;;;;;;  anything-ff-run-switch-to-eshell anything-ff-run-complete-fn-at-point
;;;;;;  anything-ff-run-delete-file anything-ff-run-symlink-file
;;;;;;  anything-ff-run-ediff-merge-file anything-ff-run-ediff-file
;;;;;;  anything-ff-run-eshell-command-on-file anything-ff-run-load-file
;;;;;;  anything-ff-run-byte-compile-file anything-ff-run-rename-file
;;;;;;  anything-ff-run-copy-file anything-ff-run-grep anything-ff-run-switch-to-history
;;;;;;  anything-buffer-switch-to-elscreen anything-buffer-switch-other-frame
;;;;;;  anything-buffer-switch-other-window anything-buffer-run-query-replace-regexp
;;;;;;  anything-buffer-run-grep anything-buffer-run-kill-buffers
;;;;;;  anything-buffer-save-persistent anything-buffer-revert-persistent
;;;;;;  anything-buffer-diff-persistent anything-toggle-all-marks
;;;;;;  anything-unmark-all anything-mark-all anything-regexp anything-kill-buffers
;;;;;;  anything-org-headlines anything-browse-code anything-occur
;;;;;;  anything-list-emacs-process anything-timers anything-bm-list
;;;;;;  anything-eev-anchors anything-emms anything-org-keywords
;;;;;;  anything-man-woman anything-register anything-c-insert-latex-math
;;;;;;  anything-c-pp-bookmarks anything-bookmarks anything-colors
;;;;;;  anything-firefox-bookmarks anything-w3m-bookmarks anything-locate
;;;;;;  anything-bbdb anything-buffers+ anything-for-buffers anything-yahoo-suggest
;;;;;;  anything-google-suggest anything-imenu anything-gentoo anything-minibuffer-history
;;;;;;  anything-show-kill-ring anything-info-emacs anything-info-at-point
;;;;;;  anything-recentf anything-for-files anything-mini anything-configuration)
;;;;;;  "anything-config/anything-config" "anything-config/anything-config.el"
;;;;;;  (19882 36530))
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
With a prefix-arg insert symbol at point.

\(fn ARG)" t nil)

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

You can specify a specific database with prefix argument ARG (C-u).
Many databases can be used: navigate and mark them.

To create a user specific db, use
\"updatedb -l 0 -o db_path -U directory\".

See man locate for more infos.

\(fn ARG)" t nil)

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

(autoload 'anything-mark-all "anything-config/anything-config" "\
Mark all visible unmarked candidates in current source.

\(fn)" t nil)

(autoload 'anything-unmark-all "anything-config/anything-config" "\
Unmark all candidates in all sources of current anything session.

\(fn)" t nil)

(autoload 'anything-toggle-all-marks "anything-config/anything-config" "\
Toggle all marks.
Mark all visible candidates of current source or unmark all candidates
visible or invisible in all sources of current anything session

\(fn)" t nil)

(autoload 'anything-buffer-diff-persistent "anything-config/anything-config" "\
Toggle diff buffer without quitting anything.

\(fn)" t nil)

(autoload 'anything-buffer-revert-persistent "anything-config/anything-config" "\
Revert buffer without quitting anything.

\(fn)" t nil)

(autoload 'anything-buffer-save-persistent "anything-config/anything-config" "\
Save buffer without quitting anything.

\(fn)" t nil)

(autoload 'anything-buffer-run-kill-buffers "anything-config/anything-config" "\
Run kill buffer action from `anything-c-source-buffer+'.

\(fn)" t nil)

(autoload 'anything-buffer-run-grep "anything-config/anything-config" "\
Run Grep action from `anything-c-source-buffer+'.

\(fn)" t nil)

(autoload 'anything-buffer-run-query-replace-regexp "anything-config/anything-config" "\
Run Query replace regexp action from `anything-c-source-buffer+'.

\(fn)" t nil)

(autoload 'anything-buffer-switch-other-window "anything-config/anything-config" "\
Run switch to other window action from `anything-c-source-buffer+'.

\(fn)" t nil)

(autoload 'anything-buffer-switch-other-frame "anything-config/anything-config" "\
Run switch to other frame action from `anything-c-source-buffer+'.

\(fn)" t nil)

(autoload 'anything-buffer-switch-to-elscreen "anything-config/anything-config" "\
Run switch to elscreen  action from `anything-c-source-buffer+'.

\(fn)" t nil)

(autoload 'anything-ff-run-switch-to-history "anything-config/anything-config" "\
Run Switch to history action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-grep "anything-config/anything-config" "\
Run Grep action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-copy-file "anything-config/anything-config" "\
Run Copy file action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-rename-file "anything-config/anything-config" "\
Run Rename file action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-byte-compile-file "anything-config/anything-config" "\
Run Byte compile file action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-load-file "anything-config/anything-config" "\
Run Load file action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-eshell-command-on-file "anything-config/anything-config" "\
Run eshell command on file action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-ediff-file "anything-config/anything-config" "\
Run Ediff file action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-ediff-merge-file "anything-config/anything-config" "\
Run Ediff merge file action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-symlink-file "anything-config/anything-config" "\
Run Symlink file action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-delete-file "anything-config/anything-config" "\
Run Delete file action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-complete-fn-at-point "anything-config/anything-config" "\
Run complete file name action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-switch-to-eshell "anything-config/anything-config" "\
Run switch to eshell action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-switch-other-window "anything-config/anything-config" "\
Run switch to other window action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-switch-other-frame "anything-config/anything-config" "\
Run switch to other frame action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-open-file-externally "anything-config/anything-config" "\
Run open file externally command action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-gnus-attach-files "anything-config/anything-config" "\
Run gnus attach files command action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-find-files "anything-config/anything-config" "\
Preconfigured `anything' for anything implementation of `find-file'.
Called with a prefix arg show history if some.
Don't call it from programs, use `anything-find-files1' instead.
This is the starting point for nearly all actions you can do on files.

\(fn ARG)" t nil)

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
The prefix arg can be passed before or after start.
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

See `anything-c-filelist-file-name' docstring for usage.

\(fn)" t nil)

(autoload 'anything-filelist+ "anything-config/anything-config" "\
Preconfigured `anything' to open files/buffers/bookmarks instantly.

This is a replacement for `anything-for-files'.
See `anything-c-filelist-file-name' docstring for usage.

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

(autoload 'anything-calcul-expression "anything-config/anything-config" "\
Preconfigured anything for `anything-c-source-calculation-result'.

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


\(fn FILE)" t nil)

(autoload 'anything-c-call-interactively "anything-config/anything-config" "\
Execute CMD-OR-NAME as Emacs command.
It is added to `extended-command-history'.
`anything-current-prefix-arg' is used as the command's prefix argument.

\(fn CMD-OR-NAME)" nil nil)

(autoload 'anything-c-set-variable "anything-config/anything-config" "\
Set value to VAR interactively.

\(fn VAR)" t nil)

(autoload 'anything-c-reset-adaptative-history "anything-config/anything-config" "\
Delete all `anything-c-adaptive-history' and his file.
Useful when you have a old or corrupted `anything-c-adaptive-history-file'.

\(fn)" t nil)

;;;***

;;;### (autoloads (vassoc set-modified-alist modify-alist remove-alist
;;;;;;  set-alist del-alist put-alist) "apel/site-lisp/apel/alist"
;;;;;;  "apel/site-lisp/apel/alist.el" (17099 15668))
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
;;;;;;  "apel/site-lisp/apel/path-util.el" (17099 15668))
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
;;;;;;  "apel/site-lisp/emu/richtext.el" (17099 15669))
;;; Generated autoloads from apel/site-lisp/emu/richtext.el

(autoload 'richtext-encode "apel/site-lisp/emu/richtext" "\
Not documented

\(fn FROM TO)" nil nil)

(autoload 'richtext-decode "apel/site-lisp/emu/richtext" "\
Not documented

\(fn FROM TO)" nil nil)

;;;***

;;;### (autoloads (turn-on-bib-cite bib-cite-minor-mode) "auctex/bib-cite"
;;;;;;  "auctex/bib-cite.el" (18341 54637))
;;; Generated autoloads from auctex/bib-cite.el

(autoload 'bib-cite-minor-mode "auctex/bib-cite" "\
Toggle bib-cite mode.
When bib-cite mode is enabled, citations, labels and refs are highlighted
when the mouse is over them.  Clicking on these highlights with [mouse-2]
runs bib-find, and [mouse-3] runs bib-display.

\(fn ARG)" t nil)

(autoload 'turn-on-bib-cite "auctex/bib-cite" "\
Unconditionally turn on Bib Cite mode.

\(fn)" nil nil)

;;;***

;;;### (autoloads (context-mode) "auctex/context" "auctex/context.el"
;;;;;;  (19707 64211))
;;; Generated autoloads from auctex/context.el

(defalias 'ConTeXt-mode 'context-mode)

(autoload 'context-mode "auctex/context" "\
Major mode in AUCTeX for editing ConTeXt files.

Special commands:
\\{ConTeXt-mode-map}

Entering `context-mode' calls the value of `text-mode-hook',
then the value of `TeX-mode-hook', and then the value
of context-mode-hook.

\(fn)" t nil)

;;;***

;;;### (autoloads (context-en-mode) "auctex/context-en" "auctex/context-en.el"
;;;;;;  (19618 16518))
;;; Generated autoloads from auctex/context-en.el

(autoload 'context-en-mode "auctex/context-en" "\
Major mode for editing files for ConTeXt using its english interface.

Special commands:
\\{ConTeXt-mode-map}

Entering `context-mode' calls the value of `text-mode-hook',
then the value of TeX-mode-hook, and then the value
of context-mode-hook.

\(fn)" t nil)

;;;***

;;;### (autoloads (context-nl-mode) "auctex/context-nl" "auctex/context-nl.el"
;;;;;;  (19618 16529))
;;; Generated autoloads from auctex/context-nl.el

(autoload 'context-nl-mode "auctex/context-nl" "\
Major mode for editing files for ConTeXt using its dutch interface.

Special commands:
\\{ConTeXt-mode-map}

Entering `context-mode' calls the value of `text-mode-hook',
then the value of TeX-mode-hook, and then the value
of context-mode-hook.

\(fn)" t nil)

;;;***

;;;### (autoloads (font-latex-setup) "auctex/font-latex" "auctex/font-latex.el"
;;;;;;  (19596 41439))
;;; Generated autoloads from auctex/font-latex.el

(autoload 'font-latex-setup "auctex/font-latex" "\
Setup this buffer for LaTeX font-lock.  Usually called from a hook.

\(fn)" nil nil)

;;;***

;;;### (autoloads (docTeX-mode TeX-latex-mode BibTeX-auto-store)
;;;;;;  "auctex/latex" "auctex/latex.el" (19818 30245))
;;; Generated autoloads from auctex/latex.el

(autoload 'BibTeX-auto-store "auctex/latex" "\
This function should be called from `bibtex-mode-hook'.
It will setup BibTeX to store keys in an auto file.

\(fn)" nil nil)

(add-to-list 'auto-mode-alist '("\\.drv\\'" . latex-mode))

(autoload 'TeX-latex-mode "auctex/latex" "\
Major mode in AUCTeX for editing LaTeX files.
See info under AUCTeX for full documentation.

Special commands:
\\{LaTeX-mode-map}

Entering LaTeX mode calls the value of `text-mode-hook',
then the value of `TeX-mode-hook', and then the value
of `LaTeX-mode-hook'.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.dtx\\'" . doctex-mode))

(autoload 'docTeX-mode "auctex/latex" "\
Major mode in AUCTeX for editing .dtx files derived from `LaTeX-mode'.
Runs `LaTeX-mode', sets a few variables and
runs the hooks in `docTeX-mode-hook'.

\(fn)" t nil)

(defalias 'TeX-doctex-mode 'docTeX-mode)

;;;***

;;;### (autoloads (multi-prompt-key-value multi-prompt) "auctex/multi-prompt"
;;;;;;  "auctex/multi-prompt.el" (18915 28236))
;;; Generated autoloads from auctex/multi-prompt.el

(autoload 'multi-prompt "auctex/multi-prompt" "\
Completing prompt for a list of strings.
The first argument SEPARATOR should be the string (of length 1) to
separate the elements in the list.  The second argument UNIQUE should
be non-nil, if each element must be unique.  The remaining elements
are the arguments to `completing-read'.  See that.

\(fn SEPARATOR UNIQUE PROMPT TABLE &optional MP-PREDICATE REQUIRE-MATCH INITIAL HISTORY)" nil nil)

(autoload 'multi-prompt-key-value "auctex/multi-prompt" "\
Read multiple strings, with completion and key=value support.
PROMPT is a string to prompt with, usually ending with a colon
and a space.  TABLE is an alist.  The car of each element should
be a string representing a key and the optional cdr should be a
list with strings to be used as values for the key.

See the documentation for `completing-read' for details on the
other arguments: PREDICATE, REQUIRE-MATCH, INITIAL-INPUT, HIST,
DEF, and INHERIT-INPUT-METHOD.

The return value is the string as entered in the minibuffer.

\(fn PROMPT TABLE &optional PREDICATE REQUIRE-MATCH INITIAL-INPUT HIST DEF INHERIT-INPUT-METHOD)" nil nil)

;;;***

;;;### (autoloads (ams-tex-mode TeX-plain-tex-mode) "auctex/plain-tex"
;;;;;;  "auctex/plain-tex.el" (19707 64159))
;;; Generated autoloads from auctex/plain-tex.el

(autoload 'TeX-plain-tex-mode "auctex/plain-tex" "\
Major mode in AUCTeX for editing plain TeX files.
See info under AUCTeX for documentation.

Special commands:
\\{plain-TeX-mode-map}

Entering `plain-tex-mode' calls the value of `text-mode-hook',
then the value of `TeX-mode-hook', and then the value
of plain-TeX-mode-hook.

\(fn)" t nil)

(autoload 'ams-tex-mode "auctex/plain-tex" "\
Major mode in AUCTeX for editing AmS-TeX files.
See info under AUCTeX for documentation.

Special commands:
\\{AmSTeX-mode-map}

Entering AmS-tex-mode calls the value of `text-mode-hook',
then the value of `TeX-mode-hook', and then the value
of `AmS-TeX-mode-hook'.

\(fn)" t nil)

;;;***

;;;### (autoloads (preview-report-bug LaTeX-preview-setup preview-install-styles)
;;;;;;  "auctex/preview/preview" "auctex/preview/preview.el" (19772
;;;;;;  31043))
;;; Generated autoloads from auctex/preview/preview.el

(autoload 'preview-install-styles "auctex/preview/preview" "\
Installs the TeX style files into a permanent location.
This must be in the TeX search path.  If FORCE-OVERWRITE is greater
than 1, files will get overwritten without query, if it is less
than 1 or nil, the operation will fail.  The default of 1 for interactive
use will query.

Similarly FORCE-SAVE can be used for saving
`preview-TeX-style-dir' to record the fact that the uninstalled
files are no longer needed in the search path.

\(fn DIR &optional FORCE-OVERWRITE FORCE-SAVE)" t nil)

(autoload 'LaTeX-preview-setup "auctex/preview/preview" "\
Hook function for embedding the preview package into AUCTeX.
This is called by `LaTeX-mode-hook' and changes AUCTeX variables
to add the preview functionality.

\(fn)" nil nil)
(add-hook 'LaTeX-mode-hook #'LaTeX-preview-setup)

(autoload 'preview-report-bug "auctex/preview/preview" "\
Report a bug in the preview-latex package.

\(fn)" t nil)

;;;***

;;;### (autoloads (TeX-submit-bug-report TeX-auto-generate-global
;;;;;;  TeX-auto-generate TeX-tex-mode) "auctex/tex" "auctex/tex.el"
;;;;;;  (19781 37039))
;;; Generated autoloads from auctex/tex.el

(autoload 'TeX-tex-mode "auctex/tex" "\
Major mode in AUCTeX for editing TeX or LaTeX files.
Tries to guess whether this file is for plain TeX or LaTeX.

The algorithm is as follows:

   1) if the file is empty or `TeX-force-default-mode' is not set to nil,
      `TeX-default-mode' is chosen
   2) If \\documentstyle or \\begin{, \\section{, \\part{ or \\chapter{ is
      found, `latex-mode' is selected.
   3) Otherwise, use `plain-tex-mode'

\(fn)" t nil)

(autoload 'TeX-auto-generate "auctex/tex" "\
Generate style file for TEX and store it in AUTO.
If TEX is a directory, generate style files for all files in the directory.

\(fn TEX AUTO)" t nil)

(autoload 'TeX-auto-generate-global "auctex/tex" "\
Create global auto directory for global TeX macro definitions.

\(fn)" t nil)

(autoload 'TeX-submit-bug-report "auctex/tex" "\
Submit a bug report on AUCTeX via mail.

Don't hesitate to report any problems or inaccurate documentation.

If you don't have setup sending mail from (X)Emacs, please copy the
output buffer into your mail program, as it gives us important
information about your AUCTeX version and AUCTeX configuration.

\(fn)" t nil)

;;;***

;;;### (autoloads (LaTeX-install-toolbar TeX-install-toolbar) "auctex/tex-bar"
;;;;;;  "auctex/tex-bar.el" (18580 49499))
;;; Generated autoloads from auctex/tex-bar.el

(autoload 'TeX-install-toolbar "auctex/tex-bar" "\
Install toolbar buttons for TeX mode.

\(fn)" t nil)

(autoload 'LaTeX-install-toolbar "auctex/tex-bar" "\
Install toolbar buttons for LaTeX mode.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "auctex/tex-fold" "auctex/tex-fold.el" (19772
;;;;;;  30949))
;;; Generated autoloads from auctex/tex-fold.el
(autoload 'TeX-fold-mode "tex-fold" "Minor mode for hiding and revealing macros and environments." t)

(defalias 'tex-fold-mode 'TeX-fold-mode)

;;;***

;;;### (autoloads (tex-font-setup) "auctex/tex-font" "auctex/tex-font.el"
;;;;;;  (18341 54636))
;;; Generated autoloads from auctex/tex-font.el

(autoload 'tex-font-setup "auctex/tex-font" "\
Setup font lock support for TeX.

\(fn)" nil nil)

;;;***

;;;### (autoloads (TeX-texinfo-mode) "auctex/tex-info" "auctex/tex-info.el"
;;;;;;  (19514 6672))
;;; Generated autoloads from auctex/tex-info.el

(defalias 'Texinfo-mode 'texinfo-mode)

(autoload 'TeX-texinfo-mode "auctex/tex-info" "\
Major mode in AUCTeX for editing Texinfo files.

Special commands:
\\{Texinfo-mode-map}

Entering Texinfo mode calls the value of `text-mode-hook'  and then the
value of `Texinfo-mode-hook'.

\(fn)" t nil)

;;;***

;;;### (autoloads (japanese-latex-mode japanese-plain-tex-mode) "auctex/tex-jp"
;;;;;;  "auctex/tex-jp.el" (18768 5174))
;;; Generated autoloads from auctex/tex-jp.el

(autoload 'japanese-plain-tex-mode "auctex/tex-jp" "\
Major mode in AUCTeX for editing Japanese plain TeX files.
Set `japanese-TeX-mode' to t, and enter `TeX-plain-tex-mode'.

\(fn)" t nil)

(autoload 'japanese-latex-mode "auctex/tex-jp" "\
Major mode in AUCTeX for editing Japanese LaTeX files.
Set `japanese-TeX-mode' to t, and enter `TeX-latex-mode'.

\(fn)" t nil)

;;;***

;;;### (autoloads (texmathp-match-switch texmathp) "auctex/texmathp"
;;;;;;  "auctex/texmathp.el" (18489 3128))
;;; Generated autoloads from auctex/texmathp.el

(autoload 'texmathp "auctex/texmathp" "\
Determine if point is inside (La)TeX math mode.
Returns t or nil.  Additional info is placed into `texmathp-why'.
The functions assumes that you have (almost) syntactically correct (La)TeX in
the buffer.
See the variable `texmathp-tex-commands' about which commands are checked.

\(fn)" t nil)

(autoload 'texmathp-match-switch "auctex/texmathp" "\
Search backward for any of the math switches.
Limit searched to BOUND.

\(fn BOUND)" nil nil)

;;;***

;;;### (autoloads nil "auctex/toolbar-x" "auctex/toolbar-x.el" (18580
;;;;;;  49487))
;;; Generated autoloads from auctex/toolbar-x.el
(autoload 'toolbarx-install-toolbar "toolbar-x")

;;;***

;;;### (autoloads (cedet-update-autoloads) "cedet/common/cedet-autogen"
;;;;;;  "cedet/common/cedet-autogen.el" (19840 61079))
;;; Generated autoloads from cedet/common/cedet-autogen.el

(autoload 'cedet-update-autoloads "cedet/common/cedet-autogen" "\
Update autoloads in file LOADDEFS from sources.
Optional argument DIRECTORY, specifies the directory to scan for
autoloads.  It defaults to the current directory.
DIRECTORIES is a list of extra directory to scan.  Those directory
names are relative to DIRECTORY.  If DIRECTORIES is nil try to scan
sub directories of DIRECTORY where a `cedet-autogen-tagfile' file
exists.

\(fn LOADDEFS &optional DIRECTORY &rest DIRECTORIES)" t nil)

;;;***

;;;### (autoloads (cedet-compat-utest) "cedet/common/cedet-compat"
;;;;;;  "cedet/common/cedet-compat.el" (19840 61079))
;;; Generated autoloads from cedet/common/cedet-compat.el

(if (or (featurep 'xemacs) (inversion-test 'emacs "22.0")) (defalias 'cedet-split-string 'cedet-split-string-1) (defalias 'cedet-split-string 'split-string))

(when (not (fboundp 'with-no-warnings)) (put 'with-no-warnings 'lisp-indent-function 0) (defun with-no-warnings (&rest body) "Copied from `with-no-warnings' in Emacs 23.\nLike `progn', but prevents compiler warnings in the body.\nNote: Doesn't work if this version is being loaded." (car (last body))))

(autoload 'cedet-compat-utest "cedet/common/cedet-compat" "\
Test compatability functions.

\(fn)" t nil)

;;;***

;;;### (autoloads (cedet-cscope-version-check cedet-cscope-expand-filename
;;;;;;  cedet-cscope-search cedet-cscope-command) "cedet/common/cedet-cscope"
;;;;;;  "cedet/common/cedet-cscope.el" (19840 61079))
;;; Generated autoloads from cedet/common/cedet-cscope.el

(defvar cedet-cscope-command "cscope" "\
Command name for the CScope executable.")

(custom-autoload 'cedet-cscope-command "cedet/common/cedet-cscope" t)

(autoload 'cedet-cscope-search "cedet/common/cedet-cscope" "\
Perform a search with CScope, return the created buffer.
SEARCHTEXT is text to find.
TEXTTYPE is the type of text, such as 'regexp, 'string, 'tagname,
'tagregexp, or 'tagcompletions.
TYPE is the type of search, meaning that SEARCHTEXT is compared to
filename, tagname (tags table), references (uses of a tag) , or
symbol (uses of something not in the tag table.)
SCOPE is the scope of the search, such as 'project or 'subdirs.

\(fn SEARCHTEXT TEXTTYPE TYPE SCOPE)" nil nil)

(autoload 'cedet-cscope-expand-filename "cedet/common/cedet-cscope" "\
Expand the FILENAME with CScope.
Return a fully qualified filename.

\(fn FILENAME)" t nil)

(autoload 'cedet-cscope-version-check "cedet/common/cedet-cscope" "\
Check the version of the installed CScope command.
If optional programatic argument NOERROR is non-nil, then
instead of throwing an error if cscope isn't available, then
return nil.

\(fn &optional NOERROR)" t nil)

;;;***

;;;### (autoloads nil "cedet/common/cedet-edebug" "cedet/common/cedet-edebug.el"
;;;;;;  (19840 61079))
;;; Generated autoloads from cedet/common/cedet-edebug.el

(add-hook 'edebug-setup-hook (lambda nil (require 'cedet-edebug) (defalias 'edebug-prin1-to-string 'cedet-edebug-prin1-to-string) (define-key edebug-mode-map "A" 'data-debug-edebug-expr)))

(add-hook 'debugger-mode-hook (lambda nil (require 'cedet-edebug) (define-key debugger-mode-map "A" 'data-debug-edebug-expr)))

;;;***

;;;### (autoloads (cedet-files-utest) "cedet/common/cedet-files"
;;;;;;  "cedet/common/cedet-files.el" (19840 61079))
;;; Generated autoloads from cedet/common/cedet-files.el

(autoload 'cedet-files-utest "cedet/common/cedet-files" "\
Test out some file name conversions.

\(fn)" t nil)

;;;***

;;;### (autoloads (cedet-gnu-global-version-check cedet-gnu-global-root
;;;;;;  cedet-gnu-global-show-root cedet-gnu-global-expand-filename
;;;;;;  cedet-gnu-global-search cedet-global-gtags-command cedet-global-command)
;;;;;;  "cedet/common/cedet-global" "cedet/common/cedet-global.el"
;;;;;;  (19840 61079))
;;; Generated autoloads from cedet/common/cedet-global.el

(defvar cedet-global-command "global" "\
Command name for the GNU Global executable.")

(custom-autoload 'cedet-global-command "cedet/common/cedet-global" t)

(defvar cedet-global-gtags-command "gtags" "\
Command name for the GNU Global gtags executable.
GTAGS is used to create the tags table queried by the 'global' command.")

(custom-autoload 'cedet-global-gtags-command "cedet/common/cedet-global" t)

(autoload 'cedet-gnu-global-search "cedet/common/cedet-global" "\
Perform a search with GNU Global, return the created buffer.
SEARCHTEXT is text to find.
TEXTTYPE is the type of text, such as 'regexp, 'string, 'tagname,
'tagregexp, or 'tagcompletions.
TYPE is the type of search, meaning that SEARCHTEXT is compared to
filename, tagname (tags table), references (uses of a tag) , or
symbol (uses of something not in the tag table.)
SCOPE is the scope of the search, such as 'project or 'subdirs.

\(fn SEARCHTEXT TEXTTYPE TYPE SCOPE)" nil nil)

(autoload 'cedet-gnu-global-expand-filename "cedet/common/cedet-global" "\
Expand the FILENAME with GNU Global.
Return a fully qualified filename.

\(fn FILENAME)" t nil)

(autoload 'cedet-gnu-global-show-root "cedet/common/cedet-global" "\
Show the root of a GNU Global area under the current buffer.

\(fn)" t nil)

(autoload 'cedet-gnu-global-root "cedet/common/cedet-global" "\
Return the root of any GNU Global scanned project.
If a default starting DIR is not specified, the current buffer's
`default-directory' is used.

\(fn &optional DIR)" nil nil)

(autoload 'cedet-gnu-global-version-check "cedet/common/cedet-global" "\
Check the version of the installed GNU Global command.
If optional programatic argument NOERROR is non-nil, then
instead of throwing an error if Global isn't available, then
return nil.

\(fn &optional NOERROR)" t nil)

;;;***

;;;### (autoloads (cedet-graphviz-dot-version-check cedet-graphviz-neato-command
;;;;;;  cedet-graphviz-dot-command) "cedet/common/cedet-graphviz"
;;;;;;  "cedet/common/cedet-graphviz.el" (19840 61079))
;;; Generated autoloads from cedet/common/cedet-graphviz.el

(defvar cedet-graphviz-dot-command "dot" "\
Command name for the Graphviz DOT executable.")

(custom-autoload 'cedet-graphviz-dot-command "cedet/common/cedet-graphviz" t)

(defvar cedet-graphviz-neato-command "neato" "\
Command name for the Graphviz NEATO executable.")

(custom-autoload 'cedet-graphviz-neato-command "cedet/common/cedet-graphviz" t)

(autoload 'cedet-graphviz-dot-version-check "cedet/common/cedet-graphviz" "\
Check the version of the installed Graphviz dot command.
If optional programatic argument NOERROR is non-nil, then
instead of throwing an error if Global isn't available, then
return nil.

\(fn &optional NOERROR)" t nil)

;;;***

;;;### (autoloads (cedet-idutils-version-check cedet-idutils-expand-filename
;;;;;;  cedet-idutils-make-command cedet-idutils-token-command cedet-idutils-file-command)
;;;;;;  "cedet/common/cedet-idutils" "cedet/common/cedet-idutils.el"
;;;;;;  (19840 61079))
;;; Generated autoloads from cedet/common/cedet-idutils.el

(defvar cedet-idutils-file-command "fnid" "\
Command name for the ID Utils executable for searching file names.")

(custom-autoload 'cedet-idutils-file-command "cedet/common/cedet-idutils" t)

(defvar cedet-idutils-token-command "lid" "\
Command name for the ID Utils executable for searching for tokens.")

(custom-autoload 'cedet-idutils-token-command "cedet/common/cedet-idutils" t)

(defvar cedet-idutils-make-command "mkid" "\
Command name for the ID Utils executable for creating token databases.")

(custom-autoload 'cedet-idutils-make-command "cedet/common/cedet-idutils" t)

(autoload 'cedet-idutils-expand-filename "cedet/common/cedet-idutils" "\
Expand the FILENAME with ID Utils.
Return a filename relative to the default directory.

\(fn FILENAME)" t nil)

(autoload 'cedet-idutils-version-check "cedet/common/cedet-idutils" "\
Check the version of the installed ID Utils command.
If optional programatic argument NOERROR is non-nil, then
instead of throwing an error if Global isn't available, then
return nil.

\(fn &optional NOERROR)" t nil)

;;;***

;;;### (autoloads (global-cedet-m3-minor-mode cedet-m3-minor-mode)
;;;;;;  "cedet/common/cedet-m3" "cedet/common/cedet-m3.el" (19840
;;;;;;  61079))
;;; Generated autoloads from cedet/common/cedet-m3.el

(autoload 'cedet-m3-minor-mode "cedet/common/cedet-m3" "\
Toggle cedet-m3 minor mode, a mouse 3 context menu.
With prefix argument ARG, turn on if positive, otherwise off.  The
minor mode can be turned on only if semantic feature is available and
the current buffer was set up for parsing.  Return non-nil if the
minor mode is enabled.

\\{cedet-m3-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'global-cedet-m3-minor-mode "cedet/common/cedet-m3" "\
Toggle global use of cedet-m3 minor mode.
If ARG is positive, enable, if it is negative, disable.
If ARG is nil, then toggle.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (cedet-utest-batch cedet-utest) "cedet/common/cedet-utests"
;;;;;;  "cedet/common/cedet-utests.el" (19840 61079))
;;; Generated autoloads from cedet/common/cedet-utests.el

(autoload 'cedet-utest "cedet/common/cedet-utests" "\
Run the CEDET unittests.
EXIT-ON-ERROR causes the test suite to exit on an error, instead
of just logging the error.

\(fn &optional EXIT-ON-ERROR)" t nil)

(autoload 'cedet-utest-batch "cedet/common/cedet-utests" "\
Run the CEDET unit test in BATCH mode.

\(fn)" nil nil)

;;;***

;;;### (autoloads (data-debug-eval-expression data-debug-edebug-expr
;;;;;;  data-debug-show-stuff data-debug-new-buffer data-debug-mode
;;;;;;  data-debug-insert-thing data-debug-insert-stuff-vector data-debug-insert-stuff-list
;;;;;;  data-debug-insert-widget-properties data-debug-insert-hash-table
;;;;;;  data-debug-insert-property-list) "cedet/common/data-debug"
;;;;;;  "cedet/common/data-debug.el" (19840 61079))
;;; Generated autoloads from cedet/common/data-debug.el

(autoload 'data-debug-insert-property-list "cedet/common/data-debug" "\
Insert the property list PROPLIST.
Each line starts with PREFIX.
The attributes belong to the tag PARENT.

\(fn PROPLIST PREFIX &optional PARENT)" nil nil)

(autoload 'data-debug-insert-hash-table "cedet/common/data-debug" "\
Insert the contents of HASH-TABLE inserting PREFIX before each element.

\(fn HASH-TABLE PREFIX)" nil nil)

(autoload 'data-debug-insert-widget-properties "cedet/common/data-debug" "\
Insert the contents of WIDGET inserting PREFIX before each element.

\(fn WIDGET PREFIX)" nil nil)

(autoload 'data-debug-insert-stuff-list "cedet/common/data-debug" "\
Insert all the parts of STUFFLIST.
PREFIX specifies what to insert at the start of each line.

\(fn STUFFLIST PREFIX)" nil nil)

(autoload 'data-debug-insert-stuff-vector "cedet/common/data-debug" "\
Insert all the parts of STUFFVECTOR.
PREFIX specifies what to insert at the start of each line.

\(fn STUFFVECTOR PREFIX)" nil nil)

(autoload 'data-debug-insert-thing "cedet/common/data-debug" "\
Insert THING with PREFIX.
PREBUTTONTEXT is some text to insert between prefix and the thing
that is not included in the indentation calculation of any children.
If PARENT is non-nil, it is somehow related as a parent to thing.

\(fn THING PREFIX PREBUTTONTEXT &optional PARENT)" nil nil)

(autoload 'data-debug-mode "cedet/common/data-debug" "\
Major-mode for the Analyzer debugger.

\\{data-debug-map}

\(fn)" t nil)

(autoload 'data-debug-new-buffer "cedet/common/data-debug" "\
Create a new ddebug buffer with NAME.

\(fn NAME)" nil nil)

(autoload 'data-debug-show-stuff "cedet/common/data-debug" "\
Data debug STUFF in a buffer named *NAME DDebug*.

\(fn STUFF NAME)" nil nil)

(autoload 'data-debug-edebug-expr "cedet/common/data-debug" "\
Dump out the contents of some expression EXPR in edebug with ddebug.

\(fn EXPR)" t nil)

(autoload 'data-debug-eval-expression "cedet/common/data-debug" "\
Evaluate EXPR and display the value.
If the result is something simple, show it in the echo area.
If the result is a list or vector, then use the data debugger to display it.

\(fn EXPR)" t nil)

;;;***

;;;### (autoloads (define-fame-channel) "cedet/common/fame" "cedet/common/fame.el"
;;;;;;  (19840 61079))
;;; Generated autoloads from cedet/common/fame.el

(autoload 'define-fame-channel "cedet/common/fame" "\
Define the new message channel CHANNEL.
CHANNEL must be a non-nil symbol.
The optional argument DEFAULT specifies the default value of message
levels for this channel.  By default it is the value of
`fame-default-level-values'.
DOCSTRING is an optional channel documentation.

This defines the option `CHANNEL-fame-levels' to customize the current
value of message levels.  And the functions `CHANNEL-send-debug',
`CHANNEL-send-info', `CHANNEL-send-warning', and `CHANNEL-send-error',
that respectively send debug, informational, warning, and error
messages to CHANNEL.

\(fn CHANNEL &optional DEFAULT DOCSTRING)" nil (quote macro))

;;;***

;;;### (autoloads (inversion-upgrade-package inversion-add-to-load-path
;;;;;;  inversion-find-version inversion-require-emacs inversion-require)
;;;;;;  "cedet/common/inversion" "cedet/common/inversion.el" (19840
;;;;;;  61079))
;;; Generated autoloads from cedet/common/inversion.el

(autoload 'inversion-require "cedet/common/inversion" "\
Declare that you need PACKAGE with at least VERSION.
PACKAGE might be found in FILE.  (See `require'.)
Throws an error if VERSION is incompatible with what is installed.
Optional argument DIRECTORY is a location where new versions of
this tool can be located.  If there is a versioning problem and
DIRECTORY is provided, inversion will offer to download the file.
Optional argument RESERVED is saved for later use.

\(fn PACKAGE VERSION &optional FILE DIRECTORY &rest RESERVED)" nil nil)

(autoload 'inversion-require-emacs "cedet/common/inversion" "\
Declare that you need either EMACS-VER, or XEMACS-VER.
Only checks one based on which kind of Emacs is being run.

\(fn EMACS-VER XEMACS-VER)" nil nil)

(autoload 'inversion-find-version "cedet/common/inversion" "\
Search for the version and incompatible version of PACKAGE.
Does not load PACKAGE nor requires that it has been previously loaded.
Search in the directories in `load-path' for a PACKAGE.el library.
Visit the file found and search for the declarations of variables or
constants `PACKAGE-version' and `PACKAGE-incompatible-version'.  The
value of these variables must be a version string.

Return a pair (VERSION-STRING . INCOMPATIBLE-VERSION-STRING) where
INCOMPATIBLE-VERSION-STRING can be nil.
Return nil when VERSION-STRING was not found.

\(fn PACKAGE)" nil nil)

(autoload 'inversion-add-to-load-path "cedet/common/inversion" "\
Add the PACKAGE path to `load-path' if necessary.
MINIMUM is the minimum version requirement of PACKAGE.
Optional argument INSTALLDIR is the base directory where PACKAGE is
installed.  It defaults to `default-directory'/PACKAGE.
SUBDIRS are sub-directories to add to `load-path', following the main
INSTALLDIR path.

\(fn PACKAGE MINIMUM &optional INSTALLDIR &rest SUBDIRS)" nil nil)

(autoload 'inversion-upgrade-package "cedet/common/inversion" "\
Try to upgrade PACKAGE in DIRECTORY is available.

\(fn PACKAGE &optional DIRECTORY)" t nil)

;;;***

;;;### (autoloads (mode-local-read-function) "cedet/common/mode-local"
;;;;;;  "cedet/common/mode-local.el" (19840 61079))
;;; Generated autoloads from cedet/common/mode-local.el

(autoload 'mode-local-read-function "cedet/common/mode-local" "\
Interactively read in the name of a mode-local function.
PROMPT, INITIAL, HIST, and DEFAULT are the same as for `completing-read'.

\(fn PROMPT &optional INITIAL HIST DEFAULT)" nil nil)

;;;***

;;;### (autoloads (pprint-function pprint pprint-to-string) "cedet/common/pprint"
;;;;;;  "cedet/common/pprint.el" (19840 61079))
;;; Generated autoloads from cedet/common/pprint.el

(autoload 'pprint-to-string "cedet/common/pprint" "\
Return a string containing the pretty-printed representation of OBJECT.
OBJECT can be any Lisp object.  Quoting characters are used as needed
to make output that `read' can handle, whenever this is possible.  The
pretty printer try as much as possible to limit the length of lines to
given WIDTH.  WIDTH value defaults to `fill-column'.

\(fn OBJECT &optional WIDTH)" nil nil)

(autoload 'pprint "cedet/common/pprint" "\
Output the pretty-printed representation of OBJECT, any Lisp object.
Quoting characters are printed as needed to make output that `read'
can handle, whenever this is possible.  Output stream is STREAM, or
value of `standard-output' (which see).  The pretty printer try as
much as possible to limit the length of lines to given WIDTH.  WIDTH
value defaults to `fill-column'.

\(fn OBJECT &optional STREAM WIDTH)" nil nil)

(autoload 'pprint-function "cedet/common/pprint" "\
See a pretty-printed representation of FUNCTION-NAME.

\(fn FUNCTION-NAME)" t nil)

;;;***

;;;### (autoloads (pulse-line-hook-function pulse-toggle-integration-advice
;;;;;;  pulse-momentary-highlight-region pulse-momentary-highlight-one-line
;;;;;;  pulse-momentary-highlight-overlay pulse-test pulse) "cedet/common/pulse"
;;;;;;  "cedet/common/pulse.el" (19840 61079))
;;; Generated autoloads from cedet/common/pulse.el

(autoload 'pulse "cedet/common/pulse" "\
Pulse the colors on our highlight face.
If optional FACE is provide, reset the face to FACE color,
instead of `pulse-highlight-start-face'.
Be sure to call `pulse-reset-face' after calling pulse.

\(fn &optional FACE)" nil nil)

(autoload 'pulse-test "cedet/common/pulse" "\
Test the lightening function for pulsing a line.
When optional NO-ERROR Don't throw an error if we can't run tests.

\(fn &optional NO-ERROR)" t nil)

(autoload 'pulse-momentary-highlight-overlay "cedet/common/pulse" "\
Pulse the overlay O, unhighlighting before next command.
Optional argument FACE specifies the fact to do the highlighting.

\(fn O &optional FACE)" nil nil)

(autoload 'pulse-momentary-highlight-one-line "cedet/common/pulse" "\
Highlight the line around POINT, unhighlighting before next command.
Optional argument FACE specifies the face to do the highlighting.

\(fn POINT &optional FACE)" nil nil)

(autoload 'pulse-momentary-highlight-region "cedet/common/pulse" "\
Highlight between START and END, unhighlighting before next command.
Optional argument FACE specifies the fact to do the highlighting.

\(fn START END &optional FACE)" nil nil)

(autoload 'pulse-toggle-integration-advice "cedet/common/pulse" "\
Toggle activation of advised functions that will now pulse.
Wint no ARG, toggle the pulse advice.
With a negative ARG, disable pulse advice.
With a positive ARG, enable pulse advice.
Currently advised functions include:
  `goto-line'
  `exchange-point-and-mark'
  `find-tag'
  `tags-search'
  `tags-loop-continue'
  `pop-tag-mark'
  `imenu-default-goto-function'
Pulsing via `pulse-line-hook-function' has also been added to
the following hook:
  `next-error-hook'

\(fn ARG)" t nil)

(autoload 'pulse-line-hook-function "cedet/common/pulse" "\
Function used in hooks to pulse the current line.
Only pulses the line if `pulse-command-advice-flag' is non-nil.

\(fn)" nil nil)

;;;***

;;;### (autoloads (diminished-modes diminish-undo diminish) "diminish/diminish"
;;;;;;  "diminish/diminish.el" (19839 8400))
;;; Generated autoloads from diminish/diminish.el

(autoload 'diminish "diminish/diminish" "\
Diminish mode-line display of minor mode MODE to TO-WHAT (default \"\").

Interactively, enter (with completion) the name of any minor mode, followed
on the next line by what you want it diminished to (default empty string).
The response to neither prompt should be quoted.  However, in Lisp code,
both args must be quoted, the first as a symbol, the second as a string,
as in (diminish 'jiggle-mode \" Jgl\").

The mode-line displays of minor modes usually begin with a space, so
the modes' names appear as separate words on the mode line.  However, if
you're having problems with a cramped mode line, you may choose to use single
letters for some modes, without leading spaces.  Capitalizing them works
best; if you then diminish some mode to \"X\" but have abbrev-mode enabled as
well, you'll get a display like \"AbbrevX\".  This function prepends a space
to TO-WHAT if it's > 1 char long & doesn't already begin with a space.

\(fn MODE &optional TO-WHAT)" t nil)

(autoload 'diminish-undo "diminish/diminish" "\
Restore mode-line display of diminished mode MODE to its minor-mode value.
Do nothing if the arg is a minor mode that hasn't been diminished.

Interactively, enter (with completion) the name of any diminished mode (a
mode that was formerly a minor mode on which you invoked M-x diminish).
To restore all diminished modes to minor status, answer `diminished-modes'.
The response to the prompt shouldn't be quoted.  However, in Lisp code,
the arg must be quoted as a symbol, as in (diminish-undo 'diminished-modes).

\(fn MODE)" t nil)

(autoload 'diminished-modes "diminish/diminish" "\
Echo all active diminished or minor modes as if they were minor.
The display goes in the echo area; if it's too long even for that,
you can see the whole thing in the *Messages* buffer.
This doesn't change the status of any modes; it just lets you see
what diminished modes would be on the mode-line if they were still minor.

\(fn)" t nil)

;;;***

;;;### (autoloads (egg-minor-mode-find-file-hook egg-minor-mode)
;;;;;;  "egg/egg" "egg/egg.el" (19839 7939))
;;; Generated autoloads from egg/egg.el

(autoload 'egg-minor-mode "egg/egg" "\
Turn-on egg-minor-mode which would enable key bindings for
egg in current buffer.\\<egg-minor-mode-map>
\\[egg-start-new-branch] start a new branch from the current HEAD.
\\[egg-status] shows the repo's current status
\\[egg-commit-log-edit] start editing the commit message for the current staged changes.
\\[egg-file-stage-current-file] stage new changes of the current file
\\[egg-log] shows repo's history
\\[egg-file-checkout-other-version] checkout another version of the current file
\\[egg-file-cancel-modifications] delete unstaged modifications in the current file
\\[egg-next-action] perform the next logical action
\\[egg-file-diff] compare file with index or other commits
\\[egg-file-version-other-window] show other version of the current file.

\\{egg-minor-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'egg-minor-mode-find-file-hook "egg/egg" "\
Not documented

\(fn)" nil nil)

;;;***

;;;### (autoloads (egg-grep egg-grep-mode egg-grep-process-setup)
;;;;;;  "egg/egg-grep" "egg/egg-grep.el" (19839 7939))
;;; Generated autoloads from egg/egg-grep.el

(autoload 'egg-grep-process-setup "egg/egg-grep" "\
Setup compilation variables and buffer for `egg-grep'.
Set up `compilation-exit-message-function' and run `egg-grep-setup-hook'.

\(fn)" nil nil)

(autoload 'egg-grep-mode "egg/egg-grep" "\
Sets `compilation-last-buffer' and `compilation-window-height'.

\(fn)" nil nil)

(autoload 'egg-grep "egg/egg-grep" "\
Not documented

\(fn LEVEL)" t nil)

;;;***

;;;### (autoloads (highlight-parentheses-mode) "highlight-parentheses/highlight-parentheses"
;;;;;;  "highlight-parentheses/highlight-parentheses.el" (19839 8486))
;;; Generated autoloads from highlight-parentheses/highlight-parentheses.el

(autoload 'highlight-parentheses-mode "highlight-parentheses/highlight-parentheses" "\
Minor mode to highlight the surrounding parentheses.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (highlight-symbol-query-replace highlight-symbol-prev-in-defun
;;;;;;  highlight-symbol-next-in-defun highlight-symbol-prev highlight-symbol-next
;;;;;;  highlight-symbol-remove-all highlight-symbol-at-point highlight-symbol-mode)
;;;;;;  "highlight-symbol/highlight-symbol" "highlight-symbol/highlight-symbol.el"
;;;;;;  (19840 54866))
;;; Generated autoloads from highlight-symbol/highlight-symbol.el

(autoload 'highlight-symbol-mode "highlight-symbol/highlight-symbol" "\
Minor mode that highlights the symbol under point throughout the buffer.
Highlighting takes place after `highlight-symbol-idle-delay'.

\(fn &optional ARG)" t nil)

(autoload 'highlight-symbol-at-point "highlight-symbol/highlight-symbol" "\
Toggle highlighting of the symbol at point.
This highlights or unhighlights the symbol at point using the first
element in of `highlight-symbol-faces'.

\(fn)" t nil)

(autoload 'highlight-symbol-remove-all "highlight-symbol/highlight-symbol" "\
Remove symbol highlighting in buffer.

\(fn)" t nil)

(autoload 'highlight-symbol-next "highlight-symbol/highlight-symbol" "\
Jump to the next location of the symbol at point within the function.

\(fn)" t nil)

(autoload 'highlight-symbol-prev "highlight-symbol/highlight-symbol" "\
Jump to the previous location of the symbol at point within the function.

\(fn)" t nil)

(autoload 'highlight-symbol-next-in-defun "highlight-symbol/highlight-symbol" "\
Jump to the next location of the symbol at point within the defun.

\(fn)" t nil)

(autoload 'highlight-symbol-prev-in-defun "highlight-symbol/highlight-symbol" "\
Jump to the previous location of the symbol at point within the defun.

\(fn)" t nil)

(autoload 'highlight-symbol-query-replace "highlight-symbol/highlight-symbol" "\
*Replace the symbol at point.

\(fn REPLACEMENT)" t nil)

;;;***

;;;### (autoloads (turn-on-hungry-delete-mode hungry-delete-mode
;;;;;;  hungry-delete-backward hungry-delete-forward) "hungury-delete/hungry-delete"
;;;;;;  "hungury-delete/hungry-delete.el" (19843 12761))
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

;;;### (autoloads (iedit-mode) "iedit/iedit" "iedit/iedit.el" (19844
;;;;;;  58740))
;;; Generated autoloads from iedit/iedit.el

(autoload 'iedit-mode "iedit/iedit" "\
Toggle iedit mode.
If iedit mode is off, turn iedit mode on, off otherwise.

In Transient Mark mode, when iedit mode is turned on, all the
occurrences of the current region are highlighted. If one
occurrence is modified, the change are propagated to all other
occurrences simultaneously.

If Transient Mark mode is disabled or the region is not active,
the `current-word' is used as occurrence. All the occurrences of
the `current-word' are highlighted.

You can also switch to iedit mode from isearch mode directly. The
current search string is used as occurrence.  All occurrences of
the current search string are highlighted.

With a prefix argument, the occurrence when iedit is turned off
last time is used as occurrence.  This is intended to recover
last iedit which is turned off by mistake.

Commands:
\\{iedit-mode-map}

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (lusty-launch-dired lusty-select-current-name lusty-select-match
;;;;;;  lusty-open-this lusty-highlight-previous-column lusty-highlight-next-column
;;;;;;  lusty-highlight-previous lusty-highlight-next lusty-buffer-explorer
;;;;;;  lusty-file-explorer) "lusty-explorer/lusty-explorer" "lusty-explorer/lusty-explorer.el"
;;;;;;  (19839 9912))
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
;;;;;;  (19842 19339))
;;; Generated autoloads from multi-term/multi-term.el

(autoload 'multi-term "multi-term/multi-term" "\
Create new term buffer.
Will prompt you shell name when you type `C-u' before this command.

\(fn)" t nil)

;;;***

;;;### (autoloads (nav) "nav/nav" "nav/nav.el" (19878 59256))
;;; Generated autoloads from nav/nav.el

(autoload 'nav "nav/nav" "\
Run nav-mode in a narrow window on the left side.

\(fn)" t nil)

;;;***

;;;### (autoloads (paredit-mode) "paredit/paredit" "paredit/paredit.el"
;;;;;;  (19840 51791))
;;; Generated autoloads from paredit/paredit.el

(autoload 'paredit-mode "paredit/paredit" "\
Minor mode for pseudo-structurally editing Lisp code.
With a prefix argument, enable Paredit Mode even if there are
  imbalanced parentheses in the buffer.
Paredit behaves badly if parentheses are imbalanced, so exercise
  caution when forcing Paredit Mode to be enabled, and consider
  fixing imbalanced parentheses instead.
\\<paredit-mode-map>

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (global-pretty-lambda-mode pretty-lambda-mode pretty-lambda-for-modes
;;;;;;  pretty-lambda-auto-modes pretty-lambda) "pretty-lambdada/pretty-lambdada"
;;;;;;  "pretty-lambdada/pretty-lambdada.el" (19840 59424))
;;; Generated autoloads from pretty-lambdada/pretty-lambdada.el

(let ((loads (get 'pretty-lambda 'custom-loads))) (if (member '"pretty-lambdada/pretty-lambdada" loads) nil (put 'pretty-lambda 'custom-loads (cons '"pretty-lambdada/pretty-lambdada" loads))))

(defvar pretty-lambda-auto-modes '(lisp-mode emacs-lisp-mode lisp-interaction-mode scheme-mode) "\
*Modes affected by `pretty-lambda-for-modes'.")

(custom-autoload 'pretty-lambda-auto-modes "pretty-lambdada/pretty-lambdada" t)

(autoload 'pretty-lambda-for-modes "pretty-lambdada/pretty-lambdada" "\
Use `pretty-lambda-mode' for modes in `pretty-lambda-auto-modes'.
`C-u' to turn off.

\(fn &optional TURN-OFF)" t nil)

(autoload 'pretty-lambda-mode "pretty-lambdada/pretty-lambdada" "\
Buffer-local minor mode to display the word `lambda' as the Greek letter.
With ARG, turn mode on if ARG is positive, off otherwise.

\(fn &optional ARG)" t nil)

(defvar global-pretty-lambda-mode nil "\
Non-nil if Global-Pretty-Lambda mode is enabled.
See the command `global-pretty-lambda-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-pretty-lambda-mode'.")

(custom-autoload 'global-pretty-lambda-mode "pretty-lambdada/pretty-lambdada" nil)

(autoload 'global-pretty-lambda-mode "pretty-lambdada/pretty-lambdada" "\
Toggle Pretty-Lambda mode in every possible buffer.
With prefix ARG, turn Global-Pretty-Lambda mode on if and only if
ARG is positive.
Pretty-Lambda mode is enabled in all buffers where
`turn-on-pretty-lambda-mode' would do it.
See `pretty-lambda-mode' for more information on Pretty-Lambda mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (svn-status svn-checkout) "psvn/psvn" "psvn/psvn.el"
;;;;;;  (19878 53359))
;;; Generated autoloads from psvn/psvn.el

(autoload 'svn-checkout "psvn/psvn" "\
Run svn checkout REPOS-URL PATH.

\(fn REPOS-URL PATH)" t nil)
(defalias 'svn-examine 'svn-status)

(autoload 'svn-status "psvn/psvn" "\
Examine the status of Subversion working copy in directory DIR.
If ARG is -, allow editing of the parameters. One could add -N to
run svn status non recursively to make it faster.
For every other non nil ARG pass the -u argument to `svn status', which
asks svn to connect to the repository and check to see if there are updates
there.

If there is no .svn directory, examine if there is CVS and run
`cvs-examine'. Otherwise ask if to run `dired'.

\(fn DIR &optional ARG)" t nil)

;;;***

;;;### (autoloads (pylookup-update-all pylookup-update pylookup-lookup)
;;;;;;  "pylookup/pylookup" "pylookup/pylookup.el" (19840 56836))
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

;;;### (autoloads (doctest-mode doctest-register-mmm-classes) "python-mode/doctest-mode"
;;;;;;  "python-mode/doctest-mode.el" (19878 53736))
;;; Generated autoloads from python-mode/doctest-mode.el

(autoload 'doctest-register-mmm-classes "python-mode/doctest-mode" "\
Register doctest's mmm classes, allowing doctest to be used as a
submode region in other major modes, such as python-mode and rst-mode.
Two classes are registered:

`doctest-docstring'

    Used to edit docstrings containing doctest examples in python-
    mode.  Docstring submode regions start and end with triple-quoted
    strings (\"\"\").  In order to avoid confusing start-string
    markers and end-string markers, all triple-quote strings in the
    buffer are treated as submode regions (even if they're not
    actually docstrings).  Use (C-c % C-d) to insert a new doctest-
    docstring region.  When `doctest-execute' (C-c C-c) is called
    inside a doctest-docstring region, it executes just the current
    docstring.  The globals for this execution are constructed by
    importing the current buffer's contents in Python.

`doctest-example'

    Used to edit doctest examples in text-editing modes, such as
    `rst-mode' or `text-mode'.  Docstring submode regions start with
    optionally indented prompts (>>>) and end with blank lines.  Use
    (C-c % C-e) to insert a new doctest-example region.  When
    `doctest-execute' (C-c C-c) is called inside a doctest-example
    region, it executes all examples in the buffer.

If ADD-MODE-EXT-CLASSES is true, then register the new classes in
`mmm-mode-ext-classes-alist', which will cause them to be used by
default in the following modes:

    doctest-docstring:  python-mode
    doctest-example:    rst-mode

If FIX-MMM-FONTIFY-REGION-BUG is true, then register a hook that will
fix a bug in `mmm-fontify-region' that affects some (but not all)
versions of emacs.  (See `doctest-fixed-mmm-fontify-region' for more
info.)

\(fn &optional ADD-MODE-EXT-CLASSES FIX-MMM-FONTIFY-REGION-BUG)" t nil)

(add-to-list 'auto-mode-alist '("\\.doctest$" . doctest-mode))

(autoload 'doctest-mode "python-mode/doctest-mode" "\
A major mode for editing text files that contain Python
doctest examples.  Doctest is a testing framework for Python that
emulates an interactive session, and checks the result of each
command.  For more information, see the Python library reference:
<http://docs.python.org/lib/module-doctest.html>

`doctest-mode' defines three kinds of line, each of which is
treated differently:

  - 'Source lines' are lines consisting of a Python prompt
    ('>>>' or '...'), followed by source code.  Source lines are
    colored (similarly to `python-mode') and auto-indented.

  - 'Output lines' are non-blank lines immediately following
    source lines.  They are colored using several doctest-
    specific output faces.

  - 'Text lines' are any other lines.  They are not processed in
    any special way.

\\{doctest-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads (py-shell python-mode) "python-mode/python-mode"
;;;;;;  "python-mode/python-mode.el" (19878 53736))
;;; Generated autoloads from python-mode/python-mode.el

(autoload 'python-mode "python-mode/python-mode" "\
Major mode for editing Python files.
To submit a problem report, enter `\\[py-submit-bug-report]' from a
`python-mode' buffer.  Do `\\[py-describe-mode]' for detailed
documentation.  To see what version of `python-mode' you are running,
enter `\\[py-version]'.

This mode knows about Python indentation, tokens, comments and
continuation lines.  Paragraphs are separated by blank lines only.

COMMANDS
\\{py-mode-map}
VARIABLES

py-indent-offset		indentation increment
py-block-comment-prefix		comment string used by `comment-region'
py-python-command		shell command to invoke Python interpreter
py-temp-directory		directory used for temp files (if needed)
py-beep-if-tab-change		ring the bell if `tab-width' is changed

\(fn)" t nil)

(let ((modes '(("jython" . jython-mode) ("python" . python-mode) ("python3" . python-mode)))) (while modes (when (not (assoc (car modes) interpreter-mode-alist)) (push (car modes) interpreter-mode-alist)) (setq modes (cdr modes))))

(when (not (or (rassq 'python-mode auto-mode-alist) (rassq 'jython-mode auto-mode-alist))) (push '("\\.py$" . python-mode) auto-mode-alist))

(autoload 'py-shell "python-mode/python-mode" "\
Start an interactive Python interpreter in another window.
This is like Shell mode, except that Python is running in the window
instead of a shell.  See the `Interactive Shell' and `Shell Mode'
sections of the Emacs manual for details, especially for the key
bindings active in the `*Python*' buffer.

With optional \\[universal-argument], the user is prompted for the
flags to pass to the Python interpreter.  This has no effect when this
command is used to switch to an existing process, only when a new
process is started.  If you use this, you will probably want to ensure
that the current arguments are retained (they will be included in the
prompt).  This argument is ignored when this function is called
programmatically, or when running in Emacs 19.34 or older.

Note: You can toggle between using the CPython interpreter and the
Jython interpreter by hitting \\[py-toggle-shells].  This toggles
buffer local variables which control whether all your subshell
interactions happen to the `*Jython*' or `*Python*' buffers (the
latter is the name used for the CPython buffer).

Warning: Don't use an interactive Python if you change sys.ps1 or
sys.ps2 from their default values, or if you're running code that
prints `>>> ' or `... ' at the start of a line.  `python-mode' can't
distinguish your output from Python's output, and assumes that `>>> '
at the start of a line is a prompt from Python.  Similarly, the Emacs
Shell mode code assumes that both `>>> ' and `... ' at the start of a
line are Python prompts.  Bad things can happen if you fool either
mode.

Warning:  If you do any editing *in* the process buffer *while* the
buffer is accepting output from Python, do NOT attempt to `undo' the
changes.  Some of the output (nowhere near the parts you changed!) may
be lost if you do.  This appears to be an Emacs bug, an unfortunate
interaction between undo and process filters; the same problem exists in
non-Python process buffers using the default (Emacs-supplied) process
filter.

\(fn &optional ARGPROMPT)" t nil)

;;;***

;;;### (autoloads (rainbow-mode) "rainbow-mode/rainbow-mode" "rainbow-mode/rainbow-mode.el"
;;;;;;  (19843 28306))
;;; Generated autoloads from rainbow-mode/rainbow-mode.el

(autoload 'rainbow-mode "rainbow-mode/rainbow-mode" "\
Colorize strings that represent colors.
This will fontify with colors the string like \"#aabbcc\" or \"blue\".

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (smex-initialize) "smex/smex" "smex/smex.el" (19842
;;;;;;  14351))
;;; Generated autoloads from smex/smex.el

(autoload 'smex-initialize "smex/smex" "\
Not documented

\(fn)" t nil)

;;;***

;;;### (autoloads (turn-on-tempbuf-mode tempbuf-mode) "tempbuf/tempbuf"
;;;;;;  "tempbuf/tempbuf.el" (19842 18616))
;;; Generated autoloads from tempbuf/tempbuf.el

(autoload 'tempbuf-mode "tempbuf/tempbuf" "\
Toggle tempbuf mode.

With prefix ARG, turn the mode on if ARG is positive.
After mode activation, `tempbuf-mode-hook' is run.

\(fn &optional ARG)" t nil)

(autoload 'turn-on-tempbuf-mode "tempbuf/tempbuf" "\
Turn on tempbuf mode.

See also function `tempbuf-mode'.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil nil ("ac-anything/ac-anything.el" "ac-company/ac-company.el"
;;;;;;  "ac-slime/ac-slime.el" "anything-complete/anything-complete.el"
;;;;;;  "anything-config/anything-match-plugin.el" "anything-config/anything-startup.el"
;;;;;;  "anything-include/anything-include.el" "anything-match-plugin/anything-match-plugin.el"
;;;;;;  "anything-show-completion/anything-show-completion.el" "apel/site-lisp/apel/calist.el"
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
;;;;;;  "auctex/auctex.el" "auctex/auto-loads.el" "auctex/lpath.el"
;;;;;;  "auctex/preview/auto.el" "auctex/preview/preview-latex.el"
;;;;;;  "auctex/preview/prv-emacs.el" "auctex/preview/prv-install.el"
;;;;;;  "auctex/preview/prv-xemacs.el" "auctex/tex-buf.el" "auctex/tex-mik.el"
;;;;;;  "auctex/tex-site.el" "auctex/tex-style.el" "auctex/tex-wizard.el"
;;;;;;  "auto-complete-clang/auto-complete-clang.el" "auto-complete-extension/auto-complete-extension.el"
;;;;;;  "c-eldoc/c-eldoc.el" "cedet/common/cedet-load.el" "cedet/common/cedet-loaddefs.el"
;;;;;;  "cedet/common/cedet.el" "cedet/common/ezimage.el" "cedet/common/working.el"
;;;;;;  "el-get/el-get-install.el" "eldoc-extension/eldoc-extension.el"
;;;;;;  "escreen-tab/escreen-tab.el" "escreen/escreen.el" "highlight-cl/highlight-cl.el"
;;;;;;  "ibuffer-git/ibuffer-git.el" "ipython/ipython.el" "nav/nav-dev.el"
;;;;;;  "nav/nav-test.el" "popup-pos-tip/popup-pos-tip.el" "pymacs/pymacs.el"
;;;;;;  "python-mode/highlight-indentation.el" "python-mode/pars-part-output.el"
;;;;;;  "python-mode/py-bug-numbered-tests.el" "python-mode/pycomplete.el"
;;;;;;  "quack/quack.el" "sdcv/sdcv.el" "shell-pop/shell-pop.el"
;;;;;;  "showtip/showtip.el" "ssh-config/ssh-config.el" "undo-tree/undo-tree.el"
;;;;;;  "unicad/unicad.el" "xcscope+/xcscope+.el") (19887 855 649585))

;;;***

(provide '.loaddefs)
;; Local Variables:
;; version-control: never
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; .loaddefs.el ends here
