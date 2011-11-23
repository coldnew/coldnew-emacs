;;; .loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (anything-other-buffer anything-at-point anything)
;;;;;;  "anything-config/anything" "anything-config/anything.el"
;;;;;;  (20154 59011))
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
;;;;;;  anything-c-complete-file-name-at-point anything-lisp-completion-at-point-or-indent
;;;;;;  anything-lisp-completion-at-point anything-esh-pcomplete
;;;;;;  anything-completion-mode anything-c-shell-command-if-needed
;;;;;;  anything-apt anything-world-time anything-select-xfont anything-top
;;;;;;  anything-create anything-execute-anything-command anything-call-source
;;;;;;  anything-surfraw anything-calcul-expression anything-eval-expression-with-eldoc
;;;;;;  anything-eval-expression anything-yaoddmuse-emacswiki-post-library
;;;;;;  anything-yaoddmuse-emacswiki-edit-or-view anything-yaoddmuse-cache-pages
;;;;;;  anything-all-mark-rings anything-global-mark-ring anything-mark-ring
;;;;;;  anything-simple-call-tree anything-bookmark-ext anything-manage-advice
;;;;;;  anything-M-x anything-filelist+ anything-filelist anything-etags-help
;;;;;;  anything-yank-text-at-point anything-pdfgrep-help anything-grep-help
;;;;;;  anything-c-goto-next-file anything-c-goto-precedent-file
;;;;;;  anything-do-zgrep anything-do-grep anything-dired-mode anything-dired-hardlink-file
;;;;;;  anything-dired-symlink-file anything-dired-copy-file anything-dired-rename-file
;;;;;;  anything-insert-file anything-write-file anything-find-files
;;;;;;  anything-ff-run-kill-buffer-persistent anything-ff-properties-persistent
;;;;;;  anything-ff-run-print-file anything-ff-run-gnus-attach-files
;;;;;;  anything-ff-run-open-file-externally anything-ff-run-switch-other-frame
;;;;;;  anything-ff-run-switch-other-window anything-ff-run-switch-to-eshell
;;;;;;  anything-ff-run-complete-fn-at-point anything-ff-run-delete-file
;;;;;;  anything-ff-run-symlink-file anything-ff-run-ediff-merge-file
;;;;;;  anything-ff-run-ediff-file anything-ff-run-eshell-command-on-file
;;;;;;  anything-ff-run-load-file anything-ff-run-byte-compile-file
;;;;;;  anything-ff-run-rename-file anything-ff-run-copy-file anything-ff-run-zgrep
;;;;;;  anything-ff-run-pdfgrep anything-ff-run-grep anything-ff-run-switch-to-history
;;;;;;  anything-ff-run-toggle-auto-update anything-buffer-switch-to-elscreen
;;;;;;  anything-buffer-switch-other-frame anything-buffer-switch-other-window
;;;;;;  anything-buffer-run-query-replace anything-buffer-run-query-replace-regexp
;;;;;;  anything-buffer-run-zgrep anything-buffer-run-grep anything-buffer-run-kill-buffers
;;;;;;  anything-buffer-save-persistent anything-buffer-revert-persistent
;;;;;;  anything-buffer-diff-persistent anything-toggle-all-marks
;;;;;;  anything-unmark-all anything-mark-all anything-regexp anything-kill-buffers
;;;;;;  anything-info-gnus anything-org-headlines anything-browse-code
;;;;;;  anything-occur anything-list-emacs-process anything-timers
;;;;;;  anything-bm-list anything-eev-anchors anything-emms anything-org-keywords
;;;;;;  anything-man-woman anything-register anything-c-insert-latex-math
;;;;;;  anything-c-pp-bookmarks anything-bookmarks anything-colors
;;;;;;  anything-firefox-bookmarks anything-w3m-bookmarks anything-locate
;;;;;;  anything-bbdb anything-buffers+ anything-for-buffers anything-yahoo-suggest
;;;;;;  anything-google-suggest anything-imenu anything-gentoo anything-minibuffer-history
;;;;;;  anything-show-kill-ring anything-info-emacs anything-info-at-point
;;;;;;  anything-recentf anything-for-files anything-mini anything-configuration)
;;;;;;  "anything-config/anything-config" "anything-config/anything-config.el"
;;;;;;  (20154 59013))
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
Preconfigured `anything' for `kill-ring'.
It is drop-in replacement of `yank-pop'.
You may bind this command to M-y.
First call open the kill-ring browser, next calls move to next line.

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
Note: you can add locate options after entering pattern.
See 'man locate' for valid options.

You can specify a specific database with prefix argument ARG (C-u).
Many databases can be used: navigate and mark them.
See also `anything-locate-with-db'.

To create a user specific db, use
\"updatedb -l 0 -o db_path -U directory\".
Where db_path is a filename matched by
`anything-locate-db-file-regexp'.

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

(autoload 'anything-info-gnus "anything-config/anything-config" "\
Preconfigured anything to browse Gnus Manual.

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

(autoload 'anything-buffer-run-zgrep "anything-config/anything-config" "\
Run Grep action from `anything-c-source-buffer+'.

\(fn)" t nil)

(autoload 'anything-buffer-run-query-replace-regexp "anything-config/anything-config" "\
Run Query replace regexp action from `anything-c-source-buffer+'.

\(fn)" t nil)

(autoload 'anything-buffer-run-query-replace "anything-config/anything-config" "\
Run Query replace action from `anything-c-source-buffer+'.

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

(autoload 'anything-ff-run-toggle-auto-update "anything-config/anything-config" "\


\(fn)" t nil)

(autoload 'anything-ff-run-switch-to-history "anything-config/anything-config" "\
Run Switch to history action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-grep "anything-config/anything-config" "\
Run Grep action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-pdfgrep "anything-config/anything-config" "\
Run Pdfgrep action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-zgrep "anything-config/anything-config" "\
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

(autoload 'anything-ff-run-print-file "anything-config/anything-config" "\
Run Print file action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-properties-persistent "anything-config/anything-config" "\
Show properties without quitting anything.

\(fn)" t nil)

(autoload 'anything-ff-run-kill-buffer-persistent "anything-config/anything-config" "\
Execute `anything-ff-kill-buffer-fname' whitout quitting.

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

(defvar anything-dired-mode "Enable anything completion in Dired functions.\nBindings affected are C, R, S, H." "\
Non-nil if Anything-Dired mode is enabled.
See the command `anything-dired-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `anything-dired-mode'.")

(custom-autoload 'anything-dired-mode "anything-config/anything-config" nil)

(autoload 'anything-dired-mode "anything-config/anything-config" "\
Toggle Anything-Dired mode on or off.
Interactively, with no prefix argument, toggle the mode.
With universal prefix ARG turn mode on.
With zero or negative ARG turn mode off.
\\{anything-dired-mode-map}

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

(autoload 'anything-do-zgrep "anything-config/anything-config" "\
Preconfigured anything for zgrep.

\(fn CANDIDATE)" nil nil)

(autoload 'anything-c-goto-precedent-file "anything-config/anything-config" "\
Go to precedent file in anything grep/etags buffers.

\(fn)" t nil)

(autoload 'anything-c-goto-next-file "anything-config/anything-config" "\
Go to precedent file in anything grep/etags buffers.

\(fn)" t nil)

(autoload 'anything-grep-help "anything-config/anything-config" "\


\(fn)" t nil)

(autoload 'anything-pdfgrep-help "anything-config/anything-config" "\


\(fn)" t nil)

(autoload 'anything-yank-text-at-point "anything-config/anything-config" "\
Yank text at point in minibuffer.

\(fn)" t nil)

(autoload 'anything-etags-help "anything-config/anything-config" "\


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
Needs bookmark-ext.el:
<http://mercurial.intuxication.org/hg/emacs-bookmark-extension>.
Contain also `anything-c-source-google-suggest'.

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

(defvar anything-completion-mode nil "\
Non-nil if Anything-Completion mode is enabled.
See the command `anything-completion-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `anything-completion-mode'.")

(custom-autoload 'anything-completion-mode "anything-config/anything-config" nil)

(autoload 'anything-completion-mode "anything-config/anything-config" "\
Toggle generic anything completion.

\(fn &optional ARG)" t nil)

(autoload 'anything-esh-pcomplete "anything-config/anything-config" "\
Preconfigured anything to provide anything completion in eshell.

\(fn)" t nil)

(autoload 'anything-lisp-completion-at-point "anything-config/anything-config" "\
Anything lisp symbol completion at point.

\(fn)" t nil)

(autoload 'anything-lisp-completion-at-point-or-indent "anything-config/anything-config" "\
First call indent and second call complete lisp symbol.
The second call should happen before `anything-lisp-completion-or-indent-delay',
after this delay, next call will indent again.
After completion, next call is always indent.
See that like click and double mouse click.
One hit indent, two quick hits maybe indent and complete.

\(fn ARG)" t nil)

(autoload 'anything-c-complete-file-name-at-point "anything-config/anything-config" "\
Complete file name at point.

\(fn)" t nil)

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
;;;;;;  "apel/site-lisp/apel/alist.el" (20154 59082))
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
;;;;;;  "apel/site-lisp/apel/path-util.el" (20154 59082))
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
;;;;;;  "apel/site-lisp/emu/richtext.el" (20154 59082))
;;; Generated autoloads from apel/site-lisp/emu/richtext.el

(autoload 'richtext-encode "apel/site-lisp/emu/richtext" "\


\(fn FROM TO)" nil nil)

(autoload 'richtext-decode "apel/site-lisp/emu/richtext" "\


\(fn FROM TO)" nil nil)

;;;***

;;;### (autoloads (ascii-off ascii-on ascii-display ascii-customize)
;;;;;;  "ascii/ascii" "ascii/ascii.el" (20154 59344))
;;; Generated autoloads from ascii/ascii.el

(autoload 'ascii-customize "ascii/ascii" "\
Customize ASCII options.

\(fn)" t nil)

(autoload 'ascii-display "ascii/ascii" "\
Toggle ASCII code display.

If ARG is null, toggle ASCII code display.
If ARG is a number and is greater than zero, turn on display; otherwise, turn
off display.
If ARG is anything else, turn on display.

\(fn &optional ARG)" t nil)

(autoload 'ascii-on "ascii/ascii" "\
Turn on ASCII code display.

\(fn)" t nil)

(autoload 'ascii-off "ascii/ascii" "\
Turn off ASCII code display.

\(fn)" t nil)

;;;***

;;;### (autoloads (turn-on-bib-cite bib-cite-minor-mode) "auctex/bib-cite"
;;;;;;  "auctex/bib-cite.el" (20154 59253))
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
;;;;;;  (20154 59259))
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
;;;;;;  (20154 59260))
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
;;;;;;  (20154 59253))
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
;;;;;;  (20154 59260))
;;; Generated autoloads from auctex/font-latex.el

(autoload 'font-latex-setup "auctex/font-latex" "\
Setup this buffer for LaTeX font-lock.  Usually called from a hook.

\(fn)" nil nil)

;;;***

;;;### (autoloads (docTeX-mode TeX-latex-mode BibTeX-auto-store)
;;;;;;  "auctex/latex" "auctex/latex.el" (20154 59254))
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
;;;;;;  "auctex/multi-prompt.el" (20154 59254))
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
;;;;;;  "auctex/plain-tex.el" (20154 59260))
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
;;;;;;  "auctex/preview/preview" "auctex/preview/preview.el" (20154
;;;;;;  59251))
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
;;;;;;  (20154 59253))
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
;;;;;;  "auctex/tex-bar.el" (20154 59253))
;;; Generated autoloads from auctex/tex-bar.el

(autoload 'TeX-install-toolbar "auctex/tex-bar" "\
Install toolbar buttons for TeX mode.

\(fn)" t nil)

(autoload 'LaTeX-install-toolbar "auctex/tex-bar" "\
Install toolbar buttons for LaTeX mode.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "auctex/tex-fold" "auctex/tex-fold.el" (20154
;;;;;;  59259))
;;; Generated autoloads from auctex/tex-fold.el
(autoload 'TeX-fold-mode "tex-fold" "Minor mode for hiding and revealing macros and environments." t)

(defalias 'tex-fold-mode 'TeX-fold-mode)

;;;***

;;;### (autoloads (tex-font-setup) "auctex/tex-font" "auctex/tex-font.el"
;;;;;;  (20154 59253))
;;; Generated autoloads from auctex/tex-font.el

(autoload 'tex-font-setup "auctex/tex-font" "\
Setup font lock support for TeX.

\(fn)" nil nil)

;;;***

;;;### (autoloads (TeX-texinfo-mode) "auctex/tex-info" "auctex/tex-info.el"
;;;;;;  (20154 59259))
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
;;;;;;  "auctex/tex-jp.el" (20154 59254))
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
;;;;;;  "auctex/texmathp.el" (20154 59249))
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

;;;### (autoloads nil "auctex/toolbar-x" "auctex/toolbar-x.el" (20154
;;;;;;  59255))
;;; Generated autoloads from auctex/toolbar-x.el
(autoload 'toolbarx-install-toolbar "toolbar-x")

;;;***

;;;### (autoloads (diminished-modes diminish-undo diminish) "diminish/diminish"
;;;;;;  "diminish/diminish.el" (20154 59078))
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
;;;;;;  "egg/egg" "egg/egg.el" (20154 59337))
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


\(fn)" nil nil)

;;;***

;;;### (autoloads (egg-grep egg-grep-mode egg-grep-process-setup)
;;;;;;  "egg/egg-grep" "egg/egg-grep.el" (20154 59337))
;;; Generated autoloads from egg/egg-grep.el

(autoload 'egg-grep-process-setup "egg/egg-grep" "\
Setup compilation variables and buffer for `egg-grep'.
Set up `compilation-exit-message-function' and run `egg-grep-setup-hook'.

\(fn)" nil nil)

(autoload 'egg-grep-mode "egg/egg-grep" "\
Sets `compilation-last-buffer' and `compilation-window-height'.

\(fn)" nil nil)

(autoload 'egg-grep "egg/egg-grep" "\


\(fn LEVEL)" t nil)

;;;***

;;;### (autoloads (graphviz-dot-mode) "graphviz-dot-mode/graphviz-dot-mode"
;;;;;;  "graphviz-dot-mode/graphviz-dot-mode.el" (20169 58741))
;;; Generated autoloads from graphviz-dot-mode/graphviz-dot-mode.el

(autoload 'graphviz-dot-mode "graphviz-dot-mode/graphviz-dot-mode" "\
Major mode for the dot language. \\<graphviz-dot-mode-map> 
TAB indents for graph lines. 

\\[graphviz-dot-indent-graph]	- Indentaion function.
\\[graphviz-dot-preview]	- Previews graph in a buffer.
\\[graphviz-dot-view]	- Views graph in an external viewer.
\\[graphviz-dot-indent-line]	- Indents current line of code.
\\[graphviz-dot-complete-word]	- Completes the current word.
\\[electric-graphviz-dot-terminate-line]	- Electric newline.
\\[electric-graphviz-dot-open-brace]	- Electric open braces.
\\[electric-graphviz-dot-close-brace]	- Electric close braces.
\\[electric-graphviz-dot-semi]	- Electric semi colons.

Variables specific to this mode:

graphviz-dot-dot-program            (default `dot')
Location of the dot program.
graphviz-dot-view-command           (default `doted %s')
Command to run when `graphviz-dot-view' is executed.
graphviz-dot-view-edit-command      (default nil)
If the user should be asked to edit the view command.
graphviz-dot-save-before-view       (default t)
Automatically save current buffer berore `graphviz-dot-view'.
graphviz-dot-preview-extension      (default `png')
File type to use for `graphviz-dot-preview'.
graphviz-dot-auto-indent-on-newline (default t)
Whether to run `electric-graphviz-dot-terminate-line' when 
newline is entered.
graphviz-dot-auto-indent-on-braces (default t)
Whether to run `electric-graphviz-dot-open-brace' and
`electric-graphviz-dot-close-brace' when braces are 
entered.
graphviz-dot-auto-indent-on-semi (default t)
Whether to run `electric-graphviz-dot-semi' when semi colon
is typed.
graphviz-dot-toggle-completions  (default nil)
If completions should be displayed in the buffer instead of a
completion buffer when \\[graphviz-dot-complete-word] is
pressed repeatedly.

This mode can be customized by running \\[graphviz-dot-customize].

Turning on Graphviz Dot mode calls the value of the variable 
`graphviz-dot-mode-hook' with no args, if that value is non-nil.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.dot\\'" . graphviz-dot-mode))

;;;***

;;;### (autoloads (highlight-parentheses-mode) "highlight-parentheses/highlight-parentheses"
;;;;;;  "highlight-parentheses/highlight-parentheses.el" (20154 59337))
;;; Generated autoloads from highlight-parentheses/highlight-parentheses.el

(autoload 'highlight-parentheses-mode "highlight-parentheses/highlight-parentheses" "\
Minor mode to highlight the surrounding parentheses.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (highlight-symbol-query-replace highlight-symbol-prev-in-defun
;;;;;;  highlight-symbol-next-in-defun highlight-symbol-prev highlight-symbol-next
;;;;;;  highlight-symbol-remove-all highlight-symbol-at-point highlight-symbol-mode)
;;;;;;  "highlight-symbol/highlight-symbol" "highlight-symbol/highlight-symbol.el"
;;;;;;  (20154 59156))
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
;;;;;;  "hungury-delete/hungry-delete.el" (20154 59150))
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

;;;### (autoloads (iedit-mode) "iedit/iedit" "iedit/iedit.el" (20154
;;;;;;  59029))
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
;;;;;;  (20154 59080))
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
;;;;;;  (20154 59361))
;;; Generated autoloads from multi-term/multi-term.el

(autoload 'multi-term "multi-term/multi-term" "\
Create new term buffer.
Will prompt you shell name when you type `C-u' before this command.

\(fn)" t nil)

;;;***

;;;### (autoloads (nav) "nav/nav" "nav/nav.el" (20154 59150))
;;; Generated autoloads from nav/nav.el

(autoload 'nav "nav/nav" "\
Run nav-mode in a narrow window on the left side.

\(fn)" t nil)

;;;***

;;;### (autoloads (org2blog/wp-preview-subtree-post org2blog/wp-preview-buffer-post
;;;;;;  org2blog/wp-track-subtree org2blog/wp-track-buffer org2blog/wp-post-subtree
;;;;;;  org2blog/wp-new-entry org2blog/wp-login org2blog/wp-mode)
;;;;;;  "org2blog/org2blog" "org2blog/org2blog.el" (20154 59335))
;;; Generated autoloads from org2blog/org2blog.el

(autoload 'org2blog/wp-mode "org2blog/org2blog" "\
Toggle org2blog/wp mode.
With no argument, the mode is toggled on/off.  
Non-nil argument turns mode on. 
Nil argument turns mode off.

Commands:
\\{org2blog/wp-entry-mode-map}

Entry to this mode calls the value of `org2blog/wp-mode-hook'.

\(fn &optional ARG)" t nil)

(autoload 'org2blog/wp-login "org2blog/org2blog" "\
Logs into the blog. Initializes the internal data structures.

\(fn)" t nil)

(autoload 'org2blog/wp-new-entry "org2blog/org2blog" "\
Creates a new blog entry.

\(fn)" t nil)

(autoload 'org2blog/wp-post-subtree "org2blog/org2blog" "\
Post the current entry as a draft. Publish if PUBLISH is non-nil.

\(fn &optional PUBLISH)" t nil)

(autoload 'org2blog/wp-track-buffer "org2blog/org2blog" "\
Save details of current buffer in the tracking file.

\(fn)" t nil)

(autoload 'org2blog/wp-track-subtree "org2blog/org2blog" "\
Save details of current subtree in the tracking file.

\(fn)" t nil)

(autoload 'org2blog/wp-preview-buffer-post "org2blog/org2blog" "\
Preview the present buffer in browser, if posted.

\(fn)" t nil)

(autoload 'org2blog/wp-preview-subtree-post "org2blog/org2blog" "\
Preview the present subtree in browser, if posted.

\(fn)" t nil)

;;;***

;;;### (autoloads (paredit-mode) "paredit/paredit" "paredit/paredit.el"
;;;;;;  (20154 59135))
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
;;;;;;  "pretty-lambdada/pretty-lambdada.el" (20154 59340))
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
Toggle Pretty-Lambda mode in all buffers.
With prefix ARG, enable Global-Pretty-Lambda mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Pretty-Lambda mode is enabled in all buffers where
`turn-on-pretty-lambda-mode' would do it.
See `pretty-lambda-mode' for more information on Pretty-Lambda mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (svn-status svn-checkout) "psvn/psvn" "psvn/psvn.el"
;;;;;;  (20154 59337))
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
;;;;;;  "pylookup/pylookup" "pylookup/pylookup.el" (20154 59237))
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
;;;;;;  "python-mode/doctest-mode.el" (20154 59261))
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
;;;;;;  "python-mode/python-mode.el" (20154 59261))
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

;;;### (autoloads (rainbow-delimiters-mode) "rainbow-delimiters/rainbow-delimiters"
;;;;;;  "rainbow-delimiters/rainbow-delimiters.el" (20154 59080))
;;; Generated autoloads from rainbow-delimiters/rainbow-delimiters.el

(autoload 'rainbow-delimiters-mode "rainbow-delimiters/rainbow-delimiters" "\
Color nested parentheses, brackets, and braces according to their depth.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (rainbow-mode) "rainbow-mode/rainbow-mode" "rainbow-mode/rainbow-mode.el"
;;;;;;  (20154 59344))
;;; Generated autoloads from rainbow-mode/rainbow-mode.el

(autoload 'rainbow-mode "rainbow-mode/rainbow-mode" "\
Colorize strings that represent colors.
This will fontify with colors the string like \"#aabbcc\" or \"blue\".

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (scala-mode) "scala-mode/scala-mode" "scala-mode/scala-mode.el"
;;;;;;  (20154 59359))
;;; Generated autoloads from scala-mode/scala-mode.el

(autoload 'scala-mode "scala-mode/scala-mode" "\
Major mode for editing Scala code.
When started, run `scala-mode-hook'.
\\{scala-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads (scala-quit-interpreter scala-load-file scala-eval-buffer
;;;;;;  scala-eval-definition scala-eval-region scala-switch-to-interpreter
;;;;;;  scala-run-scala scala-interpreter-running-p-1) "scala-mode/scala-mode-inf"
;;;;;;  "scala-mode/scala-mode-inf.el" (20154 59360))
;;; Generated autoloads from scala-mode/scala-mode-inf.el

(autoload 'scala-interpreter-running-p-1 "scala-mode/scala-mode-inf" "\


\(fn)" nil nil)

(autoload 'scala-run-scala "scala-mode/scala-mode-inf" "\
Run a Scala interpreter in an Emacs buffer

\(fn CMD-LINE)" t nil)

(autoload 'scala-switch-to-interpreter "scala-mode/scala-mode-inf" "\
Switch to buffer containing the interpreter

\(fn)" t nil)

(autoload 'scala-eval-region "scala-mode/scala-mode-inf" "\
Send current region to Scala interpreter.

\(fn START END)" t nil)

(autoload 'scala-eval-definition "scala-mode/scala-mode-inf" "\
Send the current 'definition' to the Scala interpreter.
This function's idea of a definition is the block of text ending
in the current line (or the first non-empty line going
backwards), and begins in the first line that is not empty and
does not start with whitespace or '{'.

For example:

println( \"aja\")
println( \"hola\" )

if the cursor is somewhere in the second print statement, the
interpreter should output 'hola'.

In the following case, if the cursor is in the second line, then
the complete function definition will be send to the interpreter:

def foo =
  1 + 2

\(fn)" t nil)

(autoload 'scala-eval-buffer "scala-mode/scala-mode-inf" "\
Send whole buffer to Scala interpreter.

\(fn)" t nil)

(autoload 'scala-load-file "scala-mode/scala-mode-inf" "\
Load a file in the Scala interpreter.

\(fn FILE-NAME)" t nil)

(autoload 'scala-quit-interpreter "scala-mode/scala-mode-inf" "\
Quit Scala interpreter.

\(fn)" t nil)

;;;***

;;;### (autoloads (smex-initialize) "smex/smex" "smex/smex.el" (20154
;;;;;;  59336))
;;; Generated autoloads from smex/smex.el

(autoload 'smex-initialize "smex/smex" "\


\(fn)" t nil)

;;;***

;;;### (autoloads (turn-on-tempbuf-mode tempbuf-mode) "tempbuf/tempbuf"
;;;;;;  "tempbuf/tempbuf.el" (20154 59341))
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

(provide '.loaddefs)
;; Local Variables:
;; version-control: never
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; .loaddefs.el ends here
