;;; .loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (anything-other-buffer anything-at-point anything)
;;;;;;  "anything-config/anything" "anything-config/anything.el"
;;;;;;  (20171 20002))
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
;;;;;;  (20171 20003))
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
;;;;;;  "apel/site-lisp/apel/alist.el" (20171 20093))
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
;;;;;;  "apel/site-lisp/apel/path-util.el" (20171 20093))
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
;;;;;;  "apel/site-lisp/emu/richtext.el" (20171 20093))
;;; Generated autoloads from apel/site-lisp/emu/richtext.el

(autoload 'richtext-encode "apel/site-lisp/emu/richtext" "\


\(fn FROM TO)" nil nil)

(autoload 'richtext-decode "apel/site-lisp/emu/richtext" "\


\(fn FROM TO)" nil nil)

;;;***

;;;### (autoloads (ascii-off ascii-on ascii-display ascii-customize)
;;;;;;  "ascii/ascii" "ascii/ascii.el" (20171 20505))
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
;;;;;;  "auctex/bib-cite.el" (20171 20344))
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
;;;;;;  (20171 20368))
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
;;;;;;  (20171 20369))
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
;;;;;;  (20171 20344))
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
;;;;;;  (20171 20368))
;;; Generated autoloads from auctex/font-latex.el

(autoload 'font-latex-setup "auctex/font-latex" "\
Setup this buffer for LaTeX font-lock.  Usually called from a hook.

\(fn)" nil nil)

;;;***

;;;### (autoloads (docTeX-mode TeX-latex-mode BibTeX-auto-store)
;;;;;;  "auctex/latex" "auctex/latex.el" (20171 20345))
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
;;;;;;  "auctex/multi-prompt.el" (20171 20345))
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
;;;;;;  "auctex/plain-tex.el" (20171 20369))
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
;;;;;;  "auctex/preview/preview" "auctex/preview/preview.el" (20171
;;;;;;  20342))
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
;;;;;;  (20171 20344))
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
;;;;;;  "auctex/tex-bar.el" (20171 20344))
;;; Generated autoloads from auctex/tex-bar.el

(autoload 'TeX-install-toolbar "auctex/tex-bar" "\
Install toolbar buttons for TeX mode.

\(fn)" t nil)

(autoload 'LaTeX-install-toolbar "auctex/tex-bar" "\
Install toolbar buttons for LaTeX mode.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "auctex/tex-fold" "auctex/tex-fold.el" (20171
;;;;;;  20368))
;;; Generated autoloads from auctex/tex-fold.el
(autoload 'TeX-fold-mode "tex-fold" "Minor mode for hiding and revealing macros and environments." t)

(defalias 'tex-fold-mode 'TeX-fold-mode)

;;;***

;;;### (autoloads (tex-font-setup) "auctex/tex-font" "auctex/tex-font.el"
;;;;;;  (20171 20344))
;;; Generated autoloads from auctex/tex-font.el

(autoload 'tex-font-setup "auctex/tex-font" "\
Setup font lock support for TeX.

\(fn)" nil nil)

;;;***

;;;### (autoloads (TeX-texinfo-mode) "auctex/tex-info" "auctex/tex-info.el"
;;;;;;  (20171 20368))
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
;;;;;;  "auctex/tex-jp.el" (20171 20345))
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
;;;;;;  "auctex/texmathp.el" (20171 20340))
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

;;;### (autoloads nil "auctex/toolbar-x" "auctex/toolbar-x.el" (20171
;;;;;;  20345))
;;; Generated autoloads from auctex/toolbar-x.el
(autoload 'toolbarx-install-toolbar "toolbar-x")

;;;***

;;;### (autoloads (cperl-perldoc-at-point cperl-perldoc cperl-mode)
;;;;;;  "cperl-mode/cperl-mode" "cperl-mode/cperl-mode.el" (20172
;;;;;;  47039))
;;; Generated autoloads from cperl-mode/cperl-mode.el
(put 'cperl-indent-level 'safe-local-variable 'integerp)
(put 'cperl-brace-offset 'safe-local-variable 'integerp)
(put 'cperl-continued-brace-offset 'safe-local-variable 'integerp)
(put 'cperl-label-offset 'safe-local-variable 'integerp)
(put 'cperl-continued-statement-offset 'safe-local-variable 'integerp)
(put 'cperl-extra-newline-before-brace 'safe-local-variable 'booleanp)
(put 'cperl-merge-trailing-else 'safe-local-variable 'booleanp)

(autoload 'cperl-mode "cperl-mode/cperl-mode" "\
Major mode for editing Perl code.
Expression and list commands understand all C brackets.
Tab indents for Perl code.
Paragraphs are separated by blank lines only.
Delete converts tabs to spaces as it moves back.

Various characters in Perl almost always come in pairs: {}, (), [],
sometimes <>.  When the user types the first, she gets the second as
well, with optional special formatting done on {}.  (Disabled by
default.)  You can always quote (with \\[quoted-insert]) the left
\"paren\" to avoid the expansion.  The processing of < is special,
since most the time you mean \"less\".  CPerl mode tries to guess
whether you want to type pair <>, and inserts is if it
appropriate.  You can set `cperl-electric-parens-string' to the string that
contains the parenths from the above list you want to be electrical.
Electricity of parenths is controlled by `cperl-electric-parens'.
You may also set `cperl-electric-parens-mark' to have electric parens
look for active mark and \"embrace\" a region if possible.'

CPerl mode provides expansion of the Perl control constructs:

   if, else, elsif, unless, while, until, continue, do,
   for, foreach, formy and foreachmy.

and POD directives (Disabled by default, see `cperl-electric-keywords'.)

The user types the keyword immediately followed by a space, which
causes the construct to be expanded, and the point is positioned where
she is most likely to want to be.  eg. when the user types a space
following \"if\" the following appears in the buffer: if () { or if ()
} { } and the cursor is between the parentheses.  The user can then
type some boolean expression within the parens.  Having done that,
typing \\[cperl-linefeed] places you - appropriately indented - on a
new line between the braces (if you typed \\[cperl-linefeed] in a POD
directive line, then appropriate number of new lines is inserted).

If CPerl decides that you want to insert \"English\" style construct like

            bite if angry;

it will not do any expansion.  See also help on variable
`cperl-extra-newline-before-brace'.  (Note that one can switch the
help message on expansion by setting `cperl-message-electric-keyword'
to nil.)

\\[cperl-linefeed] is a convenience replacement for typing carriage
return.  It places you in the next line with proper indentation, or if
you type it inside the inline block of control construct, like

            foreach (@lines) {print; print}

and you are on a boundary of a statement inside braces, it will
transform the construct into a multiline and will place you into an
appropriately indented blank line.  If you need a usual
`newline-and-indent' behavior, it is on \\[newline-and-indent],
see documentation on `cperl-electric-linefeed'.

Use \\[cperl-invert-if-unless] to change a construction of the form

	    if (A) { B }

into

            B if A;

\\{cperl-mode-map}

Setting the variable `cperl-font-lock' to t switches on font-lock-mode
\(even with older Emacsen), `cperl-electric-lbrace-space' to t switches
on electric space between $ and {, `cperl-electric-parens-string' is
the string that contains parentheses that should be electric in CPerl
\(see also `cperl-electric-parens-mark' and `cperl-electric-parens'),
setting `cperl-electric-keywords' enables electric expansion of
control structures in CPerl.  `cperl-electric-linefeed' governs which
one of two linefeed behavior is preferable.  You can enable all these
options simultaneously (recommended mode of use) by setting
`cperl-hairy' to t.  In this case you can switch separate options off
by setting them to `null'.  Note that one may undo the extra
whitespace inserted by semis and braces in `auto-newline'-mode by
consequent \\[cperl-electric-backspace].

If your site has perl5 documentation in info format, you can use commands
\\[cperl-info-on-current-command] and \\[cperl-info-on-command] to access it.
These keys run commands `cperl-info-on-current-command' and
`cperl-info-on-command', which one is which is controlled by variable
`cperl-info-on-command-no-prompt' and `cperl-clobber-lisp-bindings'
\(in turn affected by `cperl-hairy').

Even if you have no info-format documentation, short one-liner-style
help is available on \\[cperl-get-help], and one can run perldoc or
man via menu.

It is possible to show this help automatically after some idle time.
This is regulated by variable `cperl-lazy-help-time'.  Default with
`cperl-hairy' (if the value of `cperl-lazy-help-time' is nil) is 5
secs idle time .  It is also possible to switch this on/off from the
menu, or via \\[cperl-toggle-autohelp].  Requires `run-with-idle-timer'.

Use \\[cperl-lineup] to vertically lineup some construction - put the
beginning of the region at the start of construction, and make region
span the needed amount of lines.

Variables `cperl-pod-here-scan', `cperl-pod-here-fontify',
`cperl-pod-face', `cperl-pod-head-face' control processing of POD and
here-docs sections.  With capable Emaxen results of scan are used
for indentation too, otherwise they are used for highlighting only.

Variables controlling indentation style:
 `cperl-tab-always-indent'
    Non-nil means TAB in CPerl mode should always reindent the current line,
    regardless of where in the line point is when the TAB command is used.
 `cperl-indent-left-aligned-comments'
    Non-nil means that the comment starting in leftmost column should indent.
 `cperl-auto-newline'
    Non-nil means automatically newline before and after braces,
    and after colons and semicolons, inserted in Perl code.  The following
    \\[cperl-electric-backspace] will remove the inserted whitespace.
    Insertion after colons requires both this variable and
    `cperl-auto-newline-after-colon' set.
 `cperl-auto-newline-after-colon'
    Non-nil means automatically newline even after colons.
    Subject to `cperl-auto-newline' setting.
 `cperl-indent-level'
    Indentation of Perl statements within surrounding block.
    The surrounding block's indentation is the indentation
    of the line on which the open-brace appears.
 `cperl-continued-statement-offset'
    Extra indentation given to a substatement, such as the
    then-clause of an if, or body of a while, or just a statement continuation.
 `cperl-continued-brace-offset'
    Extra indentation given to a brace that starts a substatement.
    This is in addition to `cperl-continued-statement-offset'.
 `cperl-brace-offset'
    Extra indentation for line if it starts with an open brace.
 `cperl-brace-imaginary-offset'
    An open brace following other text is treated as if it the line started
    this far to the right of the actual line indentation.
 `cperl-label-offset'
    Extra indentation for line that is a label.
 `cperl-min-label-indent'
    Minimal indentation for line that is a label.

Settings for classic indent-styles: K&R BSD=C++ GNU PerlStyle=Whitesmith
  `cperl-indent-level'                5   4       2   4
  `cperl-brace-offset'                0   0       0   0
  `cperl-continued-brace-offset'     -5  -4       0   0
  `cperl-label-offset'               -5  -4      -2  -4
  `cperl-continued-statement-offset'  5   4       2   4

CPerl knows several indentation styles, and may bulk set the
corresponding variables.  Use \\[cperl-set-style] to do this.  Use
\\[cperl-set-style-back] to restore the memorized preexisting values
\(both available from menu).  See examples in `cperl-style-examples'.

Part of the indentation style is how different parts of if/elsif/else
statements are broken into lines; in CPerl, this is reflected on how
templates for these constructs are created (controlled by
`cperl-extra-newline-before-brace'), and how reflow-logic should treat \"continuation\" blocks of else/elsif/continue, controlled by the same variable,
and by `cperl-extra-newline-before-brace-multiline',
`cperl-merge-trailing-else', `cperl-indent-region-fix-constructs'.

If `cperl-indent-level' is 0, the statement after opening brace in
column 0 is indented on
`cperl-brace-offset'+`cperl-continued-statement-offset'.

Turning on CPerl mode calls the hooks in the variable `cperl-mode-hook'
with no args.

DO NOT FORGET to read micro-docs (available from `Perl' menu)
or as help on variables `cperl-tips', `cperl-problems',
`cperl-praise', `cperl-speed'.

\(fn)" t nil)

(autoload 'cperl-perldoc "cperl-mode/cperl-mode" "\
Run `perldoc' on WORD.

\(fn WORD)" t nil)

(autoload 'cperl-perldoc-at-point "cperl-mode/cperl-mode" "\
Run a `perldoc' on the word around point.

\(fn)" t nil)

;;;***

;;;### (autoloads (diminished-modes diminish-undo diminish) "diminish/diminish"
;;;;;;  "diminish/diminish.el" (20171 20087))
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
;;;;;;  "egg/egg" "egg/egg.el" (20171 20482))
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
;;;;;;  "egg/egg-grep" "egg/egg-grep.el" (20171 20481))
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

;;;### (autoloads (apt-sources-mode) "emacs-goodies-el/elisp/debian-el/apt-sources"
;;;;;;  "emacs-goodies-el/elisp/debian-el/apt-sources.el" (19215
;;;;;;  18987))
;;; Generated autoloads from emacs-goodies-el/elisp/debian-el/apt-sources.el

(autoload 'apt-sources-mode "emacs-goodies-el/elisp/debian-el/apt-sources" "\
Major mode for editing apt's sources.list file.
Sets up command `font-lock-mode'.

\\{apt-sources-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads (apt-utils-search apt-utils-show-package) "emacs-goodies-el/elisp/debian-el/apt-utils"
;;;;;;  "emacs-goodies-el/elisp/debian-el/apt-utils.el" (19972 48294))
;;; Generated autoloads from emacs-goodies-el/elisp/debian-el/apt-utils.el

(autoload 'apt-utils-show-package "emacs-goodies-el/elisp/debian-el/apt-utils" "\
Show information for a Debian package.
A selection of known packages is presented.  See `apt-utils-mode'
for more detailed help.  If NEW-SESSION is non-nil, generate a
new `apt-utils-mode' buffer.

\(fn &optional NEW-SESSION)" t nil)

(autoload 'apt-utils-search "emacs-goodies-el/elisp/debian-el/apt-utils" "\
Search Debian packages for regular expression.
To search for multiple patterns use a string like \"foo && bar\".
The regular expression used to split the
terms (`apt-utils-search-split-regexp') is customisable.

\(fn)" t nil)

;;;***

;;;### (autoloads (deb-find deb-view-mode deb-view deb-view-dired-view)
;;;;;;  "emacs-goodies-el/elisp/debian-el/deb-view" "emacs-goodies-el/elisp/debian-el/deb-view.el"
;;;;;;  (20043 14215))
;;; Generated autoloads from emacs-goodies-el/elisp/debian-el/deb-view.el

(autoload 'deb-view-dired-view "emacs-goodies-el/elisp/debian-el/deb-view" "\
View Debian package control and data files.
Press \"q\" in either window to kill both buffers
and return to the dired buffer. See deb-view.

\(fn)" t nil)

(autoload 'deb-view "emacs-goodies-el/elisp/debian-el/deb-view" "\
View Debian package DEBFILE's control and data files.
Press \"q\" in either window to kill both buffers.

In dired, press ^d on the dired line of the .deb file to view.
Or, execute: ESCAPE x deb-view RETURN, and enter the .deb file name
at the prompt.

\(fn DEBFILE)" t nil)

(autoload 'deb-view-mode "emacs-goodies-el/elisp/debian-el/deb-view" "\
View mode for Debian Archive Files.

\(fn)" t nil)

(autoload 'deb-find "emacs-goodies-el/elisp/debian-el/deb-view" "\
Search for deb files.
Use the method specified by the variable deb-find-method, and collect
output in a buffer.  See also the variable deb-find-directory.

This command uses a special history list, so you can
easily repeat a `deb-find' command.

\(fn)" t nil)

;;;***

;;;### (autoloads (debian-bug emacs-bug-get-bug-as-email debian-bug-get-bug-as-email
;;;;;;  debian-bug-get-bug-as-file debian-bug-web-package debian-bug-web-packages
;;;;;;  debian-bug-web-this-bug-under-mouse emacs-bug-web-bug debian-bug-web-bug
;;;;;;  debian-bug-web-developer-page debian-bug-web-bugs debian-bug-intent-to-package
;;;;;;  debian-bug-request-for-package debian-bug-wnpp) "emacs-goodies-el/elisp/debian-el/debian-bug"
;;;;;;  "emacs-goodies-el/elisp/debian-el/debian-bug.el" (19489 12640))
;;; Generated autoloads from emacs-goodies-el/elisp/debian-el/debian-bug.el

(autoload 'debian-bug-wnpp "emacs-goodies-el/elisp/debian-el/debian-bug" "\
Submit a WNPP bug report to Debian.
Optional argument ACTION can be provided in programs.

\(fn &optional ACTION)" t nil)

(autoload 'debian-bug-request-for-package "emacs-goodies-el/elisp/debian-el/debian-bug" "\
Shortcut for `debian-bug-wnpp' with RFP action.

\(fn)" t nil)

(autoload 'debian-bug-intent-to-package "emacs-goodies-el/elisp/debian-el/debian-bug" "\
Shortcut for `debian-bug-wnpp' with ITP action (for Debian developers).

\(fn)" t nil)

(autoload 'debian-bug-web-bugs "emacs-goodies-el/elisp/debian-el/debian-bug" "\
Browse the BTS for this package via `browse-url'.
With optional argument prefix ARCHIVED, display archived bugs.

\(fn &optional ARCHIVED)" t nil)

(autoload 'debian-bug-web-developer-page "emacs-goodies-el/elisp/debian-el/debian-bug" "\
Browse the web for this package's developer page.

\(fn)" t nil)

(autoload 'debian-bug-web-bug "emacs-goodies-el/elisp/debian-el/debian-bug" "\
Browse the BTS for BUG-NUMBER via `browse-url'.

\(fn &optional BUG-NUMBER)" t nil)

(autoload 'emacs-bug-web-bug "emacs-goodies-el/elisp/debian-el/debian-bug" "\
Browse the Emacs BTS for BUG-NUMBER via `browse-url'.

\(fn &optional BUG-NUMBER)" t nil)

(autoload 'debian-bug-web-this-bug-under-mouse "emacs-goodies-el/elisp/debian-el/debian-bug" "\
Browse the BTS via `browse-url' for the bug report number under mouse.
In a program, mouse location is in EVENT.

\(fn EVENT)" t nil)

(autoload 'debian-bug-web-packages "emacs-goodies-el/elisp/debian-el/debian-bug" "\
Search Debian web page for this package via `browse-url'.

\(fn)" t nil)

(autoload 'debian-bug-web-package "emacs-goodies-el/elisp/debian-el/debian-bug" "\
Search Debian web page in ARCHIVE for this package via `browse-url'.

\(fn ARCHIVE)" t nil)

(autoload 'debian-bug-get-bug-as-file "emacs-goodies-el/elisp/debian-el/debian-bug" "\
Read bug report #BUG-NUMBER as a regular file.

\(fn &optional BUG-NUMBER)" t nil)

(autoload 'debian-bug-get-bug-as-email "emacs-goodies-el/elisp/debian-el/debian-bug" "\
Read bug report #BUG-NUMBER via Email interface.

\(fn &optional BUG-NUMBER)" t nil)

(autoload 'emacs-bug-get-bug-as-email "emacs-goodies-el/elisp/debian-el/debian-bug" "\
Read Emacs bug report #BUG-NUMBER via Email interface.

\(fn &optional BUG-NUMBER)" t nil)

(autoload 'debian-bug "emacs-goodies-el/elisp/debian-el/debian-bug" "\
Submit a Debian bug report.

\(fn)" t nil)

;;;***

;;;### (autoloads (preseed-mode) "emacs-goodies-el/elisp/debian-el/preseed"
;;;;;;  "emacs-goodies-el/elisp/debian-el/preseed.el" (17245 35005))
;;; Generated autoloads from emacs-goodies-el/elisp/debian-el/preseed.el

(autoload 'preseed-mode "emacs-goodies-el/elisp/debian-el/preseed" "\
Major mode for editing debian-installer preseed files colourfully.

\(fn)" t nil)

;;;***

;;;### (autoloads (emacs-bts-control debian-bts-control) "emacs-goodies-el/elisp/dpkg-dev-el/debian-bts-control"
;;;;;;  "emacs-goodies-el/elisp/dpkg-dev-el/debian-bts-control.el"
;;;;;;  (19331 13712))
;;; Generated autoloads from emacs-goodies-el/elisp/dpkg-dev-el/debian-bts-control.el

(autoload 'debian-bts-control "emacs-goodies-el/elisp/dpkg-dev-el/debian-bts-control" "\
Contruct a message with initial ACTION command for control@bugs.debian.org.
Contructs a new control command line if called from within the message
being constructed.

If prefix arg is provided, use the current buffer instead instead of
creating a new outgoing email message buffer.
The current buffer is also used if the current major mode matches one listed
in `debian-bts-control-modes-to-reuse'.

\(fn ACTION &optional ARG)" t nil)

(autoload 'emacs-bts-control "emacs-goodies-el/elisp/dpkg-dev-el/debian-bts-control" "\
Contruct a message with ACTION command for control@debbugs.gnu.org.
Contructs a new control command line if called from within the message
being constructed.

If prefix arg is provided, use the current buffer instead instead of
creating a new outgoing email message buffer.
The current buffer is also used if the current major mode matches one listed
in `debian-bts-control-modes-to-reuse'.

\(fn ACTION &optional ARG)" t nil)

;;;***

;;;### (autoloads (debian-changelog-mode debian-changelog-add-entry)
;;;;;;  "emacs-goodies-el/elisp/dpkg-dev-el/debian-changelog-mode"
;;;;;;  "emacs-goodies-el/elisp/dpkg-dev-el/debian-changelog-mode.el"
;;;;;;  (20121 57468))
;;; Generated autoloads from emacs-goodies-el/elisp/dpkg-dev-el/debian-changelog-mode.el

(autoload 'debian-changelog-add-entry "emacs-goodies-el/elisp/dpkg-dev-el/debian-changelog-mode" "\
Add a new change entry to a debian-style changelog.
If called from buffer other than a debian/changelog, this will search
for the debian/changelog file to add the entry to.

\(fn)" t nil)

(autoload 'debian-changelog-mode "emacs-goodies-el/elisp/dpkg-dev-el/debian-changelog-mode" "\
Major mode for editing Debian-style change logs.
Runs `debian-changelog-mode-hook' if it exists.

Key bindings:

\\{debian-changelog-mode-map}

If you want to use your debian.org email address for debian/changelog
entries without using it for the rest of your email, use the `customize`
interface to set it, or simply set the variable
`debian-changelog-mailing-address' in your ~/.emacs file, e.g.

 (setq debian-changelog-mailing-address \"myname@debian.org\"))

\(fn)" t nil)
(add-to-list 'auto-mode-alist '("/debian/*NEWS" . debian-changelog-mode))
(add-to-list 'auto-mode-alist '("NEWS.Debian" . debian-changelog-mode))
(add-to-list 'auto-mode-alist '("/debian/\\([[:lower:][:digit:]][[:lower:][:digit:].+-]+\\.\\)?changelog\\'" . debian-changelog-mode))
(add-to-list 'auto-mode-alist '("changelog.Debian" . debian-changelog-mode))
(add-to-list 'auto-mode-alist '("changelog.dch" . debian-changelog-mode))

;;;***

;;;### (autoloads (debian-control-mode) "emacs-goodies-el/elisp/dpkg-dev-el/debian-control-mode"
;;;;;;  "emacs-goodies-el/elisp/dpkg-dev-el/debian-control-mode.el"
;;;;;;  (20043 15570))
;;; Generated autoloads from emacs-goodies-el/elisp/dpkg-dev-el/debian-control-mode.el

(autoload 'debian-control-mode "emacs-goodies-el/elisp/dpkg-dev-el/debian-control-mode" "\
A major mode for editing Debian control files (i.e. debian/control).

\(fn)" t nil)
(add-to-list 'auto-mode-alist '("/debian/control\\'" . debian-control-mode))

;;;***

;;;### (autoloads (debian-copyright-mode) "emacs-goodies-el/elisp/dpkg-dev-el/debian-copyright"
;;;;;;  "emacs-goodies-el/elisp/dpkg-dev-el/debian-copyright.el"
;;;;;;  (19536 19929))
;;; Generated autoloads from emacs-goodies-el/elisp/dpkg-dev-el/debian-copyright.el

(autoload 'debian-copyright-mode "emacs-goodies-el/elisp/dpkg-dev-el/debian-copyright" "\
Mode to edit and read debian/copyright.
\\{debian-copyright-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("debian/.*copyright\\'" . debian-copyright-mode))

(add-to-list 'auto-mode-alist '("\\`/usr/share/doc/.*/copyright" . debian-copyright-mode))

;;;***

;;;### (autoloads (readme-debian-mode) "emacs-goodies-el/elisp/dpkg-dev-el/readme-debian"
;;;;;;  "emacs-goodies-el/elisp/dpkg-dev-el/readme-debian.el" (19181
;;;;;;  42024))
;;; Generated autoloads from emacs-goodies-el/elisp/dpkg-dev-el/readme-debian.el

(autoload 'readme-debian-mode "emacs-goodies-el/elisp/dpkg-dev-el/readme-debian" "\
Mode for reading and editing README.Debian files.
Upon saving the visited README.Debian file, the timestamp at the bottom
will be updated.

\\{readme-debian-mode-map}

\(fn)" t nil)
(add-to-list 'auto-mode-alist '("debian/.*README.*Debian$" . readme-debian-mode))
(add-to-list 'auto-mode-alist '("^/usr/share/doc/.*/README.*Debian.*$" . readme-debian-mode))

;;;***

;;;### (autoloads (align-all-strings align-string) "emacs-goodies-el/elisp/emacs-goodies-el/align-string"
;;;;;;  "emacs-goodies-el/elisp/emacs-goodies-el/align-string.el"
;;;;;;  (19105 33186))
;;; Generated autoloads from emacs-goodies-el/elisp/emacs-goodies-el/align-string.el

(autoload 'align-string "emacs-goodies-el/elisp/emacs-goodies-el/align-string" "\
Align first occurrence of REGEXP in each line of region.
If given a prefix argument, align occurrence number COUNT on each line.

\(fn BEGIN END REGEXP COUNT)" t nil)

(autoload 'align-all-strings "emacs-goodies-el/elisp/emacs-goodies-el/align-string" "\
Align all occurrences of REGEXP in each line of region.
That is to say, align the first occurrence of each line with each other,
align the second occurence of each line with each other, and so on.

\(fn BEGIN END REGEXP)" t nil)

;;;***

;;;### (autoloads (all) "emacs-goodies-el/elisp/emacs-goodies-el/all"
;;;;;;  "emacs-goodies-el/elisp/emacs-goodies-el/all.el" (16059 54755))
;;; Generated autoloads from emacs-goodies-el/elisp/emacs-goodies-el/all.el

(autoload 'all "emacs-goodies-el/elisp/emacs-goodies-el/all" "\
Show all lines in the current buffer containing a match for REGEXP.

If a match spreads across multiple lines, all those lines are shown.

Each line is displayed with NLINES lines before and after, or -NLINES
before if NLINES is negative.
NLINES defaults to `list-matching-lines-default-context-lines'.
Interactively it is the prefix arg.

The lines are shown in a buffer named `*All*'.
Any changes made in that buffer will be propagated to this buffer.

\(fn REGEXP &optional NLINES)" t nil)

;;;***

;;;### (autoloads (apache-mode) "emacs-goodies-el/elisp/emacs-goodies-el/apache-mode"
;;;;;;  "emacs-goodies-el/elisp/emacs-goodies-el/apache-mode.el"
;;;;;;  (19103 54677))
;;; Generated autoloads from emacs-goodies-el/elisp/emacs-goodies-el/apache-mode.el

(autoload 'apache-mode "emacs-goodies-el/elisp/emacs-goodies-el/apache-mode" "\
Major mode for editing Apache configuration files.

\(fn)" t nil)
(add-to-list 'auto-mode-alist '("\\.htaccess\\'"   . apache-mode))
(add-to-list 'auto-mode-alist '("httpd\\.conf\\'"  . apache-mode))
(add-to-list 'auto-mode-alist '("srm\\.conf\\'"    . apache-mode))
(add-to-list 'auto-mode-alist '("access\\.conf\\'" . apache-mode))
(add-to-list 'auto-mode-alist '("sites-\\(available\\|enabled\\)/" . apache-mode))

;;;***

;;;### (autoloads (ascii-off ascii-on ascii-display ascii-customize)
;;;;;;  "emacs-goodies-el/elisp/emacs-goodies-el/ascii" "emacs-goodies-el/elisp/emacs-goodies-el/ascii.el"
;;;;;;  (19103 54677))
;;; Generated autoloads from emacs-goodies-el/elisp/emacs-goodies-el/ascii.el

(autoload 'ascii-customize "emacs-goodies-el/elisp/emacs-goodies-el/ascii" "\
Customize ASCII options.

\(fn)" t nil)

(autoload 'ascii-display "emacs-goodies-el/elisp/emacs-goodies-el/ascii" "\
Toggle ASCII code display.

If ARG is null, toggle ASCII code display.
If ARG is a number and is greater than zero, turn on display; otherwise, turn
off display.
If ARG is anything else, turn on display.

\(fn &optional ARG)" t nil)

(autoload 'ascii-on "emacs-goodies-el/elisp/emacs-goodies-el/ascii" "\
Turn on ASCII code display.

\(fn)" t nil)

(autoload 'ascii-off "emacs-goodies-el/elisp/emacs-goodies-el/ascii" "\
Turn off ASCII code display.

\(fn)" t nil)

;;;***

;;;### (autoloads (bar-cursor-mode) "emacs-goodies-el/elisp/emacs-goodies-el/bar-cursor"
;;;;;;  "emacs-goodies-el/elisp/emacs-goodies-el/bar-cursor.el" (16013
;;;;;;  59387))
;;; Generated autoloads from emacs-goodies-el/elisp/emacs-goodies-el/bar-cursor.el

(autoload 'bar-cursor-mode "emacs-goodies-el/elisp/emacs-goodies-el/bar-cursor" "\
Toggle use of 'bar-cursor-mode'.

This quasi-minor mode changes cursor to a bar cursor in insert mode,
and a block cursor in overwrite mode.  It may only be turned on and
off globally, not on a per-buffer basis (hence the quasi- designation).

Optional ARG turns mode on iff ARG is a positive integer.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (bm-previous-mouse bm-previous bm-next-mouse bm-next
;;;;;;  bm-toggle-mouse bm-toggle) "emacs-goodies-el/elisp/emacs-goodies-el/bm"
;;;;;;  "emacs-goodies-el/elisp/emacs-goodies-el/bm.el" (19425 29270))
;;; Generated autoloads from emacs-goodies-el/elisp/emacs-goodies-el/bm.el

(autoload 'bm-toggle "emacs-goodies-el/elisp/emacs-goodies-el/bm" "\
Toggle bookmark at point.

\(fn)" t nil)

(autoload 'bm-toggle-mouse "emacs-goodies-el/elisp/emacs-goodies-el/bm" "\
Toggle a bookmark with a mouse click.
EV is the mouse event.

\(fn EV)" t nil)

(autoload 'bm-next "emacs-goodies-el/elisp/emacs-goodies-el/bm" "\
Goto next bookmark.

\(fn)" t nil)

(autoload 'bm-next-mouse "emacs-goodies-el/elisp/emacs-goodies-el/bm" "\
Go to the next bookmark with the scroll wheel.
EV is the mouse event.

\(fn EV)" t nil)

(autoload 'bm-previous "emacs-goodies-el/elisp/emacs-goodies-el/bm" "\
Goto previous bookmark.

\(fn)" t nil)

(autoload 'bm-previous-mouse "emacs-goodies-el/elisp/emacs-goodies-el/bm" "\
Go to the previous bookmark with the scroll wheel.
EV is the mouse event.

\(fn EV)" t nil)

;;;***

;;;### (autoloads (boxquote-unbox boxquote-unbox-region boxquote-fill-paragraph
;;;;;;  boxquote-kill boxquote-narrow-to-boxquote-content boxquote-narrow-to-boxquote
;;;;;;  boxquote-text boxquote-where-is boxquote-shell-command boxquote-describe-key
;;;;;;  boxquote-describe-variable boxquote-describe-function boxquote-boxquote
;;;;;;  boxquote-paragraph boxquote-defun boxquote-yank boxquote-kill-ring-save
;;;;;;  boxquote-insert-buffer boxquote-insert-file boxquote-buffer
;;;;;;  boxquote-region boxquote-title) "emacs-goodies-el/elisp/emacs-goodies-el/boxquote"
;;;;;;  "emacs-goodies-el/elisp/emacs-goodies-el/boxquote.el" (19103
;;;;;;  54677))
;;; Generated autoloads from emacs-goodies-el/elisp/emacs-goodies-el/boxquote.el

(autoload 'boxquote-title "emacs-goodies-el/elisp/emacs-goodies-el/boxquote" "\
Set the title of the current boxquote to TITLE.

If TITLE is an empty string the title is removed. Note that the title will
be formatted using `boxquote-title-format'.

\(fn TITLE)" t nil)

(autoload 'boxquote-region "emacs-goodies-el/elisp/emacs-goodies-el/boxquote" "\
Draw a box around the left hand side of a region bounding START and END.

\(fn START END)" t nil)

(autoload 'boxquote-buffer "emacs-goodies-el/elisp/emacs-goodies-el/boxquote" "\
Apply `boxquote-region' to a whole buffer.

\(fn)" t nil)

(autoload 'boxquote-insert-file "emacs-goodies-el/elisp/emacs-goodies-el/boxquote" "\
Insert the contents of a file, boxed with `boxquote-region'.

If `boxquote-title-files' is non-nil the boxquote will be given a title that
is the result of applying `boxquote-file-title-function' to FILENAME.

\(fn FILENAME)" t nil)

(autoload 'boxquote-insert-buffer "emacs-goodies-el/elisp/emacs-goodies-el/boxquote" "\
Insert the contents of a buffer, boxes with `boxquote-region'.

If `boxquote-title-buffers' is non-nil the boxquote will be given a title that
is the result of applying `boxquote-buffer-title-function' to BUFFER.

\(fn BUFFER)" t nil)

(autoload 'boxquote-kill-ring-save "emacs-goodies-el/elisp/emacs-goodies-el/boxquote" "\
Like `kill-ring-save' but remembers a title if possible.

The title is acquired by calling `boxquote-kill-ring-save-title'. The title
will be used by `boxquote-yank'.

\(fn)" t nil)

(autoload 'boxquote-yank "emacs-goodies-el/elisp/emacs-goodies-el/boxquote" "\
Do a `yank' and box it in with `boxquote-region'.

If the yanked entry was placed on the kill ring with
`boxquote-kill-ring-save' the resulting boxquote will be titled with
whatever `boxquote-kill-ring-save-title' returned at the time.

\(fn)" t nil)

(autoload 'boxquote-defun "emacs-goodies-el/elisp/emacs-goodies-el/boxquote" "\
Apply `boxquote-region' the current defun.

\(fn)" t nil)

(autoload 'boxquote-paragraph "emacs-goodies-el/elisp/emacs-goodies-el/boxquote" "\
Apply `boxquote-region' to the current paragraph.

\(fn)" t nil)

(autoload 'boxquote-boxquote "emacs-goodies-el/elisp/emacs-goodies-el/boxquote" "\
Apply `boxquote-region' to the current boxquote.

\(fn)" t nil)

(autoload 'boxquote-describe-function "emacs-goodies-el/elisp/emacs-goodies-el/boxquote" "\
Call `describe-function' and boxquote the output into the current buffer.

\(fn)" t nil)

(autoload 'boxquote-describe-variable "emacs-goodies-el/elisp/emacs-goodies-el/boxquote" "\
Call `describe-variable' and boxquote the output into the current buffer.

\(fn)" t nil)

(autoload 'boxquote-describe-key "emacs-goodies-el/elisp/emacs-goodies-el/boxquote" "\
Call `describe-key' and boxquote the output into the current buffer.

If the call to this command is prefixed with \\[universal-argument] you will also be
prompted for a buffer. The key defintion used will be taken from that buffer.

\(fn KEY)" t nil)

(autoload 'boxquote-shell-command "emacs-goodies-el/elisp/emacs-goodies-el/boxquote" "\
Call `shell-command' with COMMAND and boxquote the output.

\(fn COMMAND)" t nil)

(autoload 'boxquote-where-is "emacs-goodies-el/elisp/emacs-goodies-el/boxquote" "\
Call `where-is' with DEFINITION and boxquote the result.

\(fn DEFINITION)" t nil)

(autoload 'boxquote-text "emacs-goodies-el/elisp/emacs-goodies-el/boxquote" "\
Insert TEXT, boxquoted.

\(fn TEXT)" t nil)

(autoload 'boxquote-narrow-to-boxquote "emacs-goodies-el/elisp/emacs-goodies-el/boxquote" "\
Narrow the buffer to the current boxquote.

\(fn)" t nil)

(autoload 'boxquote-narrow-to-boxquote-content "emacs-goodies-el/elisp/emacs-goodies-el/boxquote" "\
Narrow the buffer to the content of the current boxquote.

\(fn)" t nil)

(autoload 'boxquote-kill "emacs-goodies-el/elisp/emacs-goodies-el/boxquote" "\
Kill the boxquote and its contents.

\(fn)" t nil)

(autoload 'boxquote-fill-paragraph "emacs-goodies-el/elisp/emacs-goodies-el/boxquote" "\
Perform a `fill-paragraph' inside a boxquote.

\(fn ARG)" t nil)

(autoload 'boxquote-unbox-region "emacs-goodies-el/elisp/emacs-goodies-el/boxquote" "\
Remove a box created with `boxquote-region'.

\(fn START END)" t nil)

(autoload 'boxquote-unbox "emacs-goodies-el/elisp/emacs-goodies-el/boxquote" "\
Remove the boxquote that contains `point'.

\(fn)" t nil)

;;;***

;;;### (autoloads (browse-huge-tar-copy-file-at-point browse-huge-tar-view-file-at-point
;;;;;;  browse-huge-tar-file) "emacs-goodies-el/elisp/emacs-goodies-el/browse-huge-tar"
;;;;;;  "emacs-goodies-el/elisp/emacs-goodies-el/browse-huge-tar.el"
;;;;;;  (16313 9500))
;;; Generated autoloads from emacs-goodies-el/elisp/emacs-goodies-el/browse-huge-tar.el

(autoload 'browse-huge-tar-file "emacs-goodies-el/elisp/emacs-goodies-el/browse-huge-tar" "\
Create a buffer containing a listing of FILENAME as a tar file.

\(fn FILENAME)" t nil)

(autoload 'browse-huge-tar-view-file-at-point "emacs-goodies-el/elisp/emacs-goodies-el/browse-huge-tar" "\
Extract the file at the point into a buffer for viewing.

\(fn)" t nil)

(autoload 'browse-huge-tar-copy-file-at-point "emacs-goodies-el/elisp/emacs-goodies-el/browse-huge-tar" "\
Extract the file at the point and copy to a local file OUTFILE.
This requires the value of `shell-file-name' to support redirection using \">\".

\(fn OUTFILE)" t nil)

;;;***

;;;### (autoloads (browse-kill-ring browse-kill-ring-default-keybindings)
;;;;;;  "emacs-goodies-el/elisp/emacs-goodies-el/browse-kill-ring"
;;;;;;  "emacs-goodies-el/elisp/emacs-goodies-el/browse-kill-ring.el"
;;;;;;  (19103 54677))
;;; Generated autoloads from emacs-goodies-el/elisp/emacs-goodies-el/browse-kill-ring.el

(autoload 'browse-kill-ring-default-keybindings "emacs-goodies-el/elisp/emacs-goodies-el/browse-kill-ring" "\
Set up M-y (`yank-pop') so that it can invoke `browse-kill-ring'.
Normally, if M-y was not preceeded by C-y, then it has no useful
behavior.  This function sets things up so that M-y will invoke
`browse-kill-ring'.

\(fn)" t nil)

(autoload 'browse-kill-ring "emacs-goodies-el/elisp/emacs-goodies-el/browse-kill-ring" "\
Display items in the `kill-ring' in another buffer.

\(fn)" t nil)

;;;***

;;;### (autoloads (coffee) "emacs-goodies-el/elisp/emacs-goodies-el/coffee"
;;;;;;  "emacs-goodies-el/elisp/emacs-goodies-el/coffee.el" (16222
;;;;;;  36565))
;;; Generated autoloads from emacs-goodies-el/elisp/emacs-goodies-el/coffee.el

(autoload 'coffee "emacs-goodies-el/elisp/emacs-goodies-el/coffee" "\
Submit a BREW request to an RFC2324-compliant coffee device

\(fn)" t nil)

;;;***

;;;### (autoloads (color-theme-initialize color-theme-submit color-theme-install
;;;;;;  color-theme-compare color-theme-make-snapshot color-theme-analyze-defun
;;;;;;  color-theme-print color-theme-install-at-point-for-current-frame
;;;;;;  color-theme-install-at-mouse color-theme-describe color-theme-select)
;;;;;;  "emacs-goodies-el/elisp/emacs-goodies-el/color-theme" "emacs-goodies-el/elisp/emacs-goodies-el/color-theme.el"
;;;;;;  (19425 30094))
;;; Generated autoloads from emacs-goodies-el/elisp/emacs-goodies-el/color-theme.el

(autoload 'color-theme-select "emacs-goodies-el/elisp/emacs-goodies-el/color-theme" "\
Displays a special buffer for selecting and installing a color theme.
With optional prefix ARG, this buffer will include color theme libraries
as well.  A color theme library is in itself not complete, it must be
used as part of another color theme to be useful.  Thus, color theme
libraries are mainly useful for color theme authors.

\(fn &optional ARG)" t nil)

(autoload 'color-theme-describe "emacs-goodies-el/elisp/emacs-goodies-el/color-theme" "\
Describe color theme listed at point.
This shows the documentation of the value of text-property color-theme
at point.  The text-property color-theme should be a color theme
function.  See `color-themes'.

\(fn)" t nil)

(autoload 'color-theme-install-at-mouse "emacs-goodies-el/elisp/emacs-goodies-el/color-theme" "\
Install color theme clicked upon using the mouse.
First argument EVENT is used to set point.  Then
`color-theme-install-at-point' is called.

\(fn EVENT)" t nil)

(autoload 'color-theme-install-at-point-for-current-frame "emacs-goodies-el/elisp/emacs-goodies-el/color-theme" "\
Install color theme at point for current frame only.
Binds `color-theme-is-global' to nil and calls
`color-theme-install-at-point'.

\(fn)" t nil)

(autoload 'color-theme-print "emacs-goodies-el/elisp/emacs-goodies-el/color-theme" "\
Print the current color theme function.

You can contribute this function to <URL:news:gnu.emacs.sources> or
paste it into your .emacs file and call it.  That should recreate all
the settings necessary for your color theme.

Example:

    (require 'color-theme)
    (defun my-color-theme ()
      \"Color theme by Alex Schroeder, created 2000-05-17.\"
      (interactive)
      (color-theme-install
       '(...
	 ...
	 ...)))
    (my-color-theme)

If you want to use a specific color theme function, you can call the
color theme function in your .emacs directly.

Example:

    (require 'color-theme)
    (color-theme-gnome2)

\(fn &optional BUF)" t nil)

(autoload 'color-theme-analyze-defun "emacs-goodies-el/elisp/emacs-goodies-el/color-theme" "\
Once you have a color-theme printed, check for missing faces.
This is used by maintainers who receive a color-theme submission
and want to make sure it follows the guidelines by the color-theme
author.

\(fn)" t nil)

(autoload 'color-theme-make-snapshot "emacs-goodies-el/elisp/emacs-goodies-el/color-theme" "\
Return the definition of the current color-theme.
The function returned will recreate the color-theme in use at the moment.

\(fn)" nil nil)

(autoload 'color-theme-compare "emacs-goodies-el/elisp/emacs-goodies-el/color-theme" "\
Compare two color themes.
This will print the differences between installing THEME-A and
installing THEME-B.  Note that the order is important: If a face is
defined in THEME-A and not in THEME-B, then this will not show up as a
difference, because there is no reset before installing THEME-B.  If a
face is defined in THEME-B and not in THEME-A, then this will show up as
a difference.

\(fn THEME-A THEME-B)" t nil)

(autoload 'color-theme-install "emacs-goodies-el/elisp/emacs-goodies-el/color-theme" "\
Install a color theme defined by frame parameters, variables and faces.

The theme is installed for all present and future frames; any missing
faces are created.  See `color-theme-install-faces'.

THEME is a color theme definition.  See below for more information.

If you want to install a color theme from your .emacs, use the output
generated by `color-theme-print'.  This produces color theme function
which you can copy to your .emacs.

A color theme definition is a list:
\([FUNCTION] FRAME-PARAMETERS VARIABLE-SETTINGS FACE-DEFINITIONS)

FUNCTION is the color theme function which called `color-theme-install'.
This is no longer used.  There was a time when this package supported
automatic factoring of color themes.  This has been abandoned.

FRAME-PARAMETERS is an alist of frame parameters.  These are installed
with `color-theme-install-frame-params'.  These are installed last such
that any changes to the default face can be changed by the frame
parameters.

VARIABLE-DEFINITIONS is an alist of variable settings.  These are
installed with `color-theme-install-variables'.

FACE-DEFINITIONS is an alist of face definitions.  These are installed
with `color-theme-install-faces'.

If `color-theme-is-cumulative' is nil, a color theme will undo face and
frame-parameter settings of previous color themes.

\(fn THEME)" nil nil)

(autoload 'color-theme-submit "emacs-goodies-el/elisp/emacs-goodies-el/color-theme" "\
Submit your color-theme to the maintainer.

\(fn)" t nil)

(autoload 'color-theme-initialize "emacs-goodies-el/elisp/emacs-goodies-el/color-theme" "\
Initialize the color theme package by loading color-theme-libraries.

\(fn)" t nil)

;;;***

;;;### (autoloads (ctypes-read-file ctypes-auto-parse-mode ctypes-file
;;;;;;  ctypes-dir ctypes-tags ctypes-all-buffers ctypes-buffer ctypes-define-type-in-mode
;;;;;;  ctypes-define-type) "emacs-goodies-el/elisp/emacs-goodies-el/ctypes"
;;;;;;  "emacs-goodies-el/elisp/emacs-goodies-el/ctypes.el" (16313
;;;;;;  9500))
;;; Generated autoloads from emacs-goodies-el/elisp/emacs-goodies-el/ctypes.el

(autoload 'ctypes-define-type "emacs-goodies-el/elisp/emacs-goodies-el/ctypes" "\
Add a new TYPE to current major mode and inform font-lock.

When preceded by C-u the display is not updated.

Return non-nil if the type was not known before.

\(fn TYPE &optional DELAY-ACTION MODE)" t nil)

(autoload 'ctypes-define-type-in-mode "emacs-goodies-el/elisp/emacs-goodies-el/ctypes" "\
Add TYPE to major mode MODE and inform font-lock.

When preceded by C-u the display is not updated.

\(This function is designed for interactive use, please call
`ctypes-define-type' from Lisp programs.)

\(fn TYPE &optional DELAY-ACTION MODE)" t nil)

(autoload 'ctypes-buffer "emacs-goodies-el/elisp/emacs-goodies-el/ctypes" "\
Search for types in buffer, inform font-lock if any is found.

When preceded by C-u the action is not performed.

Return non-nil if new types are found.

\(fn &optional BUF DELAY-ACTION MODE)" t nil)

(autoload 'ctypes-all-buffers "emacs-goodies-el/elisp/emacs-goodies-el/ctypes" "\
Search for types in all buffers, inform font-lock about all discoveries.

When preceded by C-u the display is not updated.

Return non-nil if new types are found.

\(fn &optional DELAY-ACTION)" t nil)

(autoload 'ctypes-tags "emacs-goodies-el/elisp/emacs-goodies-el/ctypes" "\
Search for types in files in the visited TAGS table.
Should no tags table be visited, the user will be prompted for a new.

When preceded by C-u the display is not updated.

Return non-nil if new types are found.

\(fn &optional DELAY-ACTION)" t nil)

(autoload 'ctypes-dir "emacs-goodies-el/elisp/emacs-goodies-el/ctypes" "\
Search for types in files in a directory hierarchy.

See variable `ctypes-dir-read-file' for a description of which files
are opened during scanning, and how you can change the behavior.

When preceded by C-u the display is not updated.

Return non-nil if new types are found.

\(fn &optional DIR DELAY-ACTION)" t nil)

(autoload 'ctypes-file "emacs-goodies-el/elisp/emacs-goodies-el/ctypes" "\
Search for types in file FILE.
Should FILE not be loaded it is read into a temporary buffer.

Return mode of file, if new types was found.

\(fn FILE &optional DELAY-ACTION)" t nil)

(autoload 'ctypes-auto-parse-mode "emacs-goodies-el/elisp/emacs-goodies-el/ctypes" "\
Toggle CTypes auto parse mode; search all new buffers for types.
With arg, turn types Auto Mode on if and only if arg is positive.

This a global minor mode, it does not have a private keymap, nor does
it add itself to the mode line.

Place the following in your startup file to enable this feature in
future sessions:

    (require 'ctypes)
    (ctypes-auto-parse-mode 1)

When activated, the functions in the hook `ctypes-auto-parse-mode-hook'
is called with no args.

\(fn &optional ARG)" t nil)

(autoload 'ctypes-read-file "emacs-goodies-el/elisp/emacs-goodies-el/ctypes" "\
Load types previously saved with `ctypes-write-file'.
The name of the file is given by the optional argument FILE.
Should no file name be given the value of the variable `ctypes-file-name'
is used.

Please note that the types read will be added to the current types.

When preceded by C-u the display is not updated.

The third argument, NO-ERROR, determines whether or not we should
raise an error if there should be any problem loading the file.

Should the fourth argument, QUIETLY, be non-nil no messages are
generated when the file is loaded.

Return non-nil if new types are found.

\(fn &optional FILE DELAY-ACTION NO-ERROR QUIETLY)" t nil)

;;;***

;;;### (autoloads (df) "emacs-goodies-el/elisp/emacs-goodies-el/df"
;;;;;;  "emacs-goodies-el/elisp/emacs-goodies-el/df.el" (19104 28952))
;;; Generated autoloads from emacs-goodies-el/elisp/emacs-goodies-el/df.el

(autoload 'df "emacs-goodies-el/elisp/emacs-goodies-el/df" "\
Enables display of space left on any PARTITION in mode-lines.
This display updates automatically every `df-refresh' seconds.

\(fn &optional PARTITION)" t nil)

;;;***

;;;### (autoloads (diminished-modes diminish-undo diminish) "emacs-goodies-el/elisp/emacs-goodies-el/diminish"
;;;;;;  "emacs-goodies-el/elisp/emacs-goodies-el/diminish.el" (16013
;;;;;;  59391))
;;; Generated autoloads from emacs-goodies-el/elisp/emacs-goodies-el/diminish.el

(autoload 'diminish "emacs-goodies-el/elisp/emacs-goodies-el/diminish" "\
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

(autoload 'diminish-undo "emacs-goodies-el/elisp/emacs-goodies-el/diminish" "\
Restore mode-line display of diminished mode MODE to its minor-mode value.
Do nothing if the arg is a minor mode that hasn't been diminished.

Interactively, enter (with completion) the name of any diminished mode (a
mode that was formerly a minor mode on which you invoked M-x diminish).
To restore all diminished modes to minor status, answer `diminished-modes'.
The response to the prompt shouldn't be quoted.  However, in Lisp code,
the arg must be quoted as a symbol, as in (diminish-undo 'diminished-modes).

\(fn MODE)" t nil)

(autoload 'diminished-modes "emacs-goodies-el/elisp/emacs-goodies-el/diminish" "\
Echo all active diminished or minor modes as if they were minor.
The display goes in the echo area; if it's too long even for that,
you can see the whole thing in the *Messages* buffer.
This doesn't change the status of any modes; it just lets you see
what diminished modes would be on the mode-line if they were still minor.

\(fn)" t nil)

;;;***

;;;### (autoloads (egocentric-update-regexp-list egocentric-mode-off
;;;;;;  egocentric-mode-on egocentric-mode) "emacs-goodies-el/elisp/emacs-goodies-el/egocentric"
;;;;;;  "emacs-goodies-el/elisp/emacs-goodies-el/egocentric.el" (19390
;;;;;;  42260))
;;; Generated autoloads from emacs-goodies-el/elisp/emacs-goodies-el/egocentric.el

(autoload 'egocentric-mode "emacs-goodies-el/elisp/emacs-goodies-el/egocentric" "\
Toggle egocentric mode.
Optional argument ARG is an optional boolean controling whether egocentric-mode should be turned on/off.

\(fn &optional ARG)" t nil)

(autoload 'egocentric-mode-on "emacs-goodies-el/elisp/emacs-goodies-el/egocentric" "\
Turn Egocentric mode on.

\(fn)" t nil)

(autoload 'egocentric-mode-off "emacs-goodies-el/elisp/emacs-goodies-el/egocentric" "\
Turn Egocentric mode off.

\(fn)" t nil)

(autoload 'egocentric-update-regexp-list "emacs-goodies-el/elisp/emacs-goodies-el/egocentric" "\
Update ``egocentric-regexp-list'' from $USER and $NAME variables.

\(fn)" t nil)

;;;***

;;;### (autoloads (eproject-compile eproject-eshell-cd-here eproject-multi-isearch-buffers
;;;;;;  eproject-todo eproject-grep eproject-revisit-project eproject-kill-project-buffers
;;;;;;  eproject-ibuffer eproject-find-file) "emacs-goodies-el/elisp/emacs-goodies-el/eproject-extras"
;;;;;;  "emacs-goodies-el/elisp/emacs-goodies-el/eproject-extras.el"
;;;;;;  (19974 14166))
;;; Generated autoloads from emacs-goodies-el/elisp/emacs-goodies-el/eproject-extras.el

(autoload 'eproject-find-file "emacs-goodies-el/elisp/emacs-goodies-el/eproject-extras" "\
Present the user with a list of files in the current project.
to select from, open file when selected.

\(fn)" t nil)

(autoload 'eproject-ibuffer "emacs-goodies-el/elisp/emacs-goodies-el/eproject-extras" "\
Open an IBuffer window showing all buffers in the current project, or named project if PREFIX arg is supplied.

\(fn PREFIX)" t nil)

(autoload 'eproject-kill-project-buffers "emacs-goodies-el/elisp/emacs-goodies-el/eproject-extras" "\
Kill every buffer in the current project, including the current buffer.

If PREFIX is specified, prompt for a project name and kill those
buffers instead.

\(fn PREFIX)" t nil)

(autoload 'eproject-revisit-project "emacs-goodies-el/elisp/emacs-goodies-el/eproject-extras" "\
Given a project name, visit the root directory.

If PREFIX arg is supplied, run `eproject-find-file'.

\(fn PREFIX)" t nil)

(autoload 'eproject-grep "emacs-goodies-el/elisp/emacs-goodies-el/eproject-extras" "\
Search all files in the current project for REGEXP.

\(fn REGEXP)" t nil)

(autoload 'eproject-todo "emacs-goodies-el/elisp/emacs-goodies-el/eproject-extras" "\
Display a project TODO list.

Customize `eproject-todo-expressions' to control what this function looks for.

\(fn)" t nil)

(autoload 'eproject-multi-isearch-buffers "emacs-goodies-el/elisp/emacs-goodies-el/eproject-extras" "\
Do a `multi-isearch' on opened buffers in the current project.

Run `eproject-open-all-project-files' first or just
`eproject-grep' if you want to search all project files.

\(fn)" t nil)

(autoload 'eproject-eshell-cd-here "emacs-goodies-el/elisp/emacs-goodies-el/eproject-extras" "\
If there is an EShell buffer, cd to the project root in that buffer.

With the prefix arg LOOK-IN-INVISIBLE-BUFFERS looks in buffers that are not currently displayed.

\(fn &optional LOOK-IN-INVISIBLE-BUFFERS)" t nil)

(autoload 'eproject-compile "emacs-goodies-el/elisp/emacs-goodies-el/eproject-extras" "\
Run `compile-command' in the project root.

\(fn)" t nil)

;;;***

;;;### (autoloads (ff-paths-install) "emacs-goodies-el/elisp/emacs-goodies-el/ff-paths"
;;;;;;  "emacs-goodies-el/elisp/emacs-goodies-el/ff-paths.el" (20043
;;;;;;  15601))
;;; Generated autoloads from emacs-goodies-el/elisp/emacs-goodies-el/ff-paths.el

(autoload 'ff-paths-install "emacs-goodies-el/elisp/emacs-goodies-el/ff-paths" "\
Install ff-paths as a `find-file-not-found-hooks' and to ffap package.

\(fn)" nil nil)

;;;***

;;;### (autoloads (floatbg-mode) "emacs-goodies-el/elisp/emacs-goodies-el/floatbg"
;;;;;;  "emacs-goodies-el/elisp/emacs-goodies-el/floatbg.el" (16013
;;;;;;  59393))
;;; Generated autoloads from emacs-goodies-el/elisp/emacs-goodies-el/floatbg.el

(autoload 'floatbg-mode "emacs-goodies-el/elisp/emacs-goodies-el/floatbg" "\
Toggle floatbg mode

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (folding-mode turn-on-folding-mode turn-off-folding-mode
;;;;;;  folding-mode-add-find-file-hook folding-keep-hooked folding-install-hooks
;;;;;;  folding-uninstall-hooks folding-mode-hook-no-regexp folding-mode-string
;;;;;;  folding-inside-mode-name folding-default-mouse-keys-function
;;;;;;  folding-default-keys-function) "emacs-goodies-el/elisp/emacs-goodies-el/folding"
;;;;;;  "emacs-goodies-el/elisp/emacs-goodies-el/folding.el" (19104
;;;;;;  31300))
;;; Generated autoloads from emacs-goodies-el/elisp/emacs-goodies-el/folding.el

(defvar folding-mode nil "\
When Non nil, Folding mode is active in the current buffer.")

(defvar folding-default-keys-function 'folding-bind-default-keys "\
*Function or list of functions used to define keys for Folding mode.
Possible values are:
  folding-bind-default-key
        The standard keymap.

  `folding-bind-backward-compatible-keys'
        Keys used by older versions of Folding mode. This function
        does not conform to Emacs 19.29 style conversions concerning
        key bindings. The prefix key is C - c

  `folding-bind-outline-compatible-keys'
        Define keys compatible with Outline mode.

  `folding-bind-foldout-compatible-keys'
        Define some extra keys compatible with Foldout.

All except `folding-bind-backward-compatible-keys' used the value of
the variable `folding-mode-prefix-key' as prefix the key.
The default is C - c @")

(custom-autoload 'folding-default-keys-function "emacs-goodies-el/elisp/emacs-goodies-el/folding" t)

(defvar folding-default-mouse-keys-function 'folding-bind-default-mouse "\
*Function to bind default mouse keys to `folding-mode-map'.")

(custom-autoload 'folding-default-mouse-keys-function "emacs-goodies-el/elisp/emacs-goodies-el/folding" t)

(defvar folding-inside-mode-name "Fld" "\
*Mode line addition to show inside levels of 'fold' .")

(custom-autoload 'folding-inside-mode-name "emacs-goodies-el/elisp/emacs-goodies-el/folding" t)

(defvar folding-mode-string "Fld" "\
*The minor mode string displayed when mode is on.")

(custom-autoload 'folding-mode-string "emacs-goodies-el/elisp/emacs-goodies-el/folding" t)

(defvar folding-mode-hook-no-regexp "RMAIL" "\
*Regexp which disable automatic folding mode turn on for certain files.")

(custom-autoload 'folding-mode-hook-no-regexp "emacs-goodies-el/elisp/emacs-goodies-el/folding" t)

(defvar folding-mode-marks-alist nil "\
List of (major-mode . fold mark) default combinations to use.
When Folding mode is started, the major mode is checked, and if there
are fold marks for that major mode stored in `folding-mode-marks-alist',
those marks are used by default. If none are found, the default values
of \"{{{ \" and \"}}}\" are used.

Use function  `folding-add-to-marks-list' to add more fold marks. The function
also explains the alist use in details.

Use function `folding-set-local-variables' if you change the current mode's
folding marks during the session.")

(autoload 'folding-uninstall-hooks "emacs-goodies-el/elisp/emacs-goodies-el/folding" "\
Remove hooks set by folding.

\(fn)" nil nil)

(autoload 'folding-install-hooks "emacs-goodies-el/elisp/emacs-goodies-el/folding" "\
Install folding hooks.

\(fn)" nil nil)

(autoload 'folding-keep-hooked "emacs-goodies-el/elisp/emacs-goodies-el/folding" "\
Make sure hooks are in their places.

\(fn)" nil nil)

(autoload 'folding-mode-add-find-file-hook "emacs-goodies-el/elisp/emacs-goodies-el/folding" "\
Append `folding-mode-find-file-hook' to the list `find-file-hooks'.

This has the effect that afterwards, when a folded file is visited, if
appropriate Emacs local variable entries are recognized at the end of
the file, Folding mode is started automatically.

If `inhibit-local-variables' is non-nil, this will not happen regardless
of the setting of `find-file-hooks'.

To declare a file to be folded, put `folded-file: t' in the file's
local variables. eg., at the end of a C source file, put:

/*
Local variables:
folded-file: t
*/

The local variables can be inside a fold.

\(fn)" t nil)

(autoload 'turn-off-folding-mode "emacs-goodies-el/elisp/emacs-goodies-el/folding" "\
Turn off folding.

\(fn)" nil nil)

(autoload 'turn-on-folding-mode "emacs-goodies-el/elisp/emacs-goodies-el/folding" "\
Turn on folding.

\(fn)" nil nil)

(autoload 'folding-mode "emacs-goodies-el/elisp/emacs-goodies-el/folding" "\
A folding-editor-like minor mode. ARG INTER.

These are the basic commands that Folding mode provides:

\\{folding-mode-map}

Keys starting with `folding-mode-prefix-key'

\\{folding-mode-prefix-map}

     folding-convert-buffer-for-printing:
     `\\[folding-convert-buffer-for-printing]'
     Makes a ready-to-print, formatted, unfolded copy in another buffer.

     Read the documentation for the above functions for more information.

Overview

    Folds are a way of hierarchically organizing the text in a file, so
    that the text can be viewed and edited at different levels. It is
    similar to Outline mode in that parts of the text can be hidden from
    view. A fold is a region of text, surrounded by special \"fold marks\",
    which act like brackets, grouping the text. Fold mark pairs can be
    nested, and they can have titles. When a fold is folded, the text is
    hidden from view, except for the first line, which acts like a title
    for the fold.

    Folding mode is a minor mode, designed to cooperate with many other
    major modes, so that many types of text can be folded while they are
    being edited (eg., plain text, program source code, Texinfo, etc.).

Folding-mode function

    If Folding mode is not called interactively (`(interactive-p)' is nil),
    and it is called with two or less arguments, all of which are nil, then
    the point will not be altered if `folding-folding-on-startup' is set
    and `folding-whole-buffer' is called. This is generally not a good
    thing, as it can leave the point inside a hidden region of a fold, but
    it is required if the local variables set \"mode: folding\" when the
    file is first read (see `hack-local-variables').

    Not that you should ever want to, but to call Folding mode from a
    program with the default behavior (toggling the mode), call it with
    something like `(folding-mode nil t)'.

Fold marks

    For most types of folded file, lines representing folds have \"{{{\"
    near the beginning. To enter a fold, move the point to the folded line
    and type `\\[folding-shift-in]'. You should no longer be able to see
    the rest of the file, just the contents of the fold, which you couldn't
    see before. You can use `\\[folding-shift-out]' to leave a fold, and
    you can enter and exit folds to move around the structure of the file.

    All of the text is present in a folded file all of the time. It is just
    hidden. Folded text shows up as a line (the top fold mark) with \"...\"
    at the end. If you are in a fold, the mode line displays \"inside n
    folds Narrow\", and because the buffer is narrowed you can't see outside
    of the current fold's text.

    By arranging sections of a large file in folds, and maybe subsections
    in sub-folds, you can move around a file quickly and easily, and only
    have to scroll through a couple of pages at a time. If you pick the
    titles for the folds carefully, they can be a useful form of
    documentation, and make moving though the file a lot easier. In
    general, searching through a folded file for a particular item is much
    easier than without folds.

Managing folds

    To make a new fold, set the mark at one end of the text you want in the
    new fold, and move the point to the other end. Then type
    `\\[folding-fold-region]'. The text you selected will be made into a
    fold, and the fold will be entered. If you just want a new, empty fold,
    set the mark where you want the fold, and then create a new fold there
    without moving the point. Don't worry if the point is in the middle of
    a line of text, `folding-fold-region' will not break text in the middle
    of a line. After making a fold, the fold is entered and the point is
    positioned ready to enter a title for the fold. Do not delete the fold
    marks, which are usually something like \"{{{\" and \"}}}\". There may
    also be a bit of fold mark which goes after the fold title.

    If the fold markers get messed up, or you just want to see the whole
    unfolded file, use `\\[folding-open-buffer]' to unfolded the whole
    file, so you can see all the text and all the marks. This is useful for
    checking/correcting unbalanced fold markers, and for searching for
    things. Use `\\[folding-whole-file]' to fold the buffer again.

    `folding-shift-out' will attempt to tidy the current fold just before
    exiting it. It will remove any extra blank lines at the top and bottom,
    (outside the fold marks). It will then ensure that fold marks exists,
    and if they are not, will add them (after asking). Finally, the number
    of blank lines between the fold marks and the contents of the fold is
    set to 1 (by default).

Folding package customizations

    If the fold marks are not set on entry to Folding mode, they are set to
    a default for current major mode, as defined by
    `folding-mode-marks-alist' or to \"{{{ \" and \"}}}\" if none are
    specified.

    To bind different commands to keys in Folding mode, set the bindings in
    the keymap `folding-mode-map'.

    The hooks `folding-mode-hook' and `<major-mode-name>-folding-hook' are
    called before folding the buffer and applying the key bindings in
    `folding-mode-map'. This is a good hook to set extra or different key
    bindings in `folding-mode-map'. Note that key bindings in
    `folding-mode-map' are only examined just after calling these hooks;
    new bindings in those maps only take effect when Folding mode is being
    started. The hook `folding-load-hook' is called when Folding mode is
    loaded into Emacs.

Mouse behavior

    If you want folding to detect point of actual mouse click, please see
    variable `folding-mouse-yank-at-p'.

    To customise the mouse actions, look at `folding-behave-table'.

\(fn &optional ARG INTER)" t nil)

;;;***

;;;### (autoloads (framepop-display-buffer framepop-enable framepop-disable)
;;;;;;  "emacs-goodies-el/elisp/emacs-goodies-el/framepop" "emacs-goodies-el/elisp/emacs-goodies-el/framepop.el"
;;;;;;  (16269 22230))
;;; Generated autoloads from emacs-goodies-el/elisp/emacs-goodies-el/framepop.el

(autoload 'framepop-disable "emacs-goodies-el/elisp/emacs-goodies-el/framepop" "\
Disable automatic pop-up temporary windows.

\(fn)" t nil)

(autoload 'framepop-enable "emacs-goodies-el/elisp/emacs-goodies-el/framepop" "\
Enable automatic pop-up temporary windows.

\(fn)" t nil)

(autoload 'framepop-display-buffer "emacs-goodies-el/elisp/emacs-goodies-el/framepop" "\
Display-buffer for FramePop.
Displays BUF in a separate frame -- the FramePop frame.
BUF bay be a buffer or a buffer name.

You can display a buffer in the FramePop frame with \\[framepop-display-buffer].

Several commands are available for manipulating the FramePop frame after
typing the keymap prefix (default F2).

\\{framepop-map}

\(fn BUF)" t nil)

;;;***

;;;### (autoloads (graphviz-dot-mode) "emacs-goodies-el/elisp/emacs-goodies-el/graphviz-dot-mode"
;;;;;;  "emacs-goodies-el/elisp/emacs-goodies-el/graphviz-dot-mode.el"
;;;;;;  (19810 28840))
;;; Generated autoloads from emacs-goodies-el/elisp/emacs-goodies-el/graphviz-dot-mode.el

(autoload 'graphviz-dot-mode "emacs-goodies-el/elisp/emacs-goodies-el/graphviz-dot-mode" "\
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

;;;### (autoloads (highlight-current-line-minor-mode) "emacs-goodies-el/elisp/emacs-goodies-el/highlight-current-line"
;;;;;;  "emacs-goodies-el/elisp/emacs-goodies-el/highlight-current-line.el"
;;;;;;  (19104 31300))
;;; Generated autoloads from emacs-goodies-el/elisp/emacs-goodies-el/highlight-current-line.el

(autoload 'highlight-current-line-minor-mode "emacs-goodies-el/elisp/emacs-goodies-el/highlight-current-line" "\
Toggle highlight-current-line minor mode.
With ARG, turn minor mode on if ARG is positive, off otherwise.
You can customize the face of the highlighted line and whether the entire
line is hightlighted by customizing the group highlight-current-line.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (home-end-end home-end-home) "emacs-goodies-el/elisp/emacs-goodies-el/home-end"
;;;;;;  "emacs-goodies-el/elisp/emacs-goodies-el/home-end.el" (19812
;;;;;;  22000))
;;; Generated autoloads from emacs-goodies-el/elisp/emacs-goodies-el/home-end.el

(autoload 'home-end-home "emacs-goodies-el/elisp/emacs-goodies-el/home-end" "\
Go to beginning of line/window/buffer.
First hitting key goes to beginning of line, second in a row goes to
beginning of window, third in a row goes to beginning of buffer.

\(fn &optional ARG)" t nil)

(autoload 'home-end-end "emacs-goodies-el/elisp/emacs-goodies-el/home-end" "\
Go to end of line/window/buffer.
First hitting key goes to end of line, second in a row goes to end
of window, third in a row goes to end of buffer.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (htmlize-many-files-dired htmlize-many-files htmlize-file
;;;;;;  htmlize-region htmlize-buffer) "emacs-goodies-el/elisp/emacs-goodies-el/htmlize"
;;;;;;  "emacs-goodies-el/elisp/emacs-goodies-el/htmlize.el" (19103
;;;;;;  48317))
;;; Generated autoloads from emacs-goodies-el/elisp/emacs-goodies-el/htmlize.el

(autoload 'htmlize-buffer "emacs-goodies-el/elisp/emacs-goodies-el/htmlize" "\
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

(autoload 'htmlize-region "emacs-goodies-el/elisp/emacs-goodies-el/htmlize" "\
Convert the region to HTML, preserving colors and decorations.
See `htmlize-buffer' for details.

\(fn BEG END)" t nil)

(autoload 'htmlize-file "emacs-goodies-el/elisp/emacs-goodies-el/htmlize" "\
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

(autoload 'htmlize-many-files "emacs-goodies-el/elisp/emacs-goodies-el/htmlize" "\
Convert FILES to HTML and save the corresponding HTML versions.

FILES should be a list of file names to convert.  This function calls
`htmlize-file' on each file; see that function for details.  When
invoked interactively, you are prompted for a list of files to convert,
terminated with RET.

If TARGET-DIRECTORY is specified, the HTML files will be saved to that
directory.  Normally, each HTML file is saved to the directory of the
corresponding source file.

\(fn FILES &optional TARGET-DIRECTORY)" t nil)

(autoload 'htmlize-many-files-dired "emacs-goodies-el/elisp/emacs-goodies-el/htmlize" "\
HTMLize dired-marked files.

\(fn ARG &optional TARGET-DIRECTORY)" t nil)

;;;***

;;;### (autoloads (keydef) "emacs-goodies-el/elisp/emacs-goodies-el/keydef"
;;;;;;  "emacs-goodies-el/elisp/emacs-goodies-el/keydef.el" (16013
;;;;;;  59398))
;;; Generated autoloads from emacs-goodies-el/elisp/emacs-goodies-el/keydef.el

(autoload 'keydef "emacs-goodies-el/elisp/emacs-goodies-el/keydef" "\
Define the key sequence SEQ, written in kbd form, to run CMD.
CMD is automatically wrapped in an anonymous interactive function if it
is Emacs Lisp code rather than a command name. SEQ may also have the form
\(MODE SEQ) where the car is a mode name; for example

  (keydef (latex \"C-c %\") comment-region)

means to define the given key in latex-mode-map. And this will work even
if latex-mode is not loaded yet, provided that it is possible to deduce
the file that it will be loaded from, either from the autoload info or
by searching for a matching file name in the Emacs load path.

For best results, the \"mode name\" that you use here should yield the
proper foo-mode-map symbol when \"-mode-map\" is appended; although
this will normally match the mode name as given in the mode line,
Shell-script is one example I can think of where it doesn't---the map is
named sh-mode-map. The common cases that I know about, including
shell-script-mode and latex-mode, are handled as exceptions through the
variable mode-map-alist. But for other cases you will need to look up
the name of the mode-map that goes with the given mode.

\(fn SEQ &rest CMD)" nil (quote macro))

;;;***

;;;### (autoloads (lcomp-keys-mode lcomp-mode) "emacs-goodies-el/elisp/emacs-goodies-el/lcomp"
;;;;;;  "emacs-goodies-el/elisp/emacs-goodies-el/lcomp.el" (19331
;;;;;;  13881))
;;; Generated autoloads from emacs-goodies-el/elisp/emacs-goodies-el/lcomp.el

(defvar lcomp-mode nil "\
Non-nil if Lcomp mode is enabled.
See the command `lcomp-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `lcomp-mode'.")

(custom-autoload 'lcomp-mode "emacs-goodies-el/elisp/emacs-goodies-el/lcomp" nil)

(autoload 'lcomp-mode "emacs-goodies-el/elisp/emacs-goodies-el/lcomp" "\
Auto close completion window mode.

\(fn &optional ARG)" t nil)

(defvar lcomp-keys-mode nil "\
Non-nil if Lcomp-Keys mode is enabled.
See the command `lcomp-keys-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `lcomp-keys-mode'.")

(custom-autoload 'lcomp-keys-mode "emacs-goodies-el/elisp/emacs-goodies-el/lcomp" nil)

(autoload 'lcomp-keys-mode "emacs-goodies-el/elisp/emacs-goodies-el/lcomp" "\
Add keybindings to the completions buffer.

\\{lcomp-keys-override-map}

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (map-lines-copy map-lines-kill copy-line map-lines)
;;;;;;  "emacs-goodies-el/elisp/emacs-goodies-el/map-lines" "emacs-goodies-el/elisp/emacs-goodies-el/map-lines.el"
;;;;;;  (19381 25289))
;;; Generated autoloads from emacs-goodies-el/elisp/emacs-goodies-el/map-lines.el

(autoload 'map-lines "emacs-goodies-el/elisp/emacs-goodies-el/map-lines" "\
Map a COMMAND-C (kill, copying, or a custom command) over lines matching REGEX.

\(fn COMMAND-C REGEX)" t nil)

(autoload 'copy-line "emacs-goodies-el/elisp/emacs-goodies-el/map-lines" "\
Copy a whole line to the kill ring.

\(fn)" t nil)

(autoload 'map-lines-kill "emacs-goodies-el/elisp/emacs-goodies-el/map-lines" "\
Kill all lines matching REGEX.  Yanking will insert all killed lines.

\(fn REGEX)" t nil)

(autoload 'map-lines-copy "emacs-goodies-el/elisp/emacs-goodies-el/map-lines" "\
Copy all lines matching REGEX to the kill ring.  Yanking will insert all such lines.

\(fn REGEX)" t nil)

;;;***

;;;### (autoloads (markdown-mode) "emacs-goodies-el/elisp/emacs-goodies-el/markdown-mode"
;;;;;;  "emacs-goodies-el/elisp/emacs-goodies-el/markdown-mode.el"
;;;;;;  (20043 182))
;;; Generated autoloads from emacs-goodies-el/elisp/emacs-goodies-el/markdown-mode.el

(autoload 'markdown-mode "emacs-goodies-el/elisp/emacs-goodies-el/markdown-mode" "\
Major mode for editing Markdown files.

\(fn)" t nil)

;;;***

;;;### (autoloads (matlab-shell matlab-mode) "emacs-goodies-el/elisp/emacs-goodies-el/matlab"
;;;;;;  "emacs-goodies-el/elisp/emacs-goodies-el/matlab.el" (19223
;;;;;;  14031))
;;; Generated autoloads from emacs-goodies-el/elisp/emacs-goodies-el/matlab.el

(autoload 'matlab-mode "emacs-goodies-el/elisp/emacs-goodies-el/matlab" "\
MATLAB(R) mode is a major mode for editing MATLAB dot-m files.
\\<matlab-mode-map>
Convenient editing commands are:
 \\[matlab-comment-region]   - Comment/Uncomment out a region of code.
 \\[matlab-fill-comment-line] - Fill the current comment line.
 \\[matlab-fill-region] - Fill code and comments in region.
 \\[matlab-fill-paragraph]     - Refill the current command or comment.
 \\[matlab-complete-symbol]   - Symbol completion of matlab symbolsbased on the local syntax.
 \\[matlab-indent-sexp] - Indent syntactic block of code.

Convenient navigation commands are:
 \\[matlab-beginning-of-command]   - Move to the beginning of a command.
 \\[matlab-end-of-command]   - Move to the end of a command.
 \\[matlab-beginning-of-defun] - Move to the beginning of a function.
 \\[matlab-end-of-defun] - Move do the end of a function.
 \\[matlab-forward-sexp] - Move forward over a syntactic block of code.
 \\[matlab-backward-sexp] - Move backwards over a syntactic block of code.

Convenient template insertion commands:
 \\[tempo-template-matlab-function] - Insert a function definition.
 \\[tempo-template-matlab-if] - Insert an IF END block.
 \\[tempo-template-matlab-for] - Insert a FOR END block.
 \\[tempo-template-matlab-switch] - Insert a SWITCH END statement.
 \\[matlab-insert-next-case] - Insert the next CASE condition in a SWITCH.
 \\[matlab-insert-end-block] - Insert a matched END statement.  With optional ARG, reindent.
 \\[matlab-stringify-region] - Convert plaintext in region to a string with correctly quoted chars.

Variables:
  `matlab-indent-level'		Level to indent blocks.
  `matlab-cont-level'		Level to indent continuation lines.
  `matlab-cont-requires-ellipsis' Does your MATLAB support implied elipsis.
  `matlab-case-level'		Level to unindent case statements.
  `matlab-indent-past-arg1-functions'
                                Regexp of functions to indent past the first
                                  argument on continuation lines.
  `matlab-maximum-indents'      List of maximum indents during lineups.
  `matlab-comment-column'       Goal column for on-line comments.
  `fill-column'			Column used in auto-fill.
  `matlab-indent-function-body' If non-nil, indents body of MATLAB functions.
  `matlab-functions-have-end'	If non-nil, MATLAB functions terminate with end.
  `matlab-return-function'	Customize RET handling with this function.
  `matlab-auto-fill'            Non-nil, do auto-fill at startup.
  `matlab-fill-code'            Non-nil, auto-fill code.
  `matlab-fill-strings'         Non-nil, auto-fill strings.
  `matlab-verify-on-save-flag'  Non-nil, enable code checks on save.
  `matlab-highlight-block-match-flag'
                                Enable matching block begin/end keywords.
  `matlab-vers-on-startup'	If t, show version on start-up.
  `matlab-handle-simulink'      If t, enable simulink keyword highlighting.

All Key Bindings:
\\{matlab-mode-map}

\(fn)" t nil)

(autoload 'matlab-shell "emacs-goodies-el/elisp/emacs-goodies-el/matlab" "\
Create a buffer with MATLAB running as a subprocess.

MATLAB shell cannot work on the MS Windows platform because MATLAB is not
a console application.

\(fn)" t nil)

;;;***

;;;### (autoloads (miniedit-install-for-xemacs miniedit-install miniedit
;;;;;;  miniedit-introduction miniedit-quick-start) "emacs-goodies-el/elisp/emacs-goodies-el/miniedit"
;;;;;;  "emacs-goodies-el/elisp/emacs-goodies-el/miniedit.el" (19387
;;;;;;  53923))
;;; Generated autoloads from emacs-goodies-el/elisp/emacs-goodies-el/miniedit.el

(autoload 'miniedit-quick-start "emacs-goodies-el/elisp/emacs-goodies-el/miniedit" "\
Provides electric help for function `miniedit-quick-start'.

\(fn)" t nil)

(autoload 'miniedit-introduction "emacs-goodies-el/elisp/emacs-goodies-el/miniedit" "\
Provides electric help for function `miniedit-introduction'.

\(fn)" t nil)

(autoload 'miniedit "emacs-goodies-el/elisp/emacs-goodies-el/miniedit" "\
The main miniedit function.

\(fn)" t nil)

(autoload 'miniedit-install "emacs-goodies-el/elisp/emacs-goodies-el/miniedit" "\
Install miniedit by frobbing your miniedit-local maps.

\(fn)" t nil)

(autoload 'miniedit-install-for-xemacs "emacs-goodies-el/elisp/emacs-goodies-el/miniedit" "\
Try to Install miniedit for Xemacs.

\(fn)" t nil)

;;;***

;;;### (autoloads (muttrc-mode) "emacs-goodies-el/elisp/emacs-goodies-el/muttrc-mode"
;;;;;;  "emacs-goodies-el/elisp/emacs-goodies-el/muttrc-mode.el"
;;;;;;  (19030 17715))
;;; Generated autoloads from emacs-goodies-el/elisp/emacs-goodies-el/muttrc-mode.el

(autoload 'muttrc-mode "emacs-goodies-el/elisp/emacs-goodies-el/muttrc-mode" "\
Major mode for editing Muttrc files.
This function ends by invoking the function(s) `muttrc-mode-hook'.

\\{muttrc-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads (nuke-trailing-whitespace) "emacs-goodies-el/elisp/emacs-goodies-el/nuke-trailing-whitespace"
;;;;;;  "emacs-goodies-el/elisp/emacs-goodies-el/nuke-trailing-whitespace.el"
;;;;;;  (19104 31301))
;;; Generated autoloads from emacs-goodies-el/elisp/emacs-goodies-el/nuke-trailing-whitespace.el

(autoload 'nuke-trailing-whitespace "emacs-goodies-el/elisp/emacs-goodies-el/nuke-trailing-whitespace" "\
Nuke all trailing whitespace in the buffer.
Whitespace in this case is just spaces or tabs.
This is a useful function to put on write-file-hooks.

Unless called interactively, this function uses
`nuke-trailing-whitespace-p' to determine how to behave.
However, even if this variable is `t', this function will query for
replacement if the buffer is read-only.

\(fn)" t nil)

;;;***

;;;### (autoloads (obfuscate-url) "emacs-goodies-el/elisp/emacs-goodies-el/obfusurl"
;;;;;;  "emacs-goodies-el/elisp/emacs-goodies-el/obfusurl.el" (18431
;;;;;;  47702))
;;; Generated autoloads from emacs-goodies-el/elisp/emacs-goodies-el/obfusurl.el

(autoload 'obfuscate-url "emacs-goodies-el/elisp/emacs-goodies-el/obfusurl" "\
Obfuscate an URL under `point'.

This might be useful if you're writing out an URL for someone but the URL
itself is a spoiler. The URL will still work but it won't be readable (by
most mortals anyway).

\(fn)" t nil)

;;;***

;;;### (autoloads (pack-windows) "emacs-goodies-el/elisp/emacs-goodies-el/pack-windows"
;;;;;;  "emacs-goodies-el/elisp/emacs-goodies-el/pack-windows.el"
;;;;;;  (16313 9500))
;;; Generated autoloads from emacs-goodies-el/elisp/emacs-goodies-el/pack-windows.el

(autoload 'pack-windows "emacs-goodies-el/elisp/emacs-goodies-el/pack-windows" "\
Resize all windows vertically to display as much information as possible.

Only windows that are on the left edge of the frame are taken into
account. The vertical space available in the frame is first divided
among all these windows. Then any window requireing less lines than it
got to display its whole buffer is shrinked, and the freed space is
divided equally among all the other windows.

If some vertical space remains afterwards, it is given in totality to
the currently selected window.

Do not shrink any window to less than `window-min-height'.

Shrink windows iteratively, performing at most `pack-windows-max-iteration'
iterations. The number of iterations really performed will be
displayed in the echo area if `pack-windows-verbose' is non-nil.

\(fn)" t nil)

;;;***

;;;### (autoloads (perldoc-perl-hook perldoc-at-point perldoc) "emacs-goodies-el/elisp/emacs-goodies-el/perldoc"
;;;;;;  "emacs-goodies-el/elisp/emacs-goodies-el/perldoc.el" (19536
;;;;;;  18952))
;;; Generated autoloads from emacs-goodies-el/elisp/emacs-goodies-el/perldoc.el

(autoload 'perldoc "emacs-goodies-el/elisp/emacs-goodies-el/perldoc" "\
Run perldoc on the given STRING.
If the string is a recognised function then we can call `perldoc-function',
otherwise we call `perldoc-module'.
A non-nil interactive argument forces the caches to be updated.

\(fn &optional STRING RE-CACHE)" t nil)

(autoload 'perldoc-at-point "emacs-goodies-el/elisp/emacs-goodies-el/perldoc" "\
Call `perldoc' for string at point.

\(fn)" t nil)

(autoload 'perldoc-perl-hook "emacs-goodies-el/elisp/emacs-goodies-el/perldoc" "\
A hook which binds F1 to `perldoc-at-point'.

\(fn)" nil nil)

;;;***

;;;### (autoloads (pod-mode) "emacs-goodies-el/elisp/emacs-goodies-el/pod-mode"
;;;;;;  "emacs-goodies-el/elisp/emacs-goodies-el/pod-mode.el" (19536
;;;;;;  20515))
;;; Generated autoloads from emacs-goodies-el/elisp/emacs-goodies-el/pod-mode.el

(autoload 'pod-mode "emacs-goodies-el/elisp/emacs-goodies-el/pod-mode" "\
Major mode for editing POD files (Plain Old Documentation for Perl).

Commands:\\<pod-mode-map>
\\[pod-link]  `pod-link'
\\[pod-link-section]     `pod-link-section'
\\[pod-link-module]     `pod-link-module'
\\[pod-link-module-section]     `pod-link-module-section'

Turning on pod mode calls the hooks in `pod-mode-hook'.

\(fn)" t nil)

;;;***

;;;### (autoloads (refresh-pretty-control-l pp^L-^L-display-table-entry
;;;;;;  pp^L-^L-string Pretty-Control-L) "emacs-goodies-el/elisp/emacs-goodies-el/pp-c-l"
;;;;;;  "emacs-goodies-el/elisp/emacs-goodies-el/pp-c-l.el" (19416
;;;;;;  52725))
;;; Generated autoloads from emacs-goodies-el/elisp/emacs-goodies-el/pp-c-l.el

(let ((loads (get 'Pretty-Control-L 'custom-loads))) (if (member '"emacs-goodies-el/elisp/emacs-goodies-el/pp-c-l" loads) nil (put 'Pretty-Control-L 'custom-loads (cons '"emacs-goodies-el/elisp/emacs-goodies-el/pp-c-l" loads))))

(defface pp^L-highlight (if (> emacs-major-version 21) '((((type x w32 mac graphic) (class color)) (:box (:line-width 3 :style pressed-button))) (t (:inverse-video t))) '((((type x w32 mac graphic) (class color)) (:foreground "Blue" :background "DarkSeaGreen1")) (t (:inverse-video t)))) "\
*Face used to highlight `pp^L-^L-vector'." :group (quote Pretty-Control-L) :group (quote faces))

(defvar pp^L-^L-string "          Section (Printable Page)          " "\
*Highlighted string displayed in place of each Control-l (^L) character.
If `pp^L-^L-string-function' is non-nil, then the string that function
returns is used instead of `pp^L-^L-string'.")

(custom-autoload 'pp^L-^L-string "emacs-goodies-el/elisp/emacs-goodies-el/pp-c-l" t)

(unless (fboundp 'define-minor-mode) (defcustom pretty-control-l-mode nil "*Toggle pretty display of Control-l (`^L') characters.\nSetting this variable directly does not take effect;\nuse either \\[customize] or command `pretty-control-l-mode'." :set (lambda (symbol value) (pretty-control-l-mode (if value 1 -1))) :initialize 'custom-initialize-default :type 'boolean :group 'Pretty-Control-L))

(autoload 'pp^L-^L-display-table-entry "emacs-goodies-el/elisp/emacs-goodies-el/pp-c-l" "\
Returns the display-table entry for Control-l (`^L') char in WINDOW.
A vector determining how a Control-l character is displayed in WINDOW.
Either a vector of characters or nil.  The characters are displayed in
place of the Control-l character.  nil means `^L' is displayed.

In effect, this concatenates `pp^L-^L-string-pre', `pp^L-^L-string',
and `pp^L-^L-string-post'.

\(fn WINDOW)" nil nil)

(if (fboundp 'define-minor-mode) (eval '(define-minor-mode pretty-control-l-mode "Toggle pretty display of Control-l (`^L') characters.\nWith ARG, turn pretty display of `^L' on if and only if ARG is positive." :init-value nil :global t :group 'Pretty-Control-L :link `(url-link :tag "Send Bug Report" ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=pp-c-l.el bug: &body=Describe bug here, starting with `emacs -q'.  Don't forget to mention your Emacs and library versions.")) :link '(url-link :tag "Other Libraries by Drew" "http://www.emacswiki.org/cgi-bin/wiki/DrewsElispLibraries") :link '(url-link :tag "Download" "http://www.emacswiki.org/cgi-bin/wiki/pp-c-l.el") :link '(url-link :tag "Description" "http://www.emacswiki.org/cgi-bin/wiki/PrettyControlL") :link '(emacs-commentary-link :tag "Commentary" "pp-c-l") (if pretty-control-l-mode (add-hook 'window-configuration-change-hook 'refresh-pretty-control-l) (remove-hook 'window-configuration-change-hook 'refresh-pretty-control-l)) (walk-windows (lambda (window) (let ((display-table (or (window-display-table window) (make-display-table)))) (aset display-table 12 (and pretty-control-l-mode (pp^L-^L-display-table-entry window))) (set-window-display-table window display-table))) 'no-minibuf 'visible))) (defun pretty-control-l-mode (&optional arg) "Toggle pretty display of Control-l (`^L') characters.\nWith ARG, turn pretty display of `^L' on if and only if ARG is positive." (interactive "P") (setq pretty-control-l-mode (if arg (> (prefix-numeric-value arg) 0) (not pretty-control-l-mode))) (if pretty-control-l-mode (add-hook 'window-configuration-change-hook 'refresh-pretty-control-l) (remove-hook 'window-configuration-change-hook 'refresh-pretty-control-l)) (walk-windows (lambda (window) (let ((display-table (or (window-display-table window) (make-display-table)))) (aset display-table 12 (and pretty-control-l-mode (pp^L-^L-display-table-entry window))) (set-window-display-table window display-table))) 'no-minibuf 'visible)))

(autoload 'refresh-pretty-control-l "emacs-goodies-el/elisp/emacs-goodies-el/pp-c-l" "\
Reinitialize `pretty-control-l-mode', if on, to update the display.

\(fn)" t nil)

;;;***

;;;### (autoloads (project-add) "emacs-goodies-el/elisp/emacs-goodies-el/projects"
;;;;;;  "emacs-goodies-el/elisp/emacs-goodies-el/projects.el" (19487
;;;;;;  45802))
;;; Generated autoloads from emacs-goodies-el/elisp/emacs-goodies-el/projects.el

(autoload 'project-add "emacs-goodies-el/elisp/emacs-goodies-el/projects" "\
Add the project named NAME with root directory DIRECTORY.

\(fn NAME DIRECTORY)" t nil)

;;;***

;;;### (autoloads (protect-process-buffer-from-kill-mode protect-buffer-from-kill-mode)
;;;;;;  "emacs-goodies-el/elisp/emacs-goodies-el/protbuf" "emacs-goodies-el/elisp/emacs-goodies-el/protbuf.el"
;;;;;;  (16013 59401))
;;; Generated autoloads from emacs-goodies-el/elisp/emacs-goodies-el/protbuf.el

(defvar protect-buffer-from-kill-mode nil "\
*If non-`nil', then prevent buffer from being accidentally killed.
This variable is local to all buffers.")

(defvar protect-process-buffer-from-kill-mode nil "\
*If non-`nil', then protect buffer with live process from being killed.
This variable is local to all buffers.")

(defvar protect-process-buffer-from-kill-preserve-function nil "\
*Function to run to determine whether to kill a process buffer.
If function returns non-nil, buffer is preserved.  Otherwise, the buffer
may be killed.

If this variable is undefined, default action is to test whether a process
object is using this buffer as a process buffer.

This variable is buffer-local when set.")

(autoload 'protect-buffer-from-kill-mode "emacs-goodies-el/elisp/emacs-goodies-el/protbuf" "\
Protect buffer from being killed.
To remove this protection, call this command with a negative prefix argument.

\(fn &optional PREFIX BUFFER)" t nil)

(autoload 'protect-process-buffer-from-kill-mode "emacs-goodies-el/elisp/emacs-goodies-el/protbuf" "\
Protect buffer from being killed as long as it has an active process.
To remove this protection, call this command with a negative prefix argument.

\(fn &optional PREFIX BUFFER)" t nil)

;;;***

;;;### (autoloads (protocols-clear-cache protocols-lookup) "emacs-goodies-el/elisp/emacs-goodies-el/protocols"
;;;;;;  "emacs-goodies-el/elisp/emacs-goodies-el/protocols.el" (18431
;;;;;;  47702))
;;; Generated autoloads from emacs-goodies-el/elisp/emacs-goodies-el/protocols.el

(autoload 'protocols-lookup "emacs-goodies-el/elisp/emacs-goodies-el/protocols" "\
Find a protocol and display its details.

\(fn SEARCH)" t nil)

(autoload 'protocols-clear-cache "emacs-goodies-el/elisp/emacs-goodies-el/protocols" "\
Clear the protocols \"cache\".

\(fn)" t nil)

;;;***

;;;### (autoloads (rfcview-mode rfcview-find-index rfcview-find-rfc
;;;;;;  rfcview-customize) "emacs-goodies-el/elisp/emacs-goodies-el/rfcview"
;;;;;;  "emacs-goodies-el/elisp/emacs-goodies-el/rfcview.el" (19104
;;;;;;  31301))
;;; Generated autoloads from emacs-goodies-el/elisp/emacs-goodies-el/rfcview.el

(autoload 'rfcview-customize "emacs-goodies-el/elisp/emacs-goodies-el/rfcview" "\
Enter the RFCview Custom group.

\(fn)" t nil)

(autoload 'rfcview-find-rfc "emacs-goodies-el/elisp/emacs-goodies-el/rfcview" "\
Find RFC NUMBER and view it in RFcview mode.
Interactively, prompt for the number.
See `rfcview-rfc-location-pattern' for where to search.

\(fn NUMBER)" t nil)

(autoload 'rfcview-find-index "emacs-goodies-el/elisp/emacs-goodies-el/rfcview" "\
Find the RFC index and hyperlink it.

\(fn)" t nil)

(autoload 'rfcview-mode "emacs-goodies-el/elisp/emacs-goodies-el/rfcview" "\
Major mode for viewing Internet RFCs.

http://www.loveshack.ukfsn.org/emacs/rfcview.el
http://www.neilvandyke.org/rfcview/

Key bindings:
\\{rfcview-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads (services-clear-cache services-lookup) "emacs-goodies-el/elisp/emacs-goodies-el/services"
;;;;;;  "emacs-goodies-el/elisp/emacs-goodies-el/services.el" (18431
;;;;;;  47702))
;;; Generated autoloads from emacs-goodies-el/elisp/emacs-goodies-el/services.el

(autoload 'services-lookup "emacs-goodies-el/elisp/emacs-goodies-el/services" "\
Find a service and display its details.

\(fn SEARCH PROTOCOL)" t nil)

(autoload 'services-clear-cache "emacs-goodies-el/elisp/emacs-goodies-el/services" "\
Clear the services \"cache\".

\(fn)" t nil)

;;;***

;;;### (autoloads (session-initialize session-jump-to-last-change)
;;;;;;  "emacs-goodies-el/elisp/emacs-goodies-el/session" "emacs-goodies-el/elisp/emacs-goodies-el/session.el"
;;;;;;  (19810 29252))
;;; Generated autoloads from emacs-goodies-el/elisp/emacs-goodies-el/session.el

(autoload 'session-jump-to-last-change "emacs-goodies-el/elisp/emacs-goodies-el/session" "\
Jump to the position of the last change.
Without prefix arg, jump successively to previous change positions which
differ by at least `session-jump-undo-threshold' characters by repeated
invocation of this command.  With prefix argument 0, jump to end of last
change.  With numeric prefix argument, jump to start of first change in
the ARG's undo block in the `buffer-undo-list'.

With non-numeric prefix argument (\\[universal-argument] only), set
point as oldest change position.  It might change slightly if you jump
to it due to intermediate insert/delete elements in the
`buffer-undo-list'.

\(fn &optional ARG)" t nil)

(autoload 'session-initialize "emacs-goodies-el/elisp/emacs-goodies-el/session" "\
Initialize package session and read previous session file.
Setup hooks and load `session-save-file', see variable `session-initialize'.  At
best, this function is called at the end of the Emacs startup, i.e., add
this function to `after-init-hook'.

\(fn)" t nil)

;;;***

;;;### (autoloads (shell-command-completion-mode) "emacs-goodies-el/elisp/emacs-goodies-el/shell-command"
;;;;;;  "emacs-goodies-el/elisp/emacs-goodies-el/shell-command.el"
;;;;;;  (19104 31301))
;;; Generated autoloads from emacs-goodies-el/elisp/emacs-goodies-el/shell-command.el

(autoload 'shell-command-completion-mode "emacs-goodies-el/elisp/emacs-goodies-el/shell-command" "\
Enable or disable tab-completion for some commands.
The commands are `shell-command', `shell-command-on-region', `grep',
`grep-find' and `compile'.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (show-ws-toggle-show-trailing-whitespace show-ws-toggle-show-hard-spaces
;;;;;;  show-ws-toggle-show-tabs) "emacs-goodies-el/elisp/emacs-goodies-el/show-wspace"
;;;;;;  "emacs-goodies-el/elisp/emacs-goodies-el/show-wspace.el"
;;;;;;  (19104 31301))
;;; Generated autoloads from emacs-goodies-el/elisp/emacs-goodies-el/show-wspace.el

(defalias 'toggle-show-tabs-show-ws 'show-ws-toggle-show-tabs)

(autoload 'show-ws-toggle-show-tabs "emacs-goodies-el/elisp/emacs-goodies-el/show-wspace" "\
Toggle highlighting of TABs, using face `show-ws-tab'.

\(fn)" t nil)

(defalias 'toggle-show-hard-spaces-show-ws 'show-ws-toggle-show-hard-spaces)

(autoload 'show-ws-toggle-show-hard-spaces "emacs-goodies-el/elisp/emacs-goodies-el/show-wspace" "\
Toggle highlighting of non-breaking space characters (`\240').
Uses face `show-ws-hard-space'.

\(fn)" t nil)

(defalias 'toggle-show-trailing-whitespace-show-ws 'show-ws-toggle-show-trailing-whitespace)

(autoload 'show-ws-toggle-show-trailing-whitespace "emacs-goodies-el/elisp/emacs-goodies-el/show-wspace" "\
Toggle highlighting of trailing whitespace.
Uses face `show-ws-trailing-whitespace'.

\(fn)" t nil)

;;;***

;;;### (autoloads (sm-add-all-headers sm-add-random-header) "emacs-goodies-el/elisp/emacs-goodies-el/silly-mail"
;;;;;;  "emacs-goodies-el/elisp/emacs-goodies-el/silly-mail.el" (19104
;;;;;;  31716))
;;; Generated autoloads from emacs-goodies-el/elisp/emacs-goodies-el/silly-mail.el

(autoload 'sm-add-random-header "emacs-goodies-el/elisp/emacs-goodies-el/silly-mail" "\
Insert a random silly mail header.
The choice of available headers is taken from sm-mail-header-table.

\(fn)" t nil)

(autoload 'sm-add-all-headers "emacs-goodies-el/elisp/emacs-goodies-el/silly-mail" "\
Insert one of every kind of silly mail header defined.
The choice of available headers is taken from sm-mail-header-table.

\(fn)" t nil)

;;;***

;;;### (autoloads (slang-mode) "emacs-goodies-el/elisp/emacs-goodies-el/slang-mode"
;;;;;;  "emacs-goodies-el/elisp/emacs-goodies-el/slang-mode.el" (17211
;;;;;;  12498))
;;; Generated autoloads from emacs-goodies-el/elisp/emacs-goodies-el/slang-mode.el

(autoload 'slang-mode "emacs-goodies-el/elisp/emacs-goodies-el/slang-mode" "\
Major mode for editing slang scripts.
The following keys are bound:
\\{slang-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads (sys-apropos) "emacs-goodies-el/elisp/emacs-goodies-el/sys-apropos"
;;;;;;  "emacs-goodies-el/elisp/emacs-goodies-el/sys-apropos.el"
;;;;;;  (16013 59403))
;;; Generated autoloads from emacs-goodies-el/elisp/emacs-goodies-el/sys-apropos.el

(autoload 'sys-apropos "emacs-goodies-el/elisp/emacs-goodies-el/sys-apropos" "\
Ask the system apropos command for man-pages matching QUERY.

\(fn QUERY)" t nil)

;;;***

;;;### (autoloads (tabbar-mwheel-mode tabbar-mode tabbar-local-mode
;;;;;;  tabbar-mwheel-switch-group tabbar-mwheel-switch-tab tabbar-mwheel-forward-tab
;;;;;;  tabbar-mwheel-backward-tab tabbar-mwheel-forward-group tabbar-mwheel-backward-group
;;;;;;  tabbar-mwheel-forward tabbar-mwheel-backward tabbar-press-scroll-right
;;;;;;  tabbar-press-scroll-left tabbar-press-home tabbar-forward-tab
;;;;;;  tabbar-backward-tab tabbar-forward-group tabbar-backward-group
;;;;;;  tabbar-forward tabbar-backward) "emacs-goodies-el/elisp/emacs-goodies-el/tabbar"
;;;;;;  "emacs-goodies-el/elisp/emacs-goodies-el/tabbar.el" (18106
;;;;;;  17053))
;;; Generated autoloads from emacs-goodies-el/elisp/emacs-goodies-el/tabbar.el

(autoload 'tabbar-backward "emacs-goodies-el/elisp/emacs-goodies-el/tabbar" "\
Select the previous available tab.
Depend on the setting of the option `tabbar-cycle-scope'.

\(fn)" t nil)

(autoload 'tabbar-forward "emacs-goodies-el/elisp/emacs-goodies-el/tabbar" "\
Select the next available tab.
Depend on the setting of the option `tabbar-cycle-scope'.

\(fn)" t nil)

(autoload 'tabbar-backward-group "emacs-goodies-el/elisp/emacs-goodies-el/tabbar" "\
Go to selected tab in the previous available group.

\(fn)" t nil)

(autoload 'tabbar-forward-group "emacs-goodies-el/elisp/emacs-goodies-el/tabbar" "\
Go to selected tab in the next available group.

\(fn)" t nil)

(autoload 'tabbar-backward-tab "emacs-goodies-el/elisp/emacs-goodies-el/tabbar" "\
Select the previous visible tab.

\(fn)" t nil)

(autoload 'tabbar-forward-tab "emacs-goodies-el/elisp/emacs-goodies-el/tabbar" "\
Select the next visible tab.

\(fn)" t nil)

(autoload 'tabbar-press-home "emacs-goodies-el/elisp/emacs-goodies-el/tabbar" "\
Press the tab bar home button.
That is, simulate a mouse click on that button.
A numeric prefix ARG value of 2, or 3, respectively simulates a
mouse-2, or mouse-3 click.  The default is a mouse-1 click.

\(fn &optional ARG)" t nil)

(autoload 'tabbar-press-scroll-left "emacs-goodies-el/elisp/emacs-goodies-el/tabbar" "\
Press the tab bar scroll-left button.
That is, simulate a mouse click on that button.
A numeric prefix ARG value of 2, or 3, respectively simulates a
mouse-2, or mouse-3 click.  The default is a mouse-1 click.

\(fn &optional ARG)" t nil)

(autoload 'tabbar-press-scroll-right "emacs-goodies-el/elisp/emacs-goodies-el/tabbar" "\
Press the tab bar scroll-right button.
That is, simulate a mouse click on that button.
A numeric prefix ARG value of 2, or 3, respectively simulates a
mouse-2, or mouse-3 click.  The default is a mouse-1 click.

\(fn &optional ARG)" t nil)

(autoload 'tabbar-mwheel-backward "emacs-goodies-el/elisp/emacs-goodies-el/tabbar" "\
Select the previous available tab.
EVENT is the mouse event that triggered this command.
Mouse-enabled equivalent of the command `tabbar-backward'.

\(fn EVENT)" t nil)

(autoload 'tabbar-mwheel-forward "emacs-goodies-el/elisp/emacs-goodies-el/tabbar" "\
Select the next available tab.
EVENT is the mouse event that triggered this command.
Mouse-enabled equivalent of the command `tabbar-forward'.

\(fn EVENT)" t nil)

(autoload 'tabbar-mwheel-backward-group "emacs-goodies-el/elisp/emacs-goodies-el/tabbar" "\
Go to selected tab in the previous available group.
If there is only one group, select the previous visible tab.
EVENT is the mouse event that triggered this command.
Mouse-enabled equivalent of the command `tabbar-backward-group'.

\(fn EVENT)" t nil)

(autoload 'tabbar-mwheel-forward-group "emacs-goodies-el/elisp/emacs-goodies-el/tabbar" "\
Go to selected tab in the next available group.
If there is only one group, select the next visible tab.
EVENT is the mouse event that triggered this command.
Mouse-enabled equivalent of the command `tabbar-forward-group'.

\(fn EVENT)" t nil)

(autoload 'tabbar-mwheel-backward-tab "emacs-goodies-el/elisp/emacs-goodies-el/tabbar" "\
Select the previous visible tab.
EVENT is the mouse event that triggered this command.
Mouse-enabled equivalent of the command `tabbar-backward-tab'.

\(fn EVENT)" t nil)

(autoload 'tabbar-mwheel-forward-tab "emacs-goodies-el/elisp/emacs-goodies-el/tabbar" "\
Select the next visible tab.
EVENT is the mouse event that triggered this command.
Mouse-enabled equivalent of the command `tabbar-forward-tab'.

\(fn EVENT)" t nil)

(autoload 'tabbar-mwheel-switch-tab "emacs-goodies-el/elisp/emacs-goodies-el/tabbar" "\
Select the next or previous tab according to EVENT.

\(fn EVENT)" t nil)

(autoload 'tabbar-mwheel-switch-group "emacs-goodies-el/elisp/emacs-goodies-el/tabbar" "\
Select the next or previous group of tabs according to EVENT.

\(fn EVENT)" t nil)

(autoload 'tabbar-local-mode "emacs-goodies-el/elisp/emacs-goodies-el/tabbar" "\
Toggle local display of the tab bar.
With prefix argument ARG, turn on if positive, otherwise off.
Returns non-nil if the new state is enabled.
When turned on, if a local header line is shown, it is hidden to show
the tab bar.  The tab bar is locally hidden otherwise.  When turned
off, if a local header line is hidden or the tab bar is locally
hidden, it is shown again.  Signal an error if Tabbar mode is off.

\(fn &optional ARG)" t nil)

(defvar tabbar-mode nil "\
Non-nil if Tabbar mode is enabled.
See the command `tabbar-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `tabbar-mode'.")

(custom-autoload 'tabbar-mode "emacs-goodies-el/elisp/emacs-goodies-el/tabbar" nil)

(autoload 'tabbar-mode "emacs-goodies-el/elisp/emacs-goodies-el/tabbar" "\
Toggle display of a tab bar in the header line.
With prefix argument ARG, turn on if positive, otherwise off.
Returns non-nil if the new state is enabled.

\\{tabbar-mode-map}

\(fn &optional ARG)" t nil)

(defvar tabbar-mwheel-mode nil "\
Non-nil if Tabbar-Mwheel mode is enabled.
See the command `tabbar-mwheel-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `tabbar-mwheel-mode'.")

(custom-autoload 'tabbar-mwheel-mode "emacs-goodies-el/elisp/emacs-goodies-el/tabbar" nil)

(autoload 'tabbar-mwheel-mode "emacs-goodies-el/elisp/emacs-goodies-el/tabbar" "\
Toggle use of the mouse wheel to navigate through tabs or groups.
With prefix argument ARG, turn on if positive, otherwise off.
Returns non-nil if the new state is enabled.

\\{tabbar-mwheel-mode-map}

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (tail-command tail-file) "emacs-goodies-el/elisp/emacs-goodies-el/tail"
;;;;;;  "emacs-goodies-el/elisp/emacs-goodies-el/tail.el" (19536
;;;;;;  20905))
;;; Generated autoloads from emacs-goodies-el/elisp/emacs-goodies-el/tail.el

(autoload 'tail-file "emacs-goodies-el/elisp/emacs-goodies-el/tail" "\
Tails FILE specified with argument FILE inside a new buffer.
FILE *cannot* be a remote file specified with ange-ftp syntax because it is
passed to the Unix tail command.

\(fn FILE)" t nil)

(autoload 'tail-command "emacs-goodies-el/elisp/emacs-goodies-el/tail" "\
Tails COMMAND with arguments ARGS inside a new buffer.
It is also called by `tail-file'

\(fn COMMAND &rest ARGS)" t nil)

;;;***

;;;### (autoloads (trivial-cite) "emacs-goodies-el/elisp/emacs-goodies-el/tc"
;;;;;;  "emacs-goodies-el/elisp/emacs-goodies-el/tc.el" (16295 5169))
;;; Generated autoloads from emacs-goodies-el/elisp/emacs-goodies-el/tc.el

(autoload 'trivial-cite "emacs-goodies-el/elisp/emacs-goodies-el/tc" "\
A simple citation function for use in news/mailreaders.
It parses the headers via the functions defined in `tc-header-funs', then
makes a attribution for the citation using `tc-make-attribution' and indents
the inserted text with `tc-indent-citation'.
Numeric prefix arguments is how many lines of body to cite (useful for citing
mails with long attachments).
Usage:  (auto-load 'trivial-cite \"tc\" t t)
        (add-hook 'mail-citation-hook 'trivial-cite)
Bugs:  Not very intelligent about old citation marks other than '>'.
Customization:  See variables tc-fill-column, tc-remove-signature,
tc-citation-string, tc-make-attribution and tc-header-funs.

\(fn)" nil nil)

;;;***

;;;### (autoloads (thinks-maybe-region thinks-yank thinks-region
;;;;;;  thinks) "emacs-goodies-el/elisp/emacs-goodies-el/thinks"
;;;;;;  "emacs-goodies-el/elisp/emacs-goodies-el/thinks.el" (18431
;;;;;;  47702))
;;; Generated autoloads from emacs-goodies-el/elisp/emacs-goodies-el/thinks.el

(autoload 'thinks "emacs-goodies-el/elisp/emacs-goodies-el/thinks" "\
Insert TEXT wrapped in a think bubble.

Prefix a call to this function with \\[universal-argument] if you don't want
the text to be filled for you.

\(fn TEXT)" t nil)

(autoload 'thinks-region "emacs-goodies-el/elisp/emacs-goodies-el/thinks" "\
Bubble wrap region bounding START and END.

Prefix a call to this function with \\[universal-argument] if you don't want
the text to be filled for you.

\(fn START END)" t nil)

(autoload 'thinks-yank "emacs-goodies-el/elisp/emacs-goodies-el/thinks" "\
Do a `yank' and bubble wrap the yanked text.

Prefix a call to this function with \\[universal-argument] if you don't want
the text to be filled for you.

\(fn)" t nil)

(autoload 'thinks-maybe-region "emacs-goodies-el/elisp/emacs-goodies-el/thinks" "\
If region is active, bubble wrap region bounding START and END.
If not, query for text to insert in bubble.

\(fn)" t nil)

;;;***

;;;### (autoloads (tlc-mode) "emacs-goodies-el/elisp/emacs-goodies-el/tlc"
;;;;;;  "emacs-goodies-el/elisp/emacs-goodies-el/tlc.el" (17221 29124))
;;; Generated autoloads from emacs-goodies-el/elisp/emacs-goodies-el/tlc.el

(autoload 'tlc-mode "emacs-goodies-el/elisp/emacs-goodies-el/tlc" "\
Major mode for editing Tlc files, or files found in tlc directories.

\(fn)" t nil)
(add-to-list 'auto-mode-alist '("\\.tlc$" .tlc-mode))

;;;***

;;;### (autoloads (tld) "emacs-goodies-el/elisp/emacs-goodies-el/tld"
;;;;;;  "emacs-goodies-el/elisp/emacs-goodies-el/tld.el" (18431 47702))
;;; Generated autoloads from emacs-goodies-el/elisp/emacs-goodies-el/tld.el

(autoload 'tld "emacs-goodies-el/elisp/emacs-goodies-el/tld" "\
Search the TLD list.

\(fn SEARCH)" t nil)

;;;***

;;;### (autoloads (twiddle-compile twiddle-start) "emacs-goodies-el/elisp/emacs-goodies-el/twiddle"
;;;;;;  "emacs-goodies-el/elisp/emacs-goodies-el/twiddle.el" (16013
;;;;;;  59409))
;;; Generated autoloads from emacs-goodies-el/elisp/emacs-goodies-el/twiddle.el

(autoload 'twiddle-start "emacs-goodies-el/elisp/emacs-goodies-el/twiddle" "\
Start a mode line display hack.
If called interactively with a prefix argument, prompt for the name of
a hack to run.  If called from lisp, optional argument HACK is the name of
a hack to run.
Named hacks are defined in the table `twiddle-hacks'.

\(fn &optional HACK)" t nil)

(autoload 'twiddle-compile "emacs-goodies-el/elisp/emacs-goodies-el/twiddle" "\
Like \\[compile], but run a twiddle hack during compilation.
If called with a prefix argument, prompt for a specific hack to run.

\(fn &rest COMPILE-ARGS)" t nil)

;;;***

;;;### (autoloads (underhat-region) "emacs-goodies-el/elisp/emacs-goodies-el/under"
;;;;;;  "emacs-goodies-el/elisp/emacs-goodies-el/under.el" (16262
;;;;;;  60507))
;;; Generated autoloads from emacs-goodies-el/elisp/emacs-goodies-el/under.el

(autoload 'underhat-region "emacs-goodies-el/elisp/emacs-goodies-el/under" "\
Underline the region.

\(fn)" t nil)

;;;***

;;;### (autoloads (xrdb-mode) "emacs-goodies-el/elisp/emacs-goodies-el/xrdb-mode"
;;;;;;  "emacs-goodies-el/elisp/emacs-goodies-el/xrdb-mode.el" (19391
;;;;;;  54726))
;;; Generated autoloads from emacs-goodies-el/elisp/emacs-goodies-el/xrdb-mode.el

(autoload 'xrdb-mode "emacs-goodies-el/elisp/emacs-goodies-el/xrdb-mode" "\
Major mode for editing xrdb config files.
\\{xrdb-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads (bookmark-w3m-bookmark-jump) "emacs-w3m/bookmark-w3m"
;;;;;;  "emacs-w3m/bookmark-w3m.el" (19648 52975))
;;; Generated autoloads from emacs-w3m/bookmark-w3m.el

(autoload 'bookmark-w3m-bookmark-jump "emacs-w3m/bookmark-w3m" "\
Default bookmark handler for w3m buffers.

\(fn BOOKMARK)" nil nil)

;;;***

;;;### (autoloads (mime-w3m-preview-text/html) "emacs-w3m/mime-w3m"
;;;;;;  "emacs-w3m/mime-w3m.el" (19727 57154))
;;; Generated autoloads from emacs-w3m/mime-w3m.el

(autoload 'mime-w3m-preview-text/html "emacs-w3m/mime-w3m" "\


\(fn ENTITY SITUATION)" nil nil)

;;;***

;;;### (autoloads (octet-mime-setup mime-view-octet mime-preview-octet
;;;;;;  octet-find-file octet-buffer) "emacs-w3m/octet" "emacs-w3m/octet.el"
;;;;;;  (19563 28233))
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
;;;;;;  w3m w3m-create-empty-session w3m-gohome w3m-goto-url-new-session
;;;;;;  w3m-goto-url w3m-download w3m-retrieve) "emacs-w3m/w3m" "emacs-w3m/w3m.el"
;;;;;;  (20164 25360))
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

(autoload 'w3m-create-empty-session "emacs-w3m/w3m" "\
Create an empty page as a new session and visit it.

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
;;;;;;  "emacs-w3m/w3m-antenna.el" (19187 58267))
;;; Generated autoloads from emacs-w3m/w3m-antenna.el

(autoload 'w3m-about-antenna "emacs-w3m/w3m-antenna" "\


\(fn URL &optional NO-DECODE NO-CACHE POST-DATA REFERER HANDLER)" nil nil)

(autoload 'w3m-antenna "emacs-w3m/w3m-antenna" "\
Report changes of WEB sites, which is specified in `w3m-antenna-sites'.

\(fn &optional NO-CACHE)" t nil)

;;;***

;;;### (autoloads (w3m-setup-bookmark-menu w3m-about-bookmark w3m-bookmark-view-new-session
;;;;;;  w3m-bookmark-view w3m-bookmark-add-current-url-group w3m-bookmark-add-all-urls
;;;;;;  w3m-bookmark-add-current-url w3m-bookmark-add-this-url) "emacs-w3m/w3m-bookmark"
;;;;;;  "emacs-w3m/w3m-bookmark.el" (19989 33699))
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


\(fn &rest ARGS)" nil nil)

(autoload 'w3m-setup-bookmark-menu "emacs-w3m/w3m-bookmark" "\
Setup w3m bookmark items in menubar.

\(fn)" nil nil)

;;;***

;;;### (autoloads (w3m-about-cookie w3m-cookie w3m-cookie-get w3m-cookie-set
;;;;;;  w3m-cookie-shutdown) "emacs-w3m/w3m-cookie" "emacs-w3m/w3m-cookie.el"
;;;;;;  (19449 53703))
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
;;;;;;  "emacs-w3m/w3m-dtree.el" (18964 36505))
;;; Generated autoloads from emacs-w3m/w3m-dtree.el

(autoload 'w3m-about-dtree "emacs-w3m/w3m-dtree" "\


\(fn URL &optional NODECODE ALLFILES &rest ARGS)" nil nil)

(autoload 'w3m-dtree "emacs-w3m/w3m-dtree" "\
Display directory tree on local file system.
If called with 'prefix argument', display all directorys and files.

\(fn ALLFILES PATH)" t nil)

;;;***

;;;### (autoloads (w3m-fb-mode) "emacs-w3m/w3m-fb" "emacs-w3m/w3m-fb.el"
;;;;;;  (17681 2386))
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
;;;;;;  (18560 15090))
;;; Generated autoloads from emacs-w3m/w3m-filter.el

(autoload 'w3m-filter "emacs-w3m/w3m-filter" "\
Apply filtering rule of URL against a content in this buffer.

\(fn URL)" nil nil)

;;;***

;;;### (autoloads (w3m-fontify-forms) "emacs-w3m/w3m-form" "emacs-w3m/w3m-form.el"
;;;;;;  (20119 32590))
;;; Generated autoloads from emacs-w3m/w3m-form.el

(autoload 'w3m-fontify-forms "emacs-w3m/w3m-form" "\
Process half-dumped data and fontify forms in this buffer.

\(fn)" nil nil)

;;;***

;;;### (autoloads (w3m-lnum-bookmark-add-this-url w3m-lnum-download-this-url
;;;;;;  w3m-lnum-print-this-url w3m-lnum-edit-this-url w3m-lnum-external-view-this-url
;;;;;;  w3m-lnum-save-image w3m-lnum-view-image w3m-lnum-toggle-inline-image
;;;;;;  w3m-lnum-universal w3m-lnum-follow w3m-lnum-goto w3m-lnum-mode)
;;;;;;  "emacs-w3m/w3m-lnum" "emacs-w3m/w3m-lnum.el" (20165 41176))
;;; Generated autoloads from emacs-w3m/w3m-lnum.el

(autoload 'w3m-lnum-mode "emacs-w3m/w3m-lnum" "\
Minor mode to extend point commands by using Conkeror style number selection.
With prefix ARG 0 disable battery included point functions, otherwise
enable them.  With no prefix ARG - toggle.

\(fn &optional ARG)" t nil)

(autoload 'w3m-lnum-goto "emacs-w3m/w3m-lnum" "\
Turn on link, image and form numbers and ask for one to go to.
0 corresponds to location url.

\(fn)" t nil)

(autoload 'w3m-lnum-follow "emacs-w3m/w3m-lnum" "\
Turn on link numbers, ask for one and execute appropriate action on it.
If link - visit it, when button - press, when input - activate it,
If image - toggle it.
With prefix ARG visit link in new session or don't move over
field/button/image on activation/push/toggle.
With `-' ARG, for link image - go to it and toggle it.
With -4 ARG, for link image - toggle it.
With double prefix ARG, prompt for url to visit.
With triple prefix ARG, prompt for url to visit in new session.

\(fn ARG)" t nil)

(autoload 'w3m-lnum-universal "emacs-w3m/w3m-lnum" "\
Turn on link numbers, ask for one and offer actions over it depending on selection type.
Actions may be selected either by hitting corresponding key,
pressing <return> over the action line or left clicking.

\(fn)" t nil)

(autoload 'w3m-lnum-toggle-inline-image "emacs-w3m/w3m-lnum" "\
If image at point, toggle it.
Otherwise turn on link numbers and toggle selected image.
With prefix ARG open url under image in new session.
If no such url, move over image and toggle it.

\(fn &optional ARG)" t nil)

(autoload 'w3m-lnum-view-image "emacs-w3m/w3m-lnum" "\
Display the image under point in the external viewer.
If no image at poing, turn on image numbers and display selected.
The viewer is defined in `w3m-content-type-alist' for every type of an
image.

\(fn)" t nil)

(autoload 'w3m-lnum-save-image "emacs-w3m/w3m-lnum" "\
Save the image under point to a file.
If no image at poing, turn on image numbers and save selected.
The default name will be the original name of the image.

\(fn)" t nil)

(autoload 'w3m-lnum-external-view-this-url "emacs-w3m/w3m-lnum" "\
Launch the external browser and display the link at point.
If no link at point, turn on link numbers and open selected externally.

\(fn)" t nil)

(autoload 'w3m-lnum-edit-this-url "emacs-w3m/w3m-lnum" "\
Edit the page linked from the anchor under the cursor.
If no such, turn on link numbers and edit selected.

\(fn)" t nil)

(autoload 'w3m-lnum-print-this-url "emacs-w3m/w3m-lnum" "\
Display the url under point in the echo area and put it into `kill-ring'.
If no url under point, activate numbering and select one.

\(fn)" t nil)

(autoload 'w3m-lnum-download-this-url "emacs-w3m/w3m-lnum" "\
Download the file or the page pointed to by the link under point.
If no point, activate numbering and select andchor to download.

\(fn)" t nil)

(autoload 'w3m-lnum-bookmark-add-this-url "emacs-w3m/w3m-lnum" "\
Add link under cursor to bookmarks.
If no link under point, activate numbering and ask for one.

\(fn)" t nil)

;;;***

;;;### (autoloads (w3m-namazu w3m-about-namazu) "emacs-w3m/w3m-namazu"
;;;;;;  "emacs-w3m/w3m-namazu.el" (18964 36505))
;;; Generated autoloads from emacs-w3m/w3m-namazu.el

(autoload 'w3m-about-namazu "emacs-w3m/w3m-namazu" "\


\(fn URL &optional NO-DECODE NO-CACHE &rest ARGS)" nil nil)

(autoload 'w3m-namazu "emacs-w3m/w3m-namazu" "\
Search indexed files with Namazu.

\(fn INDEX QUERY &optional RELOAD)" t nil)

;;;***

;;;### (autoloads (w3m-perldoc w3m-about-perldoc) "emacs-w3m/w3m-perldoc"
;;;;;;  "emacs-w3m/w3m-perldoc.el" (18197 61165))
;;; Generated autoloads from emacs-w3m/w3m-perldoc.el

(autoload 'w3m-about-perldoc "emacs-w3m/w3m-perldoc" "\


\(fn URL &optional NO-DECODE NO-CACHE &rest ARGS)" nil nil)

(autoload 'w3m-perldoc "emacs-w3m/w3m-perldoc" "\
View Perl documents.

\(fn DOCNAME)" t nil)

;;;***

;;;### (autoloads (w3m-search-uri-replace w3m-search-new-session
;;;;;;  w3m-search) "emacs-w3m/w3m-search" "emacs-w3m/w3m-search.el"
;;;;;;  (20119 32590))
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
;;;;;;  w3m-setup-session-menu w3m-session-select w3m-session-crash-recovery-remove
;;;;;;  w3m-session-save) "emacs-w3m/w3m-session" "emacs-w3m/w3m-session.el"
;;;;;;  (19989 33699))
;;; Generated autoloads from emacs-w3m/w3m-session.el

(autoload 'w3m-session-save "emacs-w3m/w3m-session" "\
Save list of displayed session.

\(fn)" t nil)

(autoload 'w3m-session-crash-recovery-remove "emacs-w3m/w3m-session" "\
Remove crash recovery session set.

\(fn)" nil nil)

(autoload 'w3m-session-select "emacs-w3m/w3m-session" "\
Select session from session list.

\(fn)" t nil)

(autoload 'w3m-setup-session-menu "emacs-w3m/w3m-session" "\
Setup w3m session items in menubar.

\(fn)" nil nil)

(autoload 'w3m-session-last-autosave-session "emacs-w3m/w3m-session" "\


\(fn)" nil nil)

(autoload 'w3m-session-last-crashed-session "emacs-w3m/w3m-session" "\


\(fn)" nil nil)

;;;***

;;;### (autoloads (w3m-replace-symbol) "emacs-w3m/w3m-symbol" "emacs-w3m/w3m-symbol.el"
;;;;;;  (18791 2971))
;;; Generated autoloads from emacs-w3m/w3m-symbol.el

(autoload 'w3m-replace-symbol "emacs-w3m/w3m-symbol" "\


\(fn)" nil nil)

;;;***

;;;### (autoloads (w3m-about-weather w3m-weather) "emacs-w3m/w3m-weather"
;;;;;;  "emacs-w3m/w3m-weather.el" (18197 61165))
;;; Generated autoloads from emacs-w3m/w3m-weather.el

(autoload 'w3m-weather "emacs-w3m/w3m-weather" "\
Display weather report.

\(fn AREA)" t nil)

(autoload 'w3m-about-weather "emacs-w3m/w3m-weather" "\


\(fn URL NO-DECODE NO-CACHE POST-DATA REFERER HANDLER)" nil nil)

;;;***

;;;### (autoloads (emms-cache-toggle emms-cache-disable emms-cache-enable)
;;;;;;  "emms/lisp/emms-cache" "emms/lisp/emms-cache.el" (20176 38066))
;;; Generated autoloads from emms/lisp/emms-cache.el

(autoload 'emms-cache-enable "emms/lisp/emms-cache" "\
Enable caching of Emms track data.

\(fn)" t nil)

(autoload 'emms-cache-disable "emms/lisp/emms-cache" "\
Disable caching of Emms track data.

\(fn)" t nil)

(autoload 'emms-cache-toggle "emms/lisp/emms-cache" "\
Toggle caching of Emms track data.

\(fn)" t nil)

;;;***

;;;### (autoloads (emms-lyrics-toggle emms-lyrics-disable emms-lyrics-enable)
;;;;;;  "emms/lisp/emms-lyrics" "emms/lisp/emms-lyrics.el" (20176
;;;;;;  38066))
;;; Generated autoloads from emms/lisp/emms-lyrics.el

(autoload 'emms-lyrics-enable "emms/lisp/emms-lyrics" "\
Enable displaying emms lyrics.

\(fn)" t nil)

(autoload 'emms-lyrics-disable "emms/lisp/emms-lyrics" "\
Disable displaying emms lyrics.

\(fn)" t nil)

(autoload 'emms-lyrics-toggle "emms/lisp/emms-lyrics" "\
Toggle displaying emms lyrics.

\(fn)" t nil)

;;;***

;;;### (autoloads (emms-mode-line-toggle emms-mode-line-disable emms-mode-line-enable)
;;;;;;  "emms/lisp/emms-mode-line" "emms/lisp/emms-mode-line.el"
;;;;;;  (20176 38066))
;;; Generated autoloads from emms/lisp/emms-mode-line.el

(autoload 'emms-mode-line-enable "emms/lisp/emms-mode-line" "\
Turn on `emms-mode-line'.

\(fn)" t nil)

(autoload 'emms-mode-line-disable "emms/lisp/emms-mode-line" "\
Turn off `emms-mode-line'.

\(fn)" t nil)

(autoload 'emms-mode-line-toggle "emms/lisp/emms-mode-line" "\
Toggle `emms-mode-line'.

\(fn)" t nil)

;;;***

;;;### (autoloads (emms-player-mpd-show emms-player-mpd-connect emms-player-mpd-clear)
;;;;;;  "emms/lisp/emms-player-mpd" "emms/lisp/emms-player-mpd.el"
;;;;;;  (20176 38066))
;;; Generated autoloads from emms/lisp/emms-player-mpd.el

(autoload 'emms-player-mpd-clear "emms/lisp/emms-player-mpd" "\
Clear the MusicPD playlist.

\(fn)" t nil)

(autoload 'emms-player-mpd-connect "emms/lisp/emms-player-mpd" "\
Connect to MusicPD and retrieve its current playlist.

Afterward, the status of MusicPD will be tracked.

This also has the effect of changing the current EMMS playlist to
be the same as the current MusicPD playlist.  Thus, this
function is useful to call if the contents of the EMMS playlist
buffer get out-of-sync for some reason.

\(fn)" t nil)

(autoload 'emms-player-mpd-show "emms/lisp/emms-player-mpd" "\
Describe the current EMMS track in the minibuffer.

If INSERTP is non-nil, insert the description into the current
buffer instead.

If CALLBACK is a function, call it with the current buffer and
description as arguments instead of displaying the description or
inserting it.

This function uses `emms-show-format' to format the current track.
It differs from `emms-show' in that it asks MusicPD for the current track,
rather than EMMS.

\(fn &optional INSERTP CALLBACK)" t nil)

;;;***

;;;### (autoloads (emms-playing-time-disable-display emms-playing-time-enable-display)
;;;;;;  "emms/lisp/emms-playing-time" "emms/lisp/emms-playing-time.el"
;;;;;;  (20176 38066))
;;; Generated autoloads from emms/lisp/emms-playing-time.el

(autoload 'emms-playing-time-enable-display "emms/lisp/emms-playing-time" "\
Display playing time on mode line.

\(fn)" t nil)

(autoload 'emms-playing-time-disable-display "emms/lisp/emms-playing-time" "\
Remove playing time from mode line.

\(fn)" t nil)

;;;***

;;;### (autoloads (emms-playlist-limit-toggle emms-playlist-limit-disable
;;;;;;  emms-playlist-limit-enable) "emms/lisp/emms-playlist-limit"
;;;;;;  "emms/lisp/emms-playlist-limit.el" (20176 38066))
;;; Generated autoloads from emms/lisp/emms-playlist-limit.el

(autoload 'emms-playlist-limit-enable "emms/lisp/emms-playlist-limit" "\
Turn on emms playlist limit.

\(fn)" t nil)

(autoload 'emms-playlist-limit-disable "emms/lisp/emms-playlist-limit" "\
Turn off emms playlist limit.

\(fn)" t nil)

(autoload 'emms-playlist-limit-toggle "emms/lisp/emms-playlist-limit" "\
Toggle emms playlist limit.

\(fn)" t nil)

;;;***

;;;### (autoloads (emms-playlist-mode) "emms/lisp/emms-playlist-mode"
;;;;;;  "emms/lisp/emms-playlist-mode.el" (20176 38066))
;;; Generated autoloads from emms/lisp/emms-playlist-mode.el

(autoload 'emms-playlist-mode "emms/lisp/emms-playlist-mode" "\
A major mode for Emms playlists.
\\{emms-playlist-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads (emms-score-toggle emms-score-disable emms-score-enable)
;;;;;;  "emms/lisp/emms-score" "emms/lisp/emms-score.el" (20176 38066))
;;; Generated autoloads from emms/lisp/emms-score.el

(autoload 'emms-score-enable "emms/lisp/emms-score" "\
Turn on emms-score.

\(fn)" t nil)

(autoload 'emms-score-disable "emms/lisp/emms-score" "\
Turn off emms-score.

\(fn)" t nil)

(autoload 'emms-score-toggle "emms/lisp/emms-score" "\
Toggle emms-score.

\(fn)" t nil)

;;;***

;;;### (autoloads (emms-default-players emms-devel emms-all emms-standard
;;;;;;  emms-minimalistic) "emms/lisp/emms-setup" "emms/lisp/emms-setup.el"
;;;;;;  (20176 38066))
;;; Generated autoloads from emms/lisp/emms-setup.el

(autoload 'emms-minimalistic "emms/lisp/emms-setup" "\
An Emms setup script.
Invisible playlists and all the basics for playing media.

\(fn)" nil nil)

(autoload 'emms-standard "emms/lisp/emms-setup" "\
An Emms setup script.
Everything included in the `emms-minimalistic' setup, the Emms
interactive playlist mode, reading information from tagged
audio files, and a metadata cache.

\(fn)" nil nil)

(autoload 'emms-all "emms/lisp/emms-setup" "\
An Emms setup script.
Everything included in the `emms-standard' setup and adds all the
stable features which come with the Emms distribution.

\(fn)" nil nil)

(autoload 'emms-devel "emms/lisp/emms-setup" "\
An Emms setup script.
Everything included in the `emms-all' setup and adds all the
features which come with the Emms distribution regardless of if
they are considered stable or not.  Use this if you like living
on the edge.

\(fn)" nil nil)

(autoload 'emms-default-players "emms/lisp/emms-setup" "\
Set `emms-player-list' to `emms-setup-default-player-list'.

\(fn)" nil nil)

;;;***

;;;### (autoloads (emms-locate emms-source-file-regex emms-source-file-directory-tree)
;;;;;;  "emms/lisp/emms-source-file" "emms/lisp/emms-source-file.el"
;;;;;;  (20176 38066))
;;; Generated autoloads from emms/lisp/emms-source-file.el
(autoload 'emms-play-file "emms-source-file" nil t)
(autoload 'emms-add-file "emms-source-file" nil t)
(autoload 'emms-play-directory "emms-source-file" nil t)
(autoload 'emms-add-directory "emms-source-file" nil t)
(autoload 'emms-play-directory-tree "emms-source-file" nil t)
(autoload 'emms-add-directory-tree "emms-source-file" nil t)
(autoload 'emms-play-find "emms-source-file" nil t)
(autoload 'emms-add-find "emms-source-file" nil t)
(autoload 'emms-play-dired "emms-source-file" nil t)
(autoload 'emms-add-dired "emms-source-file" nil t)

(autoload 'emms-source-file-directory-tree "emms/lisp/emms-source-file" "\
Return a list of all files under DIR that match REGEX.
This function uses `emms-source-file-directory-tree-function'.

\(fn DIR REGEX)" nil nil)

(autoload 'emms-source-file-regex "emms/lisp/emms-source-file" "\
Return a regexp that matches everything any player (that supports
files) can play.

\(fn)" nil nil)

(autoload 'emms-locate "emms/lisp/emms-source-file" "\
Search for REGEXP and display the results in a locate buffer

\(fn REGEXP)" t nil)
(autoload 'emms-play-url "emms-source-file" nil t)
(autoload 'emms-add-url "emms-source-file" nil t)
(autoload 'emms-play-streamlist "emms-source-file" nil t)
(autoload 'emms-add-streamlist "emms-source-file" nil t)
(autoload 'emms-play-lastfm "emms-lastfm" nil t)
(autoload 'emms-add-lastfm "emms-lastfm" nil t)

;;;***

;;;### (autoloads nil "emms/lisp/emms-source-playlist" "emms/lisp/emms-source-playlist.el"
;;;;;;  (20176 38066))
;;; Generated autoloads from emms/lisp/emms-source-playlist.el
(autoload 'emms-play-playlist "emms-source-playlist" nil t)
(autoload 'emms-add-playlist "emms-source-playlist" nil t)
(autoload 'emms-play-native-playlist "emms-source-playlist" nil t)
(autoload 'emms-add-native-playlist "emms-source-playlist" nil t)
(autoload 'emms-play-m3u-playlist "emms-source-playlist" nil t)
(autoload 'emms-add-m3u-playlist "emms-source-playlist" nil t)
(autoload 'emms-play-pls-playlist "emms-source-playlist" nil t)
(autoload 'emms-add-pls-playlist "emms-source-playlist" nil t)
(autoload 'emms-play-playlist-file "emms-source-playlist" nil t)
(autoload 'emms-add-playlist-file "emms-source-playlist" nil t)
(autoload 'emms-play-playlist-directory
          "emms-source-playlist" nil t)
(autoload 'emms-add-playlist-directory
          "emms-source-playlist" nil t)
(autoload 'emms-play-playlist-directory-tree
          "emms-source-playlist" nil t)
(autoload 'emms-add-playlist-directory-tree
          "emms-source-file" nil t)

;;;***

;;;### (autoloads (emms-streams) "emms/lisp/emms-streams" "emms/lisp/emms-streams.el"
;;;;;;  (20176 38066))
;;; Generated autoloads from emms/lisp/emms-streams.el

(autoload 'emms-streams "emms/lisp/emms-streams" "\
Opens the EMMS Streams interface.

\(fn)" t nil)

;;;***

;;;### (autoloads (emms-volume-mode-minus emms-volume-mode-plus emms-volume-lower
;;;;;;  emms-volume-raise) "emms/lisp/emms-volume" "emms/lisp/emms-volume.el"
;;;;;;  (20176 38066))
;;; Generated autoloads from emms/lisp/emms-volume.el

(autoload 'emms-volume-raise "emms/lisp/emms-volume" "\
Raise the speaker volume.

\(fn)" t nil)

(autoload 'emms-volume-lower "emms/lisp/emms-volume" "\
Lower the speaker volume.

\(fn)" t nil)

(autoload 'emms-volume-mode-plus "emms/lisp/emms-volume" "\
Raise volume and enable or extend the `emms-volume-minor-mode' timeout.

\(fn)" t nil)

(autoload 'emms-volume-mode-minus "emms/lisp/emms-volume" "\
Lower volume and enable or extend the `emms-volume-minor-mode' timeout.

\(fn)" t nil)

;;;***

;;;### (autoloads (emms-volume-amixer-change) "emms/lisp/emms-volume-amixer"
;;;;;;  "emms/lisp/emms-volume-amixer.el" (20176 38066))
;;; Generated autoloads from emms/lisp/emms-volume-amixer.el

(autoload 'emms-volume-amixer-change "emms/lisp/emms-volume-amixer" "\
Change amixer master volume by AMOUNT.

\(fn AMOUNT)" nil nil)

;;;***

;;;### (autoloads (tq-create) "emms/lisp/tq" "emms/lisp/tq.el" (20176
;;;;;;  38066))
;;; Generated autoloads from emms/lisp/tq.el

(autoload 'tq-create "emms/lisp/tq" "\
Create and return a transaction queue communicating with PROCESS.
PROCESS should be a subprocess capable of sending and receiving
streams of bytes.  It may be a local process, or it may be connected
to a tcp server on another machine.

\(fn PROCESS)" nil nil)

;;;***

;;;### (autoloads (graphviz-dot-mode) "graphviz-dot-mode/graphviz-dot-mode"
;;;;;;  "graphviz-dot-mode/graphviz-dot-mode.el" (20171 20580))
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
;;;;;;  "highlight-parentheses/highlight-parentheses.el" (20171 20481))
;;; Generated autoloads from highlight-parentheses/highlight-parentheses.el

(autoload 'highlight-parentheses-mode "highlight-parentheses/highlight-parentheses" "\
Minor mode to highlight the surrounding parentheses.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (highlight-symbol-query-replace highlight-symbol-prev-in-defun
;;;;;;  highlight-symbol-next-in-defun highlight-symbol-prev highlight-symbol-next
;;;;;;  highlight-symbol-remove-all highlight-symbol-at-point highlight-symbol-mode)
;;;;;;  "highlight-symbol/highlight-symbol" "highlight-symbol/highlight-symbol.el"
;;;;;;  (20171 20201))
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
;;;;;;  "hungury-delete/hungry-delete.el" (20171 20196))
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

;;;### (autoloads (iedit-mode) "iedit/iedit" "iedit/iedit.el" (20171
;;;;;;  20020))
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

;;;### (autoloads (linum-update-window linum+-generate-linum-format
;;;;;;  linum+-smart-format linum+-dynamic-format linum-format) "linum+/linum+"
;;;;;;  "linum+/linum+.el" (20176 40316))
;;; Generated autoloads from linum+/linum+.el

(defvar linum-format 'smart "\
Format used to display line numbers.

+ Either a format string like \"%7d\",
+ or `smart' to smart adapt the width by current max visible line number.
+ or `dynamic' to adapt the width as needed,
+ or a vector with one string element which uesed to generate
line number format by `format' with argument max visible line number 
of current buffer, see example `linum+-smart-format'
+ or a list with one string element which uesed to generate
line number format by `format' with argument max line number of current buffer,
see example `linum+-dynamic-format'
+ or a function that is called with a line number as its
argument and should evaluate to a string to be shown on that line.

See also `linum-before-numbering-hook'.")

(custom-autoload 'linum-format "linum+/linum+" t)

(defvar linum+-dynamic-format "%%%dd|" "\
Format used to generate line number format when `linum-format' is `dynamic'.")

(custom-autoload 'linum+-dynamic-format "linum+/linum+" t)

(defvar linum+-smart-format "%%%dd|" "\
Format used to generate line number format when `linum-format' is `smart'.")

(custom-autoload 'linum+-smart-format "linum+/linum+" t)

(autoload 'linum+-generate-linum-format "linum+/linum+" "\
Generate line number format by FORMAT-TYPE, LIMIT is `window-end' of win.

\(fn FORMAT-TYPE LIMIT)" nil nil)

(autoload 'linum-update-window "linum+/linum+" "\
Update line numbers for the portion visible in window WIN.

\(fn WIN)" nil nil)

;;;***

;;;### (autoloads (lusty-launch-dired lusty-select-current-name lusty-select-match
;;;;;;  lusty-open-this lusty-highlight-previous-column lusty-highlight-next-column
;;;;;;  lusty-highlight-previous lusty-highlight-next lusty-buffer-explorer
;;;;;;  lusty-file-explorer) "lusty-explorer/lusty-explorer" "lusty-explorer/lusty-explorer.el"
;;;;;;  (20171 20091))
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

;;;### (autoloads (mode-compile-kill mode-compile mode-compile-submit-bug-report
;;;;;;  emacs-lisp-byte-compile-dir-interactive-p mode-compile-reading-time
;;;;;;  mode-compile-expert-p mode-compile-after-kill-hook mode-compile-before-kill-hook
;;;;;;  mode-compile-after-compile-hook mode-compile-before-compile-hook
;;;;;;  mode-compile-other-frame-p mode-compile-never-edit-command-p
;;;;;;  mode-compile-always-save-buffer-p mode-compile-save-all-p
;;;;;;  mode-compile-ignore-makerule-regexp mode-compile-prefered-default-makerule
;;;;;;  mode-compile-make-options mode-compile-ignore-makefile-backups
;;;;;;  mode-compile-make-program) "mode-compile/mode-compile" "mode-compile/mode-compile.el"
;;;;;;  (20172 47030))
;;; Generated autoloads from mode-compile/mode-compile.el

(defvar mode-compile-make-program "make" "\
*The `make' program used to process makefiles.

If you have GNU make installed with name \"gmake\" use it.")

(custom-autoload 'mode-compile-make-program "mode-compile/mode-compile" t)

(defvar mode-compile-ignore-makefile-backups t "\
*Tell mode compile to ignore makefiles backup files when selecting the Makefile to use.")

(custom-autoload 'mode-compile-ignore-makefile-backups "mode-compile/mode-compile" t)

(defvar mode-compile-default-make-options "-k" "\
Default options to give to `make'.")

(defvar mode-compile-make-options (eval mode-compile-default-make-options) "\
*Options to give to `make'.
This could be any form evaluating to a string.

Some people asked me a way to modify the make options everytime a
compilation command is launched, do that:
\(defun my-mode-compile-ask-make-options()
\"*Hook called by mode-compile, asking for make options.\"
\(interactive)
\(read-string \"Make options: \"
mode-compile-default-make-options))
\(setq mode-compile-make-options
'my-mode-compile-ask-make-options)")

(custom-autoload 'mode-compile-make-options "mode-compile/mode-compile" t)

(defvar mode-compile-prefered-default-makerule 'none "\
*Default makerule you would like to see in minibuffer as a default choice
when selecting the make rule to build.

Possible values are:
'none    -- let mode-compile deciding for you.
'all     -- try hard to show you the \"all\" rule.
'default -- try hard to show you the \"default\" rule.
'file    -- try to show you the name of the file which will be
result of compilation.
The 'none action is taken as default is something fail.")

(custom-autoload 'mode-compile-prefered-default-makerule "mode-compile/mode-compile" t)

(defvar mode-compile-ignore-makerule-regexp nil "\
*Makefile rules which must be ignored when building completion list.

For example if you want to remove all `files rules' set
it to: \"\\\\.\\\\([aoc]\\\\|s[ao][.0-9]*\\\\)\". ")

(custom-autoload 'mode-compile-ignore-makerule-regexp "mode-compile/mode-compile" t)

(defvar mode-compile-save-all-p nil "\
*Non-nil means save ALL the modified buffers without asking
before launching compilation command.")

(custom-autoload 'mode-compile-save-all-p "mode-compile/mode-compile" t)

(defvar mode-compile-always-save-buffer-p nil "\
*Non-nil means save the current buffer without asking
before launching compilation command.")

(custom-autoload 'mode-compile-always-save-buffer-p "mode-compile/mode-compile" t)

(defvar mode-compile-never-edit-command-p nil "\
*Non-nil means never ask to user to edit the compile command.")

(custom-autoload 'mode-compile-never-edit-command-p "mode-compile/mode-compile" t)

(defvar mode-compile-other-frame-p nil "\
*Non-nil means compile in another frame.

A new Emacs FRAME is created and the compilation command is executed
in this other frame.  To specify the frame parameters see also
variable `mode-compile-frame-parameters-alist'.")

(custom-autoload 'mode-compile-other-frame-p "mode-compile/mode-compile" t)

(defvar mode-compile-before-compile-hook nil "\
Hook to be run before compile command is executed
when `mode-compile' is invoked.")

(custom-autoload 'mode-compile-before-compile-hook "mode-compile/mode-compile" t)

(defvar mode-compile-after-compile-hook nil "\
Hook to be run after compile command is executed
when `mode-compile' is invoked.")

(custom-autoload 'mode-compile-after-compile-hook "mode-compile/mode-compile" t)

(defvar mode-compile-before-kill-hook nil "\
Hook to be run before killing compile command is executed
when `mode-compile-kill' is invoked.")

(custom-autoload 'mode-compile-before-kill-hook "mode-compile/mode-compile" t)

(defvar mode-compile-after-kill-hook nil "\
Hook to be run after killing compile command is executed
when `mode-compile-kill' is invoked.")

(custom-autoload 'mode-compile-after-kill-hook "mode-compile/mode-compile" t)

(defvar mode-compile-choosen-compiler nil "\
*Global variable containing the name of the compiler
which will be used for compiling without makefile.

Could be used in combination with
\(cc|c++|ada|f77)-default-compiler-options
to automaticaly choose the compiler specific options.

example:
\(defun my-compiler-get-options()
\(cond
\((string= mode-compile-choosen-compiler \"gcc\")
\"-Wall -pedantic-errors\")
\((string= mode-compile-choosen-compiler \"cc\")
\"cc options whatever they are...\")
\(t
\(message \"Don't know this compiler: %s\" mode-compile-choosen-compiler)
\(read-string
\(format \"Options for %s compiler: \" mode-compile-choosen-compiler)))))

\(setq cc-default-compiler-options 'my-compiler-get-options)")

(defvar mode-compile-expert-p nil "\
*Non nil means `mode-compile' will not speaks too much.

See also variable variable mode-compile-reading-time.")

(custom-autoload 'mode-compile-expert-p "mode-compile/mode-compile" t)

(defvar mode-compile-reading-time 1 "\
*Seconds to wait in verbose mode after printing a message.

In verbose mode mode-compile print too much messages that it is
allmost impossible to read them. Just setting this delay leave you the
time to read all the messages. If you don't want any delay set it to
`0'.

See also function sit-for.")

(custom-autoload 'mode-compile-reading-time "mode-compile/mode-compile" t)

(defvar emacs-lisp-byte-compile-dir-interactive-p t "\
*Non-nil means when byte-compiling a directory ask for each file
needing to be recompiled or not.")

(custom-autoload 'emacs-lisp-byte-compile-dir-interactive-p "mode-compile/mode-compile" t)

(defconst mode-compile-version "2.28" "\
Current version of mode-compile package.

mode-compile.el,v 2.28 2003/04/01 13:52:47 boubaker Exp
Please send bugs-fixes/contributions/comments to boubaker@cena.fr")

(autoload 'mode-compile-submit-bug-report "mode-compile/mode-compile" "\
*Submit via mail a bug report on mode-compile v2.27.

\(fn)" t nil)

(autoload 'mode-compile "mode-compile/mode-compile" "\
*Compile the file in the current buffer with a dynamically built command.

The command is built according to the current major mode the function
was invoked from.

Running this command preceded by universal-argument (\\[universal-argument])
allows remote compilation, the user is prompted for a host name to run the
compilation command on.

Currently know how to compile in:
`c-mode' ,              -- function cc-compile.
`java-mode' ,           -- function java-compile.
`c++-mode',             -- function c++-compile.
`ada-mode',             -- function ada-compile.
`fortran-mode',         -- function f77-compile.
`emacs-lisp-mode'       -- function elisp-compile.
`lisp-interaction-mode' -- function elisp-compile.
`makefile-mode'         -- function makefile-compile.
`dired-mode'            -- function dired-compile.
`sh-mode'               -- function sh-compile.
`csh-mode'              -- function csh-compile.
`zsh-mode'              -- function zsh-compile.
`perl-mode'             -- function perl-compile.
`cperl-mode'            -- function perl-compile.
`tcl-mode'              -- function tcl-compile.
`python-mode'           -- function python-compile.
`ruby-mode'             -- function ruby-compile.
`fundamental-mode'      -- function guess-compile.
`text-mode'             -- function guess-compile.
`indented-text-mode'    -- function guess-compile.
`compilation-mode'      -- function default-compile.
The function `guess-compile' is called when mode is unknown.

The variable `mode-compile-modes-alist' contain description of known
modes.  The hooks variables `mode-compile-before-compile-hook' and
`mode-compile-after-compile-hook' are run just before and after
invoking the compile command of the mode.

Use the command `mode-compile-kill' (\\[mode-compile-kill]) to abort a
running compilation.

Bound on \\[mode-compile].

\(fn &optional REMOTE-HOST)" t nil)

(autoload 'mode-compile-kill "mode-compile/mode-compile" "\
*Kill the running compilation launched by `mode-compile' (\\[mode-compile]) command.

The compilation command is killed according to the current major mode
the function was invoked from.

Currently know how to kill compilations from:
`c-mode' ,              -- function kill-compilation.
`java-mode' ,           -- function kill-compilation.
`c++-mode' ,            -- function kill-compilation.
`ada-mode' ,            -- function kill-compilation.
`fortran-mode' ,        -- function kill-compilation.
`emacs-lisp-mode'       -- function keyboard-quit.
`lisp-interaction-mode' -- function keyboard-quit.
`makefile-mode'         -- function kill-compilation.
`dired-mode'            -- function kill-compilation.
`sh-mode'               -- function kill-compilation.
`csh-mode'              -- function kill-compilation.
`zsh-mode'              -- function kill-compilation.
`perl-mode'             -- function kill-compilation.
`cperl-mode'            -- function kill-compilation.
`tcl-mode'              -- function kill-compilation.
`python-mode'           -- function kill-compilation.
`ruby-mode'             -- function kill-compilation.
`fundamental-mode'      -- Bound dynamically.
`text-mode'             -- Bound dynamically.
`indented-text-mode'    -- Bound dynamically.
`compilation-mode'      -- function kill-compilation.

The variable `mode-compile-modes-alist' contain description of ALL
known modes.  The hooks variables `mode-compile-before-kill-hook' and
`mode-compile-after-kill-hook' are run just before and after invoking
the kill compile command of the mode.

Bound on \\[mode-compile-kill].

\(fn)" t nil)

;;;***

;;;### (autoloads (multi-term) "multi-term/multi-term" "multi-term/multi-term.el"
;;;;;;  (20171 20556))
;;; Generated autoloads from multi-term/multi-term.el

(autoload 'multi-term "multi-term/multi-term" "\
Create new term buffer.
Will prompt you shell name when you type `C-u' before this command.

\(fn)" t nil)

;;;***

;;;### (autoloads (nav) "nav/nav" "nav/nav.el" (20171 20196))
;;; Generated autoloads from nav/nav.el

(autoload 'nav "nav/nav" "\
Run nav-mode in a narrow window on the left side.

\(fn)" t nil)

;;;***

;;;### (autoloads (org2blog/wp-preview-subtree-post org2blog/wp-preview-buffer-post
;;;;;;  org2blog/wp-track-subtree org2blog/wp-track-buffer org2blog/wp-post-subtree
;;;;;;  org2blog/wp-new-entry org2blog/wp-login org2blog/wp-mode)
;;;;;;  "org2blog/org2blog" "org2blog/org2blog.el" (20171 20474))
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
;;;;;;  (20171 20178))
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
;;;;;;  "pretty-lambdada/pretty-lambdada.el" (20171 20501))
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
;;;;;;  (20171 20479))
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
;;;;;;  "pylookup/pylookup" "pylookup/pylookup.el" (20171 20322))
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
;;;;;;  "python-mode/doctest-mode.el" (20171 20370))
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
;;;;;;  "python-mode/python-mode.el" (20171 20370))
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
;;;;;;  "rainbow-delimiters/rainbow-delimiters.el" (20171 20091))
;;; Generated autoloads from rainbow-delimiters/rainbow-delimiters.el

(autoload 'rainbow-delimiters-mode "rainbow-delimiters/rainbow-delimiters" "\
Color nested parentheses, brackets, and braces according to their depth.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (rainbow-mode) "rainbow-mode/rainbow-mode" "rainbow-mode/rainbow-mode.el"
;;;;;;  (20171 20505))
;;; Generated autoloads from rainbow-mode/rainbow-mode.el

(autoload 'rainbow-mode "rainbow-mode/rainbow-mode" "\
Colorize strings that represent colors.
This will fontify with colors the string like \"#aabbcc\" or \"blue\".

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (scala-mode) "scala-mode/scala-mode" "scala-mode/scala-mode.el"
;;;;;;  (20171 20553))
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
;;;;;;  "scala-mode/scala-mode-inf.el" (20171 20556))
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

;;;### (autoloads (smex-initialize) "smex/smex" "smex/smex.el" (20176
;;;;;;  38485))
;;; Generated autoloads from smex/smex.el

(autoload 'smex-initialize "smex/smex" "\


\(fn)" t nil)

;;;***

;;;### (autoloads (turn-on-tempbuf-mode tempbuf-mode) "tempbuf/tempbuf"
;;;;;;  "tempbuf/tempbuf.el" (20171 20501))
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

;;;### (autoloads nil nil ("apel/site-lisp/apel/calist.el" "apel/site-lisp/apel/filename.el"
;;;;;;  "apel/site-lisp/apel/install.el" "apel/site-lisp/emu/apel-ver.el"
;;;;;;  "apel/site-lisp/emu/broken.el" "apel/site-lisp/emu/emu.el"
;;;;;;  "apel/site-lisp/emu/inv-23.el" "apel/site-lisp/emu/invisible.el"
;;;;;;  "apel/site-lisp/emu/mcharset.el" "apel/site-lisp/emu/mcs-20.el"
;;;;;;  "apel/site-lisp/emu/mcs-e20.el" "apel/site-lisp/emu/mule-caesar.el"
;;;;;;  "apel/site-lisp/emu/pccl-20.el" "apel/site-lisp/emu/pccl.el"
;;;;;;  "apel/site-lisp/emu/pces-20.el" "apel/site-lisp/emu/pces-e20.el"
;;;;;;  "apel/site-lisp/emu/pces.el" "apel/site-lisp/emu/pcustom.el"
;;;;;;  "apel/site-lisp/emu/poe.el" "apel/site-lisp/emu/poem-e20.el"
;;;;;;  "apel/site-lisp/emu/poem-e20_3.el" "apel/site-lisp/emu/poem.el"
;;;;;;  "apel/site-lisp/emu/product.el" "apel/site-lisp/emu/pym.el"
;;;;;;  "apel/site-lisp/emu/static.el" "emacs-goodies-el/elisp/debian-el/debian-el-loaddefs.el"
;;;;;;  "emacs-goodies-el/elisp/debian-el/debian-el.el" "emacs-goodies-el/elisp/debian-el/gnus-BTS.el"
;;;;;;  "emacs-goodies-el/elisp/devscripts-el/devscripts.el" "emacs-goodies-el/elisp/devscripts-el/pbuilder-log-view-mode.el"
;;;;;;  "emacs-goodies-el/elisp/devscripts-el/pbuilder-mode.el" "emacs-goodies-el/elisp/dpkg-dev-el/dpkg-dev-el-loaddefs.el"
;;;;;;  "emacs-goodies-el/elisp/dpkg-dev-el/dpkg-dev-el.el" "emacs-goodies-el/elisp/emacs-goodies-el/auto-fill-inhibit.el"
;;;;;;  "emacs-goodies-el/elisp/emacs-goodies-el/clipper.el" "emacs-goodies-el/elisp/emacs-goodies-el/color-theme-library.el"
;;;;;;  "emacs-goodies-el/elisp/emacs-goodies-el/color-theme_seldefcustom.el"
;;;;;;  "emacs-goodies-el/elisp/emacs-goodies-el/csv-mode.el" "emacs-goodies-el/elisp/emacs-goodies-el/dedicated.el"
;;;;;;  "emacs-goodies-el/elisp/emacs-goodies-el/dict.el" "emacs-goodies-el/elisp/emacs-goodies-el/dir-locals.el"
;;;;;;  "emacs-goodies-el/elisp/emacs-goodies-el/edit-env.el" "emacs-goodies-el/elisp/emacs-goodies-el/emacs-goodies-build.el"
;;;;;;  "emacs-goodies-el/elisp/emacs-goodies-el/emacs-goodies-custom.el"
;;;;;;  "emacs-goodies-el/elisp/emacs-goodies-el/emacs-goodies-el.el"
;;;;;;  "emacs-goodies-el/elisp/emacs-goodies-el/eproject.el" "emacs-goodies-el/elisp/emacs-goodies-el/filladapt.el"
;;;;;;  "emacs-goodies-el/elisp/emacs-goodies-el/highlight-beyond-fill-column.el"
;;;;;;  "emacs-goodies-el/elisp/emacs-goodies-el/highlight-completion.el"
;;;;;;  "emacs-goodies-el/elisp/emacs-goodies-el/initsplit.el" "emacs-goodies-el/elisp/emacs-goodies-el/joc-toggle-buffer.el"
;;;;;;  "emacs-goodies-el/elisp/emacs-goodies-el/joc-toggle-case.el"
;;;;;;  "emacs-goodies-el/elisp/emacs-goodies-el/keywiz.el" "emacs-goodies-el/elisp/emacs-goodies-el/maplev.el"
;;;;;;  "emacs-goodies-el/elisp/emacs-goodies-el/marker-visit.el"
;;;;;;  "emacs-goodies-el/elisp/emacs-goodies-el/minibuf-electric.el"
;;;;;;  "emacs-goodies-el/elisp/emacs-goodies-el/minibuffer-complete-cycle.el"
;;;;;;  "emacs-goodies-el/elisp/emacs-goodies-el/mutt-alias.el" "emacs-goodies-el/elisp/emacs-goodies-el/quack.el"
;;;;;;  "emacs-goodies-el/elisp/emacs-goodies-el/setnu.el" "emacs-goodies-el/elisp/emacs-goodies-el/todoo.el"
;;;;;;  "emacs-goodies-el/elisp/emacs-goodies-el/toggle-option.el"
;;;;;;  "emacs-goodies-el/elisp/emacs-goodies-el/upstart-mode.el"
;;;;;;  "emacs-goodies-el/elisp/vm-bonus-el/vm-bogofilter.el" "emacs-w3m/mew-w3m.el"
;;;;;;  "emacs-w3m/w3m-bug.el" "emacs-w3m/w3m-ccl.el" "emacs-w3m/w3m-ems.el"
;;;;;;  "emacs-w3m/w3m-favicon.el" "emacs-w3m/w3m-hist.el" "emacs-w3m/w3m-image.el"
;;;;;;  "emacs-w3m/w3m-load.el" "emacs-w3m/w3m-mail.el" "emacs-w3m/w3m-proc.el"
;;;;;;  "emacs-w3m/w3m-rss.el" "emacs-w3m/w3m-tabmenu.el" "emacs-w3m/w3m-ucs.el"
;;;;;;  "emacs-w3m/w3m-util.el" "emacs-w3m/w3m-xmas.el" "emacs-w3m/w3mhack.el"
;;;;;;  "perl-completion/perl-completion.el" "popup-kill-ring/popup-kill-ring.el"
;;;;;;  "vimpulse-surround/vimpulse-surround.el" "vimpulse/test-framework.el"
;;;;;;  "vimpulse/vimpulse-compatibility.el" "vimpulse/vimpulse-dependencies.el"
;;;;;;  "vimpulse/vimpulse-ex.el" "vimpulse/vimpulse-misc-keybindings.el"
;;;;;;  "vimpulse/vimpulse-modal.el" "vimpulse/vimpulse-operator.el"
;;;;;;  "vimpulse/vimpulse-paren-matching.el" "vimpulse/vimpulse-test.el"
;;;;;;  "vimpulse/vimpulse-text-object-system.el" "vimpulse/vimpulse-utils.el"
;;;;;;  "vimpulse/vimpulse-viper-function-redefinitions.el" "vimpulse/vimpulse-visual-mode.el"
;;;;;;  "vimpulse/vimpulse.el") (20176 45765 856710))

;;;***

(provide '.loaddefs)
;; Local Variables:
;; version-control: never
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; .loaddefs.el ends here
