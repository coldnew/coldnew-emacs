;;; .loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (set-up-slime-ac) "ac-slime/ac-slime" "ac-slime/ac-slime.el"
;;;;;;  (20383 44964))
;;; Generated autoloads from ac-slime/ac-slime.el

(defface ac-slime-menu-face '((t (:inherit 'ac-candidate-face))) "\
Face for slime candidate menu." :group (quote auto-complete))

(defface ac-slime-selection-face '((t (:inherit 'ac-selection-face))) "\
Face for the slime selected candidate." :group (quote auto-complete))

(defvar ac-source-slime-fuzzy '((init . ac-slime-init) (candidates . ac-source-slime-fuzzy-candidates) (candidate-face . ac-slime-menu-face) (selection-face . ac-slime-selection-face) (prefix . slime-symbol-start-pos) (symbol . "l") (match lambda (prefix candidates) candidates) (document . ac-slime-documentation)) "\
Source for fuzzy slime completion")

(defvar ac-source-slime-simple '((init . ac-slime-init) (candidates . ac-source-slime-simple-candidates) (candidate-face . ac-slime-menu-face) (selection-face . ac-slime-selection-face) (prefix . slime-symbol-start-pos) (symbol . "l") (document . ac-slime-documentation)) "\
Source for slime completion")

(autoload 'set-up-slime-ac "ac-slime/ac-slime" "\
Add an optionally-fuzzy slime completion source to the
front of `ac-sources' for the current buffer.

\(fn &optional FUZZY)" t nil)

;;;***

;;;### (autoloads (ace-jump-mode) "ace-jump-mode/ace-jump-mode" "ace-jump-mode/ace-jump-mode.el"
;;;;;;  (20377 29861))
;;; Generated autoloads from ace-jump-mode/ace-jump-mode.el

(autoload 'ace-jump-mode "ace-jump-mode/ace-jump-mode" "\
AceJump mode is a minor mode for you to quick jump to a
position in the curret view.
   There is three submode now:
     `ace-jump-char-mode'
     `ace-jump-word-mode'
     `ace-jump-line-mode'

You can specify the sequence about which mode should enter
by customize `ace-jump-mode-submode-list'.

If you do not want to query char for word mode, you can change
`ace-jump-word-mode-use-query-char' to nil.

If you don't like the default move keys, you can change it by
setting `ace-jump-mode-move-keys'.

You can constrol whether use the case sensitive via
`ace-jump-mode-case-fold'.

\(fn &optional PREFIX)" t nil)

;;;***

;;;### (autoloads (android-mode) "android-mode/android-mode" "android-mode/android-mode.el"
;;;;;;  (20377 6600))
;;; Generated autoloads from android-mode/android-mode.el

(autoload 'android-mode "android-mode/android-mode" "\
Android application development minor mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (anything-other-buffer anything-at-point anything)
;;;;;;  "anything/anything" "anything/anything.el" (20369 33698))
;;; Generated autoloads from anything/anything.el

(autoload 'anything "anything/anything" "\
Main function to execute anything sources.

Keywords supported:
:sources :input :prompt :resume :preselect :buffer :keymap :default :history
Extra keywords are supported and can be added, see below.

When call interactively with no arguments deprecated `anything-sources'
will be used if non--nil.

PLIST is a list like (:key1 val1 :key2 val2 ...) or
\(&optional sources input prompt resume preselect buffer keymap default history).

Basic keywords are the following:

:sources

Temporary value of `anything-sources'.  It also accepts a
symbol, interpreted as a variable of an anything source.  It
also accepts an alist representing an anything source, which is
detected by (assq 'name ANY-SOURCES)

:input

Temporary value of `anything-pattern', ie. initial input of minibuffer.

:prompt

Prompt other than \"pattern: \".

:resume

If t, Resurrect previously instance of `anything'.  Skip the initialization.
If 'noresume, this instance of `anything' cannot be resumed.

:preselect

Initially selected candidate.  Specified by exact candidate or a regexp.

:buffer

`anything-buffer' instead of *anything*.

:keymap

`anything-map' for current `anything' session.

:default

A default argument that will be inserted in minibuffer with \\<minibuffer-local-map>\\[next-history-element].
When nil of not present `thing-at-point' will be used instead.

:history

By default all minibuffer input is pushed to `minibuffer-history',
if an argument HISTORY is provided, input will be pushed to HISTORY.
History element should be a symbol.

Of course, conventional arguments are supported, the two are same.

\(anything :sources sources :input input :prompt prompt :resume resume
	   :preselect preselect :buffer buffer :keymap keymap :default default
	   :history history)
\(anything sources input prompt resume preselect buffer keymap default history)

Other keywords are interpreted as local variables of this anything session.
The `anything-' prefix can be omitted.  For example,

\(anything :sources 'anything-c-source-buffers
	   :buffer \"*buffers*\" :candidate-number-limit 10)

means starting anything session with `anything-c-source-buffers'
source in *buffers* buffer and set variable `anything-candidate-number-limit'
to 10 as session local variable.

\(fn &rest PLIST)" t nil)

(autoload 'anything-at-point "anything/anything" "\
Call anything with symbol at point as initial input.
ANY-SOURCES ANY-INPUT ANY-PROMPT ANY-RESUME ANY-PRESELECT and ANY-BUFFER
are same args as in `anything'.

\(fn &optional ANY-SOURCES ANY-INPUT ANY-PROMPT ANY-RESUME ANY-PRESELECT ANY-BUFFER)" t nil)

(autoload 'anything-other-buffer "anything/anything" "\
Simplified interface of `anything' with other `anything-buffer'.
Call `anything' with only ANY-SOURCES and ANY-BUFFER as args.

\(fn ANY-SOURCES ANY-BUFFER)" nil nil)

;;;***

;;;### (autoloads (anything-xrandr-set anything-c-apropos anything-ucs
;;;;;;  anything-ratpoison-commands anything-c-run-external-command
;;;;;;  anything-eshell-history anything-esh-pcomplete anything-apt
;;;;;;  anything-world-time anything-select-xfont anything-top anything-create
;;;;;;  anything-execute-anything-command anything-call-source anything-surfraw
;;;;;;  anything-calcul-expression anything-eval-expression-with-eldoc
;;;;;;  anything-eval-expression anything-yaoddmuse-emacswiki-post-library
;;;;;;  anything-yaoddmuse-emacswiki-edit-or-view anything-all-mark-rings
;;;;;;  anything-global-mark-ring anything-mark-ring anything-simple-call-tree
;;;;;;  anything-bookmark-ext anything-manage-advice anything-M-x
;;;;;;  anything-filelist+ anything-filelist anything-c-etags-select
;;;;;;  anything-do-pdfgrep anything-do-zgrep anything-do-grep anything-dired-hardlink-file
;;;;;;  anything-dired-symlink-file anything-dired-copy-file anything-dired-rename-file
;;;;;;  anything-insert-file anything-write-file anything-find-files
;;;;;;  anything-c-copy-files-async anything-regexp anything-org-headlines
;;;;;;  anything-browse-code anything-occur anything-list-emacs-process
;;;;;;  anything-timers anything-bm-list anything-eev-anchors anything-emms
;;;;;;  anything-org-keywords anything-man-woman anything-register
;;;;;;  anything-c-insert-latex-math anything-c-pp-bookmarks anything-bookmarks
;;;;;;  anything-colors anything-firefox-bookmarks anything-w3m-bookmarks
;;;;;;  anything-locate anything-bbdb anything-buffers-list anything-for-buffers
;;;;;;  anything-yahoo-suggest anything-google-suggest anything-imenu
;;;;;;  anything-gentoo anything-minibuffer-history anything-show-kill-ring
;;;;;;  anything-info-at-point anything-recentf anything-for-files
;;;;;;  anything-mini anything-c-reset-adaptative-history anything-c-set-variable
;;;;;;  anything-c-call-interactively anything-w32-shell-execute-open-file
;;;;;;  anything-lisp-completion-or-file-name-at-point anything-lisp-completion-at-point-or-indent
;;;;;;  anything-c-complete-file-name-at-point anything-lisp-completion-at-point
;;;;;;  anything-completion-mode anything-yaoddmuse-cache-pages anything-c-bmkext-run-edit
;;;;;;  anything-c-bookmark-run-delete anything-c-bookmark-run-jump-other-window
;;;;;;  anything-yank-text-at-point anything-c-grep-run-save-buffer
;;;;;;  anything-c-grep-run-other-window-action anything-c-grep-run-default-action
;;;;;;  anything-c-grep-run-persistent-action anything-c-goto-next-file
;;;;;;  anything-c-goto-precedent-file anything-dired-mode anything-ff-run-kill-buffer-persistent
;;;;;;  anything-ff-persistent-delete anything-ff-properties-persistent
;;;;;;  anything-ff-run-print-file anything-ff-run-etags anything-ff-run-gnus-attach-files
;;;;;;  anything-ff-run-locate anything-ff-run-open-file-externally
;;;;;;  anything-ff-run-switch-other-frame anything-ff-run-switch-other-window
;;;;;;  anything-ff-run-switch-to-eshell anything-ff-run-complete-fn-at-point
;;;;;;  anything-ff-run-delete-file anything-ff-run-hardlink-file
;;;;;;  anything-ff-run-symlink-file anything-ff-run-ediff-merge-file
;;;;;;  anything-ff-run-ediff-file anything-ff-run-eshell-command-on-file
;;;;;;  anything-ff-run-load-file anything-ff-run-byte-compile-file
;;;;;;  anything-ff-run-rename-file anything-ff-run-copy-file anything-ff-run-zgrep
;;;;;;  anything-ff-run-pdfgrep anything-ff-run-grep anything-ff-run-switch-to-history
;;;;;;  anything-ff-run-toggle-auto-update anything-buffer-run-ediff
;;;;;;  anything-buffer-switch-to-elscreen anything-buffer-switch-other-frame
;;;;;;  anything-buffer-switch-other-window anything-buffer-run-query-replace
;;;;;;  anything-buffer-run-query-replace-regexp anything-buffer-run-zgrep
;;;;;;  anything-buffer-run-grep anything-buffer-run-kill-buffers
;;;;;;  anything-buffer-save-persistent anything-buffer-revert-persistent
;;;;;;  anything-buffer-diff-persistent anything-toggle-all-marks
;;;;;;  anything-unmark-all anything-mark-all anything-insert-buffer-name
;;;;;;  anything-test-sources anything-etags-help anything-pdfgrep-help
;;;;;;  anything-grep-help anything-generic-file-help anything-read-file-name-help
;;;;;;  anything-ff-help anything-c-buffer-help anything-configuration)
;;;;;;  "anything/anything-config" "anything/anything-config.el"
;;;;;;  (20369 33698))
;;; Generated autoloads from anything/anything-config.el

(autoload 'anything-configuration "anything/anything-config" "\
Customize `anything'.

\(fn)" t nil)

(defvar anything-command-map)

(autoload 'anything-c-buffer-help "anything/anything-config" "\
Help command for anything buffers.

\(fn)" t nil)

(autoload 'anything-ff-help "anything/anything-config" "\
Help command for `anything-find-files'.

\(fn)" t nil)

(autoload 'anything-read-file-name-help "anything/anything-config" "\


\(fn)" t nil)

(autoload 'anything-generic-file-help "anything/anything-config" "\


\(fn)" t nil)

(autoload 'anything-grep-help "anything/anything-config" "\


\(fn)" t nil)

(autoload 'anything-pdfgrep-help "anything/anything-config" "\


\(fn)" t nil)

(autoload 'anything-etags-help "anything/anything-config" "\
The help function for etags.

\(fn)" t nil)

(autoload 'anything-test-sources "anything/anything-config" "\
List all anything sources for test.
The output is sexps which are evaluated by \\[eval-last-sexp].

\(fn)" t nil)

(autoload 'anything-insert-buffer-name "anything/anything-config" "\
Insert buffer name.

\(fn)" t nil)

(autoload 'anything-mark-all "anything/anything-config" "\
Mark all visible unmarked candidates in current source.

\(fn)" t nil)

(autoload 'anything-unmark-all "anything/anything-config" "\
Unmark all candidates in all sources of current anything session.

\(fn)" t nil)

(autoload 'anything-toggle-all-marks "anything/anything-config" "\
Toggle all marks.
Mark all visible candidates of current source or unmark all candidates
visible or invisible in all sources of current anything session

\(fn)" t nil)

(autoload 'anything-buffer-diff-persistent "anything/anything-config" "\
Toggle diff buffer without quitting anything.

\(fn)" t nil)

(autoload 'anything-buffer-revert-persistent "anything/anything-config" "\
Revert buffer without quitting anything.

\(fn)" t nil)

(autoload 'anything-buffer-save-persistent "anything/anything-config" "\
Save buffer without quitting anything.

\(fn)" t nil)

(autoload 'anything-buffer-run-kill-buffers "anything/anything-config" "\
Run kill buffer action from `anything-c-source-buffers-list'.

\(fn)" t nil)

(autoload 'anything-buffer-run-grep "anything/anything-config" "\
Run Grep action from `anything-c-source-buffers-list'.

\(fn)" t nil)

(autoload 'anything-buffer-run-zgrep "anything/anything-config" "\
Run Grep action from `anything-c-source-buffers-list'.

\(fn)" t nil)

(autoload 'anything-buffer-run-query-replace-regexp "anything/anything-config" "\
Run Query replace regexp action from `anything-c-source-buffers-list'.

\(fn)" t nil)

(autoload 'anything-buffer-run-query-replace "anything/anything-config" "\
Run Query replace action from `anything-c-source-buffers-list'.

\(fn)" t nil)

(autoload 'anything-buffer-switch-other-window "anything/anything-config" "\
Run switch to other window action from `anything-c-source-buffers-list'.

\(fn)" t nil)

(autoload 'anything-buffer-switch-other-frame "anything/anything-config" "\
Run switch to other frame action from `anything-c-source-buffers-list'.

\(fn)" t nil)

(autoload 'anything-buffer-switch-to-elscreen "anything/anything-config" "\
Run switch to elscreen  action from `anything-c-source-buffers-list'.

\(fn)" t nil)

(autoload 'anything-buffer-run-ediff "anything/anything-config" "\
Run ediff action from `anything-c-source-buffers-list'.

\(fn)" t nil)

(autoload 'anything-ff-run-toggle-auto-update "anything/anything-config" "\


\(fn)" t nil)

(autoload 'anything-ff-run-switch-to-history "anything/anything-config" "\
Run Switch to history action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-grep "anything/anything-config" "\
Run Grep action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-pdfgrep "anything/anything-config" "\
Run Pdfgrep action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-zgrep "anything/anything-config" "\
Run Grep action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-copy-file "anything/anything-config" "\
Run Copy file action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-rename-file "anything/anything-config" "\
Run Rename file action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-byte-compile-file "anything/anything-config" "\
Run Byte compile file action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-load-file "anything/anything-config" "\
Run Load file action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-eshell-command-on-file "anything/anything-config" "\
Run eshell command on file action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-ediff-file "anything/anything-config" "\
Run Ediff file action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-ediff-merge-file "anything/anything-config" "\
Run Ediff merge file action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-symlink-file "anything/anything-config" "\
Run Symlink file action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-hardlink-file "anything/anything-config" "\
Run Hardlink file action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-delete-file "anything/anything-config" "\
Run Delete file action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-complete-fn-at-point "anything/anything-config" "\
Run complete file name action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-switch-to-eshell "anything/anything-config" "\
Run switch to eshell action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-switch-other-window "anything/anything-config" "\
Run switch to other window action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-switch-other-frame "anything/anything-config" "\
Run switch to other frame action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-open-file-externally "anything/anything-config" "\
Run open file externally command action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-locate "anything/anything-config" "\
Run locate action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-gnus-attach-files "anything/anything-config" "\
Run gnus attach files command action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-etags "anything/anything-config" "\
Run Etags command action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-print-file "anything/anything-config" "\
Run Print file action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-properties-persistent "anything/anything-config" "\
Show properties without quitting anything.

\(fn)" t nil)

(autoload 'anything-ff-persistent-delete "anything/anything-config" "\
Delete current candidate without quitting.

\(fn)" t nil)

(autoload 'anything-ff-run-kill-buffer-persistent "anything/anything-config" "\
Execute `anything-ff-kill-buffer-fname' whitout quitting.

\(fn)" t nil)

(defvar anything-dired-mode "Enable anything completion in Dired functions.\nBindings affected are C, R, S, H.\nThis is deprecated for Emacs24+ users, use `ac-mode' instead." "\
Non-nil if Anything-Dired mode is enabled.
See the command `anything-dired-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `anything-dired-mode'.")

(custom-autoload 'anything-dired-mode "anything/anything-config" nil)

(autoload 'anything-dired-mode "anything/anything-config" "\
Toggle Anything-Dired mode on or off.
With a prefix argument ARG, enable Anything-Dired mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil, and toggle it if ARG is `toggle'.
\\{anything-dired-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'anything-c-goto-precedent-file "anything/anything-config" "\
Go to precedent file in anything grep/etags buffers.

\(fn)" t nil)

(autoload 'anything-c-goto-next-file "anything/anything-config" "\
Go to precedent file in anything grep/etags buffers.

\(fn)" t nil)

(autoload 'anything-c-grep-run-persistent-action "anything/anything-config" "\
Run grep persistent action from `anything-do-grep-1'.

\(fn)" t nil)

(autoload 'anything-c-grep-run-default-action "anything/anything-config" "\
Run grep default action from `anything-do-grep-1'.

\(fn)" t nil)

(autoload 'anything-c-grep-run-other-window-action "anything/anything-config" "\
Run grep goto other window action from `anything-do-grep-1'.

\(fn)" t nil)

(autoload 'anything-c-grep-run-save-buffer "anything/anything-config" "\
Run grep save results action from `anything-do-grep-1'.

\(fn)" t nil)

(autoload 'anything-yank-text-at-point "anything/anything-config" "\
Yank text at point in minibuffer.

\(fn)" t nil)

(autoload 'anything-c-bookmark-run-jump-other-window "anything/anything-config" "\
Jump to bookmark from keyboard.

\(fn)" t nil)

(autoload 'anything-c-bookmark-run-delete "anything/anything-config" "\
Delete bookmark from keyboard.

\(fn)" t nil)

(autoload 'anything-c-bmkext-run-edit "anything/anything-config" "\
Run `bmkext-edit-bookmark' from keyboard.

\(fn)" t nil)

(autoload 'anything-yaoddmuse-cache-pages "anything/anything-config" "\
Fetch the list of files on emacswiki and create cache file.
If load is non--nil load the file and feed `yaoddmuse-pages-hash'.

\(fn &optional LOAD)" t nil)

(defvar anything-completion-mode nil "\
Non-nil if Anything-Completion mode is enabled.
See the command `anything-completion-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `anything-completion-mode'.")

(custom-autoload 'anything-completion-mode "anything/anything-config" nil)

(autoload 'anything-completion-mode "anything/anything-config" "\
Toggle generic anything completion.

All functions in Emacs that use `completing-read'
or `read-file-name' and friends will use anything interface
when this mode is turned on.
However you can modify this behavior for functions of your choice
with `anything-completing-read-handlers-alist'.

Called with a positive arg, turn on unconditionally, with a
negative arg turn off.
You can turn it on with `ac-mode'.

Some crap emacs functions may not be supported,
e.g `ffap-alternate-file' and maybe others
You can add such functions to `anything-completing-read-handlers-alist'
with a nil value.

Note: This mode will work only partially on Emacs23.

\(fn &optional ARG)" t nil)

(autoload 'anything-lisp-completion-at-point "anything/anything-config" "\
Anything lisp symbol completion at point.

\(fn)" t nil)

(autoload 'anything-c-complete-file-name-at-point "anything/anything-config" "\
Complete file name at point.

\(fn)" t nil)

(autoload 'anything-lisp-completion-at-point-or-indent "anything/anything-config" "\
First call indent and second call complete lisp symbol.
The second call should happen before `anything-lisp-completion-or-indent-delay',
after this delay, next call will indent again.
After completion, next call is always indent.
See that like click and double mouse click.
One hit indent, two quick hits maybe indent and complete.

\(fn ARG)" t nil)

(autoload 'anything-lisp-completion-or-file-name-at-point "anything/anything-config" "\
Complete lisp symbol or filename at point.
Filename completion happen if filename is started in
or between double quotes.

\(fn)" t nil)

(autoload 'anything-w32-shell-execute-open-file "anything/anything-config" "\


\(fn FILE)" t nil)

(autoload 'anything-c-call-interactively "anything/anything-config" "\
Execute CMD-OR-NAME as Emacs command.
It is added to `extended-command-history'.
`anything-current-prefix-arg' is used as the command's prefix argument.

\(fn CMD-OR-NAME)" nil nil)

(autoload 'anything-c-set-variable "anything/anything-config" "\
Set value to VAR interactively.

\(fn VAR)" t nil)

(autoload 'anything-c-reset-adaptative-history "anything/anything-config" "\
Delete all `anything-c-adaptive-history' and his file.
Useful when you have a old or corrupted `anything-c-adaptive-history-file'.

\(fn)" t nil)

(autoload 'anything-mini "anything/anything-config" "\
Preconfigured `anything' lightweight version (buffer -> recentf).

\(fn)" t nil)

(autoload 'anything-for-files "anything/anything-config" "\
Preconfigured `anything' for opening files.
ffap -> recentf -> buffer -> bookmark -> file-cache -> files-in-current-dir -> locate.

\(fn)" t nil)

(autoload 'anything-recentf "anything/anything-config" "\
Preconfigured `anything' for `recentf'.

\(fn)" t nil)

(autoload 'anything-info-at-point "anything/anything-config" "\
Preconfigured `anything' for searching info at point.
With a prefix-arg insert symbol at point.

\(fn ARG)" t nil)

(autoload 'anything-show-kill-ring "anything/anything-config" "\
Preconfigured `anything' for `kill-ring'.
It is drop-in replacement of `yank-pop'.
You may bind this command to M-y.
First call open the kill-ring browser, next calls move to next line.

\(fn)" t nil)

(autoload 'anything-minibuffer-history "anything/anything-config" "\
Preconfigured `anything' for `minibuffer-history'.

\(fn)" t nil)

(autoload 'anything-gentoo "anything/anything-config" "\
Preconfigured `anything' for gentoo linux.

\(fn)" t nil)

(autoload 'anything-imenu "anything/anything-config" "\
Preconfigured `anything' for `imenu'.

\(fn)" t nil)

(autoload 'anything-google-suggest "anything/anything-config" "\
Preconfigured `anything' for google search with google suggest.

\(fn)" t nil)

(autoload 'anything-yahoo-suggest "anything/anything-config" "\
Preconfigured `anything' for Yahoo searching with Yahoo suggest.

\(fn)" t nil)

(autoload 'anything-for-buffers "anything/anything-config" "\
Preconfigured `anything' for buffers.

\(fn)" t nil)

(autoload 'anything-buffers-list "anything/anything-config" "\
Preconfigured `anything' to list buffers.
It is an enhanced version of `anything-for-buffers'.

\(fn)" t nil)

(autoload 'anything-bbdb "anything/anything-config" "\
Preconfigured `anything' for BBDB.

Needs BBDB.

http://bbdb.sourceforge.net/

\(fn)" t nil)

(autoload 'anything-locate "anything/anything-config" "\
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

(autoload 'anything-w3m-bookmarks "anything/anything-config" "\
Preconfigured `anything' for w3m bookmark.

Needs w3m and emacs-w3m.

http://w3m.sourceforge.net/
http://emacs-w3m.namazu.org/

\(fn)" t nil)

(autoload 'anything-firefox-bookmarks "anything/anything-config" "\
Preconfigured `anything' for firefox bookmark.
You will have to enable html bookmarks in firefox:
open about:config in firefox and double click on this line to enable value to true:

user_pref(\"browser.bookmarks.autoExportHTML\", false);

You should have now:

user_pref(\"browser.bookmarks.autoExportHTML\", true);

After closing firefox, you will be able to browse you bookmarks.

\(fn)" t nil)

(autoload 'anything-colors "anything/anything-config" "\
Preconfigured `anything' for color.

\(fn)" t nil)

(autoload 'anything-bookmarks "anything/anything-config" "\
Preconfigured `anything' for bookmarks.

\(fn)" t nil)

(autoload 'anything-c-pp-bookmarks "anything/anything-config" "\
Preconfigured `anything' for bookmarks (pretty-printed).

\(fn)" t nil)

(autoload 'anything-c-insert-latex-math "anything/anything-config" "\
Preconfigured anything for latex math symbols completion.

\(fn)" t nil)

(autoload 'anything-register "anything/anything-config" "\
Preconfigured `anything' for Emacs registers.

\(fn)" t nil)

(autoload 'anything-man-woman "anything/anything-config" "\
Preconfigured `anything' for Man and Woman pages.

\(fn)" t nil)

(autoload 'anything-org-keywords "anything/anything-config" "\
Preconfigured `anything' for org keywords.

\(fn)" t nil)

(autoload 'anything-emms "anything/anything-config" "\
Preconfigured `anything' for emms sources.

\(fn)" t nil)

(autoload 'anything-eev-anchors "anything/anything-config" "\
Preconfigured `anything' for eev anchors.

\(fn)" t nil)

(autoload 'anything-bm-list "anything/anything-config" "\
Preconfigured `anything' for visible bookmarks.

Needs bm.el

http://cvs.savannah.gnu.org/viewvc/*checkout*/bm/bm/bm.el

\(fn)" t nil)

(autoload 'anything-timers "anything/anything-config" "\
Preconfigured `anything' for timers.

\(fn)" t nil)

(autoload 'anything-list-emacs-process "anything/anything-config" "\
Preconfigured `anything' for emacs process.

\(fn)" t nil)

(autoload 'anything-occur "anything/anything-config" "\
Preconfigured Anything for Occur source.
If region is active, search only in region,
otherwise search in whole buffer.

\(fn)" t nil)

(autoload 'anything-browse-code "anything/anything-config" "\
Preconfigured anything to browse code.

\(fn)" t nil)

(autoload 'anything-org-headlines "anything/anything-config" "\
Preconfigured anything to show org headlines.

\(fn)" t nil)

(autoload 'anything-regexp "anything/anything-config" "\
Preconfigured anything to build regexps.
`query-replace-regexp' can be run from there against found regexp.

\(fn)" t nil)

(autoload 'anything-c-copy-files-async "anything/anything-config" "\
Preconfigured anything to copy file list FLIST to DEST asynchronously.

\(fn)" t nil)

(autoload 'anything-find-files "anything/anything-config" "\
Preconfigured `anything' for anything implementation of `find-file'.
Called with a prefix arg show history if some.
Don't call it from programs, use `anything-find-files-1' instead.
This is the starting point for nearly all actions you can do on files.

\(fn ARG)" t nil)

(autoload 'anything-write-file "anything/anything-config" "\
Preconfigured `anything' providing completion for `write-file'.

\(fn)" t nil)

(autoload 'anything-insert-file "anything/anything-config" "\
Preconfigured `anything' providing completion for `insert-file'.

\(fn)" t nil)

(autoload 'anything-dired-rename-file "anything/anything-config" "\
Preconfigured `anything' to rename files from dired.

\(fn)" t nil)

(autoload 'anything-dired-copy-file "anything/anything-config" "\
Preconfigured `anything' to copy files from dired.

\(fn)" t nil)

(autoload 'anything-dired-symlink-file "anything/anything-config" "\
Preconfigured `anything' to symlink files from dired.

\(fn)" t nil)

(autoload 'anything-dired-hardlink-file "anything/anything-config" "\
Preconfigured `anything' to hardlink files from dired.

\(fn)" t nil)

(autoload 'anything-do-grep "anything/anything-config" "\
Preconfigured anything for grep.
Contrarily to Emacs `grep' no default directory is given, but
the full path of candidates in ONLY.
That allow to grep different files not only in `default-directory' but anywhere
by marking them (C-<SPACE>). If one or more directory is selected
grep will search in all files of these directories.
You can use also wildcard in the base name of candidate.
If a prefix arg is given use the -r option of grep.
The prefix arg can be passed before or after start.
See also `anything-do-grep-1'.

\(fn)" t nil)

(autoload 'anything-do-zgrep "anything/anything-config" "\
Preconfigured anything for zgrep.

\(fn)" t nil)

(autoload 'anything-do-pdfgrep "anything/anything-config" "\
Preconfigured anything for pdfgrep.

\(fn)" t nil)

(autoload 'anything-c-etags-select "anything/anything-config" "\
Preconfigured anything for etags.
Called with one prefix arg use symbol at point as initial input.
Called with two prefix arg reinitialize cache.
If tag file have been modified reinitialize cache.

\(fn ARG)" t nil)

(autoload 'anything-filelist "anything/anything-config" "\
Preconfigured `anything' to open files instantly.

See `anything-c-filelist-file-name' docstring for usage.

\(fn)" t nil)

(autoload 'anything-filelist+ "anything/anything-config" "\
Preconfigured `anything' to open files/buffers/bookmarks instantly.

This is a replacement for `anything-for-files'.
See `anything-c-filelist-file-name' docstring for usage.

\(fn)" t nil)

(autoload 'anything-M-x "anything/anything-config" "\
Preconfigured `anything' for Emacs commands.
It is `anything' replacement of regular `M-x' `execute-extended-command'.

\(fn)" t nil)

(autoload 'anything-manage-advice "anything/anything-config" "\
Preconfigured `anything' to disable/enable function advices.

\(fn)" t nil)

(autoload 'anything-bookmark-ext "anything/anything-config" "\
Preconfigured `anything' for bookmark-extensions sources.
Needs bookmark-ext.el:
<http://mercurial.intuxication.org/hg/emacs-bookmark-extension>.
Contain also `anything-c-source-google-suggest'.

\(fn)" t nil)

(autoload 'anything-simple-call-tree "anything/anything-config" "\
Preconfigured `anything' for simple-call-tree. List function relationships.

Needs simple-call-tree.el.
http://www.emacswiki.org/cgi-bin/wiki/download/simple-call-tree.el

\(fn)" t nil)

(autoload 'anything-mark-ring "anything/anything-config" "\
Preconfigured `anything' for `anything-c-source-mark-ring'.

\(fn)" t nil)

(autoload 'anything-global-mark-ring "anything/anything-config" "\
Preconfigured `anything' for `anything-c-source-global-mark-ring'.

\(fn)" t nil)

(autoload 'anything-all-mark-rings "anything/anything-config" "\
Preconfigured `anything' for `anything-c-source-global-mark-ring' and `anything-c-source-mark-ring'.

\(fn)" t nil)

(autoload 'anything-yaoddmuse-emacswiki-edit-or-view "anything/anything-config" "\
Preconfigured `anything' to edit or view EmacsWiki page.

Needs yaoddmuse.el.

http://www.emacswiki.org/emacs/download/yaoddmuse.el

\(fn)" t nil)

(autoload 'anything-yaoddmuse-emacswiki-post-library "anything/anything-config" "\
Preconfigured `anything' to post library to EmacsWiki.

Needs yaoddmuse.el.

http://www.emacswiki.org/emacs/download/yaoddmuse.el

\(fn)" t nil)

(autoload 'anything-eval-expression "anything/anything-config" "\
Preconfigured anything for `anything-c-source-evaluation-result'.

\(fn ARG)" t nil)

(autoload 'anything-eval-expression-with-eldoc "anything/anything-config" "\
Preconfigured anything for `anything-c-source-evaluation-result' with `eldoc' support.

\(fn)" t nil)

(autoload 'anything-calcul-expression "anything/anything-config" "\
Preconfigured anything for `anything-c-source-calculation-result'.

\(fn)" t nil)

(autoload 'anything-surfraw "anything/anything-config" "\
Preconfigured `anything' to search PATTERN with search ENGINE.

\(fn PATTERN ENGINE)" t nil)

(autoload 'anything-call-source "anything/anything-config" "\
Preconfigured `anything' to call anything source.

\(fn)" t nil)

(autoload 'anything-execute-anything-command "anything/anything-config" "\
Preconfigured `anything' to execute preconfigured `anything'.

\(fn)" t nil)

(autoload 'anything-create "anything/anything-config" "\
Preconfigured `anything' to do many create actions from STRING.
See also `anything-create--actions'.

\(fn &optional STRING INITIAL-INPUT)" t nil)

(autoload 'anything-top "anything/anything-config" "\
Preconfigured `anything' for top command.

\(fn)" t nil)

(autoload 'anything-select-xfont "anything/anything-config" "\
Preconfigured `anything' to select Xfont.

\(fn)" t nil)

(autoload 'anything-world-time "anything/anything-config" "\
Preconfigured `anything' to show world time.

\(fn)" t nil)

(autoload 'anything-apt "anything/anything-config" "\
Preconfigured `anything' : frontend of APT package manager.
With a prefix arg reload cache.

\(fn ARG)" t nil)

(autoload 'anything-esh-pcomplete "anything/anything-config" "\
Preconfigured anything to provide anything completion in eshell.

\(fn)" t nil)

(autoload 'anything-eshell-history "anything/anything-config" "\
Preconfigured anything for eshell history.

\(fn)" t nil)

(autoload 'anything-c-run-external-command "anything/anything-config" "\
Preconfigured `anything' to run External PROGRAM asyncronously from Emacs.
If program is already running exit with error.
You can set your own list of commands with
`anything-c-external-commands-list'.

\(fn PROGRAM)" t nil)

(autoload 'anything-ratpoison-commands "anything/anything-config" "\
Preconfigured `anything' to execute ratpoison commands.

\(fn)" t nil)

(autoload 'anything-ucs "anything/anything-config" "\
Preconfigured anything for `ucs-names' math symbols.

\(fn)" t nil)

(autoload 'anything-c-apropos "anything/anything-config" "\
Preconfigured anything to describe commands, functions, variables and faces.

\(fn)" t nil)

(autoload 'anything-xrandr-set "anything/anything-config" "\


\(fn)" t nil)

;;;***

;;;### (autoloads (anything-mp-toggle-match-plugin) "anything/anything-match-plugin"
;;;;;;  "anything/anything-match-plugin.el" (20369 33698))
;;; Generated autoloads from anything/anything-match-plugin.el

(autoload 'anything-mp-toggle-match-plugin "anything/anything-match-plugin" "\
Turn on/off multiple regexp matching in anything.
i.e anything-match-plugin.

\(fn)" t nil)

;;;***

;;;### (autoloads (ascii-off ascii-on ascii-display ascii-customize)
;;;;;;  "ascii/ascii" "ascii/ascii.el" (20403 29796))
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

;;;### (autoloads (bash-completion-reset bash-completion-dynamic-complete
;;;;;;  bash-completion-setup) "bash-completion/bash-completion"
;;;;;;  "bash-completion/bash-completion.el" (20403 29114))
;;; Generated autoloads from bash-completion/bash-completion.el

(autoload 'bash-completion-setup "bash-completion/bash-completion" "\
Register bash completion for the shell buffer and shell command line.

This function adds `bash-completion-dynamic-complete' to the completion
function list of shell mode, `shell-dynamic-complete-functions' and to the
completion function list of shell-command, `shell-command-complete-functions'.

This function is convenient, but it might not be the best way of enabling
bash completion in your .emacs file because it forces you to load the module
before it is needed. For an autoload version, add:

\(autoload 'bash-completion-dynamic-complete \"bash-completion\"
\"BASH completion hook\")
\(add-hook 'shell-dynamic-complete-functions
'bash-completion-dynamic-complete)
\(add-hook 'shell-command-complete-functions
'bash-completion-dynamic-complete))

\(fn)" nil nil)

(autoload 'bash-completion-dynamic-complete "bash-completion/bash-completion" "\
Complete word at cursor using BASH completion.

This function is meant to be added into
`shell-dynamic-complete-functions' or
`shell-command-complete-functions'.  It uses `comint' to figure
out what the current command is and calls
`comint-dynamic-simple-complete' to do the completion.

If a match was found, it is displayed as is usual for comint
completion.  Return nil if no match was found.

\(fn)" nil nil)

(autoload 'bash-completion-reset "bash-completion/bash-completion" "\
Force the next completion command to start with a fresh BASH process.

This function kills any existing BASH completion process.  This way, the
next time BASH completion is requested, a new process will be created with
the latest configuration.

Call this method if you have updated your .bashrc or any bash init scripts
and would like bash completion in Emacs to take these changes into account.

\(fn)" t nil)

;;;***

;;;### (autoloads (etags-select-find-tag etags-select-find-tag-at-point
;;;;;;  etags-select-go-if-unambiguous etags-select-use-short-name-completion
;;;;;;  etags-select-highlight-delay etags-select-highlight-tag-after-jump
;;;;;;  etags-select-mode-hook etags-select-no-select-for-one-match
;;;;;;  etags-select-mode) "etags-select/etags-select" "etags-select/etags-select.el"
;;;;;;  (20403 49224))
;;; Generated autoloads from etags-select/etags-select.el

(let ((loads (get 'etags-select-mode 'custom-loads))) (if (member '"etags-select/etags-select" loads) nil (put 'etags-select-mode 'custom-loads (cons '"etags-select/etags-select" loads))))

(defvar etags-select-no-select-for-one-match t "\
*If non-nil, don't open the selection window if there is only one
matching tag.")

(custom-autoload 'etags-select-no-select-for-one-match "etags-select/etags-select" t)

(defvar etags-select-mode-hook nil "\
*List of functions to call on entry to etags-select-mode mode.")

(custom-autoload 'etags-select-mode-hook "etags-select/etags-select" t)

(defvar etags-select-highlight-tag-after-jump t "\
*If non-nil, temporarily highlight the tag after you jump to it.")

(custom-autoload 'etags-select-highlight-tag-after-jump "etags-select/etags-select" t)

(defvar etags-select-highlight-delay 1.0 "\
*How long to highlight the tag.")

(custom-autoload 'etags-select-highlight-delay "etags-select/etags-select" t)

(defface etags-select-highlight-tag-face '((t (:foreground "white" :background "cadetblue4" :bold t))) "\
Font Lock mode face used to highlight tags." :group (quote etags-select-mode))

(defvar etags-select-use-short-name-completion nil "\
*Use short tag names during completion.  For example, say you
have a function named foobar in several classes and you invoke
`etags-select-find-tag'.  If this variable is nil, you would have
to type ClassA::foo<TAB> to start completion.  Since avoiding
knowing which class a function is in is the basic idea of this
package, if you set this to t you can just type foo<TAB>.

Only works with GNU Emacs.")

(custom-autoload 'etags-select-use-short-name-completion "etags-select/etags-select" t)

(defvar etags-select-go-if-unambiguous nil "\
*If non-nil, jump by tag number if it is unambiguous.")

(custom-autoload 'etags-select-go-if-unambiguous "etags-select/etags-select" t)

(autoload 'etags-select-find-tag-at-point "etags-select/etags-select" "\
Do a find-tag-at-point, and display all exact matches.  If only one match is
found, see the `etags-select-no-select-for-one-match' variable to decide what
to do.

\(fn)" t nil)

(autoload 'etags-select-find-tag "etags-select/etags-select" "\
Do a find-tag, and display all exact matches.  If only one match is
found, see the `etags-select-no-select-for-one-match' variable to decide what
to do.

\(fn)" t nil)

;;;***

;;;### (autoloads (etags-table-search-up-depth etags-table-alist
;;;;;;  etags-table) "etags-table/etags-table" "etags-table/etags-table.el"
;;;;;;  (20403 49224))
;;; Generated autoloads from etags-table/etags-table.el

(let ((loads (get 'etags-table 'custom-loads))) (if (member '"etags-table/etags-table" loads) nil (put 'etags-table 'custom-loads (cons '"etags-table/etags-table" loads))))

(defvar etags-table-alist nil "\
*Map filename to tag file(s)

Example:

\(setq etags-table-alist
      (list
       '(\"/home/me/Projects/foo/.*\\\\.[ch]$\" \"/home/me/Projects/lib1/TAGS\" \"/home/me/Projects/lib2/TAGS\")
       '(\"/home/me/Projects/bar/.*\\\\.py$\" \"/home/me/Projects/python/common/TAGS\")
       '(\".*\\\\.[ch]$\" \"/usr/local/include/TAGS\")
       ))

A file named, for example, \"/home/me/Projects/foo/main.c\" would set the
`tags-table-list' to a list of:

\"/home/me/Projects/lib1/TAGS\"
\"/home/me/Projects/lib2/TAGS\"
\"/usr/local/include/TAGS\"

and possibly a local tags file at the head of the list if `etags-table-search-up-depth'
is non-nil.  You can use \\&, \\1, etc. in the tag file names to substitute pieces
captured with \\(\\) in the key.
")

(custom-autoload 'etags-table-alist "etags-table/etags-table" t)

(defvar etags-table-search-up-depth nil "\
*Max depth to search up for a tags file.  nil means don't search.")

(custom-autoload 'etags-table-search-up-depth "etags-table/etags-table" t)

;;;***

;;;### (autoloads (fastnav-sprint-backward fastnav-sprint-forward
;;;;;;  fastnav-delete-char-backward fastnav-delete-char-forward
;;;;;;  fastnav-execute-at-char-backward fastnav-execute-at-char-forward
;;;;;;  fastnav-insert-at-char-backward fastnav-insert-at-char-forward
;;;;;;  fastnav-replace-char-backward fastnav-replace-char-forward
;;;;;;  fastnav-zap-to-char-backward fastnav-zap-to-char-forward
;;;;;;  fastnav-zap-up-to-char-backward fastnav-zap-up-to-char-forward
;;;;;;  fastnav-mark-to-char-backward fastnav-mark-to-char-forward
;;;;;;  fastnav-mark-up-to-char-xbackward fastnav-mark-up-to-char-forward
;;;;;;  fastnav-jump-to-char-backward fastnav-jump-to-char-forward
;;;;;;  fastnav-highlight-read-char-backward fastnav-highlight-read-char
;;;;;;  fastnav-get-nth-chars fastnav-search-char-backward fastnav-search-char-forward)
;;;;;;  "fastnav/fastnav" "fastnav/fastnav.el" (20403 51680))
;;; Generated autoloads from fastnav/fastnav.el

(autoload 'fastnav-search-char-forward "fastnav/fastnav" "\
Moves to the arg-th occurrence of char forward (backward if N
is negative).  If there isn't room, go as far as possible (no
error).

\(fn ARG CHAR)" nil nil)

(autoload 'fastnav-search-char-backward "fastnav/fastnav" "\
Moves to the arg-th occurrence of char backward (forward if N
is negative).  If there isn't room, go as far as possible (no
error).

\(fn ARG CHAR)" nil nil)

(autoload 'fastnav-get-nth-chars "fastnav/fastnav" "\
Computes and returns the positions of the ARG'th occurrence of
characters of the range 1 .. 255.

\(fn ARG)" nil nil)

(autoload 'fastnav-highlight-read-char "fastnav/fastnav" "\
Highlights the ARG'th occurences of each character while
querying one using message TEXT. Negative ARG means backward
search of occurences.

\(fn TEXT ARG FORWARDER BACKWARDER)" nil nil)

(autoload 'fastnav-highlight-read-char-backward "fastnav/fastnav" "\
Highlights the backward ARG'th occurences of each character
while querying one using message TEXT.

\(fn TEXT ARG FORWARDER BACKWARDER)" nil nil)

(autoload 'fastnav-jump-to-char-forward "fastnav/fastnav" "\
Jump to the ARG'th occurence of a character that is queried
interactively while highlighting the possible positions.

\(fn ARG)" t nil)

(autoload 'fastnav-jump-to-char-backward "fastnav/fastnav" "\
Jump backward to the ARG'th occurence of a character that is
queried interactively while highlighting the possible positions.

\(fn ARG)" t nil)

(autoload 'fastnav-mark-up-to-char-forward "fastnav/fastnav" "\
Set mark before the ARG'th occurence of a character queried
interactively.

\(fn ARG)" t nil)

(autoload 'fastnav-mark-up-to-char-xbackward "fastnav/fastnav" "\
Set mark backward after the ARG'th occurence of a character
queried interactively.

\(fn ARG)" t nil)

(autoload 'fastnav-mark-to-char-forward "fastnav/fastnav" "\
Set mark before the ARG'th occurence of a character queried
interactively.

\(fn ARG)" t nil)

(autoload 'fastnav-mark-to-char-backward "fastnav/fastnav" "\
Set mark backward after the ARG'th occurence of a character
queried interactively.

\(fn ARG)" t nil)

(autoload 'fastnav-zap-up-to-char-forward "fastnav/fastnav" "\
Kill text up to the ARG'th occurence of a character queried
interactively.

\(fn ARG)" t nil)

(autoload 'fastnav-zap-up-to-char-backward "fastnav/fastnav" "\
Kill text backward to the ARG'th occurence of a character
queried interactively.

\(fn ARG)" t nil)

(autoload 'fastnav-zap-to-char-forward "fastnav/fastnav" "\
Kill text up to and including the ARG'th occurence of a character queried
interactively.

\(fn ARG)" t nil)

(autoload 'fastnav-zap-to-char-backward "fastnav/fastnav" "\
Kill text backward to the ARG'th occurence of a character
queried interactively.

\(fn ARG)" t nil)

(autoload 'fastnav-replace-char-forward "fastnav/fastnav" "\
Interactively replaces the ARG'th occurence of a character.

\(fn ARG)" t nil)

(autoload 'fastnav-replace-char-backward "fastnav/fastnav" "\
Interactively replaces the ARG'th backward occurence of a
character.

\(fn ARG)" t nil)

(autoload 'fastnav-insert-at-char-forward "fastnav/fastnav" "\
Queries for a character and a string that is insterted at
the ARG'th occurence of the character.

\(fn ARG)" t nil)

(autoload 'fastnav-insert-at-char-backward "fastnav/fastnav" "\
Queries for a character and a string that is insterted at
the backward ARG'th occurence of the character.

\(fn ARG)" t nil)

(autoload 'fastnav-execute-at-char-forward "fastnav/fastnav" "\
Queries for a character and a key sequence that is executed at
the ARG'th occurence of the character.

\(fn ARG)" t nil)

(autoload 'fastnav-execute-at-char-backward "fastnav/fastnav" "\
Queries for a character and a key sequence that is executed at
the backward ARG'th occurence of the character.

\(fn ARG)" t nil)

(autoload 'fastnav-delete-char-forward "fastnav/fastnav" "\
Deletes the ARG'th occurence of a character, which is queried
interactively while highlighting the possible positions.

\(fn ARG)" t nil)

(autoload 'fastnav-delete-char-backward "fastnav/fastnav" "\
Deletes the backward ARG'th occurence of a character, which is
queried interactively while highlighting the possible positions.

\(fn ARG)" t nil)

(autoload 'fastnav-sprint-forward "fastnav/fastnav" "\
Performs a sequence of jumping forward to the next character
matching the keyboard event.

\(fn ARG)" t nil)

(autoload 'fastnav-sprint-backward "fastnav/fastnav" "\
Performs a sequence of jumping backward to the next character
matching the keyboard event.

\(fn ARG)" t nil)

;;;***

;;;### (autoloads (ctags-update-minor-mode ctags-update) "helm-etags-plus/ctags-update"
;;;;;;  "helm-etags-plus/ctags-update.el" (20390 14825))
;;; Generated autoloads from helm-etags-plus/ctags-update.el

(autoload 'ctags-update "helm-etags-plus/ctags-update" "\
update TAGS in parent directory using `exuberant-ctags' you
can call this function directly , or enable
`ctags-update-minor-mode' or with prefix `C-u' then you can
generate a new TAGS file in directory

\(fn &optional ARGS)" t nil)

(autoload 'ctags-update-minor-mode "helm-etags-plus/ctags-update" "\
auto update TAGS using `exuberant-ctags' in parent directory.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (helm-etags+-history helm-etags+-history-go-forward
;;;;;;  helm-etags+-history-go-back helm-etags+-select-one-key helm-etags+-select-at-point
;;;;;;  helm-etags+-select) "helm-etags-plus/helm-etags+" "helm-etags-plus/helm-etags+.el"
;;;;;;  (20390 14825))
;;; Generated autoloads from helm-etags-plus/helm-etags+.el

(autoload 'helm-etags+-select "helm-etags-plus/helm-etags+" "\
Tag jump using etags and `helm'.
If SYMBOL-NAME is non-nil, jump tag position with SYMBOL-NAME.

\(fn)" t nil)

(autoload 'helm-etags+-select-at-point "helm-etags-plus/helm-etags+" "\
Tag jump with current symbol using etags and `helm'.

\(fn)" t nil)

(autoload 'helm-etags+-select-one-key "helm-etags-plus/helm-etags+" "\
you can bind this to `M-.'

\(fn &optional ARGS)" t nil)

(defvar helm-c-source-etags+-select '((name . "Etags+") (init . helm-etags+-get-available-tag-table-buffers) (candidates . helm-etags+-get-candidates-with-cache-support) (volatile) (pattern-transformer (lambda (helm-pattern) (setq helm-etags+-untransformed-helm-pattern helm-pattern) (regexp-quote (replace-regexp-in-string "\\\\_<\\|\\\\_>" "" helm-pattern)))) (requires-pattern . 3) (delayed) (action ("Goto the location" . helm-c-etags+-goto-location))))

(autoload 'helm-etags+-history-go-back "helm-etags-plus/helm-etags+" "\
Go Back.

\(fn)" t nil)

(autoload 'helm-etags+-history-go-forward "helm-etags-plus/helm-etags+" "\
Go Forward.

\(fn)" t nil)

(autoload 'helm-etags+-history "helm-etags-plus/helm-etags+" "\
show all tag historys using `helm'

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-send-bug-report-from-helm helm-send-bug-report
;;;;;;  helm-follow-mode helm-kill-selection-and-quit helm-yank-selection
;;;;;;  helm-prev-visible-mark helm-next-visible-mark helm-display-all-visible-marks
;;;;;;  helm-toggle-all-marks helm-unmark-all helm-mark-all helm-toggle-visible-mark
;;;;;;  helm-scroll-other-window-down helm-scroll-other-window helm-execute-persistent-action
;;;;;;  helm-select-2nd-action-or-end-of-line helm-select-4th-action
;;;;;;  helm-select-3rd-action helm-select-2nd-action helm-enlarge-window
;;;;;;  helm-narrow-window helm-toggle-resplit-window helm-delete-minibuffer-contents
;;;;;;  helm-delete-current-selection helm-debug-output helm-keyboard-quit
;;;;;;  helm-exit-minibuffer helm-confirm-and-exit-minibuffer helm-next-source
;;;;;;  helm-previous-source helm-end-of-buffer helm-beginning-of-buffer
;;;;;;  helm-next-page helm-previous-page helm-next-line helm-previous-line
;;;;;;  helm-select-action helm-force-update helm-other-buffer helm-resume
;;;;;;  helm-open-last-log) "helm/helm" "helm/helm.el" (20390 12785))
;;; Generated autoloads from helm/helm.el

(autoload 'helm-open-last-log "helm/helm" "\
Open helm log file of last helm session.
If `helm-last-log-file' is nil, switch to \"*Helm Log*\" buffer.

\(fn)" t nil)

(autoload 'helm-resume "helm/helm" "\
Resurrect previously invoked `helm'.
Called with a prefix arg, allow choosing among all existing
helm buffers.  i.e choose among various helm sessions.

\(fn ARG)" t nil)

(autoload 'helm-other-buffer "helm/helm" "\
Simplified interface of `helm' with other `helm-buffer'.
Call `helm' with only ANY-SOURCES and ANY-BUFFER as args.

\(fn ANY-SOURCES ANY-BUFFER)" nil nil)

(autoload 'helm-force-update "helm/helm" "\
Force recalculation and update of candidates.
If arg PRESELECT, a candidate to preselect, is provided,
It will be preselected by `helm-update', otherwise the current candidate
will be preselected is available.
If current source has `update' attribute, a function without argument,
call it before update.

\(fn &optional PRESELECT)" t nil)

(autoload 'helm-select-action "helm/helm" "\
Select an action for the currently selected candidate.
If action buffer is selected, back to the helm buffer.

\(fn)" t nil)

(autoload 'helm-previous-line "helm/helm" "\
Move selection to the previous line.

\(fn)" t nil)

(autoload 'helm-next-line "helm/helm" "\
Move selection to the next line.

\(fn)" t nil)

(autoload 'helm-previous-page "helm/helm" "\
Move selection back with a pageful.

\(fn)" t nil)

(autoload 'helm-next-page "helm/helm" "\
Move selection forward with a pageful.

\(fn)" t nil)

(autoload 'helm-beginning-of-buffer "helm/helm" "\
Move selection at the top.

\(fn)" t nil)

(autoload 'helm-end-of-buffer "helm/helm" "\
Move selection at the bottom.

\(fn)" t nil)

(autoload 'helm-previous-source "helm/helm" "\
Move selection to the previous source.

\(fn)" t nil)

(autoload 'helm-next-source "helm/helm" "\
Move selection to the next source.

\(fn)" t nil)

(autoload 'helm-confirm-and-exit-minibuffer "helm/helm" "\
Maybe ask for confirmation when exiting helm.
It is similar to `minibuffer-complete-and-exit' adapted to helm.
If `minibuffer-completion-confirm' value is 'confirm,
send in minibuffer confirm message and exit on next hit.
If `minibuffer-completion-confirm' value is t,
don't exit and send message 'no match'.

\(fn)" t nil)

(autoload 'helm-exit-minibuffer "helm/helm" "\
Select the current candidate by exiting the minibuffer.

\(fn)" t nil)

(autoload 'helm-keyboard-quit "helm/helm" "\
Quit minibuffer in helm.
If action buffer is displayed, kill it.

\(fn)" t nil)

(autoload 'helm-debug-output "helm/helm" "\
Show all helm-related variables at this time.

\(fn)" t nil)

(autoload 'helm-delete-current-selection "helm/helm" "\
Delete the currently selected item.

\(fn)" t nil)

(autoload 'helm-delete-minibuffer-contents "helm/helm" "\
Same as `delete-minibuffer-contents' but this is a command.

\(fn)" t nil)

(autoload 'helm-toggle-resplit-window "helm/helm" "\
Toggle resplit helm window, vertically or horizontally.

\(fn)" t nil)

(autoload 'helm-narrow-window "helm/helm" "\
Narrow helm window.

\(fn)" t nil)

(autoload 'helm-enlarge-window "helm/helm" "\
Enlarge helm window.

\(fn)" t nil)

(autoload 'helm-select-2nd-action "helm/helm" "\
Select the 2nd action for the currently selected candidate.

\(fn)" t nil)

(autoload 'helm-select-3rd-action "helm/helm" "\
Select the 3rd action for the currently selected candidate.

\(fn)" t nil)

(autoload 'helm-select-4th-action "helm/helm" "\
Select the 4th action for the currently selected candidate.

\(fn)" t nil)

(autoload 'helm-select-2nd-action-or-end-of-line "helm/helm" "\
Select the 2nd action for the currently selected candidate.
This happen when point is at the end of minibuffer.
Otherwise goto the end of minibuffer.

\(fn)" t nil)

(autoload 'helm-execute-persistent-action "helm/helm" "\
Perform the associated action ATTR without quitting helm.
ATTR default is 'persistent-action', but it can be helm else.
In this case you have to add this new attribute to your source.
When `helm-samewindow' and ONEWINDOW are non--nil,
the helm window is never split in persistent action.

\(fn &optional (attr (quote persistent-action)) ONEWINDOW)" t nil)

(autoload 'helm-scroll-other-window "helm/helm" "\
Scroll other window (not *Helm* window) upward.

\(fn)" t nil)

(autoload 'helm-scroll-other-window-down "helm/helm" "\
Scroll other window (not *Helm* window) downward.

\(fn)" t nil)

(autoload 'helm-toggle-visible-mark "helm/helm" "\
Toggle helm visible mark at point.

\(fn)" t nil)

(autoload 'helm-mark-all "helm/helm" "\
Mark all visible unmarked candidates in current source.

\(fn)" t nil)

(autoload 'helm-unmark-all "helm/helm" "\
Unmark all candidates in all sources of current helm session.

\(fn)" t nil)

(autoload 'helm-toggle-all-marks "helm/helm" "\
Toggle all marks.
Mark all visible candidates of current source or unmark all candidates
visible or invisible in all sources of current helm session

\(fn)" t nil)

(autoload 'helm-display-all-visible-marks "helm/helm" "\
Show all `helm' visible marks strings.
Only useful for debugging.

\(fn)" t nil)

(autoload 'helm-next-visible-mark "helm/helm" "\
Move next helm visible mark.
If PREV is non-nil move to precedent.

\(fn &optional PREV)" t nil)

(autoload 'helm-prev-visible-mark "helm/helm" "\
Move previous helm visible mark.

\(fn)" t nil)

(autoload 'helm-yank-selection "helm/helm" "\
Set minibuffer contents to current selection.

\(fn)" t nil)

(autoload 'helm-kill-selection-and-quit "helm/helm" "\
Store current selection to kill ring.
You can paste it by typing \\[yank].

\(fn)" t nil)

(autoload 'helm-follow-mode "helm/helm" "\
Execute persistent action everytime the cursor is moved when enabled.

\(fn &optional ARG)" t nil)

(autoload 'helm-send-bug-report "helm/helm" "\
Send a bug report of helm.el.

\(fn)" t nil)

(autoload 'helm-send-bug-report-from-helm "helm/helm" "\
Send a bug report of helm.el in helm session.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-c-reset-adaptative-history) "helm/helm-adaptative"
;;;;;;  "helm/helm-adaptative.el" (20390 12785))
;;; Generated autoloads from helm/helm-adaptative.el

(autoload 'helm-c-reset-adaptative-history "helm/helm-adaptative" "\
Delete all `helm-c-adaptive-history' and his file.
Useful when you have a old or corrupted `helm-c-adaptive-history-file'.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-apt) "helm/helm-apt" "helm/helm-apt.el" (20390
;;;;;;  12785))
;;; Generated autoloads from helm/helm-apt.el

(autoload 'helm-apt "helm/helm-apt" "\
Preconfigured `helm' : frontend of APT package manager.
With a prefix arg reload cache.

\(fn ARG)" t nil)

;;;***

;;;### (autoloads (helm-bbdb) "helm/helm-bbdb" "helm/helm-bbdb.el"
;;;;;;  (20390 12785))
;;; Generated autoloads from helm/helm-bbdb.el

(autoload 'helm-bbdb "helm/helm-bbdb" "\
Preconfigured `helm' for BBDB.

Needs BBDB.

http://bbdb.sourceforge.net/

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-bookmark-ext helm-c-bmkext-run-edit) "helm/helm-bmkext"
;;;;;;  "helm/helm-bmkext.el" (20390 12785))
;;; Generated autoloads from helm/helm-bmkext.el

(autoload 'helm-c-bmkext-run-edit "helm/helm-bmkext" "\
Run `bmkext-edit-bookmark' from keyboard.

\(fn)" t nil)

(autoload 'helm-bookmark-ext "helm/helm-bmkext" "\
Preconfigured `helm' for bookmark-extensions sources.
Needs bookmark-ext.el:
<http://mercurial.intuxication.org/hg/emacs-bookmark-extension>.
Contain also `helm-c-source-google-suggest'.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-c-pp-bookmarks helm-bookmarks helm-c-bookmark-run-delete
;;;;;;  helm-c-bookmark-run-jump-other-window) "helm/helm-bookmark"
;;;;;;  "helm/helm-bookmark.el" (20390 12785))
;;; Generated autoloads from helm/helm-bookmark.el

(autoload 'helm-c-bookmark-run-jump-other-window "helm/helm-bookmark" "\
Jump to bookmark from keyboard.

\(fn)" t nil)

(autoload 'helm-c-bookmark-run-delete "helm/helm-bookmark" "\
Delete bookmark from keyboard.

\(fn)" t nil)

(autoload 'helm-bookmarks "helm/helm-bookmark" "\
Preconfigured `helm' for bookmarks.

\(fn)" t nil)

(autoload 'helm-c-pp-bookmarks "helm/helm-bookmark" "\
Preconfigured `helm' for bookmarks (pretty-printed).

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-buffers-list helm-buffer-run-ediff helm-buffer-switch-to-elscreen
;;;;;;  helm-buffer-switch-other-frame helm-buffer-switch-other-window
;;;;;;  helm-buffer-run-query-replace helm-buffer-run-query-replace-regexp
;;;;;;  helm-buffer-run-zgrep helm-buffer-run-grep helm-buffer-run-kill-buffers
;;;;;;  helm-buffer-save-persistent helm-buffer-revert-persistent
;;;;;;  helm-buffer-diff-persistent) "helm/helm-buffers" "helm/helm-buffers.el"
;;;;;;  (20390 12785))
;;; Generated autoloads from helm/helm-buffers.el

(autoload 'helm-buffer-diff-persistent "helm/helm-buffers" "\
Toggle diff buffer without quitting helm.

\(fn)" t nil)

(autoload 'helm-buffer-revert-persistent "helm/helm-buffers" "\
Revert buffer without quitting helm.

\(fn)" t nil)

(autoload 'helm-buffer-save-persistent "helm/helm-buffers" "\
Save buffer without quitting helm.

\(fn)" t nil)

(autoload 'helm-buffer-run-kill-buffers "helm/helm-buffers" "\
Run kill buffer action from `helm-c-source-buffers-list'.

\(fn)" t nil)

(autoload 'helm-buffer-run-grep "helm/helm-buffers" "\
Run Grep action from `helm-c-source-buffers-list'.

\(fn)" t nil)

(autoload 'helm-buffer-run-zgrep "helm/helm-buffers" "\
Run Grep action from `helm-c-source-buffers-list'.

\(fn)" t nil)

(autoload 'helm-buffer-run-query-replace-regexp "helm/helm-buffers" "\
Run Query replace regexp action from `helm-c-source-buffers-list'.

\(fn)" t nil)

(autoload 'helm-buffer-run-query-replace "helm/helm-buffers" "\
Run Query replace action from `helm-c-source-buffers-list'.

\(fn)" t nil)

(autoload 'helm-buffer-switch-other-window "helm/helm-buffers" "\
Run switch to other window action from `helm-c-source-buffers-list'.

\(fn)" t nil)

(autoload 'helm-buffer-switch-other-frame "helm/helm-buffers" "\
Run switch to other frame action from `helm-c-source-buffers-list'.

\(fn)" t nil)

(autoload 'helm-buffer-switch-to-elscreen "helm/helm-buffers" "\
Run switch to elscreen  action from `helm-c-source-buffers-list'.

\(fn)" t nil)

(autoload 'helm-buffer-run-ediff "helm/helm-buffers" "\
Run ediff action from `helm-c-source-buffers-list'.

\(fn)" t nil)

(autoload 'helm-buffers-list "helm/helm-buffers" "\
Preconfigured `helm' to list buffers.
It is an enhanced version of `helm-for-buffers'.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-simple-call-tree) "helm/helm-call-tree" "helm/helm-call-tree.el"
;;;;;;  (20390 12785))
;;; Generated autoloads from helm/helm-call-tree.el

(autoload 'helm-simple-call-tree "helm/helm-call-tree" "\
Preconfigured `helm' for simple-call-tree. List function relationships.

Needs simple-call-tree.el.
http://www.emacswiki.org/cgi-bin/wiki/download/simple-call-tree.el

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-colors) "helm/helm-color" "helm/helm-color.el"
;;;;;;  (20390 12785))
;;; Generated autoloads from helm/helm-color.el

(autoload 'helm-colors "helm/helm-color" "\
Preconfigured `helm' for color.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-M-x) "helm/helm-command" "helm/helm-command.el"
;;;;;;  (20390 12785))
;;; Generated autoloads from helm/helm-command.el

(autoload 'helm-M-x "helm/helm-command" "\
Preconfigured `helm' for Emacs commands.
It is `helm' replacement of regular `M-x' `execute-extended-command'.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-configuration) "helm/helm-config" "helm/helm-config.el"
;;;;;;  (20390 12785))
;;; Generated autoloads from helm/helm-config.el

(define-prefix-command 'helm-command-prefix)

(autoload 'helm-configuration "helm/helm-config" "\
Customize `helm'.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-complex-command-history helm-timers helm-manage-advice
;;;;;;  helm-c-apropos helm-lisp-completion-or-file-name-at-point
;;;;;;  helm-lisp-completion-at-point-or-indent helm-c-complete-file-name-at-point
;;;;;;  helm-lisp-completion-at-point) "helm/helm-elisp" "helm/helm-elisp.el"
;;;;;;  (20390 12785))
;;; Generated autoloads from helm/helm-elisp.el

(autoload 'helm-lisp-completion-at-point "helm/helm-elisp" "\
Helm lisp symbol completion at point.

\(fn)" t nil)

(autoload 'helm-c-complete-file-name-at-point "helm/helm-elisp" "\
Complete file name at point.

\(fn)" t nil)

(autoload 'helm-lisp-completion-at-point-or-indent "helm/helm-elisp" "\
First call indent and second call complete lisp symbol.
The second call should happen before `helm-lisp-completion-or-indent-delay',
after this delay, next call will indent again.
After completion, next call is always indent.
See that like click and double mouse click.
One hit indent, two quick hits maybe indent and complete.

\(fn ARG)" t nil)

(autoload 'helm-lisp-completion-or-file-name-at-point "helm/helm-elisp" "\
Complete lisp symbol or filename at point.
Filename completion happen if filename is started in
or between double quotes.

\(fn)" t nil)

(autoload 'helm-c-apropos "helm/helm-elisp" "\
Preconfigured helm to describe commands, functions, variables and faces.

\(fn)" t nil)

(autoload 'helm-manage-advice "helm/helm-elisp" "\
Preconfigured `helm' to disable/enable function advices.

\(fn)" t nil)

(autoload 'helm-timers "helm/helm-elisp" "\
Preconfigured `helm' for timers.

\(fn)" t nil)

(autoload 'helm-complex-command-history "helm/helm-elisp" "\


\(fn)" t nil)

;;;***

;;;### (autoloads (helm-elscreen) "helm/helm-elscreen" "helm/helm-elscreen.el"
;;;;;;  (20390 12785))
;;; Generated autoloads from helm/helm-elscreen.el

(autoload 'helm-elscreen "helm/helm-elscreen" "\
Preconfigured helm to list elscreen.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-emms) "helm/helm-emms" "helm/helm-emms.el"
;;;;;;  (20390 12785))
;;; Generated autoloads from helm/helm-emms.el

(autoload 'helm-emms "helm/helm-emms" "\
Preconfigured `helm' for emms sources.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-eshell-history helm-esh-pcomplete) "helm/helm-eshell"
;;;;;;  "helm/helm-eshell.el" (20390 12785))
;;; Generated autoloads from helm/helm-eshell.el

(autoload 'helm-esh-pcomplete "helm/helm-eshell" "\
Preconfigured helm to provide helm completion in eshell.

\(fn)" t nil)

(autoload 'helm-eshell-history "helm/helm-eshell" "\
Preconfigured helm for eshell history.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-calcul-expression helm-eval-expression-with-eldoc
;;;;;;  helm-eval-expression) "helm/helm-eval" "helm/helm-eval.el"
;;;;;;  (20390 12785))
;;; Generated autoloads from helm/helm-eval.el

(autoload 'helm-eval-expression "helm/helm-eval" "\
Preconfigured helm for `helm-c-source-evaluation-result'.

\(fn ARG)" t nil)

(autoload 'helm-eval-expression-with-eldoc "helm/helm-eval" "\
Preconfigured helm for `helm-c-source-evaluation-result' with `eldoc' support.

\(fn)" t nil)

(autoload 'helm-calcul-expression "helm/helm-eval" "\
Preconfigured helm for `helm-c-source-calculation-result'.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-c-run-external-command) "helm/helm-external"
;;;;;;  "helm/helm-external.el" (20390 12785))
;;; Generated autoloads from helm/helm-external.el

(autoload 'helm-c-run-external-command "helm/helm-external" "\
Preconfigured `helm' to run External PROGRAM asyncronously from Emacs.
If program is already running exit with error.
You can set your own list of commands with
`helm-c-external-commands-list'.

\(fn PROGRAM)" t nil)

;;;***

;;;### (autoloads (helm-recentf helm-for-files helm-dired-hardlink-file
;;;;;;  helm-dired-symlink-file helm-dired-copy-file helm-dired-rename-file
;;;;;;  helm-insert-file helm-write-file helm-find-files helm-c-copy-files-async
;;;;;;  helm-dired-mode helm-ff-rotate-right-persistent helm-ff-rotate-left-persistent
;;;;;;  helm-ff-run-kill-buffer-persistent helm-ff-persistent-delete
;;;;;;  helm-ff-properties-persistent helm-find-files-down-one-level
;;;;;;  helm-ff-run-toggle-basename helm-ff-run-print-file helm-ff-run-etags
;;;;;;  helm-ff-run-gnus-attach-files helm-ff-run-locate helm-ff-run-open-file-externally
;;;;;;  helm-ff-run-switch-other-frame helm-ff-run-switch-other-window
;;;;;;  helm-ff-run-switch-to-eshell helm-ff-run-complete-fn-at-point
;;;;;;  helm-ff-run-delete-file helm-ff-run-hardlink-file helm-ff-run-symlink-file
;;;;;;  helm-ff-run-ediff-merge-file helm-ff-run-ediff-file helm-ff-run-eshell-command-on-file
;;;;;;  helm-ff-run-load-file helm-ff-run-byte-compile-file helm-ff-run-rename-file
;;;;;;  helm-ff-run-copy-file helm-ff-run-zgrep helm-ff-run-pdfgrep
;;;;;;  helm-ff-run-grep helm-ff-run-switch-to-history helm-ff-run-toggle-auto-update)
;;;;;;  "helm/helm-files" "helm/helm-files.el" (20390 12785))
;;; Generated autoloads from helm/helm-files.el

(autoload 'helm-ff-run-toggle-auto-update "helm/helm-files" "\


\(fn)" t nil)

(autoload 'helm-ff-run-switch-to-history "helm/helm-files" "\
Run Switch to history action from `helm-c-source-find-files'.

\(fn)" t nil)

(autoload 'helm-ff-run-grep "helm/helm-files" "\
Run Grep action from `helm-c-source-find-files'.

\(fn)" t nil)

(autoload 'helm-ff-run-pdfgrep "helm/helm-files" "\
Run Pdfgrep action from `helm-c-source-find-files'.

\(fn)" t nil)

(autoload 'helm-ff-run-zgrep "helm/helm-files" "\
Run Grep action from `helm-c-source-find-files'.

\(fn)" t nil)

(autoload 'helm-ff-run-copy-file "helm/helm-files" "\
Run Copy file action from `helm-c-source-find-files'.

\(fn)" t nil)

(autoload 'helm-ff-run-rename-file "helm/helm-files" "\
Run Rename file action from `helm-c-source-find-files'.

\(fn)" t nil)

(autoload 'helm-ff-run-byte-compile-file "helm/helm-files" "\
Run Byte compile file action from `helm-c-source-find-files'.

\(fn)" t nil)

(autoload 'helm-ff-run-load-file "helm/helm-files" "\
Run Load file action from `helm-c-source-find-files'.

\(fn)" t nil)

(autoload 'helm-ff-run-eshell-command-on-file "helm/helm-files" "\
Run eshell command on file action from `helm-c-source-find-files'.

\(fn)" t nil)

(autoload 'helm-ff-run-ediff-file "helm/helm-files" "\
Run Ediff file action from `helm-c-source-find-files'.

\(fn)" t nil)

(autoload 'helm-ff-run-ediff-merge-file "helm/helm-files" "\
Run Ediff merge file action from `helm-c-source-find-files'.

\(fn)" t nil)

(autoload 'helm-ff-run-symlink-file "helm/helm-files" "\
Run Symlink file action from `helm-c-source-find-files'.

\(fn)" t nil)

(autoload 'helm-ff-run-hardlink-file "helm/helm-files" "\
Run Hardlink file action from `helm-c-source-find-files'.

\(fn)" t nil)

(autoload 'helm-ff-run-delete-file "helm/helm-files" "\
Run Delete file action from `helm-c-source-find-files'.

\(fn)" t nil)

(autoload 'helm-ff-run-complete-fn-at-point "helm/helm-files" "\
Run complete file name action from `helm-c-source-find-files'.

\(fn)" t nil)

(autoload 'helm-ff-run-switch-to-eshell "helm/helm-files" "\
Run switch to eshell action from `helm-c-source-find-files'.

\(fn)" t nil)

(autoload 'helm-ff-run-switch-other-window "helm/helm-files" "\
Run switch to other window action from `helm-c-source-find-files'.

\(fn)" t nil)

(autoload 'helm-ff-run-switch-other-frame "helm/helm-files" "\
Run switch to other frame action from `helm-c-source-find-files'.

\(fn)" t nil)

(autoload 'helm-ff-run-open-file-externally "helm/helm-files" "\
Run open file externally command action from `helm-c-source-find-files'.

\(fn)" t nil)

(autoload 'helm-ff-run-locate "helm/helm-files" "\
Run locate action from `helm-c-source-find-files'.

\(fn)" t nil)

(autoload 'helm-ff-run-gnus-attach-files "helm/helm-files" "\
Run gnus attach files command action from `helm-c-source-find-files'.

\(fn)" t nil)

(autoload 'helm-ff-run-etags "helm/helm-files" "\
Run Etags command action from `helm-c-source-find-files'.

\(fn)" t nil)

(autoload 'helm-ff-run-print-file "helm/helm-files" "\
Run Print file action from `helm-c-source-find-files'.

\(fn)" t nil)

(autoload 'helm-ff-run-toggle-basename "helm/helm-files" "\


\(fn)" t nil)

(autoload 'helm-find-files-down-one-level "helm/helm-files" "\
Go down one level like unix command `cd ..'.
If prefix numeric arg is given go ARG level down.

\(fn ARG)" t nil)

(autoload 'helm-ff-properties-persistent "helm/helm-files" "\
Show properties without quitting helm.

\(fn)" t nil)

(autoload 'helm-ff-persistent-delete "helm/helm-files" "\
Delete current candidate without quitting.

\(fn)" t nil)

(autoload 'helm-ff-run-kill-buffer-persistent "helm/helm-files" "\
Execute `helm-ff-kill-buffer-fname' whitout quitting.

\(fn)" t nil)

(autoload 'helm-ff-rotate-left-persistent "helm/helm-files" "\
Rotate image left without quitting helm.

\(fn)" t nil)

(autoload 'helm-ff-rotate-right-persistent "helm/helm-files" "\
Rotate image right without quitting helm.

\(fn)" t nil)

(defvar helm-dired-mode "Enable helm completion in Dired functions.\nBindings affected are C, R, S, H.\nThis is deprecated for Emacs24+ users, use `helm-mode' instead." "\
Non-nil if Helm-Dired mode is enabled.
See the command `helm-dired-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `helm-dired-mode'.")

(custom-autoload 'helm-dired-mode "helm/helm-files" nil)

(autoload 'helm-dired-mode "helm/helm-files" "\
Toggle Helm-Dired mode on or off.
With a prefix argument ARG, enable Helm-Dired mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil, and toggle it if ARG is `toggle'.
\\{helm-dired-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'helm-c-copy-files-async "helm/helm-files" "\
Preconfigured helm to copy file list FLIST to DEST asynchronously.

\(fn)" t nil)

(autoload 'helm-find-files "helm/helm-files" "\
Preconfigured `helm' for helm implementation of `find-file'.
Called with a prefix arg show history if some.
Don't call it from programs, use `helm-find-files-1' instead.
This is the starting point for nearly all actions you can do on files.

\(fn ARG)" t nil)

(autoload 'helm-write-file "helm/helm-files" "\
Preconfigured `helm' providing completion for `write-file'.

\(fn)" t nil)

(autoload 'helm-insert-file "helm/helm-files" "\
Preconfigured `helm' providing completion for `insert-file'.

\(fn)" t nil)

(autoload 'helm-dired-rename-file "helm/helm-files" "\
Preconfigured `helm' to rename files from dired.

\(fn)" t nil)

(autoload 'helm-dired-copy-file "helm/helm-files" "\
Preconfigured `helm' to copy files from dired.

\(fn)" t nil)

(autoload 'helm-dired-symlink-file "helm/helm-files" "\
Preconfigured `helm' to symlink files from dired.

\(fn)" t nil)

(autoload 'helm-dired-hardlink-file "helm/helm-files" "\
Preconfigured `helm' to hardlink files from dired.

\(fn)" t nil)

(autoload 'helm-for-files "helm/helm-files" "\
Preconfigured `helm' for opening files.
Run all sources defined in `helm-for-files-prefered-list'.

\(fn)" t nil)

(autoload 'helm-recentf "helm/helm-files" "\
Preconfigured `helm' for `recentf'.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-firefox-bookmarks) "helm/helm-firefox" "helm/helm-firefox.el"
;;;;;;  (20390 12785))
;;; Generated autoloads from helm/helm-firefox.el

(autoload 'helm-firefox-bookmarks "helm/helm-firefox" "\
Preconfigured `helm' for firefox bookmark.
You will have to enable html bookmarks in firefox:
open about:config in firefox and double click on this line to enable value to true:

user_pref(\"browser.bookmarks.autoExportHTML\", false);

You should have now:

user_pref(\"browser.bookmarks.autoExportHTML\", true);

After closing firefox, you will be able to browse you bookmarks.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-ucs helm-select-xfont) "helm/helm-font" "helm/helm-font.el"
;;;;;;  (20390 12785))
;;; Generated autoloads from helm/helm-font.el

(autoload 'helm-select-xfont "helm/helm-font" "\
Preconfigured `helm' to select Xfont.

\(fn)" t nil)

(autoload 'helm-ucs "helm/helm-font" "\
Preconfigured helm for `ucs-names' math symbols.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-gentoo) "helm/helm-gentoo" "helm/helm-gentoo.el"
;;;;;;  (20390 12785))
;;; Generated autoloads from helm/helm-gentoo.el

(autoload 'helm-gentoo "helm/helm-gentoo" "\
Preconfigured `helm' for gentoo linux.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-do-pdfgrep helm-do-zgrep helm-do-grep helm-c-grep-run-save-buffer
;;;;;;  helm-c-grep-run-other-window-action helm-c-grep-run-default-action
;;;;;;  helm-c-grep-run-persistent-action helm-c-goto-next-file helm-c-goto-precedent-file
;;;;;;  helm-grep-mode-jump-other-window helm-grep-mode-jump-other-window-backward
;;;;;;  helm-grep-mode-jump-other-window-forward helm-grep-mode-jump
;;;;;;  helm-grep-mode-quit helm-gm-precedent-file helm-gm-next-file
;;;;;;  helm-grep-mode) "helm/helm-grep" "helm/helm-grep.el" (20390
;;;;;;  12785))
;;; Generated autoloads from helm/helm-grep.el

(autoload 'helm-grep-mode "helm/helm-grep" "\
Major mode to provide actions in helm grep saved buffer.

Special commands:
\\{helm-grep-mode-map}

\(fn)" t nil)

(autoload 'helm-gm-next-file "helm/helm-grep" "\


\(fn)" t nil)

(autoload 'helm-gm-precedent-file "helm/helm-grep" "\


\(fn)" t nil)

(autoload 'helm-grep-mode-quit "helm/helm-grep" "\


\(fn)" t nil)

(autoload 'helm-grep-mode-jump "helm/helm-grep" "\


\(fn)" t nil)

(autoload 'helm-grep-mode-jump-other-window-forward "helm/helm-grep" "\


\(fn)" t nil)

(autoload 'helm-grep-mode-jump-other-window-backward "helm/helm-grep" "\


\(fn)" t nil)

(autoload 'helm-grep-mode-jump-other-window "helm/helm-grep" "\


\(fn)" t nil)

(autoload 'helm-c-goto-precedent-file "helm/helm-grep" "\
Go to precedent file in helm grep/etags buffers.

\(fn)" t nil)

(autoload 'helm-c-goto-next-file "helm/helm-grep" "\
Go to precedent file in helm grep/etags buffers.

\(fn)" t nil)

(autoload 'helm-c-grep-run-persistent-action "helm/helm-grep" "\
Run grep persistent action from `helm-do-grep-1'.

\(fn)" t nil)

(autoload 'helm-c-grep-run-default-action "helm/helm-grep" "\
Run grep default action from `helm-do-grep-1'.

\(fn)" t nil)

(autoload 'helm-c-grep-run-other-window-action "helm/helm-grep" "\
Run grep goto other window action from `helm-do-grep-1'.

\(fn)" t nil)

(autoload 'helm-c-grep-run-save-buffer "helm/helm-grep" "\
Run grep save results action from `helm-do-grep-1'.

\(fn)" t nil)

(autoload 'helm-do-grep "helm/helm-grep" "\
Preconfigured helm for grep.
Contrarily to Emacs `grep' no default directory is given, but
the full path of candidates in ONLY.
That allow to grep different files not only in `default-directory' but anywhere
by marking them (C-<SPACE>). If one or more directory is selected
grep will search in all files of these directories.
You can use also wildcard in the base name of candidate.
If a prefix arg is given use the -r option of grep.
The prefix arg can be passed before or after start.
See also `helm-do-grep-1'.

\(fn)" t nil)

(autoload 'helm-do-zgrep "helm/helm-grep" "\
Preconfigured helm for zgrep.

\(fn)" t nil)

(autoload 'helm-do-pdfgrep "helm/helm-grep" "\
Preconfigured helm for pdfgrep.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-describe-helm-attribute helm-esh-help helm-c-bookmark-help
;;;;;;  helm-c-ucs-help helm-etags-help helm-pdfgrep-help helm-grep-help
;;;;;;  helm-generic-file-help helm-read-file-name-help helm-ff-help
;;;;;;  helm-c-buffer-help helm-help) "helm/helm-help" "helm/helm-help.el"
;;;;;;  (20390 12785))
;;; Generated autoloads from helm/helm-help.el

(defvar helm-mode-line-string "\\<helm-map>\\[helm-help]:Help \\[helm-select-action]:Act \\[helm-exit-minibuffer]/\\[helm-select-2nd-action-or-end-of-line]/\\[helm-select-3rd-action]:NthAct" "\
Help string displayed in mode-line in `helm'.
It can be a string or a list of two args, in this case,
first arg is a string that will be used as name for candidates number,
second arg any string to display in mode line.
If nil, use default `mode-line-format'.")

(autoload 'helm-help "helm/helm-help" "\
Help of `helm'.

\(fn)" t nil)

(autoload 'helm-c-buffer-help "helm/helm-help" "\
Help command for helm buffers.

\(fn)" t nil)

(autoload 'helm-ff-help "helm/helm-help" "\
Help command for `helm-find-files'.

\(fn)" t nil)

(autoload 'helm-read-file-name-help "helm/helm-help" "\


\(fn)" t nil)

(autoload 'helm-generic-file-help "helm/helm-help" "\


\(fn)" t nil)

(autoload 'helm-grep-help "helm/helm-help" "\


\(fn)" t nil)

(autoload 'helm-pdfgrep-help "helm/helm-help" "\


\(fn)" t nil)

(autoload 'helm-etags-help "helm/helm-help" "\
The help function for etags.

\(fn)" t nil)

(autoload 'helm-c-ucs-help "helm/helm-help" "\
Help command for `helm-ucs'.

\(fn)" t nil)

(autoload 'helm-c-bookmark-help "helm/helm-help" "\
Help command for bookmarks.

\(fn)" t nil)

(autoload 'helm-esh-help "helm/helm-help" "\
Help command for `helm-find-files-eshell-command-on-file'.

\(fn)" t nil)

(defvar helm-buffer-mode-line-string '("Buffer(s)" "\\<helm-c-buffer-map>\\[helm-c-buffer-help]:Help \\<helm-map>\\[helm-select-action]:Act \\[helm-exit-minibuffer]/\\[helm-select-2nd-action-or-end-of-line]/\\[helm-select-3rd-action]:NthAct" "String displayed in mode-line in `helm-c-source-buffers-list'"))

(defvar helm-ff-mode-line-string "\\<helm-find-files-map>\\[helm-ff-help]:Help \\<helm-map>\\[helm-select-action]:Act \\[helm-exit-minibuffer]/\\[helm-select-2nd-action-or-end-of-line]/\\[helm-select-3rd-action]:NthAct" "\
String displayed in mode-line in `helm-c-source-find-files'")

(defvar helm-read-file-name-mode-line-string "\\<helm-c-read-file-map>\\[helm-read-file-name-help]:Help \\<helm-map>\\[helm-select-action]:Act \\[helm-exit-minibuffer]/\\[helm-select-2nd-action-or-end-of-line]/\\[helm-select-3rd-action]:NthAct" "\
String displayed in mode-line in `helm-c-source-find-files'")

(defvar helm-generic-file-mode-line-string "\\<helm-generic-files-map>\\[helm-generic-file-help]:Help \\<helm-map>\\[helm-select-action]:Act \\[helm-exit-minibuffer]/\\[helm-select-2nd-action-or-end-of-line]/\\[helm-select-3rd-action]:NthAct" "\
String displayed in mode-line in Locate.")

(defvar helm-grep-mode-line-string "\\<helm-c-grep-map>\\[helm-grep-help]:Help \\<helm-map>\\[helm-select-action]:Act \\[helm-exit-minibuffer]/\\[helm-select-2nd-action-or-end-of-line]/\\[helm-select-3rd-action]:NthAct" "\
String displayed in mode-line in `helm-do-grep'.")

(defvar helm-pdfgrep-mode-line-string "\\<helm-c-pdfgrep-map>\\[helm-pdfgrep-help]:Help \\<helm-map>\\[helm-select-action]:Act \\[helm-exit-minibuffer]/\\[helm-select-2nd-action-or-end-of-line]/\\[helm-select-3rd-action]:NthAct" "\
String displayed in mode-line in `helm-do-pdfgrep'.")

(defvar helm-etags-mode-line-string "\\<helm-c-etags-map>\\[helm-etags-help]:Help \\<helm-map>\\[helm-select-action]:Act \\[helm-exit-minibuffer]/\\[helm-select-2nd-action-or-end-of-line]/\\[helm-select-3rd-action]:NthAct" "\
String displayed in mode-line in `helm-c-etags-select'.")

(defvar helm-c-ucs-mode-line-string "\\<helm-c-ucs-map>\\[helm-c-ucs-help]:Help \\<helm-map>\\[helm-select-action]:Act \\[helm-exit-minibuffer]/\\[helm-select-2nd-action-or-end-of-line]/\\[helm-select-3rd-action]:NthAct" "\
String displayed in mode-line in `helm-ucs'.")

(defvar helm-bookmark-mode-line-string '("Bookmark(s)" "\\<helm-c-bookmark-map>\\[helm-c-bookmark-help]:Help \\<helm-map>\\[helm-select-action]:Act \\[helm-exit-minibuffer]/\\[helm-select-2nd-action-or-end-of-line]/\\[helm-select-3rd-action]:NthAct") "\
String displayed in mode-line in `helm-c-source-buffers-list'")

(defvar helm-occur-mode-line "\\<helm-map>\\[helm-help]:Help \\<helm-occur-map>\\[helm-occur-run-query-replace-regexp]:Query replace regexp \\<helm-map>\\[helm-select-action]:Act \\[helm-exit-minibuffer]/\\[helm-select-2nd-action-or-end-of-line]/\\[helm-select-3rd-action]:NthAct")

(autoload 'helm-describe-helm-attribute "helm/helm-help" "\
Display the full documentation of HELM-ATTRIBUTE.
HELM-ATTRIBUTE should be a symbol.

\(fn HELM-ATTRIBUTE)" t nil)

;;;***

;;;### (autoloads (helm-imenu) "helm/helm-imenu" "helm/helm-imenu.el"
;;;;;;  (20390 12785))
;;; Generated autoloads from helm/helm-imenu.el

(autoload 'helm-imenu "helm/helm-imenu" "\
Preconfigured `helm' for `imenu'.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-info-at-point) "helm/helm-info" "helm/helm-info.el"
;;;;;;  (20390 12785))
;;; Generated autoloads from helm/helm-info.el

(autoload 'helm-info-at-point "helm/helm-info" "\
Preconfigured `helm' for searching info at point.
With a prefix-arg insert symbol at point.

\(fn ARG)" t nil)

;;;***

;;;### (autoloads (helm-locate) "helm/helm-locate" "helm/helm-locate.el"
;;;;;;  (20390 12785))
;;; Generated autoloads from helm/helm-locate.el

(autoload 'helm-locate "helm/helm-locate" "\
Preconfigured `helm' for Locate.
Note: you can add locate options after entering pattern.
See 'man locate' for valid options.

You can specify a specific database with prefix argument ARG (C-u).
Many databases can be used: navigate and mark them.
See also `helm-locate-with-db'.

To create a user specific db, use
\"updatedb -l 0 -o db_path -U directory\".
Where db_path is a filename matched by
`helm-locate-db-file-regexp'.

\(fn ARG)" t nil)

;;;***

;;;### (autoloads (helm-man-woman) "helm/helm-man" "helm/helm-man.el"
;;;;;;  (20390 12785))
;;; Generated autoloads from helm/helm-man.el

(autoload 'helm-man-woman "helm/helm-man" "\
Preconfigured `helm' for Man and Woman pages.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-mp-toggle-match-plugin) "helm/helm-match-plugin"
;;;;;;  "helm/helm-match-plugin.el" (20390 12785))
;;; Generated autoloads from helm/helm-match-plugin.el

(autoload 'helm-mp-toggle-match-plugin "helm/helm-match-plugin" "\
Turn on/off multiple regexp matching in helm.
i.e helm-match-plugin.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-minibuffer-history helm-mini helm-ratpoison-commands
;;;;;;  helm-eev-anchors helm-c-insert-latex-math helm-world-time)
;;;;;;  "helm/helm-misc" "helm/helm-misc.el" (20390 12785))
;;; Generated autoloads from helm/helm-misc.el

(autoload 'helm-world-time "helm/helm-misc" "\
Preconfigured `helm' to show world time.

\(fn)" t nil)

(autoload 'helm-c-insert-latex-math "helm/helm-misc" "\
Preconfigured helm for latex math symbols completion.

\(fn)" t nil)

(autoload 'helm-eev-anchors "helm/helm-misc" "\
Preconfigured `helm' for eev anchors.

\(fn)" t nil)

(autoload 'helm-ratpoison-commands "helm/helm-misc" "\
Preconfigured `helm' to execute ratpoison commands.

\(fn)" t nil)

(autoload 'helm-mini "helm/helm-misc" "\
Preconfigured `helm' lightweight version (buffer -> recentf).

\(fn)" t nil)

(autoload 'helm-minibuffer-history "helm/helm-misc" "\
Preconfigured `helm' for `minibuffer-history'.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-mode) "helm/helm-mode" "helm/helm-mode.el"
;;;;;;  (20390 12785))
;;; Generated autoloads from helm/helm-mode.el

(defvar helm-mode nil "\
Non-nil if Helm mode is enabled.
See the command `helm-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `helm-mode'.")

(custom-autoload 'helm-mode "helm/helm-mode" nil)

(autoload 'helm-mode "helm/helm-mode" "\
Toggle generic helm completion.

All functions in Emacs that use `completing-read'
or `read-file-name' and friends will use helm interface
when this mode is turned on.
However you can modify this behavior for functions of your choice
with `helm-completing-read-handlers-alist'.

Called with a positive arg, turn on unconditionally, with a
negative arg turn off.
You can turn it on with `helm-mode'.

Some crap emacs functions may not be supported,
e.g `ffap-alternate-file' and maybe others
You can add such functions to `helm-completing-read-handlers-alist'
with a nil value.

Note: This mode will work only partially on Emacs23.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (helm-yahoo-suggest helm-google-suggest helm-surfraw)
;;;;;;  "helm/helm-net" "helm/helm-net.el" (20390 12785))
;;; Generated autoloads from helm/helm-net.el

(autoload 'helm-surfraw "helm/helm-net" "\
Preconfigured `helm' to search PATTERN with search ENGINE.

\(fn PATTERN ENGINE)" t nil)

(autoload 'helm-google-suggest "helm/helm-net" "\
Preconfigured `helm' for google search with google suggest.

\(fn)" t nil)

(autoload 'helm-yahoo-suggest "helm/helm-net" "\
Preconfigured `helm' for Yahoo searching with Yahoo suggest.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-org-headlines helm-org-keywords) "helm/helm-org"
;;;;;;  "helm/helm-org.el" (20390 12785))
;;; Generated autoloads from helm/helm-org.el

(autoload 'helm-org-keywords "helm/helm-org" "\
Preconfigured `helm' for org keywords.

\(fn)" t nil)

(autoload 'helm-org-headlines "helm/helm-org" "\
Preconfigured helm to show org headlines.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-browse-code helm-occur helm-regexp) "helm/helm-regexp"
;;;;;;  "helm/helm-regexp.el" (20390 12785))
;;; Generated autoloads from helm/helm-regexp.el

(autoload 'helm-regexp "helm/helm-regexp" "\
Preconfigured helm to build regexps.
`query-replace-regexp' can be run from there against found regexp.

\(fn)" t nil)

(autoload 'helm-occur "helm/helm-regexp" "\
Preconfigured Helm for Occur source.
If region is active, search only in region,
otherwise search in whole buffer.

\(fn)" t nil)

(autoload 'helm-browse-code "helm/helm-regexp" "\
Preconfigured helm to browse code.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-show-kill-ring helm-register helm-all-mark-rings
;;;;;;  helm-global-mark-ring helm-mark-ring) "helm/helm-ring" "helm/helm-ring.el"
;;;;;;  (20390 12785))
;;; Generated autoloads from helm/helm-ring.el

(autoload 'helm-mark-ring "helm/helm-ring" "\
Preconfigured `helm' for `helm-c-source-mark-ring'.

\(fn)" t nil)

(autoload 'helm-global-mark-ring "helm/helm-ring" "\
Preconfigured `helm' for `helm-c-source-global-mark-ring'.

\(fn)" t nil)

(autoload 'helm-all-mark-rings "helm/helm-ring" "\
Preconfigured `helm' for `helm-c-source-global-mark-ring' and `helm-c-source-mark-ring'.

\(fn)" t nil)

(autoload 'helm-register "helm/helm-ring" "\
Preconfigured `helm' for Emacs registers.

\(fn)" t nil)

(autoload 'helm-show-kill-ring "helm/helm-ring" "\
Preconfigured `helm' for `kill-ring'.
It is drop-in replacement of `yank-pop'.
You may bind this command to M-y.
First call open the kill-ring browser, next calls move to next line.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-semantic-or-imenu helm-semantic) "helm/helm-semantic"
;;;;;;  "helm/helm-semantic.el" (20390 12785))
;;; Generated autoloads from helm/helm-semantic.el

(autoload 'helm-semantic "helm/helm-semantic" "\
Preconfigured `helm' for `semantic'.

\(fn)" t nil)

(autoload 'helm-semantic-or-imenu "helm/helm-semantic" "\
Run `helm' with `semantic' or `imenu'.

If `semantic-mode' is active in the current buffer, then use
semantic for generating tags, otherwise fall back to `imenu'.
Fill in the symbol at point by default.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-xrandr-set helm-list-emacs-process helm-top)
;;;;;;  "helm/helm-sys" "helm/helm-sys.el" (20390 12785))
;;; Generated autoloads from helm/helm-sys.el

(autoload 'helm-top "helm/helm-sys" "\
Preconfigured `helm' for top command.

\(fn)" t nil)

(autoload 'helm-list-emacs-process "helm/helm-sys" "\
Preconfigured `helm' for emacs process.

\(fn)" t nil)

(autoload 'helm-xrandr-set "helm/helm-sys" "\


\(fn)" t nil)

;;;***

;;;### (autoloads (helm-c-etags-select) "helm/helm-tags" "helm/helm-tags.el"
;;;;;;  (20390 12785))
;;; Generated autoloads from helm/helm-tags.el

(autoload 'helm-c-etags-select "helm/helm-tags" "\
Preconfigured helm for etags.
Called with one prefix arg use symbol at point as initial input.
Called with two prefix arg reinitialize cache.
If tag file have been modified reinitialize cache.

\(fn ARG)" t nil)

;;;***

;;;### (autoloads (helm-yank-text-at-point helm-w32-shell-execute-open-file
;;;;;;  helm-quit-and-find-file helm-show-all-in-this-source-only)
;;;;;;  "helm/helm-utils" "helm/helm-utils.el" (20390 12785))
;;; Generated autoloads from helm/helm-utils.el

(autoload 'helm-show-all-in-this-source-only "helm/helm-utils" "\
Show only current source of this helm session with all its candidates.
With a numeric prefix arg show only the ARG number of candidates.

\(fn ARG)" t nil)

(autoload 'helm-quit-and-find-file "helm/helm-utils" "\
Drop into `helm-find-files' from `helm'.
If current selection is a buffer or a file, `helm-find-files'
from its directory.

\(fn)" t nil)

(autoload 'helm-w32-shell-execute-open-file "helm/helm-utils" "\


\(fn FILE)" t nil)

(autoload 'helm-yank-text-at-point "helm/helm-utils" "\
Yank text at point in minibuffer.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-w3m-bookmarks) "helm/helm-w3m" "helm/helm-w3m.el"
;;;;;;  (20390 12785))
;;; Generated autoloads from helm/helm-w3m.el

(autoload 'helm-w3m-bookmarks "helm/helm-w3m" "\
Preconfigured `helm' for w3m bookmark.

Needs w3m and emacs-w3m.

http://w3m.sourceforge.net/
http://emacs-w3m.namazu.org/

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-yaoddmuse-emacswiki-post-library helm-yaoddmuse-emacswiki-edit-or-view
;;;;;;  helm-yaoddmuse-cache-pages) "helm/helm-yaoddmuse" "helm/helm-yaoddmuse.el"
;;;;;;  (20390 12785))
;;; Generated autoloads from helm/helm-yaoddmuse.el

(autoload 'helm-yaoddmuse-cache-pages "helm/helm-yaoddmuse" "\
Fetch the list of files on emacswiki and create cache file.
If load is non--nil load the file and feed `yaoddmuse-pages-hash'.

\(fn &optional LOAD)" t nil)

(autoload 'helm-yaoddmuse-emacswiki-edit-or-view "helm/helm-yaoddmuse" "\
Preconfigured `helm' to edit or view EmacsWiki page.

Needs yaoddmuse.el.

http://www.emacswiki.org/emacs/download/yaoddmuse.el

\(fn)" t nil)

(autoload 'helm-yaoddmuse-emacswiki-post-library "helm/helm-yaoddmuse" "\
Preconfigured `helm' to post library to EmacsWiki.

Needs yaoddmuse.el.

http://www.emacswiki.org/emacs/download/yaoddmuse.el

\(fn)" t nil)

;;;***

;;;### (autoloads (hideshowvis-enable hideshowvis-minor-mode hideshowvis-click-fringe)
;;;;;;  "hideshowvis/hideshowvis" "hideshowvis/hideshowvis.el" (20391
;;;;;;  59642))
;;; Generated autoloads from hideshowvis/hideshowvis.el

(autoload 'hideshowvis-click-fringe "hideshowvis/hideshowvis" "\


\(fn EVENT)" t nil)

(autoload 'hideshowvis-minor-mode "hideshowvis/hideshowvis" "\
Toggle Hideshowvis minor mode on or off.
With a prefix argument ARG, enable Hideshowvis minor mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil, and toggle it if ARG is `toggle'.
\\{hideshowvis-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'hideshowvis-enable "hideshowvis/hideshowvis" "\
Will enable hideshowvis minor mode

\(fn)" t nil)

;;;***

;;;### (autoloads (minimap-kill minimap-create) "minimap/minimap"
;;;;;;  "minimap/minimap.el" (20389 8546))
;;; Generated autoloads from minimap/minimap.el

(autoload 'minimap-create "minimap/minimap" "\
Create a minimap sidebar for the current window.

\(fn)" t nil)

(autoload 'minimap-kill "minimap/minimap" "\
Kill minimap for current buffer.
Cancel the idle timer if no more minimaps are active.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-projectile) "projectile/helm-projectile"
;;;;;;  "projectile/helm-projectile.el" (20403 49382))
;;; Generated autoloads from projectile/helm-projectile.el

(autoload 'helm-projectile "projectile/helm-projectile" "\
Use projectile with Helm instead of ido.

\(fn)" t nil)

;;;***

;;;### (autoloads (projectile-mode projectile-global-mode) "projectile/projectile"
;;;;;;  "projectile/projectile.el" (20403 49382))
;;; Generated autoloads from projectile/projectile.el

(defvar projectile-global-mode nil "\
Non-nil if Projectile-Global mode is enabled.
See the command `projectile-global-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `projectile-global-mode'.")

(custom-autoload 'projectile-global-mode "projectile/projectile" nil)

(autoload 'projectile-global-mode "projectile/projectile" "\
Toggle Projectile mode in all buffers.
With prefix ARG, enable Projectile-Global mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Projectile mode is enabled in all buffers where
`projectile-on' would do it.
See `projectile-mode' for more information on Projectile mode.

\(fn &optional ARG)" t nil)

(autoload 'projectile-mode "projectile/projectile" "\
Minor mode to assist project management and navigation.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("anything/contrib/anything-grep.el" "anything/contrib/anything-ipa.el"
;;;;;;  "anything/contrib/anything-menu.el" "anything/contrib/anything-migemo.el"
;;;;;;  "anything/contrib/anything-multi-sources.el" "anything/contrib/anything-startup.el"
;;;;;;  "anything/extensions/anything-complete.el" "anything/extensions/anything-gtags.el"
;;;;;;  "anything/extensions/anything-obsolete.el" "anything/extensions/anything-show-completion.el"
;;;;;;  "auto-complete-clang/auto-complete-clang.el" "auto-complete/auto-complete-config.el"
;;;;;;  "auto-complete/auto-complete-pkg.el" "auto-complete/auto-complete.el"
;;;;;;  "helm/helm-pkg.el" "helm/helm-plugin.el" "hideshowvis/hideshowvis-autoloads.el"
;;;;;;  "hideshowvis/hideshowvis-pkg.el" "switch-window/switch-window.el")
;;;;;;  (20403 51681 135893))

;;;***

(provide '.loaddefs)
;; Local Variables:
;; version-control: never
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; .loaddefs.el ends here
