;;; .loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (anything-other-buffer anything-at-point anything)
;;;;;;  "anything-config/anything" "anything-config/anything.el"
;;;;;;  (19784 14225))
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
;;;;;;  (19784 14225))
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
;;;;;;  "anything/anything" "anything/anything.el" (19784 14433))
;;; Generated autoloads from anything/anything.el

(autoload 'anything "anything/anything" "\
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

(autoload 'anything-at-point "anything/anything" "\
Same as `anything' except when C-u is pressed, the initial input is the symbol at point.

\(fn &optional ANY-SOURCES ANY-INPUT ANY-PROMPT ANY-RESUME ANY-PRESELECT ANY-BUFFER)" t nil)

(autoload 'anything-other-buffer "anything/anything" "\
Simplified interface of `anything' with other `anything-buffer'

\(fn ANY-SOURCES ANY-BUFFER)" nil nil)

;;;***

<<<<<<< HEAD
;;;### (autoloads nil nil ("ac-anything/ac-anything.el" "anything-complete/anything-complete.el"
;;;;;;  "anything-config/anything-match-plugin.el" "anything-config/anything-startup.el"
;;;;;;  "anything-grep/anything-grep.el" "anything-gtags/anything-gtags.el"
;;;;;;  "anything-include/anything-include.el" "anything-ipa/anything-ipa.el"
;;;;;;  "anything-kyr/anything-kyr.el" "anything-match-plugin/anything-match-plugin.el"
;;;;;;  "anything-menu/anything-menu.el") (19784 14492 637745))
=======
;;;### (autoloads nil nil ("async-eval/async-eval.el" "auto-async-byte-compile/auto-async-byte-compile.el"
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
;;;;;;  "auto-complete-clang/auto-complete-clang.el" "ppindent/ppindent.el"
;;;;;;  "pymacs/pymacs.el" "ssh-config/ssh-config.el") (19783 56171
;;;;;;  669826))
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
>>>>>>> parent of 608d383... update
=======
>>>>>>> parent of 608d383... update
=======
>>>>>>> parent of 608d383... update
=======
>>>>>>> parent of 608d383... update
=======
;;;;;;  "pymacs/pymacs.el" "ssh-config/ssh-config.el") (19782 16613
;;;;;;  973645))
>>>>>>> parent of 70c3fa6... update
=======
;;;;;;  "auto-complete-clang/auto-complete-clang.el" "ppindent/ppindent.el"
;;;;;;  "pymacs/pymacs.el" "ssh-config/ssh-config.el") (19783 56171
;;;;;;  669826))
>>>>>>> parent of 608d383... update
=======
;;;;;;  "auto-complete-clang/auto-complete-clang.el" "ppindent/ppindent.el"
;;;;;;  "pymacs/pymacs.el" "ssh-config/ssh-config.el") (19783 56171
;;;;;;  669826))
>>>>>>> parent of 608d383... update

;;;***

(provide '.loaddefs)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; .loaddefs.el ends here
