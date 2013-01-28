;;; .loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (ac-nrepl-setup ac-nrepl-clear-class-cache) "ac-nrepl/ac-nrepl"
;;;;;;  "ac-nrepl/ac-nrepl.el" (20621 19573))
;;; Generated autoloads from ac-nrepl/ac-nrepl.el

(autoload 'ac-nrepl-clear-class-cache "ac-nrepl/ac-nrepl" "\
Clear the class cache to prevent stale results.

\(fn)" nil nil)

(add-hook 'nrepl-connected-hook 'ac-nrepl-clear-class-cache)

(defface ac-nrepl-candidate-face '((t (:inherit ac-candidate-face))) "\
Face for nrepl candidates." :group (quote auto-complete))

(defface ac-nrepl-selection-face '((t (:inherit ac-selection-face))) "\
Face for the nrepl selected candidate." :group (quote auto-complete))

(defconst ac-nrepl-source-defaults '((available . ac-nrepl-available-p) (candidate-face . ac-nrepl-candidate-face) (selection-face . ac-nrepl-selection-face) (prefix . ac-nrepl-symbol-start-pos) (document . ac-nrepl-documentation)) "\
Defaults common to the various completion sources.")

(defvar ac-source-nrepl-ns (append '((candidates . ac-nrepl-candidates-ns) (symbol . "n")) ac-nrepl-source-defaults) "\
Auto-complete source for nrepl ns completion.")

(defvar ac-source-nrepl-vars (append '((candidates . ac-nrepl-candidates-vars) (symbol . "v")) ac-nrepl-source-defaults) "\
Auto-complete source for nrepl var completion.")

(defvar ac-source-nrepl-ns-classes (append '((candidates . ac-nrepl-candidates-ns-classes) (symbol . "c")) ac-nrepl-source-defaults) "\
Auto-complete source for nrepl ns-specific class completion.")

(defvar ac-source-nrepl-all-classes (append '((candidates . ac-nrepl-candidates-all-classes) (symbol . "c")) ac-nrepl-source-defaults) "\
Auto-complete source for nrepl all class completion.")

(defvar ac-source-nrepl-java-methods (append '((candidates . ac-nrepl-candidates-java-methods) (symbol . "m") (action . ac-nrepl-delete-java-class-hint)) ac-nrepl-source-defaults) "\
Auto-complete source for nrepl java method completion.")

(defvar ac-source-nrepl-static-methods (append '((candidates . ac-nrepl-candidates-static-methods) (symbol . "s")) ac-nrepl-source-defaults) "\
Auto-complete source for nrepl java static method completion.")

(autoload 'ac-nrepl-setup "ac-nrepl/ac-nrepl" "\
Add the nrepl completion source to the front of `ac-sources'.
This affects only the current buffer.

\(fn)" t nil)

;;;***

;;;### (autoloads (set-up-slime-ac) "ac-slime/ac-slime" "ac-slime/ac-slime.el"
;;;;;;  (20621 19571))
;;; Generated autoloads from ac-slime/ac-slime.el

(defface ac-slime-menu-face '((t (:inherit ac-candidate-face))) "\
Face for slime candidate menu." :group (quote auto-complete))

(defface ac-slime-selection-face '((t (:inherit ac-selection-face))) "\
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

;;;### (autoloads (ace-jump-mode ace-jump-line-mode ace-jump-word-mode
;;;;;;  ace-jump-char-mode ace-jump-mode-pop-mark) "ace-jump-mode/ace-jump-mode"
;;;;;;  "ace-jump-mode/ace-jump-mode.el" (20621 19574))
;;; Generated autoloads from ace-jump-mode/ace-jump-mode.el

(autoload 'ace-jump-mode-pop-mark "ace-jump-mode/ace-jump-mode" "\
Pop up a postion from `ace-jump-mode-mark-ring', and jump back to that position

\(fn)" t nil)

(autoload 'ace-jump-char-mode "ace-jump-mode/ace-jump-mode" "\
AceJump char mode

\(fn QUERY-CHAR)" t nil)

(autoload 'ace-jump-word-mode "ace-jump-mode/ace-jump-mode" "\
AceJump word mode.
You can set `ace-jump-word-mode-use-query-char' to nil to prevent
asking for a head char, that will mark all the word in current
buffer.

\(fn HEAD-CHAR)" t nil)

(autoload 'ace-jump-line-mode "ace-jump-mode/ace-jump-mode" "\
AceJump line mode.
Marked each no empty line and move there

\(fn)" t nil)

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

;;;### (autoloads (alert alert-add-rule) "alert/alert" "alert/alert.el"
;;;;;;  (20621 19576))
;;; Generated autoloads from alert/alert.el

(autoload 'alert-add-rule "alert/alert" "\
Programmatically add an alert configuration rule.

Normally, users should custoimze `alert-user-configuration'.
This facility is for module writers and users that need to do
things the Lisp way.

Here is a rule the author currently uses with ERC, so that the
fringe gets colored whenever people chat on BitlBee:

\(alert-add-rule :status   '(buried visible idle)
                :severity '(moderate high urgent)
                :mode     'erc-mode
                :predicate
                #'(lambda (info)
                    (string-match (concat \"\\\\`[^&].*@BitlBee\\\\'\")
                                  (erc-format-target-and/or-network)))
                :persistent
                #'(lambda (info)
                    ;; If the buffer is buried, or the user has been
                    ;; idle for `alert-reveal-idle-time' seconds,
                    ;; make this alert persistent.  Normally, alerts
                    ;; become persistent after
                    ;; `alert-persist-idle-time' seconds.
                    (memq (plist-get info :status) '(buried idle)))
                :style 'fringe
                :continue t)

\(fn &key SEVERITY STATUS MODE CATEGORY TITLE MESSAGE PREDICATE (style alert-default-style) PERSISTENT CONTINUE NEVER-PERSIST APPEND)" nil nil)

(autoload 'alert "alert/alert" "\
Alert the user that something has happened.
MESSAGE is what the user will see.  You may also use keyword
arguments to specify additional details.  Here is a full example:

\(alert \"This is a message\"
       :severity 'high          ;; The default severity is `normal'
       :title \"Title\"         ;; An optional title
       :category 'example       ;; A symbol to identify the message
       :mode 'text-mode         ;; Normally determined automatically
       :buffer (current-buffer) ;; This is the default
       :data nil                ;; Unused by alert.el itself
       :persistent nil          ;; Force the alert to be persistent;
                                ;; it is best not to use this
       :never-persist nil       ;; Force this alert to never persist
       :style 'fringe)          ;; Force a given style to be used;
                                ;; this is only for debugging!

If no :title is given, the buffer-name of :buffer is used.  If
:buffer is nil, it is the current buffer at the point of call.

:data is an opaque value which modules can pass through to their
own styles if they wish.

Here are some more typical examples of usage:

  ;; This is the most basic form usage
  (alert \"This is an alert\")

  ;; You can adjust the severity for more important messages
  (alert \"This is an alert\" :severity 'high)

  ;; Or decrease it for purely informative ones
  (alert \"This is an alert\" :severity 'trivial)

  ;; Alerts can have optional titles.  Otherwise, the title is the
  ;; buffer-name of the (current-buffer) where the alert originated.
  (alert \"This is an alert\" :title \"My Alert\")

  ;; Further, alerts can have categories.  This allows users to
  ;; selectively filter on them.
  (alert \"This is an alert\" :title \"My Alert\"
         :category 'some-category-or-other)

\(fn MESSAGE &key (severity (quote normal)) TITLE CATEGORY BUFFER MODE DATA STYLE PERSISTENT NEVER-PERSIST)" nil nil)

;;;***

;;;### (autoloads (android-mode) "android-mode/android-mode" "android-mode/android-mode.el"
;;;;;;  (20621 19573))
;;; Generated autoloads from android-mode/android-mode.el

(autoload 'android-mode "android-mode/android-mode" "\
Android application development minor mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (ascii-off ascii-on ascii-display ascii-customize)
;;;;;;  "ascii/ascii" "ascii/ascii.el" (20621 19571))
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

;;;### (autoloads (auto-complete) "auto-complete/auto-complete" "auto-complete/auto-complete.el"
;;;;;;  (20621 19579))
;;; Generated autoloads from auto-complete/auto-complete.el

(autoload 'auto-complete "auto-complete/auto-complete" "\
Start auto-completion at current point.

\(fn &optional SOURCES)" t nil)

;;;***

;;;### (autoloads (auto-indent-global-mode auto-indent-minor-mode-on
;;;;;;  auto-indent-minor-mode auto-indent-eol-char-newline auto-indent-eol-newline)
;;;;;;  "auto-indent-mode/auto-indent-mode" "auto-indent-mode/auto-indent-mode.el"
;;;;;;  (20621 19571))
;;; Generated autoloads from auto-indent-mode/auto-indent-mode.el

(autoload 'auto-indent-eol-newline "auto-indent-mode/auto-indent-mode" "\
Auto-indent function for `end-of-line' and then newline.

\(fn)" t nil)

(autoload 'auto-indent-eol-char-newline "auto-indent-mode/auto-indent-mode" "\
Auto-indent function for `end-of-line', insert `auto-indent-eol-char', and then newline.

\(fn)" t nil)

(defalias 'auto-indent-mode 'auto-indent-minor-mode)

(autoload 'auto-indent-minor-mode "auto-indent-mode/auto-indent-mode" "\
Auto Indent minor mode.

With no argument, this command toggles the mode.
positive prefix argument turns on the mode.
Negative prefix argument turns off the mode.

When auto-indent-minor-mode minor mode is enabled, yanking or pasting automatically indents

Fall back to default, non-indented yanking by preceding the yanking commands with C-u.

Based on auto-indentation posts, slightly redefined to allow it to be a minor mode

http://www.emacswiki.org/emacs/AutoIndentation

\(fn &optional ARG)" t nil)

(autoload 'auto-indent-minor-mode-on "auto-indent-mode/auto-indent-mode" "\
Turn on auto-indent minor mode.

\(fn)" t nil)

(defvar auto-indent-global-mode nil "\
Non-nil if Auto-Indent-Global mode is enabled.
See the command `auto-indent-global-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `auto-indent-global-mode'.")

(custom-autoload 'auto-indent-global-mode "auto-indent-mode/auto-indent-mode" nil)

(autoload 'auto-indent-global-mode "auto-indent-mode/auto-indent-mode" "\
Toggle Auto-Indent minor mode in all buffers.
With prefix ARG, enable Auto-Indent-Global mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Auto-Indent minor mode is enabled in all buffers where
`auto-indent-minor-mode-on' would do it.
See `auto-indent-minor-mode' for more information on Auto-Indent minor mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (bm-previous-mouse bm-previous bm-next-mouse bm-next
;;;;;;  bm-toggle-mouse bm-toggle) "bm/bm" "bm/bm.el" (20621 19579))
;;; Generated autoloads from bm/bm.el

(autoload 'bm-toggle "bm/bm" "\
Toggle bookmark at point.

\(fn)" t nil)

(autoload 'bm-toggle-mouse "bm/bm" "\
Toggle a bookmark with a mouse click.
EV is the mouse event.

\(fn EV)" t nil)

(autoload 'bm-next "bm/bm" "\
Goto next bookmark.

\(fn)" t nil)

(autoload 'bm-next-mouse "bm/bm" "\
Go to the next bookmark with the scroll wheel.
EV is the mouse event.

\(fn EV)" t nil)

(autoload 'bm-previous "bm/bm" "\
Goto previous bookmark.

\(fn)" t nil)

(autoload 'bm-previous-mouse "bm/bm" "\
Go to the previous bookmark with the scroll wheel.
EV is the mouse event.

\(fn EV)" t nil)

;;;***

;;;### (autoloads (c-eldoc-print-current-symbol-info c-turn-on-eldoc-mode)
;;;;;;  "c-eldoc/c-eldoc" "c-eldoc/c-eldoc.el" (20621 19579))
;;; Generated autoloads from c-eldoc/c-eldoc.el

(autoload 'c-turn-on-eldoc-mode "c-eldoc/c-eldoc" "\
Enable c-eldoc-mode

\(fn)" t nil)

(autoload 'c-eldoc-print-current-symbol-info "c-eldoc/c-eldoc" "\
Returns documentation string for the current symbol.

\(fn)" nil nil)

;;;***

;;;### (autoloads (clojure-enable-slime-on-existing-buffers clojure-jack-in
;;;;;;  clojure-mode) "clojure-mode/clojure-mode" "clojure-mode/clojure-mode.el"
;;;;;;  (20621 19578))
;;; Generated autoloads from clojure-mode/clojure-mode.el

(autoload 'clojure-mode "clojure-mode/clojure-mode" "\
Major mode for editing Clojure code - similar to Lisp mode.
Commands:
Delete converts tabs to spaces as it moves back.
Blank lines separate paragraphs.  Semicolons start comments.
\\{clojure-mode-map}
Note that `run-lisp' may be used either to start an inferior Lisp job
or to switch back to an existing one.

Entry to this mode calls the value of `clojure-mode-hook'
if that value is non-nil.

\(fn)" t nil)

(autoload 'clojure-jack-in "clojure-mode/clojure-mode" "\


\(fn)" t nil)

(autoload 'clojure-enable-slime-on-existing-buffers "clojure-mode/clojure-mode" "\


\(fn)" t nil)

(add-hook 'slime-connected-hook 'clojure-enable-slime-on-existing-buffers)

(put 'clojure-test-ns-segment-position 'safe-local-variable 'integerp)

(put 'clojure-mode-load-command 'safe-local-variable 'stringp)

(put 'clojure-swank-command 'safe-local-variable 'stringp)

(add-hook 'slime-connected-hook 'clojure-enable-slime-on-existing-buffers)

(add-hook 'slime-indentation-update-hooks 'put-clojure-indent)

(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))

(add-to-list 'interpreter-mode-alist '("jark" . clojure-mode))

(add-to-list 'interpreter-mode-alist '("cake" . clojure-mode))

;;;***

;;;### (autoloads (clojure-test-mode) "clojure-mode/clojure-test-mode"
;;;;;;  "clojure-mode/clojure-test-mode.el" (20621 19578))
;;; Generated autoloads from clojure-mode/clojure-test-mode.el

(autoload 'clojure-test-mode "clojure-mode/clojure-test-mode" "\
A minor mode for running Clojure tests.

\(fn &optional ARG)" t nil)

(defun clojure-test-maybe-enable nil "\
Enable clojure-test-mode if the current buffer contains a namespace
with a \"test.\" bit on it." (let ((ns (clojure-find-package))) (when (and ns (string-match "test\\(\\.\\|$\\)" ns)) (save-window-excursion (clojure-test-mode t)))))

(add-hook 'clojure-mode-hook 'clojure-test-maybe-enable)

;;;***

;;;### (autoloads (clojurescript-mode) "clojure-mode/clojurescript-mode"
;;;;;;  "clojure-mode/clojurescript-mode.el" (20621 19578))
;;; Generated autoloads from clojure-mode/clojurescript-mode.el

(autoload 'clojurescript-mode "clojure-mode/clojurescript-mode" "\
Major mode for ClojureScript

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.cljs$" . clojurescript-mode))

;;;***

;;;### (autoloads (ctags-update-minor-mode ctags-update) "ctags-update/ctags-update"
;;;;;;  "ctags-update/ctags-update.el" (20621 19579))
;;; Generated autoloads from ctags-update/ctags-update.el

(autoload 'ctags-update "ctags-update/ctags-update" "\
update TAGS in parent directory using `exuberant-ctags' you
can call this function directly , or enable
`ctags-update-minor-mode' or with prefix `C-u' then you can
generate a new TAGS file in directory

\(fn &optional ARGS)" t nil)

(autoload 'ctags-update-minor-mode "ctags-update/ctags-update" "\
auto update TAGS using `exuberant-ctags' in parent directory.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (ctypes-read-file ctypes-auto-parse-mode ctypes-file
;;;;;;  ctypes-dir ctypes-tags ctypes-all-buffers ctypes-buffer ctypes-define-type-in-mode
;;;;;;  ctypes-define-type) "ctypes/ctypes" "ctypes/ctypes.el" (20621
;;;;;;  19579))
;;; Generated autoloads from ctypes/ctypes.el

(autoload 'ctypes-define-type "ctypes/ctypes" "\
Add a new TYPE to current major mode and inform font-lock.

When preceded by C-u the display is not updated.

Return non-nil if the type was not known before.

\(fn TYPE &optional DELAY-ACTION MODE)" t nil)

(autoload 'ctypes-define-type-in-mode "ctypes/ctypes" "\
Add TYPE to major mode MODE and inform font-lock.

When preceded by C-u the display is not updated.

\(This function is designed for interactive use, please call
`ctypes-define-type' from Lisp programs.)

\(fn TYPE &optional DELAY-ACTION MODE)" t nil)

(autoload 'ctypes-buffer "ctypes/ctypes" "\
Search for types in buffer, inform font-lock if any is found.

When preceded by C-u the action is not performed.

Return non-nil if new types are found.

\(fn &optional BUF DELAY-ACTION MODE)" t nil)

(autoload 'ctypes-all-buffers "ctypes/ctypes" "\
Search for types in all buffers, inform font-lock about all discoveries.

When preceded by C-u the display is not updated.

Return non-nil if new types are found.

\(fn &optional DELAY-ACTION)" t nil)

(autoload 'ctypes-tags "ctypes/ctypes" "\
Search for types in files in the visited TAGS table.
Should no tags table be visited, the user will be prompted for a new.

When preceded by C-u the display is not updated.

Return non-nil if new types are found.

\(fn &optional DELAY-ACTION)" t nil)

(autoload 'ctypes-dir "ctypes/ctypes" "\
Search for types in files in a directory hierarchy.

See variable `ctypes-dir-read-file' for a description of which files
are opened during scanning, and how you can change the behavior.

When preceded by C-u the display is not updated.

Return non-nil if new types are found.

\(fn &optional DIR DELAY-ACTION)" t nil)

(autoload 'ctypes-file "ctypes/ctypes" "\
Search for types in file FILE.
Should FILE not be loaded it is read into a temporary buffer.

Return mode of file, if new types was found.

\(fn FILE &optional DELAY-ACTION)" t nil)

(autoload 'ctypes-auto-parse-mode "ctypes/ctypes" "\
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

(autoload 'ctypes-read-file "ctypes/ctypes" "\
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

;;;### (autoloads (ecb-byte-compile ecb-minor-mode ecb-activate)
;;;;;;  "ecb-snapshot/ecb" "ecb-snapshot/ecb.el" (20621 19579))
;;; Generated autoloads from ecb-snapshot/ecb.el

(autoload 'ecb-activate "ecb-snapshot/ecb" "\
Activates ECB and creates the special buffers for the choosen layout.
For the layout see `ecb-layout-name'. This function raises always the
ECB-frame if called from another frame. This is the same as calling
`ecb-minor-mode' with a positive argument.

\(fn)" t nil)

(autoload 'ecb-minor-mode "ecb-snapshot/ecb" "\
Toggle ECB minor mode.
With prefix argument ARG, turn on if positive, otherwise off. Return non-nil
if the minor mode is enabled.

\\{ecb-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'ecb-byte-compile "ecb-snapshot/ecb" "\
Byte-compiles the ECB package.
This is done for all lisp-files of ECB if FORCE-ALL is not nil or for each
lisp-file FILE.el which is either newer than FILE.elc or if FILE.elc doesn't
exist.

\(fn &optional FORCE-ALL)" t nil)

;;;***

;;;### (autoloads (ecb-show-help) "ecb-snapshot/ecb-help" "ecb-snapshot/ecb-help.el"
;;;;;;  (20621 19579))
;;; Generated autoloads from ecb-snapshot/ecb-help.el

(autoload 'ecb-show-help "ecb-snapshot/ecb-help" "\
Shows the online help of ECB in Info or HTML-format.
The format depends on the setting in `ecb-show-help-format'. If called with
prefix argument, i.e. if FORMAT is not nil then the user is prompted to choose
the format of the help (Info or Html).

If an error about not finding the needed help-file occurs please take a look
at the options `ecb-help-info-start-file' and `ecb-help-html-start-file'!

Note: If you got ECB as a standard XEmacs-package maybe the
HTML-online-documentation is not included.

\(fn &optional FORMAT)" t nil)

;;;***

;;;### (autoloads nil "ecb-snapshot/ecb-util" "ecb-snapshot/ecb-util.el"
;;;;;;  (20621 19579))
;;; Generated autoloads from ecb-snapshot/ecb-util.el

(defconst ecb-running-xemacs (featurep 'xemacs))

;;;***

;;;### (autoloads (egg-minor-mode-find-file-hook egg-minor-mode)
;;;;;;  "egg/egg" "egg/egg.el" (20621 19571))
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

(add-hook 'find-file-hook 'egg-git-dir)

(add-hook 'find-file-hook 'egg-minor-mode-find-file-hook)

;;;***

;;;### (autoloads (egg-grep egg-grep-mode egg-grep-process-setup)
;;;;;;  "egg/egg-grep" "egg/egg-grep.el" (20621 19571))
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

;;;### (autoloads (el-get-checksum el-get-make-recipes el-get-cd
;;;;;;  el-get-self-update el-get-update-all el-get-version) "el-get/el-get"
;;;;;;  "el-get/el-get.el" (20621 19576))
;;; Generated autoloads from el-get/el-get.el

(autoload 'el-get-version "el-get/el-get" "\
Message the current el-get version

\(fn)" t nil)

(autoload 'el-get-update-all "el-get/el-get" "\
Performs update of all installed packages.

\(fn &optional NO-PROMPT)" t nil)

(autoload 'el-get-self-update "el-get/el-get" "\
Update el-get itself.  The standard recipe takes care of reloading the code.

\(fn)" t nil)

(autoload 'el-get-cd "el-get/el-get" "\
Open dired in the package directory.

\(fn PACKAGE)" t nil)

(autoload 'el-get-make-recipes "el-get/el-get" "\
Loop over `el-get-sources' and write a recipe file for each
entry which is not a symbol and is not already a known recipe.

\(fn &optional DIR)" t nil)

(autoload 'el-get-checksum "el-get/el-get" "\
Compute the checksum of the given package, and put it in the kill-ring

\(fn PACKAGE &optional PACKAGE-STATUS-ALIST)" t nil)

;;;***

;;;### (autoloads (el-get-list-packages) "el-get/el-get-list-packages"
;;;;;;  "el-get/el-get-list-packages.el" (20621 19574))
;;; Generated autoloads from el-get/el-get-list-packages.el

(autoload 'el-get-list-packages "el-get/el-get-list-packages" "\
Display a list of packages.

\(fn)" t nil)

;;;***

;;;### (autoloads (elscreen-start) "elscreen/elscreen" "elscreen/elscreen.el"
;;;;;;  (20621 19574))
;;; Generated autoloads from elscreen/elscreen.el

(autoload 'elscreen-start "elscreen/elscreen" "\


\(fn)" t nil)

;;;***

;;;### (autoloads (bookmark-w3m-bookmark-jump) "emacs-w3m/bookmark-w3m"
;;;;;;  "emacs-w3m/bookmark-w3m.el" (20621 19578))
;;; Generated autoloads from emacs-w3m/bookmark-w3m.el

(autoload 'bookmark-w3m-bookmark-jump "emacs-w3m/bookmark-w3m" "\
Default bookmark handler for w3m buffers.

\(fn BOOKMARK)" nil nil)

;;;***

;;;### (autoloads (mime-w3m-preview-text/html) "emacs-w3m/mime-w3m"
;;;;;;  "emacs-w3m/mime-w3m.el" (20621 19578))
;;; Generated autoloads from emacs-w3m/mime-w3m.el

(autoload 'mime-w3m-preview-text/html "emacs-w3m/mime-w3m" "\


\(fn ENTITY SITUATION)" nil nil)

;;;***

;;;### (autoloads (octet-mime-setup mime-view-octet mime-preview-octet
;;;;;;  octet-find-file octet-buffer) "emacs-w3m/octet" "emacs-w3m/octet.el"
;;;;;;  (20621 19578))
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
;;;;;;  (20621 19578))
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

\(fn &optional URL FILENAME NO-CACHE HANDLER POST-DATA)" t nil)

(autoload 'w3m-goto-url "emacs-w3m/w3m" "\
Visit World Wide Web pages.  This is the primitive function of `w3m'.
If the second argument RELOAD is non-nil, reload a content of URL.
Except that if it is 'redisplay, re-display the page without reloading.
The third argument CHARSET specifies a charset to be used for decoding
a content.
The fourth argument POST-DATA should be a string or a cons cell.
If it is a string, it makes this function request a body as if
the content-type is \"x-www-form-urlencoded\".  If it is a cons cell,
the car of a cell is used as the content-type and the cdr of a cell is
used as the body.
If the fifth argument REFERER is specified, it is used for a Referer:
field for this request.
The remaining HANDLER, ELEMENT[1], NO-POPUP, and SAVE-POS[2] are for
the internal operations of emacs-w3m.
You can also use \"quicksearch\" url schemes such as \"gg:emacs\" which
would search for the term \"emacs\" with the Google search engine.
See the `w3m-search' function and the variable `w3m-uri-replace-alist'.

Notes for the developers:
\[1] ELEMENT is a history element which has already been registered in
the `w3m-history-flat' variable.  It is corresponding to URL to be
retrieved at this time, not for the url of the current page.

\[2] SAVE-POS leads this function to save the current emacs-w3m window
configuration; i.e. to run `w3m-history-store-position'.
`w3m-history-store-position' should be called in a w3m-mode buffer, so
this will be convenient if a command that calls this function may be
invoked in other than a w3m-mode buffer.

\(fn URL &optional RELOAD CHARSET POST-DATA REFERER HANDLER ELEMENT NO-POPUP SAVE-POS)" t nil)

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
;;;;;;  "emacs-w3m/w3m-antenna.el" (20621 19578))
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
;;;;;;  "emacs-w3m/w3m-bookmark.el" (20621 19578))
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
;;;;;;  (20621 19578))
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
;;;;;;  "emacs-w3m/w3m-dtree.el" (20621 19578))
;;; Generated autoloads from emacs-w3m/w3m-dtree.el

(autoload 'w3m-about-dtree "emacs-w3m/w3m-dtree" "\


\(fn URL &optional NODECODE ALLFILES &rest ARGS)" nil nil)

(autoload 'w3m-dtree "emacs-w3m/w3m-dtree" "\
Display directory tree on local file system.
If called with 'prefix argument', display all directorys and files.

\(fn ALLFILES PATH)" t nil)

;;;***

;;;### (autoloads (w3m-fb-mode) "emacs-w3m/w3m-fb" "emacs-w3m/w3m-fb.el"
;;;;;;  (20621 19578))
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
;;;;;;  (20621 19578))
;;; Generated autoloads from emacs-w3m/w3m-filter.el

(autoload 'w3m-filter "emacs-w3m/w3m-filter" "\
Apply filtering rule of URL against a content in this buffer.

\(fn URL)" nil nil)

;;;***

;;;### (autoloads (w3m-fontify-forms) "emacs-w3m/w3m-form" "emacs-w3m/w3m-form.el"
;;;;;;  (20621 19578))
;;; Generated autoloads from emacs-w3m/w3m-form.el

(autoload 'w3m-fontify-forms "emacs-w3m/w3m-form" "\
Process half-dumped data and fontify forms in this buffer.

\(fn)" nil nil)

;;;***

;;;### (autoloads (w3m-lnum-bookmark-add-this-url w3m-lnum-download-this-url
;;;;;;  w3m-lnum-print-this-url w3m-lnum-edit-this-url w3m-lnum-external-view-this-url
;;;;;;  w3m-lnum-save-image w3m-lnum-view-image w3m-lnum-toggle-inline-image
;;;;;;  w3m-lnum-universal w3m-lnum-follow w3m-lnum-goto w3m-lnum-mode)
;;;;;;  "emacs-w3m/w3m-lnum" "emacs-w3m/w3m-lnum.el" (20621 19578))
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
;;;;;;  "emacs-w3m/w3m-namazu.el" (20621 19578))
;;; Generated autoloads from emacs-w3m/w3m-namazu.el

(autoload 'w3m-about-namazu "emacs-w3m/w3m-namazu" "\


\(fn URL &optional NO-DECODE NO-CACHE &rest ARGS)" nil nil)

(autoload 'w3m-namazu "emacs-w3m/w3m-namazu" "\
Search indexed files with Namazu.

\(fn INDEX QUERY &optional RELOAD)" t nil)

;;;***

;;;### (autoloads (w3m-perldoc w3m-about-perldoc) "emacs-w3m/w3m-perldoc"
;;;;;;  "emacs-w3m/w3m-perldoc.el" (20621 19578))
;;; Generated autoloads from emacs-w3m/w3m-perldoc.el

(autoload 'w3m-about-perldoc "emacs-w3m/w3m-perldoc" "\


\(fn URL &optional NO-DECODE NO-CACHE &rest ARGS)" nil nil)

(autoload 'w3m-perldoc "emacs-w3m/w3m-perldoc" "\
View Perl documents.

\(fn DOCNAME)" t nil)

;;;***

;;;### (autoloads (w3m-search-uri-replace w3m-search-new-session
;;;;;;  w3m-search) "emacs-w3m/w3m-search" "emacs-w3m/w3m-search.el"
;;;;;;  (20621 19578))
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
;;;;;;  (20621 19578))
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
;;;;;;  (20621 19578))
;;; Generated autoloads from emacs-w3m/w3m-symbol.el

(autoload 'w3m-replace-symbol "emacs-w3m/w3m-symbol" "\


\(fn)" nil nil)

;;;***

;;;### (autoloads (w3m-about-weather w3m-weather) "emacs-w3m/w3m-weather"
;;;;;;  "emacs-w3m/w3m-weather.el" (20621 19578))
;;; Generated autoloads from emacs-w3m/w3m-weather.el

(autoload 'w3m-weather "emacs-w3m/w3m-weather" "\
Display weather report.

\(fn AREA)" t nil)

(autoload 'w3m-about-weather "emacs-w3m/w3m-weather" "\


\(fn URL NO-DECODE NO-CACHE POST-DATA REFERER HANDLER)" nil nil)

;;;***

;;;### (autoloads nil "evil/evil-core" "evil/evil-core.el" (20621
;;;;;;  19571))
;;; Generated autoloads from evil/evil-core.el
 (autoload 'evil-mode "evil" "Toggle evil in all buffers" t)

;;;***

;;;### (autoloads (er/expand-region) "expand-region/expand-region"
;;;;;;  "expand-region/expand-region.el" (20621 19574))
;;; Generated autoloads from expand-region/expand-region.el

(autoload 'er/expand-region "expand-region/expand-region" "\
Increase selected region by semantic units.

With prefix argument expands the region that many times.
If prefix argument is negative calls `er/contract-region'.
If prefix argument is 0 it resets point and mark to their state
before calling `er/expand-region' for the first time.

\(fn ARG)" t nil)

;;;***

;;;### (autoloads (expand-region-exclude-text-mode-expansions expand-region-reset-fast-key
;;;;;;  expand-region-contract-fast-key expand-region-fast-keys-enabled
;;;;;;  expand-region-skip-whitespace expand-region-autocopy-register
;;;;;;  expand-region-guess-python-mode expand-region-preferred-python-mode
;;;;;;  expand-region) "expand-region/expand-region-custom" "expand-region/expand-region-custom.el"
;;;;;;  (20621 19574))
;;; Generated autoloads from expand-region/expand-region-custom.el

(let ((loads (get 'expand-region 'custom-loads))) (if (member '"expand-region/expand-region-custom" loads) nil (put 'expand-region 'custom-loads (cons '"expand-region/expand-region-custom" loads))))

(defvar expand-region-preferred-python-mode 'python "\
The name of your preferred python mode")

(custom-autoload 'expand-region-preferred-python-mode "expand-region/expand-region-custom" t)

(defvar expand-region-guess-python-mode t "\
If expand-region should attempt to guess your preferred python mode")

(custom-autoload 'expand-region-guess-python-mode "expand-region/expand-region-custom" t)

(defvar expand-region-autocopy-register "" "\
If set to a string of a single character (try \"e\"), then the
contents of the most recent expand or contract command will
always be copied to the register named after that character.")

(custom-autoload 'expand-region-autocopy-register "expand-region/expand-region-custom" t)

(defvar expand-region-skip-whitespace t "\
If expand-region should skip past whitespace on initial expansion")

(custom-autoload 'expand-region-skip-whitespace "expand-region/expand-region-custom" t)

(defvar expand-region-fast-keys-enabled t "\
If expand-region should bind fast keys after initial expand/contract")

(custom-autoload 'expand-region-fast-keys-enabled "expand-region/expand-region-custom" t)

(defvar expand-region-contract-fast-key "-" "\
Key to use after an initial expand/contract to contract once more.")

(custom-autoload 'expand-region-contract-fast-key "expand-region/expand-region-custom" t)

(defvar expand-region-reset-fast-key "0" "\
Key to use after an initial expand/contract to undo.")

(custom-autoload 'expand-region-reset-fast-key "expand-region/expand-region-custom" t)

(defvar expand-region-exclude-text-mode-expansions '(html-mode nxml-mode) "\
List of modes which derive from `text-mode' for which text mode expansions are not appropriate.")

(custom-autoload 'expand-region-exclude-text-mode-expansions "expand-region/expand-region-custom" t)

;;;***

;;;### (autoloads (flymake-shell-load) "flymake-shell/flymake-shell"
;;;;;;  "flymake-shell/flymake-shell.el" (20621 19573))
;;; Generated autoloads from flymake-shell/flymake-shell.el

(autoload 'flymake-shell-load "flymake-shell/flymake-shell" "\
Configure flymake mode to check the current buffer's shell-script syntax.

This function is designed to be called in `sh-set-shell-hook'; it
does not alter flymake's global configuration, so function
`flymake-mode' alone will not suffice.

\(fn)" t nil)

;;;***

;;;### (autoloads (git-reblame git-blame-mode) "git-emacs/git-blame"
;;;;;;  "git-emacs/git-blame.el" (20621 19572))
;;; Generated autoloads from git-emacs/git-blame.el

(autoload 'git-blame-mode "git-emacs/git-blame" "\
Toggle minor mode for displaying Git blame

With prefix ARG, turn the mode on if ARG is positive.

\(fn &optional ARG)" t nil)

(autoload 'git-reblame "git-emacs/git-blame" "\
Recalculate all blame information in the current buffer

\(fn)" t nil)

;;;***

;;;### (autoloads (godoc gofmt-before-save gofmt go-mode) "go-mode/go-mode"
;;;;;;  "go-mode/go-mode.el" (20670 56138))
;;; Generated autoloads from go-mode/go-mode.el

(autoload 'go-mode "go-mode/go-mode" "\
Major mode for editing Go source text.

This provides basic syntax highlighting for keywords, built-ins,
functions, and some types.  It also provides indentation that is
\(almost) identical to gofmt.

\(fn)" t nil)

(add-to-list 'auto-mode-alist (cons "\\.go$" #'go-mode))

(autoload 'gofmt "go-mode/go-mode" "\
Pipe the current buffer through the external tool `gofmt`.
Replace the current buffer on success; display errors on failure.

\(fn)" t nil)

(autoload 'gofmt-before-save "go-mode/go-mode" "\
Add this to .emacs to run gofmt on the current buffer when saving:
 (add-hook 'before-save-hook #'gofmt-before-save)

\(fn)" t nil)

(autoload 'godoc "go-mode/go-mode" "\
Show go documentation for a query, much like M-x man.

\(fn QUERY)" t nil)

;;;***

;;;### (autoloads (org-google-weather) "google-weather/org-google-weather"
;;;;;;  "google-weather/org-google-weather.el" (20621 19578))
;;; Generated autoloads from google-weather/org-google-weather.el

(autoload 'org-google-weather "google-weather/org-google-weather" "\
Return Org entry with the weather for LOCATION in LANGUAGE.
If LOCATION is not set, use org-google-weather-location.

\(fn &optional LOCATION LANGUAGE)" nil nil)

;;;***

;;;### (autoloads (guess-style-guess-all guess-style-guess-variable
;;;;;;  guess-style-set-variable) "guess-style/guess-style" "guess-style/guess-style.el"
;;;;;;  (20621 19579))
;;; Generated autoloads from guess-style/guess-style.el

(autoload 'guess-style-set-variable "guess-style/guess-style" "\
Override VARIABLE's guessed value for future guesses.
If FILE is a directory, the variable will be overridden for the entire
directory, unless single files are later overridden.
If called interactively, the current buffer's file name will be used for FILE.
With a prefix argument a directory name may be entered.

\(fn VARIABLE VALUE FILE)" t nil)

(autoload 'guess-style-guess-variable "guess-style/guess-style" "\
Guess a value for VARIABLE according to `guess-style-guesser-alist'.
If GUESSER is set, it's used instead of the default.

\(fn VARIABLE &optional GUESSER)" nil nil)

(autoload 'guess-style-guess-all "guess-style/guess-style" "\
Guess all variables in `guess-style-guesser-alist'.
Special care is taken so no guesser is called twice.

\(fn)" t nil)

;;;***

;;;### (autoloads (turn-on-ctags-auto-update-mode ctags-auto-update-mode
;;;;;;  ctags-update) "helm-etags-plus/ctags-update" "helm-etags-plus/ctags-update.el"
;;;;;;  (20621 19578))
;;; Generated autoloads from helm-etags-plus/ctags-update.el

(autoload 'ctags-update "helm-etags-plus/ctags-update" "\
update TAGS in parent directory using `exuberant-ctags'.
1. you can call this function directly,
2. enable `ctags-auto-update-mode',
3. with prefix `C-u' then you can generate a new TAGS file in directory,
4. with prefix `C-uC-u' save the command to king-ring instead of execute it.

\(fn &optional ARGS)" t nil)

(autoload 'ctags-auto-update-mode "helm-etags-plus/ctags-update" "\
auto update TAGS using `exuberant-ctags' in parent directory.

\(fn &optional ARG)" t nil)

(autoload 'turn-on-ctags-auto-update-mode "helm-etags-plus/ctags-update" "\
turn on `ctags-auto-update-mode'.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-etags+-history helm-etags+-history-go-forward
;;;;;;  helm-etags+-history-go-back helm-etags+-select-one-key helm-etags+-select-at-point
;;;;;;  helm-etags+-select) "helm-etags-plus/helm-etags+" "helm-etags-plus/helm-etags+.el"
;;;;;;  (20621 19578))
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

;;;### (autoloads (htmlize-many-files-dired htmlize-many-files htmlize-file
;;;;;;  htmlize-region htmlize-buffer) "htmlize/htmlize" "htmlize/htmlize.el"
;;;;;;  (20621 19573))
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
;;;;;;  hungry-delete-backward hungry-delete-forward) "hungry-delete/hungry-delete"
;;;;;;  "hungry-delete/hungry-delete.el" (20621 19580))
;;; Generated autoloads from hungry-delete/hungry-delete.el

(autoload 'hungry-delete-forward "hungry-delete/hungry-delete" "\
Delete the following character or all following whitespace up
to the next non-whitespace character.  See
\\[c-hungry-delete-backward].

\(fn)" t nil)

(autoload 'hungry-delete-backward "hungry-delete/hungry-delete" "\
Delete the preceding character or all preceding whitespace
back to the previous non-whitespace character.  See also
\\[c-hungry-delete-forward].

\(fn)" t nil)

(autoload 'hungry-delete-mode "hungry-delete/hungry-delete" "\
Minor mode to enable hungry deletion.  This will delete all
whitespace after or before point when the deletion command is
executed.

\(fn &optional ARG)" t nil)

(autoload 'turn-on-hungry-delete-mode "hungry-delete/hungry-delete" "\
Turns on hungry delete mode if the buffer is appropriate.

\(fn)" nil nil)

;;;***

;;;### (autoloads (iedit-rectangle-mode iedit-mode-on-function iedit-mode)
;;;;;;  "iedit/iedit" "iedit/iedit.el" (20621 19572))
;;; Generated autoloads from iedit/iedit.el

(autoload 'iedit-mode "iedit/iedit" "\
Toggle Iedit mode.
This command behaves differently, depending on the mark, point,
prefix argument and variable `iedit-transient-mark-sensitive'.

If Iedit mode is off, turn Iedit mode on.

When Iedit mode is turned on, all the occurrences of the current
region are highlighted.  If one occurrence is modified, the
change are propagated to all other occurrences simultaneously.

If region is not active, the current symbol (returns from
`current-word') is used as the occurrence by default.  The
occurrences of the current symbol, but not include occurrences
that are part of other symbols, are highlighted.  If you still
want to match all the occurrences, even though they are parts of
other symbols, you may have to mark the symbol first.

In the above two situations, with digit prefix argument 0, only
occurrences in current function are matched.  This is good for
renaming refactoring in programming.

You can also switch to Iedit mode from isearch mode directly. The
current search string is used as occurrence.  All occurrences of
the current search string are highlighted.

With an universal prefix argument, the occurrence when Iedit mode
is turned off last time in current buffer is used as occurrence.
This is intended to recover last Iedit mode which is turned off.
If region active, Iedit mode is limited within the current
region.

With repeated universal prefix argument, the occurrence when
Iedit mode is turned off last time (might be in other buffer) is used
as occurrence.  If region active, Iedit mode is limited within
the current region.

If Iedit mode is on and region is active, Iedit mode is
restricted in the region, e.g. the occurrences outside of the region
is excluded.

If Iedit mode is on and region is active, with an universal
prefix argument, Iedit mode is restricted outside of the region,
e.g. the occurrences in the region is excluded.

Turn off Iedit mode in other situations.

Commands:
\\{iedit-current-keymap}

\(fn &optional ARG)" t nil)

(autoload 'iedit-mode-on-function "iedit/iedit" "\
Toggle Iedit mode on current function.

\(fn)" t nil)

(autoload 'iedit-rectangle-mode "iedit/iedit" "\
Toggle iedit-RECT mode.

\(fn)" t nil)

;;;***

;;;### (autoloads (jabber-info jabber-customize jabber-debug-keep-process-buffers
;;;;;;  jabber-debug-log-xml jabber-default-priority jabber-default-status
;;;;;;  jabber-default-show jabber-account-list) "jabber/jabber"
;;;;;;  "jabber/jabber.el" (20621 19578))
;;; Generated autoloads from jabber/jabber.el

(defvar jabber-account-list nil "\
List of Jabber accounts.
Each element of the list is a cons cell describing a Jabber account,
where the car is a JID and the CDR is an alist.

JID is a full Jabber ID string (e.g. foo@bar.tld). You can also
specify the resource (e.g. foo@bar.tld/emacs).
The following keys can be present in the alist:
:password is a string to authenticate ourself against the server.
It can be empty.
:network-server is a string identifying the address to connect to,
if it's different from the server part of the JID.
:port is the port to use (default depends on connection type).
:connection-type is a symbol. Valid symbols are `starttls',
`network' and `ssl'.

Only JID is mandatory.  The rest can be guessed at run-time.

Examples:

Two accounts without any special configuration:
\((\"foo@example.com\") (\"bar@example.net\"))

One disabled account with a non-standard port:
\((\"romeo@montague.net\" (:port . 5242) (:disabled . t)))

If you don't have SRV and STARTTLS capabilities in your Emacs,
configure a Google Talk account like this:
\((\"username@gmail.com\"
  (:network-server . \"talk.google.com\")
  (:connection-type . ssl)))")

(custom-autoload 'jabber-account-list "jabber/jabber" t)

(defvar jabber-default-show "" "\
default show state")

(custom-autoload 'jabber-default-show "jabber/jabber" t)

(defvar jabber-default-status "" "\
default status string")

(custom-autoload 'jabber-default-status "jabber/jabber" t)

(defvar jabber-default-priority 10 "\
default priority")

(custom-autoload 'jabber-default-priority "jabber/jabber" t)

(defvar *jabber-current-status* nil "\
the users current presence status")

(defvar *jabber-current-show* nil "\
the users current presence show")

(defvar *jabber-current-priority* nil "\
the user's current priority")

(defvar jabber-debug-log-xml nil "\
log all XML i/o in *-jabber-xml-log-JID-*")

(custom-autoload 'jabber-debug-log-xml "jabber/jabber" t)

(defvar jabber-debug-keep-process-buffers nil "\
If nil, kill process buffers when the process dies.
Contents of process buffers might be useful for debugging.")

(custom-autoload 'jabber-debug-keep-process-buffers "jabber/jabber" t)

(defconst jabber-presence-faces '(("" . jabber-roster-user-online) ("away" . jabber-roster-user-away) ("xa" . jabber-roster-user-xa) ("dnd" . jabber-roster-user-dnd) ("chat" . jabber-roster-user-chatty) ("error" . jabber-roster-user-error) (nil . jabber-roster-user-offline)) "\
Mapping from presence types to faces")

(autoload 'jabber-customize "jabber/jabber" "\
customize jabber options

\(fn)" t nil)

(autoload 'jabber-info "jabber/jabber" "\
open jabber.el manual

\(fn)" t nil)

;;;***

;;;### (autoloads (jabber-activity-mode) "jabber/jabber-activity"
;;;;;;  "jabber/jabber-activity.el" (20621 19578))
;;; Generated autoloads from jabber/jabber-activity.el

(defvar jabber-activity-mode t "\
Non-nil if Jabber-Activity mode is enabled.
See the command `jabber-activity-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `jabber-activity-mode'.")

(custom-autoload 'jabber-activity-mode "jabber/jabber-activity" nil)

(autoload 'jabber-activity-mode "jabber/jabber-activity" "\
Toggle display of activity in hidden jabber buffers in the mode line.

With a numeric arg, enable this display if arg is positive.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (jabber-autoaway-start) "jabber/jabber-autoaway"
;;;;;;  "jabber/jabber-autoaway.el" (20621 19578))
;;; Generated autoloads from jabber/jabber-autoaway.el

(autoload 'jabber-autoaway-start "jabber/jabber-autoaway" "\
Start autoaway timer.
The IGNORED argument is there so you can put this function in
`jabber-post-connect-hooks'.

\(fn &optional IGNORED)" t nil)

;;;***

;;;### (autoloads (jabber-edit-bookmarks jabber-get-bookmarks-from-cache
;;;;;;  jabber-get-bookmarks jabber-parse-conference-bookmark jabber-get-conference-data)
;;;;;;  "jabber/jabber-bookmarks" "jabber/jabber-bookmarks.el" (20621
;;;;;;  19578))
;;; Generated autoloads from jabber/jabber-bookmarks.el

(autoload 'jabber-get-conference-data "jabber/jabber-bookmarks" "\
Get bookmark data for CONFERENCE-JID.
KEY may be nil or one of :name, :autojoin, :nick and :password.
If KEY is nil, a plist containing the above keys is returned.
CONT is called when the result is available, with JC and the
result as arguments.  If CONT is nil, return the requested data
immediately, and return nil if it is not in the cache.

\(fn JC CONFERENCE-JID CONT &optional KEY)" nil nil)

(autoload 'jabber-parse-conference-bookmark "jabber/jabber-bookmarks" "\
Convert a <conference/> tag into a plist.
The plist may contain the keys :jid, :name, :autojoin,
:nick and :password.

\(fn NODE)" nil nil)

(autoload 'jabber-get-bookmarks "jabber/jabber-bookmarks" "\
Retrieve bookmarks (if needed) and call CONT.
Arguments to CONT are JC and the bookmark list.  CONT will be
called as the result of a filter function or a timer.
If REFRESH is non-nil, always fetch bookmarks.

\(fn JC CONT &optional REFRESH)" nil nil)

(autoload 'jabber-get-bookmarks-from-cache "jabber/jabber-bookmarks" "\
Return cached bookmarks for JC.
If bookmarks have not yet been fetched by `jabber-get-bookmarks',
return nil.

\(fn JC)" nil nil)

(autoload 'jabber-edit-bookmarks "jabber/jabber-bookmarks" "\
Create a buffer for editing bookmarks interactively.

\(fn JC)" t nil)

;;;***

;;;### (autoloads (jabber-chat-get-buffer) "jabber/jabber-chat" "jabber/jabber-chat.el"
;;;;;;  (20621 19578))
;;; Generated autoloads from jabber/jabber-chat.el

(defvar jabber-chatting-with nil "\
JID of the person you are chatting with")

(autoload 'jabber-chat-get-buffer "jabber/jabber-chat" "\
Return the chat buffer for chatting with CHAT-WITH (bare or full JID).
Either a string or a buffer is returned, so use `get-buffer' or
`get-buffer-create'.

\(fn CHAT-WITH)" nil nil)

;;;***

;;;### (autoloads nil "jabber/jabber-chatbuffer" "jabber/jabber-chatbuffer.el"
;;;;;;  (20621 19578))
;;; Generated autoloads from jabber/jabber-chatbuffer.el

(defvar jabber-buffer-connection nil "\
The connection used by this buffer.")

(make-variable-buffer-local 'jabber-buffer-connection)

;;;***

;;;### (autoloads (jabber-compose) "jabber/jabber-compose" "jabber/jabber-compose.el"
;;;;;;  (20621 19578))
;;; Generated autoloads from jabber/jabber-compose.el

(autoload 'jabber-compose "jabber/jabber-compose" "\
Create a buffer for composing a Jabber message.

\(fn JC &optional RECIPIENT)" t nil)

;;;***

;;;### (autoloads nil "jabber/jabber-core" "jabber/jabber-core.el"
;;;;;;  (20621 19578))
;;; Generated autoloads from jabber/jabber-core.el
 (autoload 'jabber-connect-all "jabber" "Connect to all configured Jabber accounts.\nSee `jabber-account-list'.\nIf no accounts are configured (or ARG supplied), call `jabber-connect' interactively." t)
 (autoload 'jabber-connect "jabber" "Connect to the Jabber server and start a Jabber XML stream.\nWith prefix argument, register a new account.\nWith double prefix argument, specify more connection details." t)

;;;***

;;;### (autoloads (jabber-import-roster jabber-export-roster) "jabber/jabber-export"
;;;;;;  "jabber/jabber-export.el" (20621 19578))
;;; Generated autoloads from jabber/jabber-export.el

(autoload 'jabber-export-roster "jabber/jabber-export" "\
Export roster for connection JC.

\(fn JC)" t nil)

(autoload 'jabber-import-roster "jabber/jabber-export" "\
Create buffer for roster import for connection JC from FILE.

\(fn JC FILE)" t nil)

;;;***

;;;### (autoloads (jabber-gmail-query jabber-gmail-subscribe) "jabber/jabber-gmail"
;;;;;;  "jabber/jabber-gmail.el" (20621 19578))
;;; Generated autoloads from jabber/jabber-gmail.el

(autoload 'jabber-gmail-subscribe "jabber/jabber-gmail" "\
Subscribe to gmail notifications.
See http://code.google.com/apis/talk/jep_extensions/usersettings.html#4

\(fn JC)" t nil)

(autoload 'jabber-gmail-query "jabber/jabber-gmail" "\
Request mail information from the Google Talk server (a.k.a. one shot query).
See http://code.google.com/apis/talk/jep_extensions/gmail.html#requestmail

\(fn JC)" t nil)

;;;***

;;;### (autoloads (jabber-whitespace-ping-start jabber-whitespace-ping-interval
;;;;;;  jabber-keepalive-start jabber-keepalive-timeout jabber-keepalive-interval
;;;;;;  jabber-keepalive) "jabber/jabber-keepalive" "jabber/jabber-keepalive.el"
;;;;;;  (20621 19578))
;;; Generated autoloads from jabber/jabber-keepalive.el

(let ((loads (get 'jabber-keepalive 'custom-loads))) (if (member '"jabber/jabber-keepalive" loads) nil (put 'jabber-keepalive 'custom-loads (cons '"jabber/jabber-keepalive" loads))))

(defvar jabber-keepalive-interval 600 "\
Interval in seconds between connection checks.")

(custom-autoload 'jabber-keepalive-interval "jabber/jabber-keepalive" t)

(defvar jabber-keepalive-timeout 20 "\
Seconds to wait for response from server.")

(custom-autoload 'jabber-keepalive-timeout "jabber/jabber-keepalive" t)

(autoload 'jabber-keepalive-start "jabber/jabber-keepalive" "\
Activate keepalive.
That is, regularly send a ping request to the server, and
disconnect if it doesn't answer.  See `jabber-keepalive-interval'
and `jabber-keepalive-timeout'.

The JC argument makes it possible to add this function to
`jabber-post-connect-hooks'; it is ignored.  Keepalive is activated
for all accounts regardless of the argument.

\(fn &optional JC)" t nil)

(defvar jabber-whitespace-ping-interval 30 "\
Send a space character to the server with this interval, in seconds.

This is a traditional remedy for a number of problems: to keep NAT
boxes from considering the connection dead, to have the OS discover
earlier that the connection is lost, and to placate servers which rely
on the client doing this, e.g. Openfire.

If you want to verify that the server is able to answer, see
`jabber-keepalive-start' for another mechanism.")

(custom-autoload 'jabber-whitespace-ping-interval "jabber/jabber-keepalive" t)

(autoload 'jabber-whitespace-ping-start "jabber/jabber-keepalive" "\
Start sending whitespace pings at regular intervals.
See `jabber-whitespace-ping-interval'.

The JC argument is ignored; whitespace pings are enabled for all
accounts.

\(fn &optional JC)" t nil)

;;;***

;;;### (autoloads nil "jabber/jabber-keymap" "jabber/jabber-keymap.el"
;;;;;;  (20621 19578))
;;; Generated autoloads from jabber/jabber-keymap.el

(defvar jabber-global-keymap (let ((map (make-sparse-keymap))) (define-key map "" 'jabber-connect-all) (define-key map "" 'jabber-disconnect) (define-key map "" 'jabber-switch-to-roster-buffer) (define-key map "\n" 'jabber-chat-with) (define-key map "\f" 'jabber-activity-switch-to) (define-key map "" 'jabber-send-away-presence) (define-key map "" 'jabber-send-default-presence) (define-key map "" 'jabber-send-xa-presence) (define-key map "" 'jabber-send-presence) map) "\
Global Jabber keymap (usually under C-x C-j)")

(define-key ctl-x-map "\n" jabber-global-keymap)

;;;***

;;;### (autoloads (jabber-display-menu) "jabber/jabber-menu" "jabber/jabber-menu.el"
;;;;;;  (20621 19578))
;;; Generated autoloads from jabber/jabber-menu.el

(defvar jabber-menu (let ((map (make-sparse-keymap "jabber-menu"))) (define-key map [jabber-menu-connect] '("Connect" . jabber-connect-all)) (define-key map [jabber-menu-disconnect] '("Disconnect" . jabber-disconnect)) (define-key map [jabber-menu-roster] '("Switch to roster" . jabber-switch-to-roster-buffer)) (define-key map [jabber-menu-customize] '("Customize" . jabber-customize)) (define-key map [jabber-menu-info] '("Help" . jabber-info)) (define-key map [jabber-menu-status] (cons "Set Status" (make-sparse-keymap "set-status"))) (define-key map [jabber-menu-status jabber-menu-status-chat] '("Chatty" lambda nil (interactive) (jabber-send-presence "chat" (jabber-read-with-input-method "status message: " *jabber-current-status* '*jabber-status-history*) *jabber-current-priority*))) (define-key map [jabber-menu-status jabber-menu-status-dnd] '("Do not Disturb" lambda nil (interactive) (jabber-send-presence "dnd" (jabber-read-with-input-method "status message: " *jabber-current-status* '*jabber-status-history*) *jabber-current-priority*))) (define-key map [jabber-menu-status jabber-menu-status-xa] '("Extended Away" . jabber-send-xa-presence)) (define-key map [jabber-menu-status jabber-menu-status-away] '("Away" . jabber-send-away-presence)) (define-key map [jabber-menu-status jabber-menu-status-online] '("Online" . jabber-send-default-presence)) map))

(defvar jabber-display-menu 'maybe "\
Decide whether the \"Jabber\" menu is displayed in the menu bar.
If t, always display.
If nil, never display.
If maybe, display if any of `jabber-account-list' or `jabber-connections'
is non-nil.")

(custom-autoload 'jabber-display-menu "jabber/jabber-menu" t)

(define-key-after (lookup-key global-map [menu-bar]) [jabber-menu] (list 'menu-item "Jabber" jabber-menu :visible '(or (eq jabber-display-menu t) (and (eq jabber-display-menu 'maybe) (or jabber-account-list (bound-and-true-p jabber-connections))))))

;;;***

;;;### (autoloads (jabber-muc-private-message-p jabber-muc-sender-p
;;;;;;  jabber-muc-message-p jabber-muc-private-get-buffer jabber-muc-get-buffer
;;;;;;  jabber-muc-autojoin jabber-muc-default-nicknames) "jabber/jabber-muc"
;;;;;;  "jabber/jabber-muc.el" (20621 19578))
;;; Generated autoloads from jabber/jabber-muc.el

(defvar *jabber-active-groupchats* nil "\
alist of groupchats and nicknames
Keys are strings, the bare JID of the room.
Values are strings.")

(defvar jabber-muc-default-nicknames nil "\
Default nickname for specific MUC rooms.")

(custom-autoload 'jabber-muc-default-nicknames "jabber/jabber-muc" t)

(defvar jabber-muc-autojoin nil "\
List of MUC rooms to automatically join on connection.
This list is saved in your Emacs customizations.  You can also store
such a list on the Jabber server, where it is available to every
client; see `jabber-edit-bookmarks'.")

(custom-autoload 'jabber-muc-autojoin "jabber/jabber-muc" t)

(defvar jabber-muc-printers 'nil "\
List of functions that may be able to print part of a MUC message.
This gets prepended to `jabber-chat-printers', which see.")

(autoload 'jabber-muc-get-buffer "jabber/jabber-muc" "\
Return the chat buffer for chatroom GROUP.
Either a string or a buffer is returned, so use `get-buffer' or
`get-buffer-create'.

\(fn GROUP)" nil nil)

(autoload 'jabber-muc-private-get-buffer "jabber/jabber-muc" "\
Return the chat buffer for private chat with NICKNAME in GROUP.
Either a string or a buffer is returned, so use `get-buffer' or
`get-buffer-create'.

\(fn GROUP NICKNAME)" nil nil)

(autoload 'jabber-muc-message-p "jabber/jabber-muc" "\
Return non-nil if MESSAGE is a groupchat message.
That does not include private messages in a groupchat, but does
include groupchat invites.

\(fn MESSAGE)" nil nil)

(autoload 'jabber-muc-sender-p "jabber/jabber-muc" "\
Return non-nil if JID is a full JID of an MUC participant.

\(fn JID)" nil nil)

(autoload 'jabber-muc-private-message-p "jabber/jabber-muc" "\
Return non-nil if MESSAGE is a private message in a groupchat.

\(fn MESSAGE)" nil nil)

;;;***

;;;### (autoloads (jabber-muc-looks-like-personal-p) "jabber/jabber-muc-nick-completion"
;;;;;;  "jabber/jabber-muc-nick-completion.el" (20621 19578))
;;; Generated autoloads from jabber/jabber-muc-nick-completion.el

(autoload 'jabber-muc-looks-like-personal-p "jabber/jabber-muc-nick-completion" "\
Return non-nil if jabber MESSAGE is addresed to me.
Optional argument GROUP to look.

\(fn MESSAGE &optional GROUP)" nil nil)

;;;***

;;;### (autoloads (jabber-send-default-presence jabber-send-presence)
;;;;;;  "jabber/jabber-presence" "jabber/jabber-presence.el" (20621
;;;;;;  19578))
;;; Generated autoloads from jabber/jabber-presence.el

(autoload 'jabber-send-presence "jabber/jabber-presence" "\
Set presence for all accounts.

\(fn SHOW STATUS PRIORITY)" t nil)

(autoload 'jabber-send-default-presence "jabber/jabber-presence" "\
Send default presence.
Default presence is specified by `jabber-default-show',
`jabber-default-status', and `jabber-default-priority'.

\(fn &optional IGNORE)" t nil)

;;;***

;;;### (autoloads (jabber-private-set jabber-private-get) "jabber/jabber-private"
;;;;;;  "jabber/jabber-private.el" (20621 19578))
;;; Generated autoloads from jabber/jabber-private.el

(autoload 'jabber-private-get "jabber/jabber-private" "\
Retrieve an item from private XML storage.
The item to retrieve is identified by NODE-NAME (a symbol) and
NAMESPACE (a string).

On success, SUCCESS-CALLBACK is called with JC and the retrieved
XML fragment.

On error, ERROR-CALLBACK is called with JC and the entire IQ
result.

\(fn JC NODE-NAME NAMESPACE SUCCESS-CALLBACK ERROR-CALLBACK)" nil nil)

(autoload 'jabber-private-set "jabber/jabber-private" "\
Store FRAGMENT in private XML storage.
SUCCESS-CALLBACK, SUCCESS-CLOSURE-DATA, ERROR-CALLBACK and
ERROR-CLOSURE-DATA are used as in `jabber-send-iq'.

\(fn JC FRAGMENT &optional SUCCESS-CALLBACK SUCCESS-CLOSURE-DATA ERROR-CALLBACK ERROR-CLOSURE-DATA)" nil nil)

;;;***

;;;### (autoloads (jabber-roster-update jabber-switch-to-roster-buffer)
;;;;;;  "jabber/jabber-roster" "jabber/jabber-roster.el" (20621 19578))
;;; Generated autoloads from jabber/jabber-roster.el

(autoload 'jabber-switch-to-roster-buffer "jabber/jabber-roster" "\
Switch to roster buffer.
Optional JC argument is ignored; it's there so this function can
be used in `jabber-post-connection-hooks'.

\(fn &optional JC)" t nil)

(autoload 'jabber-roster-update "jabber/jabber-roster" "\
Update roster, in memory and on display.
Add NEW-ITEMS, update CHANGED-ITEMS and remove DELETED-ITEMS, all
three being lists of JID symbols.

\(fn JC NEW-ITEMS CHANGED-ITEMS DELETED-ITEMS)" nil nil)

;;;***

;;;### (autoloads (js2-imenu-extras-setup) "js2-mode/js2-imenu-extras"
;;;;;;  "js2-mode/js2-imenu-extras.el" (20621 19572))
;;; Generated autoloads from js2-mode/js2-imenu-extras.el

(autoload 'js2-imenu-extras-setup "js2-mode/js2-imenu-extras" "\


\(fn)" nil nil)

;;;***

;;;### (autoloads (js2-mode) "js2-mode/js2-mode" "js2-mode/js2-mode.el"
;;;;;;  (20621 19573))
;;; Generated autoloads from js2-mode/js2-mode.el
 (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(autoload 'js2-mode "js2-mode/js2-mode" "\
Major mode for editing JavaScript code.

\\{js2-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads (key-chord-define key-chord-define-global key-chord-mode)
;;;;;;  "key-chord/key-chord" "key-chord/key-chord.el" (20621 19571))
;;; Generated autoloads from key-chord/key-chord.el

(autoload 'key-chord-mode "key-chord/key-chord" "\
Toggle key chord mode.
With positive ARG enable the mode. With zero or negative arg disable the mode.
A key chord is two keys that are pressed simultaneously, or one key quickly
pressed twice.
See functions `key-chord-define-global' or `key-chord-define'
and variables `key-chord-two-keys-delay' and `key-chord-one-key-delay'.

\(fn ARG)" t nil)

(autoload 'key-chord-define-global "key-chord/key-chord" "\
Define a key-chord of two keys in KEYS starting a COMMAND.

KEYS can be a string or a vector of two elements. Currently only elements
that corresponds to ascii codes in the range 32 to 126 can be used.

COMMAND can be an interactive function, a string, or nil.
If COMMAND is nil, the key-chord is removed.

\(fn KEYS COMMAND)" t nil)

(autoload 'key-chord-define "key-chord/key-chord" "\
Define in KEYMAP, a key-chord of two keys in KEYS starting a COMMAND.

KEYS can be a string or a vector of two elements. Currently only elements
that corresponds to ascii codes in the range 32 to 126 can be used.

COMMAND can be an interactive function, a string, or nil.
If COMMAND is nil, the key-chord is removed.

\(fn KEYMAP KEYS COMMAND)" nil nil)

;;;***

;;;### (autoloads (flymake-less-css-init less-css-mode less-css-compile)
;;;;;;  "less-css-mode/less-css-mode" "less-css-mode/less-css-mode.el"
;;;;;;  (20621 19578))
;;; Generated autoloads from less-css-mode/less-css-mode.el

(autoload 'less-css-compile "less-css-mode/less-css-mode" "\
Compiles the current buffer to css using `less-css-lessc-command'.

\(fn)" t nil)

(autoload 'less-css-mode "less-css-mode/less-css-mode" "\
Major mode for editing LESS files, http://lesscss.org/
Special commands:
\\{less-css-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.less" . less-css-mode))

(autoload 'flymake-less-css-init "less-css-mode/less-css-mode" "\
Flymake support for LESS files

\(fn)" nil nil)

;;;***

;;;### (autoloads (lusty-launch-dired lusty-select-current-name lusty-select-match
;;;;;;  lusty-open-this lusty-highlight-previous-column lusty-highlight-next-column
;;;;;;  lusty-highlight-previous lusty-highlight-next lusty-buffer-explorer
;;;;;;  lusty-file-explorer) "lusty-explorer/lusty-explorer" "lusty-explorer/lusty-explorer.el"
;;;;;;  (20621 19579))
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

;;;### (autoloads (magit-status) "magit/magit" "magit/magit.el" (20621
;;;;;;  19571))
;;; Generated autoloads from magit/magit.el

(autoload 'magit-status "magit/magit" "\
Open a Magit status buffer for the Git repository containing
DIR.  If DIR is not within a Git repository, offer to create a
Git repository in DIR.

Interactively, a prefix argument means to ask the user which Git
repository to use even if `default-directory' is under Git control.
Two prefix arguments means to ignore `magit-repo-dirs' when asking for
user input.

\(fn DIR)" t nil)

;;;***

;;;### (autoloads (magit-blame-mode) "magit/magit-blame" "magit/magit-blame.el"
;;;;;;  (20621 19571))
;;; Generated autoloads from magit/magit-blame.el

(autoload 'magit-blame-mode "magit/magit-blame" "\
Display blame information inline.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (turn-on-magit-stgit magit-stgit-mode) "magit/magit-stgit"
;;;;;;  "magit/magit-stgit.el" (20621 19571))
;;; Generated autoloads from magit/magit-stgit.el

(autoload 'magit-stgit-mode "magit/magit-stgit" "\
StGit support for Magit

\(fn &optional ARG)" t nil)

(autoload 'turn-on-magit-stgit "magit/magit-stgit" "\
Unconditionally turn on `magit-stgit-mode'.

\(fn)" nil nil)

;;;***

;;;### (autoloads (turn-on-magit-svn magit-svn-mode) "magit/magit-svn"
;;;;;;  "magit/magit-svn.el" (20621 19571))
;;; Generated autoloads from magit/magit-svn.el

(autoload 'magit-svn-mode "magit/magit-svn" "\
SVN support for Magit

\(fn &optional ARG)" t nil)

(autoload 'turn-on-magit-svn "magit/magit-svn" "\
Unconditionally turn on `magit-svn-mode'.

\(fn)" nil nil)

;;;***

;;;### (autoloads (turn-on-magit-topgit magit-topgit-mode) "magit/magit-topgit"
;;;;;;  "magit/magit-topgit.el" (20621 19571))
;;; Generated autoloads from magit/magit-topgit.el

(autoload 'magit-topgit-mode "magit/magit-topgit" "\
Topgit support for Magit

\(fn &optional ARG)" t nil)

(autoload 'turn-on-magit-topgit "magit/magit-topgit" "\
Unconditionally turn on `magit-topgit-mode'.

\(fn)" nil nil)

;;;***

;;;### (autoloads (global-magit-wip-save-mode magit-wip-save-mode
;;;;;;  magit-wip-mode) "magit/magit-wip" "magit/magit-wip.el" (20621
;;;;;;  19571))
;;; Generated autoloads from magit/magit-wip.el

(defvar magit-wip-mode nil "\
Non-nil if Magit-Wip mode is enabled.
See the command `magit-wip-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `magit-wip-mode'.")

(custom-autoload 'magit-wip-mode "magit/magit-wip" nil)

(autoload 'magit-wip-mode "magit/magit-wip" "\
In Magit log buffers; give wip refs a special appearance.

\(fn &optional ARG)" t nil)

(autoload 'magit-wip-save-mode "magit/magit-wip" "\
Magit support for committing to a work-in-progress ref.

When this minor mode is turned on and a file is saved inside a writable
git repository then it is also committed to a special work-in-progress
ref.

\(fn &optional ARG)" t nil)

(defvar global-magit-wip-save-mode nil "\
Non-nil if Global-Magit-Wip-Save mode is enabled.
See the command `global-magit-wip-save-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-magit-wip-save-mode'.")

(custom-autoload 'global-magit-wip-save-mode "magit/magit-wip" nil)

(autoload 'global-magit-wip-save-mode "magit/magit-wip" "\
Toggle Magit-Wip-Save mode in all buffers.
With prefix ARG, enable Global-Magit-Wip-Save mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Magit-Wip-Save mode is enabled in all buffers where
`turn-on-magit-wip-save' would do it.
See `magit-wip-save-mode' for more information on Magit-Wip-Save mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (rebase-mode) "magit/rebase-mode" "magit/rebase-mode.el"
;;;;;;  (20621 19571))
;;; Generated autoloads from magit/rebase-mode.el

(autoload 'rebase-mode "magit/rebase-mode" "\
Major mode for editing of a Git rebase file.

Rebase files are generated when you run 'git rebase -i' or run
`magit-interactive-rebase'.  They describe how Git should perform
the rebase.  See the documentation for git-rebase (e.g., by
running 'man git-rebase' at the command line) for details.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("git-rebase-todo" . rebase-mode))

;;;***

;;;### (autoloads (inferior-moz-mode moz-minor-mode) "moz-repl/moz"
;;;;;;  "moz-repl/moz.el" (20621 19576))
;;; Generated autoloads from moz-repl/moz.el

(autoload 'moz-minor-mode "moz-repl/moz" "\
MozRepl minor mode for interaction with Firefox.
With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.

When this minor mode is enabled, some commands become available
to send current code area (as understood by c-mark-function) or
region or buffer to an inferior MozRepl process (which will be
started as needed).

The following keys are bound in this minor mode:

\\{moz-minor-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'inferior-moz-mode "moz-repl/moz" "\
Major mode for interacting with Firefox via MozRepl.

\(fn)" t nil)

;;;***

;;;### (autoloads (mu4e-speedbar-buttons) "mu4e/mu4e/mu4e-speedbar"
;;;;;;  "mu4e/mu4e/mu4e-speedbar.el" (20621 19582))
;;; Generated autoloads from mu4e/mu4e/mu4e-speedbar.el

(autoload 'mu4e-speedbar-buttons "mu4e/mu4e/mu4e-speedbar" "\
Create buttons for any mu4e BUFFER.

\(fn BUFFER)" t nil)

;;;***

;;;### (autoloads (multi-term) "multi-term/multi-term" "multi-term/multi-term.el"
;;;;;;  (20621 19580))
;;; Generated autoloads from multi-term/multi-term.el

(autoload 'multi-term "multi-term/multi-term" "\
Create new term buffer.
Will prompt you shell name when you type `C-u' before this command.

\(fn)" t nil)

;;;***

;;;### (autoloads (multi-web-mode) "multi-web-mode/multi-web-mode"
;;;;;;  "multi-web-mode/multi-web-mode.el" (20690 26222))
;;; Generated autoloads from multi-web-mode/multi-web-mode.el

(autoload 'multi-web-mode "multi-web-mode/multi-web-mode" "\
Enables the multi web mode chunk detection and indentation

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (assistant) "nognus/lisp/assistant" "nognus/lisp/assistant.el"
;;;;;;  (20621 19577))
;;; Generated autoloads from nognus/lisp/assistant.el

(autoload 'assistant "nognus/lisp/assistant" "\
Assist setting up Emacs based on FILE.

\(fn FILE)" t nil)

;;;***

;;;### (autoloads (auth-source-cache-expiry) "nognus/lisp/auth-source"
;;;;;;  "nognus/lisp/auth-source.el" (20621 19577))
;;; Generated autoloads from nognus/lisp/auth-source.el

(defvar auth-source-cache-expiry 7200 "\
How many seconds passwords are cached, or nil to disable
expiring.  Overrides `password-cache-expiry' through a
let-binding.")

(custom-autoload 'auth-source-cache-expiry "nognus/lisp/auth-source" t)

;;;***

;;;### (autoloads (binhex-decode-region binhex-decode-region-external
;;;;;;  binhex-decode-region-internal) "nognus/lisp/binhex" "nognus/lisp/binhex.el"
;;;;;;  (20621 19577))
;;; Generated autoloads from nognus/lisp/binhex.el

(defconst binhex-begin-line "^:...............................................................$" "\
Regular expression matching the start of a BinHex encoded region.")

(autoload 'binhex-decode-region-internal "nognus/lisp/binhex" "\
Binhex decode region between START and END without using an external program.
If HEADER-ONLY is non-nil only decode header and return filename.

\(fn START END &optional HEADER-ONLY)" t nil)

(autoload 'binhex-decode-region-external "nognus/lisp/binhex" "\
Binhex decode region between START and END using external decoder.

\(fn START END)" t nil)

(autoload 'binhex-decode-region "nognus/lisp/binhex" "\
Binhex decode region between START and END.

\(fn START END)" t nil)

;;;***

;;;### (autoloads (canlock-verify canlock-insert-header) "nognus/lisp/canlock"
;;;;;;  "nognus/lisp/canlock.el" (20621 19577))
;;; Generated autoloads from nognus/lisp/canlock.el

(autoload 'canlock-insert-header "nognus/lisp/canlock" "\
Insert a Cancel-Key and/or a Cancel-Lock header if possible.

\(fn &optional ID-FOR-KEY ID-FOR-LOCK PASSWORD)" nil nil)

(autoload 'canlock-verify "nognus/lisp/canlock" "\
Verify Cancel-Lock or Cancel-Key in BUFFER.
If BUFFER is nil, the current buffer is assumed.  Signal an error if
it fails.

\(fn &optional BUFFER)" t nil)

;;;***

;;;### (autoloads (color-name-to-rgb) "nognus/lisp/color" "nognus/lisp/color.el"
;;;;;;  (20621 19577))
;;; Generated autoloads from nognus/lisp/color.el

(autoload 'color-name-to-rgb "nognus/lisp/color" "\
Convert COLOR string to a list of normalized RGB components.
COLOR should be a color name (e.g. \"white\") or an RGB triplet
string (e.g. \"#ff12ec\").

Normally the return value is a list of three floating-point
numbers, (RED GREEN BLUE), each between 0.0 and 1.0 inclusive.

Optional argument FRAME specifies the frame where the color is to be
displayed.  If FRAME is omitted or nil, use the selected frame.
If FRAME cannot display COLOR, return nil.

\(fn COLOR &optional FRAME)" nil nil)

;;;***

;;;### (autoloads (gnus-article-outlook-deuglify-article gnus-outlook-deuglify-article
;;;;;;  gnus-article-outlook-repair-attribution gnus-article-outlook-unwrap-lines)
;;;;;;  "nognus/lisp/deuglify" "nognus/lisp/deuglify.el" (20621 19577))
;;; Generated autoloads from nognus/lisp/deuglify.el

(autoload 'gnus-article-outlook-unwrap-lines "nognus/lisp/deuglify" "\
Unwrap lines that appear to be wrapped citation lines.
You can control what lines will be unwrapped by frobbing
`gnus-outlook-deuglify-unwrap-min' and `gnus-outlook-deuglify-unwrap-max',
indicating the minimum and maximum length of an unwrapped citation line.  If
NODISPLAY is non-nil, don't redisplay the article buffer.

\(fn &optional NODISPLAY)" t nil)

(autoload 'gnus-article-outlook-repair-attribution "nognus/lisp/deuglify" "\
Repair a broken attribution line.
If NODISPLAY is non-nil, don't redisplay the article buffer.

\(fn &optional NODISPLAY)" t nil)

(autoload 'gnus-outlook-deuglify-article "nognus/lisp/deuglify" "\
Full deuglify of broken Outlook (Express) articles.
Treat dumbquotes, unwrap lines, repair attribution and rearrange citation.  If
NODISPLAY is non-nil, don't redisplay the article buffer.

\(fn &optional NODISPLAY)" t nil)

(autoload 'gnus-article-outlook-deuglify-article "nognus/lisp/deuglify" "\
Deuglify broken Outlook (Express) articles and redisplay.

\(fn)" t nil)

;;;***

;;;### (autoloads (dig) "nognus/lisp/dig" "nognus/lisp/dig.el" (20621
;;;;;;  19577))
;;; Generated autoloads from nognus/lisp/dig.el

(autoload 'dig "nognus/lisp/dig" "\
Query addresses of a DOMAIN using dig, by calling `dig-invoke'.
Optional arguments are passed to `dig-invoke'.

\(fn DOMAIN &optional QUERY-TYPE QUERY-CLASS QUERY-OPTION DIG-OPTION SERVER)" t nil)

;;;***

;;;### (autoloads (dns-mode-soa-increment-serial dns-mode) "nognus/lisp/dns-mode"
;;;;;;  "nognus/lisp/dns-mode.el" (20621 19577))
;;; Generated autoloads from nognus/lisp/dns-mode.el

(autoload 'dns-mode "nognus/lisp/dns-mode" "\
Major mode for viewing and editing DNS master files.
This mode is inherited from text mode.  It add syntax
highlighting, and some commands for handling DNS master files.
Its keymap inherits from `text-mode' and it has the same
variables for customizing indentation.  It has its own abbrev
table and its own syntax table.

Turning on DNS mode runs `dns-mode-hook'.

\(fn)" t nil)
 (defalias 'zone-mode 'dns-mode)

(autoload 'dns-mode-soa-increment-serial "nognus/lisp/dns-mode" "\
Locate SOA record and increment the serial field.

\(fn)" t nil)

;;;***

;;;### (autoloads (ecomplete-setup) "nognus/lisp/ecomplete" "nognus/lisp/ecomplete.el"
;;;;;;  (20621 19577))
;;; Generated autoloads from nognus/lisp/ecomplete.el

(autoload 'ecomplete-setup "nognus/lisp/ecomplete" "\


\(fn)" nil nil)

;;;***

;;;### (autoloads (fill-flowed fill-flowed-encode) "nognus/lisp/flow-fill"
;;;;;;  "nognus/lisp/flow-fill.el" (20621 19577))
;;; Generated autoloads from nognus/lisp/flow-fill.el

(autoload 'fill-flowed-encode "nognus/lisp/flow-fill" "\


\(fn &optional BUFFER)" nil nil)

(autoload 'fill-flowed "nognus/lisp/flow-fill" "\


\(fn &optional BUFFER DELETE-SPACE)" nil nil)

;;;***

;;;### (autoloads (gmm-tool-bar-from-list gmm-widget-p gmm-error
;;;;;;  gmm-message gmm-regexp-concat) "nognus/lisp/gmm-utils" "nognus/lisp/gmm-utils.el"
;;;;;;  (20621 19577))
;;; Generated autoloads from nognus/lisp/gmm-utils.el

(autoload 'gmm-regexp-concat "nognus/lisp/gmm-utils" "\
Potentially concat a list of regexps into a single one.
The concatenation is done with logical ORs.

\(fn REGEXP)" nil nil)

(autoload 'gmm-message "nognus/lisp/gmm-utils" "\
If LEVEL is lower than `gmm-verbose' print ARGS using `message'.

Guideline for numbers:
1 - error messages
3 - non-serious error messages
5 - messages for things that take a long time
7 - not very important messages on stuff
9 - messages inside loops.

\(fn LEVEL &rest ARGS)" nil nil)

(autoload 'gmm-error "nognus/lisp/gmm-utils" "\
Beep an error if LEVEL is equal to or less than `gmm-verbose'.
ARGS are passed to `message'.

\(fn LEVEL &rest ARGS)" nil nil)

(autoload 'gmm-widget-p "nognus/lisp/gmm-utils" "\
Non-nil if SYMBOL is a widget.

\(fn SYMBOL)" nil nil)

(autoload 'gmm-tool-bar-from-list "nognus/lisp/gmm-utils" "\
Make a tool bar from ICON-LIST.

Within each entry of ICON-LIST, the first element is a menu
command, the second element is an icon file name and the third
element is a test function.  You can use \\[describe-key]
<menu-entry> to find out the name of a menu command.  The fourth
and all following elements are passed as the PROPS argument to the
function `tool-bar-local-item'.

If ZAP-LIST is a list, remove those item from the default
`tool-bar-map'.  If it is t, start with a new sparse map.  You
can use \\[describe-key] <icon> to find out the name of an icon
item.  When \\[describe-key] <icon> shows \"<tool-bar> <new-file>
runs the command find-file\", then use `new-file' in ZAP-LIST.

DEFAULT-MAP specifies the default key map for ICON-LIST.

\(fn ICON-LIST ZAP-LIST DEFAULT-MAP)" nil nil)

;;;***

;;;### (autoloads (gnus gnus-other-frame gnus-slave gnus-no-server
;;;;;;  gnus-slave-no-server) "nognus/lisp/gnus" "nognus/lisp/gnus.el"
;;;;;;  (20621 19577))
;;; Generated autoloads from nognus/lisp/gnus.el
(when (fboundp 'custom-autoload)
 (custom-autoload 'gnus-select-method "gnus"))

(autoload 'gnus-slave-no-server "nognus/lisp/gnus" "\
Read network news as a slave, without connecting to the local server.

\(fn &optional ARG)" t nil)

(autoload 'gnus-no-server "nognus/lisp/gnus" "\
Read network news.
If ARG is a positive number, Gnus will use that as the startup
level. If ARG is nil, Gnus will be started at level 2.  If ARG is
non-nil and not a positive number, Gnus will prompt the user for the
name of an NNTP server to use.
As opposed to `gnus', this command will not connect to the local
server.

\(fn &optional ARG SLAVE)" t nil)

(autoload 'gnus-slave "nognus/lisp/gnus" "\
Read news as a slave.

\(fn &optional ARG)" t nil)

(autoload 'gnus-other-frame "nognus/lisp/gnus" "\
Pop up a frame to read news.
This will call one of the Gnus commands which is specified by the user
option `gnus-other-frame-function' (default `gnus') with the argument
ARG if Gnus is not running, otherwise just pop up a Gnus frame.  The
optional second argument DISPLAY should be a standard display string
such as \"unix:0\" to specify where to pop up a frame.  If DISPLAY is
omitted or the function `make-frame-on-display' is not available, the
current display is used.

\(fn &optional ARG DISPLAY)" t nil)

(autoload 'gnus "nognus/lisp/gnus" "\
Read network news.
If ARG is non-nil and a positive number, Gnus will use that as the
startup level.  If ARG is non-nil and not a positive number, Gnus will
prompt the user for the name of an NNTP server to use.

\(fn &optional ARG DONT-CONNECT SLAVE)" t nil)

;;;***

;;;### (autoloads (gnus-agent-regenerate gnus-agent-batch gnus-agent-batch-fetch
;;;;;;  gnus-agent-find-parameter gnus-agent-possibly-alter-active
;;;;;;  gnus-agent-get-undownloaded-list gnus-agent-delete-group
;;;;;;  gnus-agent-rename-group gnus-agent-possibly-save-gcc gnus-agentize
;;;;;;  gnus-slave-unplugged gnus-plugged gnus-unplugged) "nognus/lisp/gnus-agent"
;;;;;;  "nognus/lisp/gnus-agent.el" (20621 19577))
;;; Generated autoloads from nognus/lisp/gnus-agent.el

(autoload 'gnus-unplugged "nognus/lisp/gnus-agent" "\
Start Gnus unplugged.

\(fn)" t nil)

(autoload 'gnus-plugged "nognus/lisp/gnus-agent" "\
Start Gnus plugged.

\(fn)" t nil)

(autoload 'gnus-slave-unplugged "nognus/lisp/gnus-agent" "\
Read news as a slave unplugged.

\(fn &optional ARG)" t nil)

(autoload 'gnus-agentize "nognus/lisp/gnus-agent" "\
Allow Gnus to be an offline newsreader.

The gnus-agentize function is now called internally by gnus when
gnus-agent is set.  If you wish to avoid calling gnus-agentize,
customize gnus-agent to nil.

This will modify the `gnus-setup-news-hook', and
`message-send-mail-real-function' variables, and install the Gnus agent
minor mode in all Gnus buffers.

\(fn)" t nil)

(autoload 'gnus-agent-possibly-save-gcc "nognus/lisp/gnus-agent" "\
Save GCC if Gnus is unplugged.

\(fn)" nil nil)

(autoload 'gnus-agent-rename-group "nognus/lisp/gnus-agent" "\
Rename fully-qualified OLD-GROUP as NEW-GROUP.
Always updates the agent, even when disabled, as the old agent
files would corrupt gnus when the agent was next enabled.
Depends upon the caller to determine whether group renaming is
supported.

\(fn OLD-GROUP NEW-GROUP)" nil nil)

(autoload 'gnus-agent-delete-group "nognus/lisp/gnus-agent" "\
Delete fully-qualified GROUP.
Always updates the agent, even when disabled, as the old agent
files would corrupt gnus when the agent was next enabled.
Depends upon the caller to determine whether group deletion is
supported.

\(fn GROUP)" nil nil)

(autoload 'gnus-agent-get-undownloaded-list "nognus/lisp/gnus-agent" "\
Construct list of articles that have not been downloaded.

\(fn)" nil nil)

(autoload 'gnus-agent-possibly-alter-active "nognus/lisp/gnus-agent" "\
Possibly expand a group's active range to include articles
downloaded into the agent.

\(fn GROUP ACTIVE &optional INFO)" nil nil)

(autoload 'gnus-agent-find-parameter "nognus/lisp/gnus-agent" "\
Search for GROUPs SYMBOL in the group's parameters, the group's
topic parameters, the group's category, or the customizable
variables.  Returns the first non-nil value found.

\(fn GROUP SYMBOL)" nil nil)

(autoload 'gnus-agent-batch-fetch "nognus/lisp/gnus-agent" "\
Start Gnus and fetch session.

\(fn)" t nil)

(autoload 'gnus-agent-batch "nognus/lisp/gnus-agent" "\
Start Gnus, send queue and fetch session.

\(fn)" t nil)

(autoload 'gnus-agent-regenerate "nognus/lisp/gnus-agent" "\
Regenerate all agent covered files.
If CLEAN, obsolete (ignore).

\(fn &optional CLEAN REREAD)" t nil)

;;;***

;;;### (autoloads (gnus-article-prepare-display) "nognus/lisp/gnus-art"
;;;;;;  "nognus/lisp/gnus-art.el" (20621 19577))
;;; Generated autoloads from nognus/lisp/gnus-art.el

(autoload 'gnus-article-prepare-display "nognus/lisp/gnus-art" "\
Make the current buffer look like a nice article.

\(fn)" nil nil)

;;;***

;;;### (autoloads (gnus-bookmark-bmenu-list gnus-bookmark-jump gnus-bookmark-set)
;;;;;;  "nognus/lisp/gnus-bookmark" "nognus/lisp/gnus-bookmark.el"
;;;;;;  (20621 19577))
;;; Generated autoloads from nognus/lisp/gnus-bookmark.el

(autoload 'gnus-bookmark-set "nognus/lisp/gnus-bookmark" "\
Set a bookmark for this article.

\(fn)" t nil)

(autoload 'gnus-bookmark-jump "nognus/lisp/gnus-bookmark" "\
Jump to a Gnus bookmark (BMK-NAME).

\(fn &optional BMK-NAME)" t nil)

(autoload 'gnus-bookmark-bmenu-list "nognus/lisp/gnus-bookmark" "\
Display a list of existing Gnus bookmarks.
The list is displayed in a buffer named `*Gnus Bookmark List*'.
The leftmost column displays a D if the bookmark is flagged for
deletion, or > if it is flagged for displaying.

\(fn)" t nil)

;;;***

;;;### (autoloads (gnus-cache-delete-group gnus-cache-rename-group
;;;;;;  gnus-cache-generate-nov-databases gnus-cache-generate-active
;;;;;;  gnus-jog-cache) "nognus/lisp/gnus-cache" "nognus/lisp/gnus-cache.el"
;;;;;;  (20621 19577))
;;; Generated autoloads from nognus/lisp/gnus-cache.el

(autoload 'gnus-jog-cache "nognus/lisp/gnus-cache" "\
Go through all groups and put the articles into the cache.

Usage:
$ emacs -batch -l ~/.emacs -l gnus -f gnus-jog-cache

\(fn)" t nil)

(autoload 'gnus-cache-generate-active "nognus/lisp/gnus-cache" "\
Generate the cache active file.

\(fn &optional DIRECTORY)" t nil)

(autoload 'gnus-cache-generate-nov-databases "nognus/lisp/gnus-cache" "\
Generate NOV files recursively starting in DIR.

\(fn DIR)" t nil)

(autoload 'gnus-cache-rename-group "nognus/lisp/gnus-cache" "\
Rename OLD-GROUP as NEW-GROUP.
Always updates the cache, even when disabled, as the old cache
files would corrupt Gnus when the cache was next enabled.  It
depends on the caller to determine whether group renaming is
supported.

\(fn OLD-GROUP NEW-GROUP)" nil nil)

(autoload 'gnus-cache-delete-group "nognus/lisp/gnus-cache" "\
Delete GROUP from the cache.
Always updates the cache, even when disabled, as the old cache
files would corrupt gnus when the cache was next enabled.
Depends upon the caller to determine whether group deletion is
supported.

\(fn GROUP)" nil nil)

;;;***

;;;### (autoloads (gnus-delay-initialize gnus-delay-send-queue gnus-delay-article)
;;;;;;  "nognus/lisp/gnus-delay" "nognus/lisp/gnus-delay.el" (20621
;;;;;;  19577))
;;; Generated autoloads from nognus/lisp/gnus-delay.el

(autoload 'gnus-delay-article "nognus/lisp/gnus-delay" "\
Delay this article by some time.
DELAY is a string, giving the length of the time.  Possible values are:

* <digits><units> for <units> in minutes (`m'), hours (`h'), days (`d'),
  weeks (`w'), months (`M'), or years (`Y');

* YYYY-MM-DD for a specific date.  The time of day is given by the
  variable `gnus-delay-default-hour', minute and second are zero.

* hh:mm for a specific time.  Use 24h format.  If it is later than this
  time, then the deadline is tomorrow, else today.

\(fn DELAY)" t nil)

(autoload 'gnus-delay-send-queue "nognus/lisp/gnus-delay" "\
Send all the delayed messages that are due now.

\(fn)" t nil)

(autoload 'gnus-delay-initialize "nognus/lisp/gnus-delay" "\
Initialize the gnus-delay package.
This sets up a key binding in `message-mode' to delay a message.
This tells Gnus to look for delayed messages after getting new news.

The optional arg NO-KEYMAP is ignored.
Checking delayed messages is skipped if optional arg NO-CHECK is non-nil.

\(fn &optional NO-KEYMAP NO-CHECK)" nil nil)

;;;***

;;;### (autoloads (gnus-user-format-function-D gnus-user-format-function-d)
;;;;;;  "nognus/lisp/gnus-diary" "nognus/lisp/gnus-diary.el" (20621
;;;;;;  19577))
;;; Generated autoloads from nognus/lisp/gnus-diary.el

(autoload 'gnus-user-format-function-d "nognus/lisp/gnus-diary" "\


\(fn HEADER)" nil nil)

(autoload 'gnus-user-format-function-D "nognus/lisp/gnus-diary" "\


\(fn HEADER)" nil nil)

;;;***

;;;### (autoloads (turn-on-gnus-dired-mode) "nognus/lisp/gnus-dired"
;;;;;;  "nognus/lisp/gnus-dired.el" (20621 19577))
;;; Generated autoloads from nognus/lisp/gnus-dired.el

(autoload 'turn-on-gnus-dired-mode "nognus/lisp/gnus-dired" "\
Convenience method to turn on gnus-dired-mode.

\(fn)" t nil)

;;;***

;;;### (autoloads (gnus-draft-reminder) "nognus/lisp/gnus-draft"
;;;;;;  "nognus/lisp/gnus-draft.el" (20621 19577))
;;; Generated autoloads from nognus/lisp/gnus-draft.el

(autoload 'gnus-draft-reminder "nognus/lisp/gnus-draft" "\
Reminder user if there are unsent drafts.

\(fn)" t nil)

;;;***

;;;### (autoloads (gnus-convert-png-to-face gnus-convert-face-to-png
;;;;;;  gnus-face-from-file gnus-x-face-from-file gnus-insert-random-x-face-header
;;;;;;  gnus-random-x-face) "nognus/lisp/gnus-fun" "nognus/lisp/gnus-fun.el"
;;;;;;  (20621 19577))
;;; Generated autoloads from nognus/lisp/gnus-fun.el

(autoload 'gnus-random-x-face "nognus/lisp/gnus-fun" "\
Return X-Face header data chosen randomly from `gnus-x-face-directory'.

\(fn)" t nil)

(autoload 'gnus-insert-random-x-face-header "nognus/lisp/gnus-fun" "\
Insert a random X-Face header from `gnus-x-face-directory'.

\(fn)" t nil)

(autoload 'gnus-x-face-from-file "nognus/lisp/gnus-fun" "\
Insert an X-Face header based on an image file.

Depending on `gnus-convert-image-to-x-face-command' it may accept
different input formats.

\(fn FILE)" t nil)

(autoload 'gnus-face-from-file "nognus/lisp/gnus-fun" "\
Return a Face header based on an image file.

Depending on `gnus-convert-image-to-face-command' it may accept
different input formats.

\(fn FILE)" t nil)

(autoload 'gnus-convert-face-to-png "nognus/lisp/gnus-fun" "\
Convert FACE (which is base64-encoded) to a PNG.
The PNG is returned as a string.

\(fn FACE)" nil nil)

(autoload 'gnus-convert-png-to-face "nognus/lisp/gnus-fun" "\
Convert FILE to a Face.
FILE should be a PNG file that's 48x48 and smaller than or equal to
726 bytes.

\(fn FILE)" nil nil)

;;;***

;;;### (autoloads (gnus-treat-mail-gravatar gnus-treat-from-gravatar)
;;;;;;  "nognus/lisp/gnus-gravatar" "nognus/lisp/gnus-gravatar.el"
;;;;;;  (20621 19577))
;;; Generated autoloads from nognus/lisp/gnus-gravatar.el

(autoload 'gnus-treat-from-gravatar "nognus/lisp/gnus-gravatar" "\
Display gravatar in the From header.
If gravatar is already displayed, remove it.

\(fn &optional FORCE)" t nil)

(autoload 'gnus-treat-mail-gravatar "nognus/lisp/gnus-gravatar" "\
Display gravatars in the Cc and To headers.
If gravatars are already displayed, remove them.

\(fn &optional FORCE)" t nil)

;;;***

;;;### (autoloads (gnus-fetch-group-other-frame gnus-fetch-group)
;;;;;;  "nognus/lisp/gnus-group" "nognus/lisp/gnus-group.el" (20621
;;;;;;  19577))
;;; Generated autoloads from nognus/lisp/gnus-group.el

(autoload 'gnus-fetch-group "nognus/lisp/gnus-group" "\
Start Gnus if necessary and enter GROUP.
If ARTICLES, display those articles.
Returns whether the fetching was successful or not.

\(fn GROUP &optional ARTICLES)" t nil)

(autoload 'gnus-fetch-group-other-frame "nognus/lisp/gnus-group" "\
Pop up a frame and enter GROUP.

\(fn GROUP)" t nil)

;;;***

;;;### (autoloads (gnus-html-prefetch-images gnus-article-html) "nognus/lisp/gnus-html"
;;;;;;  "nognus/lisp/gnus-html.el" (20621 19577))
;;; Generated autoloads from nognus/lisp/gnus-html.el

(autoload 'gnus-article-html "nognus/lisp/gnus-html" "\


\(fn &optional HANDLE)" nil nil)

(autoload 'gnus-html-prefetch-images "nognus/lisp/gnus-html" "\


\(fn SUMMARY)" nil nil)

;;;***

;;;### (autoloads (gnus-batch-score) "nognus/lisp/gnus-kill" "nognus/lisp/gnus-kill.el"
;;;;;;  (20621 19577))
;;; Generated autoloads from nognus/lisp/gnus-kill.el

(defalias 'gnus-batch-kill 'gnus-batch-score)

(autoload 'gnus-batch-score "nognus/lisp/gnus-kill" "\
Run batched scoring.
Usage: emacs -batch -l ~/.emacs -l gnus -f gnus-batch-score

\(fn)" t nil)

;;;***

;;;### (autoloads (gnus-mailing-list-mode gnus-mailing-list-insinuate
;;;;;;  turn-on-gnus-mailing-list-mode) "nognus/lisp/gnus-ml" "nognus/lisp/gnus-ml.el"
;;;;;;  (20621 19577))
;;; Generated autoloads from nognus/lisp/gnus-ml.el

(autoload 'turn-on-gnus-mailing-list-mode "nognus/lisp/gnus-ml" "\


\(fn)" nil nil)

(autoload 'gnus-mailing-list-insinuate "nognus/lisp/gnus-ml" "\
Setup group parameters from List-Post header.
If FORCE is non-nil, replace the old ones.

\(fn &optional FORCE)" t nil)

(autoload 'gnus-mailing-list-mode "nognus/lisp/gnus-ml" "\
Minor mode for providing mailing-list commands.

\\{gnus-mailing-list-mode-map}

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (gnus-group-split-fancy gnus-group-split gnus-group-split-update
;;;;;;  gnus-group-split-setup) "nognus/lisp/gnus-mlspl" "nognus/lisp/gnus-mlspl.el"
;;;;;;  (20621 19577))
;;; Generated autoloads from nognus/lisp/gnus-mlspl.el

(autoload 'gnus-group-split-setup "nognus/lisp/gnus-mlspl" "\
Set up the split for `nnmail-split-fancy'.
Sets things up so that nnmail-split-fancy is used for mail
splitting, and defines the variable nnmail-split-fancy according with
group parameters.

If AUTO-UPDATE is non-nil (prefix argument accepted, if called
interactively), it makes sure nnmail-split-fancy is re-computed before
getting new mail, by adding `gnus-group-split-update' to
`nnmail-pre-get-new-mail-hook'.

A non-nil CATCH-ALL replaces the current value of
`gnus-group-split-default-catch-all-group'.  This variable is only used
by gnus-group-split-update, and only when its CATCH-ALL argument is
nil.  This argument may contain any fancy split, that will be added as
the last split in a `|' split produced by `gnus-group-split-fancy',
unless overridden by any group marked as a catch-all group.  Typical
uses are as simple as the name of a default mail group, but more
elaborate fancy splits may also be useful to split mail that doesn't
match any of the group-specified splitting rules.  See
`gnus-group-split-fancy' for details.

\(fn &optional AUTO-UPDATE CATCH-ALL)" t nil)

(autoload 'gnus-group-split-update "nognus/lisp/gnus-mlspl" "\
Computes nnmail-split-fancy from group params and CATCH-ALL.
It does this by calling by calling (gnus-group-split-fancy nil
nil CATCH-ALL).

If CATCH-ALL is nil, `gnus-group-split-default-catch-all-group' is used
instead.  This variable is set by `gnus-group-split-setup'.

\(fn &optional CATCH-ALL)" t nil)

(autoload 'gnus-group-split "nognus/lisp/gnus-mlspl" "\
Use information from group parameters in order to split mail.
See `gnus-group-split-fancy' for more information.

`gnus-group-split' is a valid value for `nnmail-split-methods'.

\(fn)" nil nil)

(autoload 'gnus-group-split-fancy "nognus/lisp/gnus-mlspl" "\
Uses information from group parameters in order to split mail.
It can be embedded into `nnmail-split-fancy' lists with the SPLIT

\(: gnus-group-split-fancy GROUPS NO-CROSSPOST CATCH-ALL)

GROUPS may be a regular expression or a list of group names, that will
be used to select candidate groups.  If it is omitted or nil, all
existing groups are considered.

if NO-CROSSPOST is omitted or nil, a & split will be returned,
otherwise, a | split, that does not allow crossposting, will be
returned.

For each selected group, a SPLIT is composed like this: if SPLIT-SPEC
is specified, this split is returned as-is (unless it is nil: in this
case, the group is ignored).  Otherwise, if TO-ADDRESS, TO-LIST and/or
EXTRA-ALIASES are specified, a regexp that matches any of them is
constructed (extra-aliases may be a list).  Additionally, if
SPLIT-REGEXP is specified, the regexp will be extended so that it
matches this regexp too, and if SPLIT-EXCLUDE is specified, RESTRICT
clauses will be generated.

If CATCH-ALL is nil, no catch-all handling is performed, regardless of
catch-all marks in group parameters.  Otherwise, if there is no
selected group whose SPLIT-REGEXP matches the empty string, nor is
there a selected group whose SPLIT-SPEC is 'catch-all, this fancy
split (say, a group name) will be appended to the returned SPLIT list,
as the last element of a '| SPLIT.

For example, given the following group parameters:

nnml:mail.bar:
\((to-address . \"bar@femail.com\")
 (split-regexp . \".*@femail\\\\.com\"))
nnml:mail.foo:
\((to-list . \"foo@nowhere.gov\")
 (extra-aliases \"foo@localhost\" \"foo-redist@home\")
 (split-exclude \"bugs-foo\" \"rambling-foo\")
 (admin-address . \"foo-request@nowhere.gov\"))
nnml:mail.others:
\((split-spec . catch-all))

Calling (gnus-group-split-fancy nil nil \"mail.others\") returns:

\(| (& (any \"\\\\(bar@femail\\\\.com\\\\|.*@femail\\\\.com\\\\)\"
	   \"mail.bar\")
      (any \"\\\\(foo@nowhere\\\\.gov\\\\|foo@localhost\\\\|foo-redist@home\\\\)\"
	   - \"bugs-foo\" - \"rambling-foo\" \"mail.foo\"))
   \"mail.others\")

\(fn &optional GROUPS NO-CROSSPOST CATCH-ALL)" nil nil)

;;;***

;;;### (autoloads (gnus-button-reply gnus-button-mailto gnus-msg-mail)
;;;;;;  "nognus/lisp/gnus-msg" "nognus/lisp/gnus-msg.el" (20621 19577))
;;; Generated autoloads from nognus/lisp/gnus-msg.el

(autoload 'gnus-msg-mail "nognus/lisp/gnus-msg" "\
Start editing a mail message to be sent.
Like `message-mail', but with Gnus paraphernalia, particularly the
Gcc: header for archiving purposes.
If Gnus isn't running, a plain `message-mail' setup is used
instead.

\(fn &optional TO SUBJECT OTHER-HEADERS CONTINUE SWITCH-ACTION YANK-ACTION SEND-ACTIONS RETURN-ACTION)" t nil)

(autoload 'gnus-button-mailto "nognus/lisp/gnus-msg" "\
Mail to ADDRESS.

\(fn ADDRESS)" nil nil)

(autoload 'gnus-button-reply "nognus/lisp/gnus-msg" "\
Like `message-reply'.

\(fn &optional TO-ADDRESS WIDE)" t nil)

(define-mail-user-agent 'gnus-user-agent 'gnus-msg-mail 'message-send-and-exit 'message-kill-buffer 'message-send-hook)

;;;***

;;;### (autoloads (gnus-notifications) "nognus/lisp/gnus-notifications"
;;;;;;  "nognus/lisp/gnus-notifications.el" (20621 19577))
;;; Generated autoloads from nognus/lisp/gnus-notifications.el

(autoload 'gnus-notifications "nognus/lisp/gnus-notifications" "\
Send a notification on new message.
This check for new messages that are in group with a level lower
or equal to `gnus-notifications-minimum-level' and send a
notification using `notifications-notify' for it.

This is typically a function to add in
`gnus-after-getting-new-news-hook'

\(fn)" nil nil)

;;;***

;;;### (autoloads (gnus-treat-newsgroups-picon gnus-treat-mail-picon
;;;;;;  gnus-treat-from-picon) "nognus/lisp/gnus-picon" "nognus/lisp/gnus-picon.el"
;;;;;;  (20621 19577))
;;; Generated autoloads from nognus/lisp/gnus-picon.el

(autoload 'gnus-treat-from-picon "nognus/lisp/gnus-picon" "\
Display picons in the From header.
If picons are already displayed, remove them.

\(fn)" t nil)

(autoload 'gnus-treat-mail-picon "nognus/lisp/gnus-picon" "\
Display picons in the Cc and To headers.
If picons are already displayed, remove them.

\(fn)" t nil)

(autoload 'gnus-treat-newsgroups-picon "nognus/lisp/gnus-picon" "\
Display picons in the Newsgroups and Followup-To headers.
If picons are already displayed, remove them.

\(fn)" t nil)

;;;***

;;;### (autoloads (gnus-add-to-sorted-list gnus-sorted-nunion gnus-sorted-union
;;;;;;  gnus-sorted-nintersection gnus-sorted-range-intersection
;;;;;;  gnus-sorted-intersection gnus-intersection gnus-sorted-complement
;;;;;;  gnus-sorted-ndifference gnus-sorted-difference) "nognus/lisp/gnus-range"
;;;;;;  "nognus/lisp/gnus-range.el" (20621 19577))
;;; Generated autoloads from nognus/lisp/gnus-range.el

(autoload 'gnus-sorted-difference "nognus/lisp/gnus-range" "\
Return a list of elements of LIST1 that do not appear in LIST2.
Both lists have to be sorted over <.
The tail of LIST1 is not copied.

\(fn LIST1 LIST2)" nil nil)

(autoload 'gnus-sorted-ndifference "nognus/lisp/gnus-range" "\
Return a list of elements of LIST1 that do not appear in LIST2.
Both lists have to be sorted over <.
LIST1 is modified.

\(fn LIST1 LIST2)" nil nil)

(autoload 'gnus-sorted-complement "nognus/lisp/gnus-range" "\
Return a list of elements that are in LIST1 or LIST2 but not both.
Both lists have to be sorted over <.

\(fn LIST1 LIST2)" nil nil)

(autoload 'gnus-intersection "nognus/lisp/gnus-range" "\


\(fn LIST1 LIST2)" nil nil)

(autoload 'gnus-sorted-intersection "nognus/lisp/gnus-range" "\
Return intersection of LIST1 and LIST2.
LIST1 and LIST2 have to be sorted over <.

\(fn LIST1 LIST2)" nil nil)

(autoload 'gnus-sorted-range-intersection "nognus/lisp/gnus-range" "\
Return intersection of RANGE1 and RANGE2.
RANGE1 and RANGE2 have to be sorted over <.

\(fn RANGE1 RANGE2)" nil nil)

(defalias 'gnus-set-sorted-intersection 'gnus-sorted-nintersection)

(autoload 'gnus-sorted-nintersection "nognus/lisp/gnus-range" "\
Return intersection of LIST1 and LIST2 by modifying cdr pointers of LIST1.
LIST1 and LIST2 have to be sorted over <.

\(fn LIST1 LIST2)" nil nil)

(autoload 'gnus-sorted-union "nognus/lisp/gnus-range" "\
Return union of LIST1 and LIST2.
LIST1 and LIST2 have to be sorted over <.

\(fn LIST1 LIST2)" nil nil)

(autoload 'gnus-sorted-nunion "nognus/lisp/gnus-range" "\
Return union of LIST1 and LIST2 by modifying cdr pointers of LIST1.
LIST1 and LIST2 have to be sorted over <.

\(fn LIST1 LIST2)" nil nil)

(autoload 'gnus-add-to-sorted-list "nognus/lisp/gnus-range" "\
Add NUM into sorted LIST by side effect.

\(fn LIST NUM)" nil nil)

;;;***

;;;### (autoloads (gnus-registry-install-hooks gnus-registry-initialize)
;;;;;;  "nognus/lisp/gnus-registry" "nognus/lisp/gnus-registry.el"
;;;;;;  (20621 19577))
;;; Generated autoloads from nognus/lisp/gnus-registry.el

(autoload 'gnus-registry-initialize "nognus/lisp/gnus-registry" "\
Initialize the Gnus registry.

\(fn)" t nil)

(autoload 'gnus-registry-install-hooks "nognus/lisp/gnus-registry" "\
Install the registry hooks.

\(fn)" t nil)

;;;***

;;;### (autoloads (gnus-sieve-article-add-rule gnus-sieve-generate
;;;;;;  gnus-sieve-update) "nognus/lisp/gnus-sieve" "nognus/lisp/gnus-sieve.el"
;;;;;;  (20621 19577))
;;; Generated autoloads from nognus/lisp/gnus-sieve.el

(autoload 'gnus-sieve-update "nognus/lisp/gnus-sieve" "\
Update the Sieve script in gnus-sieve-file, by replacing the region
between gnus-sieve-region-start and gnus-sieve-region-end with
\(gnus-sieve-script gnus-sieve-select-method gnus-sieve-crosspost), then
execute gnus-sieve-update-shell-command.
See the documentation for these variables and functions for details.

\(fn)" t nil)

(autoload 'gnus-sieve-generate "nognus/lisp/gnus-sieve" "\
Generate the Sieve script in gnus-sieve-file, by replacing the region
between gnus-sieve-region-start and gnus-sieve-region-end with
\(gnus-sieve-script gnus-sieve-select-method gnus-sieve-crosspost).
See the documentation for these variables and functions for details.

\(fn)" t nil)

(autoload 'gnus-sieve-article-add-rule "nognus/lisp/gnus-sieve" "\


\(fn)" t nil)

;;;***

;;;### (autoloads (gnus-update-format) "nognus/lisp/gnus-spec" "nognus/lisp/gnus-spec.el"
;;;;;;  (20621 19577))
;;; Generated autoloads from nognus/lisp/gnus-spec.el

(autoload 'gnus-update-format "nognus/lisp/gnus-spec" "\
Update the format specification near point.

\(fn VAR)" t nil)

;;;***

;;;### (autoloads (gnus-declare-backend) "nognus/lisp/gnus-start"
;;;;;;  "nognus/lisp/gnus-start.el" (20621 19577))
;;; Generated autoloads from nognus/lisp/gnus-start.el

(autoload 'gnus-declare-backend "nognus/lisp/gnus-start" "\
Declare back end NAME with ABILITIES as a Gnus back end.

\(fn NAME &rest ABILITIES)" nil nil)

;;;***

;;;### (autoloads (gnus-summary-bookmark-jump) "nognus/lisp/gnus-sum"
;;;;;;  "nognus/lisp/gnus-sum.el" (20621 19577))
;;; Generated autoloads from nognus/lisp/gnus-sum.el

(autoload 'gnus-summary-bookmark-jump "nognus/lisp/gnus-sum" "\
Handler function for record returned by `gnus-summary-bookmark-make-record'.
BOOKMARK is a bookmark name or a bookmark record.

\(fn BOOKMARK)" nil nil)

;;;***

;;;### (autoloads (gnus-sync-install-hooks gnus-sync-initialize)
;;;;;;  "nognus/lisp/gnus-sync" "nognus/lisp/gnus-sync.el" (20621
;;;;;;  19577))
;;; Generated autoloads from nognus/lisp/gnus-sync.el

(autoload 'gnus-sync-initialize "nognus/lisp/gnus-sync" "\
Initialize the Gnus sync facility.

\(fn)" t nil)

(autoload 'gnus-sync-install-hooks "nognus/lisp/gnus-sync" "\
Install the sync hooks.

\(fn)" t nil)

;;;***

;;;### (autoloads (gnus-add-configuration) "nognus/lisp/gnus-win"
;;;;;;  "nognus/lisp/gnus-win.el" (20621 19577))
;;; Generated autoloads from nognus/lisp/gnus-win.el

(autoload 'gnus-add-configuration "nognus/lisp/gnus-win" "\
Add the window configuration CONF to `gnus-buffer-configuration'.

\(fn CONF)" nil nil)

;;;***

;;;### (autoloads (gravatar-retrieve-synchronously gravatar-retrieve)
;;;;;;  "nognus/lisp/gravatar" "nognus/lisp/gravatar.el" (20621 19577))
;;; Generated autoloads from nognus/lisp/gravatar.el

(autoload 'gravatar-retrieve "nognus/lisp/gravatar" "\
Retrieve MAIL-ADDRESS gravatar and call CB on retrieval.
You can provide a list of argument to pass to CB in CBARGS.

\(fn MAIL-ADDRESS CB &optional CBARGS)" nil nil)

(autoload 'gravatar-retrieve-synchronously "nognus/lisp/gravatar" "\
Retrieve MAIL-ADDRESS gravatar and returns it.

\(fn MAIL-ADDRESS)" nil nil)

;;;***

;;;### (autoloads (mail-check-payment mail-add-payment-async mail-add-payment
;;;;;;  hashcash-verify-payment hashcash-insert-payment-async hashcash-insert-payment)
;;;;;;  "nognus/lisp/hashcash" "nognus/lisp/hashcash.el" (20621 19577))
;;; Generated autoloads from nognus/lisp/hashcash.el

(autoload 'hashcash-insert-payment "nognus/lisp/hashcash" "\
Insert X-Payment and X-Hashcash headers with a payment for ARG

\(fn ARG)" t nil)

(autoload 'hashcash-insert-payment-async "nognus/lisp/hashcash" "\
Insert X-Payment and X-Hashcash headers with a payment for ARG
Only start calculation.  Results are inserted when ready.

\(fn ARG)" t nil)

(autoload 'hashcash-verify-payment "nognus/lisp/hashcash" "\
Verify a hashcash payment

\(fn TOKEN &optional RESOURCE AMOUNT)" nil nil)

(autoload 'mail-add-payment "nognus/lisp/hashcash" "\
Add X-Payment: and X-Hashcash: headers with a hashcash payment
for each recipient address.  Prefix arg sets default payment temporarily.
Set ASYNC to t to start asynchronous calculation.  (See
`mail-add-payment-async').

\(fn &optional ARG ASYNC)" t nil)

(autoload 'mail-add-payment-async "nognus/lisp/hashcash" "\
Add X-Payment: and X-Hashcash: headers with a hashcash payment
for each recipient address.  Prefix arg sets default payment temporarily.
Calculation is asynchronous.

\(fn &optional ARG)" t nil)

(autoload 'mail-check-payment "nognus/lisp/hashcash" "\
Look for a valid X-Payment: or X-Hashcash: header.
Prefix arg sets default accept amount temporarily.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (html2text) "nognus/lisp/html2text" "nognus/lisp/html2text.el"
;;;;;;  (20621 19577))
;;; Generated autoloads from nognus/lisp/html2text.el

(autoload 'html2text "nognus/lisp/html2text" "\
Convert HTML to plain text in the current buffer.

\(fn)" t nil)

;;;***

;;;### (autoloads (message-unbold-region message-bold-region message-news-other-frame
;;;;;;  message-news-other-window message-mail-other-frame message-mail-other-window
;;;;;;  message-bounce message-resend message-insinuate-rmail message-forward-rmail-make-body
;;;;;;  message-forward-make-body message-forward message-recover
;;;;;;  message-supersede message-cancel-news message-followup message-wide-reply
;;;;;;  message-reply message-news message-mail message-mode) "nognus/lisp/message"
;;;;;;  "nognus/lisp/message.el" (20621 19577))
;;; Generated autoloads from nognus/lisp/message.el

(define-mail-user-agent 'message-user-agent 'message-mail 'message-send-and-exit 'message-kill-buffer 'message-send-hook)

(autoload 'message-mode "nognus/lisp/message" "\
Major mode for editing mail and news to be sent.
Like Text Mode but with these additional commands:\\<message-mode-map>
C-c C-s  `message-send' (send the message)  C-c C-c  `message-send-and-exit'
C-c C-d  Postpone sending the message       C-c C-k  Kill the message
C-c C-f  move to a header field (and create it if there isn't):
	 C-c C-f C-t  move to To	C-c C-f C-s  move to Subject
	 C-c C-f C-c  move to Cc	C-c C-f C-b  move to Bcc
	 C-c C-f C-w  move to Fcc	C-c C-f C-r  move to Reply-To
	 C-c C-f C-u  move to Summary	C-c C-f C-n  move to Newsgroups
	 C-c C-f C-k  move to Keywords	C-c C-f C-d  move to Distribution
	 C-c C-f C-o  move to From (\"Originator\")
	 C-c C-f C-f  move to Followup-To
	 C-c C-f C-m  move to Mail-Followup-To
	 C-c C-f C-e  move to Expires
	 C-c C-f C-i  cycle through Importance values
	 C-c C-f s    change subject and append \"(was: <Old Subject>)\"
	 C-c C-f x    crossposting with FollowUp-To header and note in body
	 C-c C-f t    replace To: header with contents of Cc: or Bcc:
	 C-c C-f a    Insert X-No-Archive: header and a note in the body
C-c C-t  `message-insert-to' (add a To header to a news followup)
C-c C-l  `message-to-list-only' (removes all but list address in to/cc)
C-c C-n  `message-insert-newsgroups' (add a Newsgroup header to a news reply)
C-c C-b  `message-goto-body' (move to beginning of message text).
C-c C-i  `message-goto-signature' (move to the beginning of the signature).
C-c C-w  `message-insert-signature' (insert `message-signature-file' file).
C-c C-y  `message-yank-original' (insert current message, if any).
C-c C-q  `message-fill-yanked-message' (fill what was yanked).
C-c C-e  `message-elide-region' (elide the text between point and mark).
C-c C-v  `message-delete-not-region' (remove the text outside the region).
C-c C-z  `message-kill-to-signature' (kill the text up to the signature).
C-c C-r  `message-caesar-buffer-body' (rot13 the message body).
C-c C-a  `mml-attach-file' (attach a file as MIME).
C-c C-u  `message-insert-or-toggle-importance'  (insert or cycle importance).
C-c M-n  `message-insert-disposition-notification-to'  (request receipt).
C-c M-m  `message-mark-inserted-region' (mark region with enclosing tags).
C-c M-f  `message-mark-insert-file' (insert file marked with enclosing tags).
M-RET    `message-newline-and-reformat' (break the line and reformat).

\(fn)" t nil)

(autoload 'message-mail "nognus/lisp/message" "\
Start editing a mail message to be sent.
OTHER-HEADERS is an alist of header/value pairs.  CONTINUE says whether
to continue editing a message already being composed.  SWITCH-FUNCTION
is a function used to switch to and display the mail buffer.

\(fn &optional TO SUBJECT OTHER-HEADERS CONTINUE SWITCH-FUNCTION YANK-ACTION SEND-ACTIONS RETURN-ACTION &rest IGNORED)" t nil)

(autoload 'message-news "nognus/lisp/message" "\
Start editing a news article to be sent.

\(fn &optional NEWSGROUPS SUBJECT)" t nil)

(autoload 'message-reply "nognus/lisp/message" "\
Start editing a reply to the article in the current buffer.

\(fn &optional TO-ADDRESS WIDE SWITCH-FUNCTION)" t nil)

(autoload 'message-wide-reply "nognus/lisp/message" "\
Make a \"wide\" reply to the message in the current buffer.

\(fn &optional TO-ADDRESS)" t nil)

(autoload 'message-followup "nognus/lisp/message" "\
Follow up to the message in the current buffer.
If TO-NEWSGROUPS, use that as the new Newsgroups line.

\(fn &optional TO-NEWSGROUPS)" t nil)

(autoload 'message-cancel-news "nognus/lisp/message" "\
Cancel an article you posted.
If ARG, allow editing of the cancellation message.

\(fn &optional ARG)" t nil)

(autoload 'message-supersede "nognus/lisp/message" "\
Start composing a message to supersede the current message.
This is done simply by taking the old article and adding a Supersedes
header line with the old Message-ID.

\(fn)" t nil)

(autoload 'message-recover "nognus/lisp/message" "\
Reread contents of current buffer from its last auto-save file.

\(fn)" t nil)

(autoload 'message-forward "nognus/lisp/message" "\
Forward the current message via mail.
Optional NEWS will use news to forward instead of mail.
Optional DIGEST will use digest to forward.

\(fn &optional NEWS DIGEST)" t nil)

(autoload 'message-forward-make-body "nognus/lisp/message" "\


\(fn FORWARD-BUFFER &optional DIGEST)" nil nil)

(autoload 'message-forward-rmail-make-body "nognus/lisp/message" "\


\(fn FORWARD-BUFFER)" nil nil)

(autoload 'message-insinuate-rmail "nognus/lisp/message" "\
Let RMAIL use message to forward.

\(fn)" t nil)

(autoload 'message-resend "nognus/lisp/message" "\
Resend the current article to ADDRESS.

\(fn ADDRESS)" t nil)

(autoload 'message-bounce "nognus/lisp/message" "\
Re-mail the current message.
This only makes sense if the current message is a bounce message that
contains some mail you have written which has been bounced back to
you.

\(fn)" t nil)

(autoload 'message-mail-other-window "nognus/lisp/message" "\
Like `message-mail' command, but display mail buffer in another window.

\(fn &optional TO SUBJECT)" t nil)

(autoload 'message-mail-other-frame "nognus/lisp/message" "\
Like `message-mail' command, but display mail buffer in another frame.

\(fn &optional TO SUBJECT)" t nil)

(autoload 'message-news-other-window "nognus/lisp/message" "\
Start editing a news article to be sent.

\(fn &optional NEWSGROUPS SUBJECT)" t nil)

(autoload 'message-news-other-frame "nognus/lisp/message" "\
Start editing a news article to be sent.

\(fn &optional NEWSGROUPS SUBJECT)" t nil)

(autoload 'message-bold-region "nognus/lisp/message" "\
Bold all nonblank characters in the region.
Works by overstriking characters.
Called from program, takes two arguments START and END
which specify the range to operate on.

\(fn START END)" t nil)

(autoload 'message-unbold-region "nognus/lisp/message" "\
Remove all boldness (overstruck characters) in the region.
Called from program, takes two arguments START and END
which specify the range to operate on.

\(fn START END)" t nil)

;;;***

;;;### (autoloads (mm-default-file-encoding) "nognus/lisp/mm-encode"
;;;;;;  "nognus/lisp/mm-encode.el" (20621 19577))
;;; Generated autoloads from nognus/lisp/mm-encode.el

(autoload 'mm-default-file-encoding "nognus/lisp/mm-encode" "\
Return a default encoding for FILE.

\(fn FILE)" nil nil)

;;;***

;;;### (autoloads (mm-inline-external-body mm-extern-cache-contents)
;;;;;;  "nognus/lisp/mm-extern" "nognus/lisp/mm-extern.el" (20621
;;;;;;  19577))
;;; Generated autoloads from nognus/lisp/mm-extern.el

(autoload 'mm-extern-cache-contents "nognus/lisp/mm-extern" "\
Put the external-body part of HANDLE into its cache.

\(fn HANDLE)" nil nil)

(autoload 'mm-inline-external-body "nognus/lisp/mm-extern" "\
Show the external-body part of HANDLE.
This function replaces the buffer of HANDLE with a buffer contains
the entire message.
If NO-DISPLAY is nil, display it. Otherwise, do nothing after replacing.

\(fn HANDLE &optional NO-DISPLAY)" nil nil)

;;;***

;;;### (autoloads (mm-inline-partial) "nognus/lisp/mm-partial" "nognus/lisp/mm-partial.el"
;;;;;;  (20621 19577))
;;; Generated autoloads from nognus/lisp/mm-partial.el

(autoload 'mm-inline-partial "nognus/lisp/mm-partial" "\
Show the partial part of HANDLE.
This function replaces the buffer of HANDLE with a buffer contains
the entire message.
If NO-DISPLAY is nil, display it. Otherwise, do nothing after replacing.

\(fn HANDLE &optional NO-DISPLAY)" nil nil)

;;;***

;;;### (autoloads (mm-url-insert-file-contents-external mm-url-insert-file-contents)
;;;;;;  "nognus/lisp/mm-url" "nognus/lisp/mm-url.el" (20621 19577))
;;; Generated autoloads from nognus/lisp/mm-url.el

(autoload 'mm-url-insert-file-contents "nognus/lisp/mm-url" "\
Insert file contents of URL.
If `mm-url-use-external' is non-nil, use `mm-url-program'.

\(fn URL)" nil nil)

(autoload 'mm-url-insert-file-contents-external "nognus/lisp/mm-url" "\
Insert file contents of URL using `mm-url-program'.

\(fn URL)" nil nil)

;;;***

;;;### (autoloads (mm-uu-dissect-text-parts mm-uu-dissect) "nognus/lisp/mm-uu"
;;;;;;  "nognus/lisp/mm-uu.el" (20621 19577))
;;; Generated autoloads from nognus/lisp/mm-uu.el

(autoload 'mm-uu-dissect "nognus/lisp/mm-uu" "\
Dissect the current buffer and return a list of uu handles.
The optional NOHEADER means there's no header in the buffer.
MIME-TYPE specifies a MIME type and parameters, which defaults to the
value of `mm-uu-text-plain-type'.

\(fn &optional NOHEADER MIME-TYPE)" nil nil)

(autoload 'mm-uu-dissect-text-parts "nognus/lisp/mm-uu" "\
Dissect text parts and put uu handles into HANDLE.
Assume text has been decoded if DECODED is non-nil.

\(fn HANDLE &optional DECODED)" nil nil)

;;;***

;;;### (autoloads (mml-attach-file mml-to-mime) "nognus/lisp/mml"
;;;;;;  "nognus/lisp/mml.el" (20621 19577))
;;; Generated autoloads from nognus/lisp/mml.el

(autoload 'mml-to-mime "nognus/lisp/mml" "\
Translate the current buffer from MML to MIME.

\(fn)" nil nil)

(autoload 'mml-attach-file "nognus/lisp/mml" "\
Attach a file to the outgoing MIME message.
The file is not inserted or encoded until you send the message with
`\\[message-send-and-exit]' or `\\[message-send]' in Message mode,
or `\\[mail-send-and-exit]' or `\\[mail-send]' in Mail mode.

FILE is the name of the file to attach.  TYPE is its
content-type, a string of the form \"type/subtype\".  DESCRIPTION
is a one-line description of the attachment.  The DISPOSITION
specifies how the attachment is intended to be displayed.  It can
be either \"inline\" (displayed automatically within the message
body) or \"attachment\" (separate from the body).

\(fn FILE &optional TYPE DESCRIPTION DISPOSITION)" t nil)

;;;***

;;;### (autoloads (mml1991-sign mml1991-encrypt) "nognus/lisp/mml1991"
;;;;;;  "nognus/lisp/mml1991.el" (20621 19577))
;;; Generated autoloads from nognus/lisp/mml1991.el

(autoload 'mml1991-encrypt "nognus/lisp/mml1991" "\


\(fn CONT &optional SIGN)" nil nil)

(autoload 'mml1991-sign "nognus/lisp/mml1991" "\


\(fn CONT)" nil nil)

;;;***

;;;### (autoloads (mml2015-self-encrypt mml2015-sign mml2015-encrypt
;;;;;;  mml2015-verify-test mml2015-verify mml2015-decrypt-test mml2015-decrypt)
;;;;;;  "nognus/lisp/mml2015" "nognus/lisp/mml2015.el" (20621 19577))
;;; Generated autoloads from nognus/lisp/mml2015.el

(autoload 'mml2015-decrypt "nognus/lisp/mml2015" "\


\(fn HANDLE CTL)" nil nil)

(autoload 'mml2015-decrypt-test "nognus/lisp/mml2015" "\


\(fn HANDLE CTL)" nil nil)

(autoload 'mml2015-verify "nognus/lisp/mml2015" "\


\(fn HANDLE CTL)" nil nil)

(autoload 'mml2015-verify-test "nognus/lisp/mml2015" "\


\(fn HANDLE CTL)" nil nil)

(autoload 'mml2015-encrypt "nognus/lisp/mml2015" "\


\(fn CONT &optional SIGN)" nil nil)

(autoload 'mml2015-sign "nognus/lisp/mml2015" "\


\(fn CONT)" nil nil)

(autoload 'mml2015-self-encrypt "nognus/lisp/mml2015" "\


\(fn)" nil nil)

;;;***

;;;### (autoloads (netrc-credentials) "nognus/lisp/netrc" "nognus/lisp/netrc.el"
;;;;;;  (20621 19577))
;;; Generated autoloads from nognus/lisp/netrc.el

(autoload 'netrc-credentials "nognus/lisp/netrc" "\
Return a user name/password pair.
Port specifications will be prioritized in the order they are
listed in the PORTS list.

\(fn MACHINE &rest PORTS)" nil nil)

;;;***

;;;### (autoloads (nndiary-generate-nov-databases) "nognus/lisp/nndiary"
;;;;;;  "nognus/lisp/nndiary.el" (20621 19577))
;;; Generated autoloads from nognus/lisp/nndiary.el

(autoload 'nndiary-generate-nov-databases "nognus/lisp/nndiary" "\
Generate NOV databases in all nndiary directories.

\(fn &optional SERVER)" t nil)

;;;***

;;;### (autoloads (nndoc-add-type) "nognus/lisp/nndoc" "nognus/lisp/nndoc.el"
;;;;;;  (20621 19577))
;;; Generated autoloads from nognus/lisp/nndoc.el

(autoload 'nndoc-add-type "nognus/lisp/nndoc" "\
Add document DEFINITION to the list of nndoc document definitions.
If POSITION is nil or `last', the definition will be added
as the last checked definition, if t or `first', add as the
first definition, and if any other symbol, add after that
symbol in the alist.

\(fn DEFINITION &optional POSITION)" nil nil)

;;;***

;;;### (autoloads (nnfolder-generate-active-file) "nognus/lisp/nnfolder"
;;;;;;  "nognus/lisp/nnfolder.el" (20621 19577))
;;; Generated autoloads from nognus/lisp/nnfolder.el

(autoload 'nnfolder-generate-active-file "nognus/lisp/nnfolder" "\
Look for mbox folders in the nnfolder directory and make them into groups.
This command does not work if you use short group names.

\(fn)" t nil)

;;;***

;;;### (autoloads (nnml-generate-nov-databases) "nognus/lisp/nnml"
;;;;;;  "nognus/lisp/nnml.el" (20621 19577))
;;; Generated autoloads from nognus/lisp/nnml.el

(autoload 'nnml-generate-nov-databases "nognus/lisp/nnml" "\
Generate NOV databases in all nnml directories.

\(fn &optional SERVER)" t nil)

;;;***

;;;### (autoloads (parse-time-string) "nognus/lisp/parse-time" "nognus/lisp/parse-time.el"
;;;;;;  (20621 19577))
;;; Generated autoloads from nognus/lisp/parse-time.el
(put 'parse-time-rules 'risky-local-variable t)

(autoload 'parse-time-string "nognus/lisp/parse-time" "\
Parse the time-string STRING into (SEC MIN HOUR DAY MON YEAR DOW DST TZ).
The values are identical to those of `decode-time', but any values that are
unknown are returned as nil.

\(fn STRING)" nil nil)

;;;***

;;;### (autoloads (password-in-cache-p password-cache-expiry password-cache)
;;;;;;  "nognus/lisp/password-cache" "nognus/lisp/password-cache.el"
;;;;;;  (20621 19577))
;;; Generated autoloads from nognus/lisp/password-cache.el

(defvar password-cache t "\
Whether to cache passwords.")

(custom-autoload 'password-cache "nognus/lisp/password-cache" t)

(defvar password-cache-expiry 16 "\
How many seconds passwords are cached, or nil to disable expiring.
Whether passwords are cached at all is controlled by `password-cache'.")

(custom-autoload 'password-cache-expiry "nognus/lisp/password-cache" t)

(autoload 'password-in-cache-p "nognus/lisp/password-cache" "\
Check if KEY is in the cache.

\(fn KEY)" nil nil)

;;;***

;;;### (autoloads (plstore-mode plstore-open) "nognus/lisp/plstore"
;;;;;;  "nognus/lisp/plstore.el" (20621 19577))
;;; Generated autoloads from nognus/lisp/plstore.el

(autoload 'plstore-open "nognus/lisp/plstore" "\
Create a plstore instance associated with FILE.

\(fn FILE)" nil nil)

(autoload 'plstore-mode "nognus/lisp/plstore" "\
Major mode for editing PLSTORE files.

\(fn)" t nil)

;;;***

;;;### (autoloads (pop3-movemail) "nognus/lisp/pop3" "nognus/lisp/pop3.el"
;;;;;;  (20621 19577))
;;; Generated autoloads from nognus/lisp/pop3.el

(autoload 'pop3-movemail "nognus/lisp/pop3" "\
Transfer contents of a maildrop to the specified FILE.
Use streaming commands.

\(fn FILE)" nil nil)

;;;***

;;;### (autoloads (open-protocol-stream) "nognus/lisp/proto-stream"
;;;;;;  "nognus/lisp/proto-stream.el" (20621 19577))
;;; Generated autoloads from nognus/lisp/proto-stream.el

(autoload 'open-protocol-stream "nognus/lisp/proto-stream" "\
Open a network stream to HOST, possibly with encryption.
Normally, return a network process object; with a non-nil
:return-list parameter, return a list instead (see below).

The first four parameters, NAME, BUFFER, HOST, and SERVICE, have
the same meanings as in `open-network-stream'.  The remaining
PARAMETERS should be a sequence of keywords and values:

:type specifies the connection type, one of the following:
  nil or `network'
             -- Begin with an ordinary network connection, and if
                the parameters :success and :capability-command
                are also supplied, try to upgrade to an encrypted
                connection via STARTTLS.  Even if that
                fails (e.g. if HOST does not support TLS), retain
                an unencrypted connection.
  `plain'    -- An ordinary, unencrypted network connection.
  `starttls' -- Begin with an ordinary connection, and try
                upgrading via STARTTLS.  If that fails for any
                reason, drop the connection; in that case the
                returned object is a killed process.
  `tls'      -- A TLS connection.
  `ssl'      -- Equivalent to `tls'.
  `shell'    -- A shell connection.

:return-list specifies this function's return value.
  If omitted or nil, return a process object.  A non-nil means to
  return (PROC . PROPS), where PROC is a process object and PROPS
  is a plist of connection properties, with these keywords:
   :greeting -- the greeting returned by HOST (a string), or nil.
   :capabilities -- a string representing HOST's capabilities,
                    or nil if none could be found.
   :type -- the resulting connection type; `plain' (unencrypted)
            or `tls' (TLS-encrypted).

:end-of-command specifies a regexp matching the end of a command.
  If non-nil, it defaults to \"\\n\".

:end-of-capability specifies a regexp matching the end of the
  response to the command specified for :capability-command.
  It defaults to the regexp specified for :end-of-command.

:success specifies a regexp matching a message indicating a
  successful STARTTLS negotiation.  For instance, the default
  should be \"^3\" for an NNTP connection.

:capability-command specifies a command used to query the HOST
  for its capabilities.  For instance, for IMAP this should be
  \"1 CAPABILITY\\r\\n\".

:starttls-function specifies a function for handling STARTTLS.
  This function should take one parameter, the response to the
  capability command, and should return the command to switch on
  STARTTLS if the server supports STARTTLS, and nil otherwise.

\(fn NAME BUFFER HOST SERVICE &rest PARAMETERS)" nil nil)

;;;***

;;;### (autoloads (quoted-printable-decode-region) "nognus/lisp/qp"
;;;;;;  "nognus/lisp/qp.el" (20621 19577))
;;; Generated autoloads from nognus/lisp/qp.el

(autoload 'quoted-printable-decode-region "nognus/lisp/qp" "\
Decode quoted-printable in the region between FROM and TO, per RFC 2045.
If CODING-SYSTEM is non-nil, decode bytes into characters with that
coding-system.

Interactively, you can supply the CODING-SYSTEM argument
with \\[universal-coding-system-argument].

The CODING-SYSTEM argument is a historical hangover and is deprecated.
QP encodes raw bytes and should be decoded into raw bytes.  Decoding
them into characters should be done separately.

\(fn FROM TO &optional CODING-SYSTEM)" t nil)

;;;***

;;;### (autoloads (gnus-score-mode) "nognus/lisp/score-mode" "nognus/lisp/score-mode.el"
;;;;;;  (20621 19577))
;;; Generated autoloads from nognus/lisp/score-mode.el

(autoload 'gnus-score-mode "nognus/lisp/score-mode" "\
Mode for editing Gnus score files.
This mode is an extended emacs-lisp mode.

\\{gnus-score-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads (sha1) "nognus/lisp/sha1" "nognus/lisp/sha1.el"
;;;;;;  (20621 19577))
;;; Generated autoloads from nognus/lisp/sha1.el

(autoload 'sha1 "nognus/lisp/sha1" "\
Return the SHA1 (Secure Hash Algorithm) of an object.
OBJECT is either a string or a buffer.
Optional arguments BEG and END denote buffer positions for computing the
hash of a portion of OBJECT.
If BINARY is non-nil, return a string in binary form.

\(fn OBJECT &optional BEG END BINARY)" nil nil)

;;;***

;;;### (autoloads (shr-insert-document) "nognus/lisp/shr" "nognus/lisp/shr.el"
;;;;;;  (20621 19577))
;;; Generated autoloads from nognus/lisp/shr.el

(autoload 'shr-insert-document "nognus/lisp/shr" "\
Render the parsed document DOM into the current buffer.
DOM should be a parse tree as generated by
`libxml-parse-html-region' or similar.

\(fn DOM)" nil nil)

;;;***

;;;### (autoloads (sieve-upload-and-kill sieve-upload-and-bury sieve-upload
;;;;;;  sieve-manage) "nognus/lisp/sieve" "nognus/lisp/sieve.el"
;;;;;;  (20621 19577))
;;; Generated autoloads from nognus/lisp/sieve.el

(autoload 'sieve-manage "nognus/lisp/sieve" "\


\(fn SERVER &optional PORT)" t nil)

(autoload 'sieve-upload "nognus/lisp/sieve" "\


\(fn &optional NAME)" t nil)

(autoload 'sieve-upload-and-bury "nognus/lisp/sieve" "\


\(fn &optional NAME)" t nil)

(autoload 'sieve-upload-and-kill "nognus/lisp/sieve" "\


\(fn &optional NAME)" t nil)

;;;***

;;;### (autoloads (sieve-mode) "nognus/lisp/sieve-mode" "nognus/lisp/sieve-mode.el"
;;;;;;  (20621 19577))
;;; Generated autoloads from nognus/lisp/sieve-mode.el

(autoload 'sieve-mode "nognus/lisp/sieve-mode" "\
Major mode for editing Sieve code.
This is much like C mode except for the syntax of comments.  Its keymap
inherits from C mode's and it has the same variables for customizing
indentation.  It has its own abbrev table and its own syntax table.

Turning on Sieve mode runs `sieve-mode-hook'.

\(fn)" t nil)

;;;***

;;;### (autoloads (smiley-buffer smiley-region) "nognus/lisp/smiley"
;;;;;;  "nognus/lisp/smiley.el" (20621 19577))
;;; Generated autoloads from nognus/lisp/smiley.el

(autoload 'smiley-region "nognus/lisp/smiley" "\
Replace in the region `smiley-regexp-alist' matches with corresponding images.
A list of images is returned.

\(fn START END)" t nil)

(autoload 'smiley-buffer "nognus/lisp/smiley" "\
Run `smiley-region' at the BUFFER, specified in the argument or
interactively.  If there's no argument, do it at the current buffer.

\(fn &optional BUFFER)" t nil)

;;;***

;;;### (autoloads (spam-initialize) "nognus/lisp/spam" "nognus/lisp/spam.el"
;;;;;;  (20621 19577))
;;; Generated autoloads from nognus/lisp/spam.el

(autoload 'spam-initialize "nognus/lisp/spam" "\
Install the spam.el hooks and do other initialization.
When SYMBOLS is given, set those variables to t.  This is so you
can call `spam-initialize' before you set spam-use-* variables on
explicitly, and matters only if you need the extra headers
installed through `spam-necessary-extra-headers'.

\(fn &rest SYMBOLS)" t nil)

;;;***

;;;### (autoloads (spam-report-deagentize spam-report-agentize spam-report-url-to-file
;;;;;;  spam-report-url-ping-mm-url spam-report-process-queue) "nognus/lisp/spam-report"
;;;;;;  "nognus/lisp/spam-report.el" (20621 19577))
;;; Generated autoloads from nognus/lisp/spam-report.el

(autoload 'spam-report-process-queue "nognus/lisp/spam-report" "\
Report all queued requests from `spam-report-requests-file'.

If FILE is given, use it instead of `spam-report-requests-file'.
If KEEP is t, leave old requests in the file.  If KEEP is the
symbol `ask', query before flushing the queue file.

\(fn &optional FILE KEEP)" t nil)

(autoload 'spam-report-url-ping-mm-url "nognus/lisp/spam-report" "\
Ping a host through HTTP, addressing a specific GET resource. Use
the external program specified in `mm-url-program' to connect to
server.

\(fn HOST REPORT)" nil nil)

(autoload 'spam-report-url-to-file "nognus/lisp/spam-report" "\
Collect spam report requests in `spam-report-requests-file'.
Customize `spam-report-url-ping-function' to use this function.

\(fn HOST REPORT)" nil nil)

(autoload 'spam-report-agentize "nognus/lisp/spam-report" "\
Add spam-report support to the Agent.
Spam reports will be queued with \\[spam-report-url-to-file] when
the Agent is unplugged, and will be submitted in a batch when the
Agent is plugged.

\(fn)" t nil)

(autoload 'spam-report-deagentize "nognus/lisp/spam-report" "\
Remove spam-report support from the Agent.
Spam reports will be queued with the method used when
\\[spam-report-agentize] was run.

\(fn)" t nil)

;;;***

;;;### (autoloads (starttls-open-stream) "nognus/lisp/starttls" "nognus/lisp/starttls.el"
;;;;;;  (20621 19577))
;;; Generated autoloads from nognus/lisp/starttls.el

(autoload 'starttls-open-stream "nognus/lisp/starttls" "\
Open a TLS connection for a port to a host.
Returns a subprocess object to represent the connection.
Input and output work as for subprocesses; `delete-process' closes it.
Args are NAME BUFFER HOST PORT.
NAME is name for process.  It is modified if necessary to make it unique.
BUFFER is the buffer (or `buffer-name') to associate with the process.
 Process output goes at end of that buffer, unless you specify
 an output stream or filter function to handle the output.
 BUFFER may be also nil, meaning that this process is not associated
 with any buffer
Third arg is name of the host to connect to, or its IP address.
Fourth arg PORT is an integer specifying a port to connect to.
If `starttls-use-gnutls' is nil, this may also be a service name, but
GnuTLS requires a port number.

\(fn NAME BUFFER HOST PORT)" nil nil)

;;;***

;;;### (autoloads (format-seconds safe-date-to-time time-to-days
;;;;;;  time-to-day-in-year date-leap-year-p days-between date-to-day
;;;;;;  time-add time-subtract time-since days-to-time time-less-p
;;;;;;  seconds-to-time date-to-time) "nognus/lisp/time-date" "nognus/lisp/time-date.el"
;;;;;;  (20621 19577))
;;; Generated autoloads from nognus/lisp/time-date.el

(autoload 'date-to-time "nognus/lisp/time-date" "\
Parse a string DATE that represents a date-time and return a time value.
If DATE lacks timezone information, GMT is assumed.

\(fn DATE)" nil nil)
(if (or (featurep 'emacs)
       (and (fboundp 'float-time)
            (subrp (symbol-function 'float-time))))
   (progn
     (defalias 'time-to-seconds 'float-time)
     (make-obsolete 'time-to-seconds 'float-time "21.1"))
 (autoload 'time-to-seconds "time-date"))

(autoload 'seconds-to-time "nognus/lisp/time-date" "\
Convert SECONDS (a floating point number) to a time value.

\(fn SECONDS)" nil nil)

(autoload 'time-less-p "nognus/lisp/time-date" "\
Return non-nil if time value T1 is earlier than time value T2.

\(fn T1 T2)" nil nil)

(autoload 'days-to-time "nognus/lisp/time-date" "\
Convert DAYS into a time value.

\(fn DAYS)" nil nil)

(autoload 'time-since "nognus/lisp/time-date" "\
Return the time elapsed since TIME.
TIME should be either a time value or a date-time string.

\(fn TIME)" nil nil)

(defalias 'subtract-time 'time-subtract)

(autoload 'time-subtract "nognus/lisp/time-date" "\
Subtract two time values, T1 minus T2.
Return the difference in the format of a time value.

\(fn T1 T2)" nil nil)

(autoload 'time-add "nognus/lisp/time-date" "\
Add two time values T1 and T2.  One should represent a time difference.

\(fn T1 T2)" nil nil)

(autoload 'date-to-day "nognus/lisp/time-date" "\
Return the number of days between year 1 and DATE.
DATE should be a date-time string.

\(fn DATE)" nil nil)

(autoload 'days-between "nognus/lisp/time-date" "\
Return the number of days between DATE1 and DATE2.
DATE1 and DATE2 should be date-time strings.

\(fn DATE1 DATE2)" nil nil)

(autoload 'date-leap-year-p "nognus/lisp/time-date" "\
Return t if YEAR is a leap year.

\(fn YEAR)" nil nil)

(autoload 'time-to-day-in-year "nognus/lisp/time-date" "\
Return the day number within the year corresponding to TIME.

\(fn TIME)" nil nil)

(autoload 'time-to-days "nognus/lisp/time-date" "\
The number of days between the Gregorian date 0001-12-31bce and TIME.
TIME should be a time value.
The Gregorian date Sunday, December 31, 1bce is imaginary.

\(fn TIME)" nil nil)

(autoload 'safe-date-to-time "nognus/lisp/time-date" "\
Parse a string DATE that represents a date-time and return a time value.
If DATE is malformed, return a time value of zeros.

\(fn DATE)" nil nil)

(autoload 'format-seconds "nognus/lisp/time-date" "\
Use format control STRING to format the number SECONDS.
The valid format specifiers are:
%y is the number of (365-day) years.
%d is the number of days.
%h is the number of hours.
%m is the number of minutes.
%s is the number of seconds.
%z is a non-printing control flag (see below).
%% is a literal \"%\".

Upper-case specifiers are followed by the unit-name (e.g. \"years\").
Lower-case specifiers return only the unit.

\"%\" may be followed by a number specifying a width, with an
optional leading \".\" for zero-padding.  For example, \"%.3Y\" will
return something of the form \"001 year\".

The \"%z\" specifier does not print anything.  When it is used, specifiers
must be given in order of decreasing size.  To the left of \"%z\", nothing
is output until the first non-zero unit is encountered.

This function does not work for SECONDS greater than `most-positive-fixnum'.

\(fn STRING SECONDS)" nil nil)

;;;***

;;;### (autoloads (utf7-encode) "nognus/lisp/utf7" "nognus/lisp/utf7.el"
;;;;;;  (20621 19577))
;;; Generated autoloads from nognus/lisp/utf7.el

(autoload 'utf7-encode "nognus/lisp/utf7" "\
Encode UTF-7 STRING.  Use IMAP modification if FOR-IMAP is non-nil.

\(fn STRING &optional FOR-IMAP)" nil nil)

;;;***

;;;### (autoloads (uudecode-decode-region uudecode-decode-region-internal
;;;;;;  uudecode-decode-region-external) "nognus/lisp/uudecode" "nognus/lisp/uudecode.el"
;;;;;;  (20621 19577))
;;; Generated autoloads from nognus/lisp/uudecode.el

(autoload 'uudecode-decode-region-external "nognus/lisp/uudecode" "\
Uudecode region between START and END using external program.
If FILE-NAME is non-nil, save the result to FILE-NAME.  The program
used is specified by `uudecode-decoder-program'.

\(fn START END &optional FILE-NAME)" t nil)

(autoload 'uudecode-decode-region-internal "nognus/lisp/uudecode" "\
Uudecode region between START and END without using an external program.
If FILE-NAME is non-nil, save the result to FILE-NAME.

\(fn START END &optional FILE-NAME)" t nil)

(autoload 'uudecode-decode-region "nognus/lisp/uudecode" "\
Uudecode region between START and END.
If FILE-NAME is non-nil, save the result to FILE-NAME.

\(fn START END &optional FILE-NAME)" nil nil)

;;;***

;;;### (autoloads (yenc-extract-filename yenc-decode-region) "nognus/lisp/yenc"
;;;;;;  "nognus/lisp/yenc.el" (20621 19577))
;;; Generated autoloads from nognus/lisp/yenc.el

(autoload 'yenc-decode-region "nognus/lisp/yenc" "\
Yenc decode region between START and END using an internal decoder.

\(fn START END)" t nil)

(autoload 'yenc-extract-filename "nognus/lisp/yenc" "\
Extract file name from an yenc header.

\(fn)" nil nil)

;;;***

;;;### (autoloads (nrepl-ritz-jack-in) "nrepl-ritz/nrepl-ritz" "nrepl-ritz/nrepl-ritz.el"
;;;;;;  (20621 19578))
;;; Generated autoloads from nrepl-ritz/nrepl-ritz.el

(autoload 'nrepl-ritz-jack-in "nrepl-ritz/nrepl-ritz" "\


\(fn PROMPT-PROJECT)" t nil)

;;;***

;;;### (autoloads (nrepl nrepl-jack-in nrepl-disable-on-existing-clojure-buffers
;;;;;;  nrepl-enable-on-existing-clojure-buffers nrepl-interaction-mode)
;;;;;;  "nrepl/nrepl" "nrepl/nrepl.el" (20621 19572))
;;; Generated autoloads from nrepl/nrepl.el

(autoload 'nrepl-interaction-mode "nrepl/nrepl" "\
Minor mode for nrepl interaction from a Clojure buffer.

\(fn &optional ARG)" t nil)

(autoload 'nrepl-enable-on-existing-clojure-buffers "nrepl/nrepl" "\


\(fn)" t nil)

(autoload 'nrepl-disable-on-existing-clojure-buffers "nrepl/nrepl" "\


\(fn)" t nil)

(autoload 'nrepl-jack-in "nrepl/nrepl" "\


\(fn &optional PROMPT-PROJECT)" t nil)

(add-hook 'nrepl-connected-hook 'nrepl-enable-on-existing-clojure-buffers)

(autoload 'nrepl "nrepl/nrepl" "\


\(fn HOST PORT)" t nil)

;;;***

;;;### (autoloads (nxhtml-byte-recompile-file nxhtml-byte-compile-file
;;;;;;  nxhtml-get-missing-files nxhtml-update-existing-files nxhtml-setup-download-all
;;;;;;  nxhtml-setup-auto-download nxhtml-setup-install) "nxhtml/nxhtml-web-vcs"
;;;;;;  "nxhtml/nxhtml-web-vcs.el" (20690 26309))
;;; Generated autoloads from nxhtml/nxhtml-web-vcs.el

(autoload 'nxhtml-setup-install "nxhtml/nxhtml-web-vcs" "\
Setup and start nXhtml installation.

This is for installation and updating directly from the nXhtml
development sources.

There are two different ways to install:

  (1) Download all at once: `nxhtml-setup-download-all'
  (2) Automatically download part by part: `nxhtml-setup-auto-download'

You can convert between those ways by calling this function again.
You can also do this by setting the option `nxhtml-autoload-web' yourself.

When you have nXhtml installed you can update it:

  (3) Update new files in nXhtml: `nxhtml-update-existing-files'

To learn more about nXhtml visit its home page at URL
`http://www.emacswiki.com/NxhtmlMode/'.

If you want to test auto download (but not use it further) there
is a special function for that, you answer T here:

   (T) Test automatic download part by part: `nxhtml-setup-test-auto-download'

======
*Note*
If you want to download a zip file with latest released version instead then
please see URL `http://ourcomments.org/Emacs/nXhtml/doc/nxhtml.html'.

\(fn WAY)" t nil)

(autoload 'nxhtml-setup-auto-download "nxhtml/nxhtml-web-vcs" "\
Set up to autoload nXhtml files from the web.

This function will download some initial files and then setup to
download the rest when you need them.

Files will be downloaded under the directory root you specify in
DL-DIR.

Note that files will not be upgraded automatically.  The auto
downloading is just for files you are missing. (This may change a
bit in the future.) If you want to upgrade those files that you
have downloaded you can just call `nxhtml-update-existing-files'.

You can easily switch between this mode of downloading or
downloading the whole of nXhtml by once.  To switch just call the
command `nxhtml-setup-install'.

See also the command `nxhtml-setup-download-all'.

Note: If your nXhtml is to old you can't use this function
      directly.  You have to upgrade first, se the function
      above. Version 2.07 or above is good for this.

\(fn DL-DIR)" t nil)

(autoload 'nxhtml-setup-download-all "nxhtml/nxhtml-web-vcs" "\
Download or update all of nXhtml.

You can download all if nXhtml with this command.

To update existing files use `nxhtml-update-existing-files'.

If you want to download only those files you are actually using
then call `nxhtml-setup-auto-download' instead.

See the command `nxhtml-setup-install' for a convenient way to
call these commands.

For more information about auto download of nXhtml files see
`nxhtml-setup-auto-download'.

\(fn DL-DIR)" t nil)

(autoload 'nxhtml-update-existing-files "nxhtml/nxhtml-web-vcs" "\
Update existing nXhtml files from the development sources.
Only files you already have will be updated.

Note that this works both if you have setup nXhtml to auto
download files as you need them or if you have downloaded all of
nXhtml at once.

For more information about installing and updating nXhtml see the
command `nxhtml-setup-install'.

\(fn)" t nil)

(autoload 'nxhtml-get-missing-files "nxhtml/nxhtml-web-vcs" "\
Download to SUB-DIR missing files matching FILE-NAME-LIST.
If FILE-NAME-LIST is nil download all missing files.
If it is a list download all missing files in the list.
If it is a regexp download all missing matching files.

\(fn SUB-DIR FILE-NAME-LIST)" nil nil)

(autoload 'nxhtml-byte-compile-file "nxhtml/nxhtml-web-vcs" "\


\(fn FILE &optional LOAD)" nil nil)

(autoload 'nxhtml-byte-recompile-file "nxhtml/nxhtml-web-vcs" "\
Byte recompile FILE file if necessary.
For more information see `nxhtml-byte-compile-file'.
Loading is done if recompiled and LOAD is t.

\(fn FILE &optional LOAD)" t nil)

;;;***

;;;### (autoloads (nxhtmlmaint-byte-uncompile-all nxhtmlmaint-byte-recompile
;;;;;;  nxhtmlmaint-start-byte-compilation) "nxhtml/nxhtmlmaint"
;;;;;;  "nxhtml/nxhtmlmaint.el" (20690 26309))
;;; Generated autoloads from nxhtml/nxhtmlmaint.el

(autoload 'nxhtmlmaint-start-byte-compilation "nxhtml/nxhtmlmaint" "\
Start byte compilation of nXhtml in new Emacs instance.
Byte compiling in general makes elisp code run 5-10 times faster
which is quite noticeable when you use nXhtml.

This will also update the file nxhtml-loaddefs.el.

You must restart Emacs to use the byte compiled files.

If for some reason the byte compiled files does not work you can
remove then with `nxhtmlmaint-byte-uncompile-all'.

See also `nxhtmlmaint-byte-recompile'

\(fn)" t nil)

(autoload 'nxhtmlmaint-byte-recompile "nxhtml/nxhtmlmaint" "\
Recompile or compile all nXhtml files in current Emacs.
Byte compile all elisp libraries whose .el files are newer their
.elc files.

\(fn)" t nil)

(autoload 'nxhtmlmaint-byte-uncompile-all "nxhtml/nxhtmlmaint" "\
Delete byte compiled files in nXhtml.
This will also update the file nxhtml-loaddefs.el.

See `nxhtmlmaint-start-byte-compilation' for byte compiling.

\(fn)" t nil)

;;;***

;;;### (autoloads (web-vcs-investigate-elisp-file web-vcs-url-copy-file
;;;;;;  web-vcs-url-retrieve-synch web-vcs-byte-compile-file web-vcs-message-with-face
;;;;;;  web-vcs-get-files-from-root web-vcs-log-edit web-vcs-default-download-directory)
;;;;;;  "nxhtml/web-vcs" "nxhtml/web-vcs.el" (20690 26309))
;;; Generated autoloads from nxhtml/web-vcs.el

(autoload 'web-vcs-default-download-directory "nxhtml/web-vcs" "\
Try to find a suitable place.
Use the choice in `web-vcs-default-download-directory'.
If this does not fit fall back to \"~/.emacs.d/\".

\(fn)" nil nil)

(autoload 'web-vcs-log-edit "nxhtml/web-vcs" "\
Open log file.

\(fn)" t nil)

(autoload 'web-vcs-get-files-from-root "nxhtml/web-vcs" "\
Download a file tree from VCS system using the web interface.
Use WEB-VCS entry in variable `web-vcs-links-regexp' to download
files via http from FULL-URL to directory DL-DIR.

Show FULL-URL first and offer to visit the page.  That page will
give you information about version control system (VCS) system
used etc.

\(fn WEB-VCS FULL-URL DL-DIR)" nil nil)

(autoload 'web-vcs-message-with-face "nxhtml/web-vcs" "\
Display a colored message at the bottom of the string.
FACE is the face to use for the message.
FORMAT-STRING and ARGS are the same as for `message'.

Also put FACE on the message in *Messages* buffer.

\(fn FACE FORMAT-STRING &rest ARGS)" nil nil)

(autoload 'web-vcs-byte-compile-file "nxhtml/web-vcs" "\
Byte compile FILE in a new Emacs sub process.
EXTRA-LOAD-PATH is added to the front of `load-path' during
compilation.

FILE is set to `buffer-file-name' when called interactively.
If LOAD

\(fn FILE &optional LOAD EXTRA-LOAD-PATH COMP-DIR)" t nil)

(autoload 'web-vcs-url-retrieve-synch "nxhtml/web-vcs" "\
Retrieve URL, return cons with buffer and http status.

\(fn URL)" nil nil)

(autoload 'web-vcs-url-copy-file "nxhtml/web-vcs" "\
Copy URL to NEWNAME.  Both args must be strings.
Signals a `file-already-exists' error if file NEWNAME already exists,
unless a third argument OK-IF-ALREADY-EXISTS is supplied and non-nil.
A number as third arg means request confirmation if NEWNAME already exists.
This is what happens in interactive use with M-x.
Fourth arg KEEP-TIME non-nil means give the new file the same
last-modified time as the old one.  (This works on only some systems.)
Fifth arg PRESERVE-UID-GID is ignored.
A prefix arg makes KEEP-TIME non-nil.

\(fn URL NEWNAME &optional OK-IF-ALREADY-EXISTS KEEP-TIME PRESERVE-UID-GID)" nil nil)

(autoload 'web-vcs-investigate-elisp-file "nxhtml/web-vcs" "\


\(fn FILE-OR-BUFFER)" t nil)

;;;***

;;;### (autoloads (org-publish-blog-sync org-publish-blog o-blog-version)
;;;;;;  "o-blog/o-blog" "o-blog/o-blog.el" (20621 19580))
;;; Generated autoloads from o-blog/o-blog.el

(autoload 'o-blog-version "o-blog/o-blog" "\
Message the current o-blog version. If call using
`universal-argument', insert value in current position.

\(fn &optional HERE)" t nil)

(autoload 'org-publish-blog "o-blog/o-blog" "\
Publish FILE as a blog synchronously execpt ib ASYNC is
defined, or interactivelly called with `prefix-arg'.

\(fn &optional FILE ASYNC)" t nil)

(autoload 'org-publish-blog-sync "o-blog/o-blog" "\
Publish FILE synchronously.

\(fn FILE)" nil nil)

;;;***

;;;### (autoloads (o-blog-publish-alert) "o-blog/o-blog-alert" "o-blog/o-blog-alert.el"
;;;;;;  (20621 19579))
;;; Generated autoloads from o-blog/o-blog-alert.el

(autoload 'o-blog-publish-alert "o-blog/o-blog-alert" "\
Publish an alert in HTML mode.

Admonitions are specially marked topics that can appear
anywhere an ordinary body element can. They contain arbitrary
body elements. Typically, an alert is rendered as an offset
block in a document, sometimes outlined or shaded, with a title
matching the alert type. For example:

#+BEGIN_O_BLOG_ALERT type title
Some text inside the alert
#+END_O_BLOG_ALERT

Where type can be on of:

  - info
  - success
  - warning
  - error

This directive might be rendered something like this:

+----------------------------+
| Title                      |
|                            |
| Some text inside the alert |
+----------------------------+

In an HTML context, previous directive would be expanded as:

#+BEGIN_HTML
<div class=\"alert alert-type\">
<p class=\"alert-heading\">Title</p>
#+END_HTML
Some text inside the alert
#+BEGIN_HTML
<div>
#+END_HTML

The default replacement text could be changed using variables
`o-blog-alert-header', `o-blog-alert-footer' and
`o-blog-alert-title'.

\(fn)" nil nil)

;;;***

;;;### (autoloads (o-blog-publish-grid) "o-blog/o-blog-grid" "o-blog/o-blog-grid.el"
;;;;;;  (20621 19580))
;;; Generated autoloads from o-blog/o-blog-grid.el

(autoload 'o-blog-publish-grid "o-blog/o-blog-grid" "\
Publish a grid in HTML mode.

The default replacement text could be changed using variables
`o-blog-grid-header', `o-blog-grid-footer' and
`o-blog-grid-title'.

\(fn)" nil nil)

;;;***

;;;### (autoloads (o-blog-publish-source) "o-blog/o-blog-source"
;;;;;;  "o-blog/o-blog-source.el" (20621 19580))
;;; Generated autoloads from o-blog/o-blog-source.el

(autoload 'o-blog-publish-source "o-blog/o-blog-source" "\
Publish an sourced file in HTML mode.

A source file is defined using:

    #+O_BLOG_SOURCE: path/to/file [mode]

and is converted to

    #+BEGIN_HTML
    <div class=\"o-blog-source\">
    <div class=\"title\">file</div>
    <div style=\"display:none;\" class=\"content\">
    #+END_HTML
    #+BEGIN_SRC mode
    (file content)
    #+END_SRC
    #+BEGIN_HTML
    </div></div>
    #+END_HTML

The default replacement text could be changed using variables
`o-blog-source-header' and `o-blog-source-footer'.

\(fn)" nil nil)

;;;***

;;;### (autoloads (offlineimap) "offlineimap/offlineimap" "offlineimap/offlineimap.el"
;;;;;;  (20621 19571))
;;; Generated autoloads from offlineimap/offlineimap.el

(autoload 'offlineimap "offlineimap/offlineimap" "\
Start OfflineIMAP.

\(fn)" t nil)

;;;***

;;;### (autoloads (paredit-mode) "paredit/paredit" "paredit/paredit.el"
;;;;;;  (20621 19579))
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

;;;### (autoloads (popwin:messages popwin:find-file-tail popwin:find-file
;;;;;;  popwin:popup-buffer-tail popwin:one-window popwin:universal-display
;;;;;;  popwin:pop-to-buffer popwin:display-last-buffer popwin:display-buffer
;;;;;;  popwin:popup-buffer) "popwin/popwin" "popwin/popwin.el" (20621
;;;;;;  19578))
;;; Generated autoloads from popwin/popwin.el

(autoload 'popwin:popup-buffer "popwin/popwin" "\
Show BUFFER in a popup window and return the popup window. If
NOSELECT is non-nil, the popup window will not be selected. If
STICK is non-nil, the popup window will be stuck. If TAIL is
non-nil, the popup window will show the last contents. Calling
`popwin:popup-buffer' during `popwin:popup-buffer' is allowed. In
that case, the buffer of the popup window will be replaced with
BUFFER.

\(fn BUFFER &key (width popwin:popup-window-width) (height popwin:popup-window-height) (position popwin:popup-window-position) NOSELECT DEDICATED STICK TAIL)" t nil)

(autoload 'popwin:display-buffer "popwin/popwin" "\
Display BUFFER-OR-NAME, if possible, in a popup window, or as
usual. This function can be used as a value of
`display-buffer-function'.

\(fn BUFFER-OR-NAME &optional NOT-THIS-WINDOW)" t nil)

(autoload 'popwin:display-last-buffer "popwin/popwin" "\
Display the lastly shown buffer by `popwin:display-buffer' and
`popwin:special-display-popup-window'.

\(fn)" t nil)

(autoload 'popwin:pop-to-buffer "popwin/popwin" "\
Same as `pop-to-buffer' except that this function will use
`popwin:display-buffer-1' instead of `display-buffer'.

\(fn BUFFER &optional OTHER-WINDOW NORECORD)" t nil)

(autoload 'popwin:universal-display "popwin/popwin" "\
Call the following command interactively with letting
`popwin:special-display-config' be
`popwin:universal-display-config'. This wil be useful when
displaying buffers in popup windows temporarily.

\(fn)" t nil)

(autoload 'popwin:one-window "popwin/popwin" "\
Delete other window than the popup window. C-g restores the
original window configuration.

\(fn)" t nil)

(autoload 'popwin:popup-buffer-tail "popwin/popwin" "\
Same as `popwin:popup-buffer' except that the buffer will be
`recenter'ed at the bottom.

\(fn &rest SAME-AS-POPWIN:POPUP-BUFFER)" t nil)

(autoload 'popwin:find-file "popwin/popwin" "\
Edit file FILENAME with popup window by `popwin:popup-buffer'.

\(fn FILENAME &optional WILDCARDS)" t nil)

(autoload 'popwin:find-file-tail "popwin/popwin" "\
Edit file FILENAME with popup window by
`popwin:popup-buffer-tail'.

\(fn FILE &optional WILDCARD)" t nil)

(autoload 'popwin:messages "popwin/popwin" "\
Display *Messages* buffer in a popup window.

\(fn)" t nil)

;;;***

;;;### (autoloads (global-pretty-lambda-mode pretty-lambda-mode pretty-lambda-for-modes
;;;;;;  pretty-lambda-auto-modes pretty-lambda) "pretty-lambdada/pretty-lambdada"
;;;;;;  "pretty-lambdada/pretty-lambdada.el" (20621 19571))
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

;;;### (autoloads (helm-projectile) "projectile/helm-projectile"
;;;;;;  "projectile/helm-projectile.el" (20621 19570))
;;; Generated autoloads from projectile/helm-projectile.el

(autoload 'helm-projectile "projectile/helm-projectile" "\
Use projectile with Helm instead of ido.

\(fn)" t nil)

;;;***

;;;### (autoloads (projectile-mode projectile-global-mode) "projectile/projectile"
;;;;;;  "projectile/projectile.el" (20621 19570))
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

;;;### (autoloads (global-rainbow-delimiters-mode rainbow-delimiters-mode-disable
;;;;;;  rainbow-delimiters-mode-enable rainbow-delimiters-mode) "rainbow-delimiters/rainbow-delimiters"
;;;;;;  "rainbow-delimiters/rainbow-delimiters.el" (20621 19577))
;;; Generated autoloads from rainbow-delimiters/rainbow-delimiters.el

(autoload 'rainbow-delimiters-mode "rainbow-delimiters/rainbow-delimiters" "\
Highlight nested parentheses, brackets, and braces according to their depth.

\(fn &optional ARG)" t nil)

(autoload 'rainbow-delimiters-mode-enable "rainbow-delimiters/rainbow-delimiters" "\


\(fn)" nil nil)

(autoload 'rainbow-delimiters-mode-disable "rainbow-delimiters/rainbow-delimiters" "\


\(fn)" nil nil)

(defvar global-rainbow-delimiters-mode nil "\
Non-nil if Global-Rainbow-Delimiters mode is enabled.
See the command `global-rainbow-delimiters-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-rainbow-delimiters-mode'.")

(custom-autoload 'global-rainbow-delimiters-mode "rainbow-delimiters/rainbow-delimiters" nil)

(autoload 'global-rainbow-delimiters-mode "rainbow-delimiters/rainbow-delimiters" "\
Toggle Rainbow-Delimiters mode in all buffers.
With prefix ARG, enable Global-Rainbow-Delimiters mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Rainbow-Delimiters mode is enabled in all buffers where
`rainbow-delimiters-mode-enable' would do it.
See `rainbow-delimiters-mode' for more information on Rainbow-Delimiters mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (rainbow-mode) "rainbow-mode/rainbow-mode" "rainbow-mode/rainbow-mode.el"
;;;;;;  (20621 19578))
;;; Generated autoloads from rainbow-mode/rainbow-mode.el

(autoload 'rainbow-mode "rainbow-mode/rainbow-mode" "\
Colorize strings that represent colors.
This will fontify with colors the string like \"#aabbcc\" or \"blue\".

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (ert-describe-test ert-run-tests-interactively
;;;;;;  ert-run-tests-batch-and-exit ert-run-tests-batch ert-deftest)
;;;;;;  "s/ert" "s/ert.el" (20621 19573))
;;; Generated autoloads from s/ert.el

(autoload 'ert-deftest "s/ert" "\
Define NAME (a symbol) as a test.

BODY is evaluated as a `progn' when the test is run.  It should
signal a condition on failure or just return if the test passes.

`should', `should-not' and `should-error' are useful for
assertions in BODY.

Use `ert' to run tests interactively.

Tests that are expected to fail can be marked as such
using :expected-result.  See `ert-test-result-type-p' for a
description of valid values for RESULT-TYPE.

\(fn NAME () [DOCSTRING] [:expected-result RESULT-TYPE] [:tags '(TAG...)] BODY...)" nil (quote macro))

(put 'ert-deftest 'lisp-indent-function '2)

(put 'ert-deftest 'doc-string-elt '3)

(put 'ert-deftest 'lisp-indent-function 2)

(put 'ert-info 'lisp-indent-function 1)

(autoload 'ert-run-tests-batch "s/ert" "\
Run the tests specified by SELECTOR, printing results to the terminal.

SELECTOR works as described in `ert-select-tests', except if
SELECTOR is nil, in which case all tests rather than none will be
run; this makes the command line \"emacs -batch -l my-tests.el -f
ert-run-tests-batch-and-exit\" useful.

Returns the stats object.

\(fn &optional SELECTOR)" nil nil)

(autoload 'ert-run-tests-batch-and-exit "s/ert" "\
Like `ert-run-tests-batch', but exits Emacs when done.

The exit status will be 0 if all test results were as expected, 1
on unexpected results, or 2 if the framework detected an error
outside of the tests (e.g. invalid SELECTOR or bug in the code
that runs the tests).

\(fn &optional SELECTOR)" nil nil)

(autoload 'ert-run-tests-interactively "s/ert" "\
Run the tests specified by SELECTOR and display the results in a buffer.

SELECTOR works as described in `ert-select-tests'.
OUTPUT-BUFFER-NAME and MESSAGE-FN should normally be nil; they
are used for automated self-tests and specify which buffer to use
and how to display message.

\(fn SELECTOR &optional OUTPUT-BUFFER-NAME MESSAGE-FN)" t nil)

(defalias 'ert 'ert-run-tests-interactively)

(autoload 'ert-describe-test "s/ert" "\
Display the documentation for TEST-OR-TEST-NAME (a symbol or ert-test).

\(fn TEST-OR-TEST-NAME)" t nil)

;;;***

;;;### (autoloads (sauron-start-hidden sauron-start) "sauron/sauron"
;;;;;;  "sauron/sauron.el" (20621 19570))
;;; Generated autoloads from sauron/sauron.el

(autoload 'sauron-start "sauron/sauron" "\
Start sauron. If the optional parameter HIDDEN is non-nil,
don't show the sauron window.

\(fn &optional HIDDEN)" t nil)

(autoload 'sauron-start-hidden "sauron/sauron" "\
Start sauron, but don't show the window.

\(fn)" t nil)

;;;***

;;;### (autoloads (slime-hyperspec-lookup slime-connect slime slime-mode
;;;;;;  slime-lisp-mode-hook) "slime/slime" "slime/slime.el" (20621
;;;;;;  19579))
;;; Generated autoloads from slime/slime.el

(defvar slime-lisp-modes '(lisp-mode))

(defvar slime-setup-contribs nil)

(defun slime-setup (&optional contribs) "\
Setup Emacs so that lisp-mode buffers always use SLIME.
CONTRIBS is a list of contrib packages to load." (when (member (quote lisp-mode) slime-lisp-modes) (add-hook (quote lisp-mode-hook) (quote slime-lisp-mode-hook))) (setq slime-setup-contribs contribs) (slime-setup-contribs))

(autoload 'slime-lisp-mode-hook "slime/slime" "\


\(fn)" nil nil)

(autoload 'slime-mode "slime/slime" "\
\\<slime-mode-map>SLIME: The Superior Lisp Interaction Mode for Emacs (minor-mode).

Commands to compile the current buffer's source file and visually
highlight any resulting compiler notes and warnings:
\\[slime-compile-and-load-file]	- Compile and load the current buffer's file.
\\[slime-compile-file]	- Compile (but not load) the current buffer's file.
\\[slime-compile-defun]	- Compile the top-level form at point.

Commands for visiting compiler notes:
\\[slime-next-note]	- Goto the next form with a compiler note.
\\[slime-previous-note]	- Goto the previous form with a compiler note.
\\[slime-remove-notes]	- Remove compiler-note annotations in buffer.

Finding definitions:
\\[slime-edit-definition]	- Edit the definition of the function called at point.
\\[slime-pop-find-definition-stack]	- Pop the definition stack to go back from a definition.

Documentation commands:
\\[slime-describe-symbol]	- Describe symbol.
\\[slime-apropos]	- Apropos search.
\\[slime-disassemble-symbol]	- Disassemble a function.

Evaluation commands:
\\[slime-eval-defun]	- Evaluate top-level from containing point.
\\[slime-eval-last-expression]	- Evaluate sexp before point.
\\[slime-pprint-eval-last-expression]	- Evaluate sexp before point, pretty-print result.

Full set of commands:
\\{slime-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'slime "slime/slime" "\
Start an inferior^_superior Lisp and connect to its Swank server.

\(fn &optional COMMAND CODING-SYSTEM)" t nil)

(autoload 'slime-connect "slime/slime" "\
Connect to a running Swank server. Return the connection.

\(fn HOST PORT &optional CODING-SYSTEM)" t nil)

(autoload 'slime-hyperspec-lookup "slime/slime" "\
A wrapper for `hyperspec-lookup'

\(fn SYMBOL-NAME)" t nil)

;;;***

;;;### (autoloads (smallurl smallurl-replace-at-point) "smallurl/smallurl"
;;;;;;  "smallurl/smallurl.el" (20621 19572))
;;; Generated autoloads from smallurl/smallurl.el

(autoload 'smallurl-replace-at-point "smallurl/smallurl" "\
Replace the url at point with a tiny version.

\(fn)" t nil)

(autoload 'smallurl "smallurl/smallurl" "\
Print a tiny version of the url given at prompt. By defualt
will ask you for the url at point, if any.

\(fn)" t nil)

;;;***

;;;### (autoloads (smart-tab-mode smart-tab-mode-on smart-tab) "smart-tab/smart-tab"
;;;;;;  "smart-tab/smart-tab.el" (20621 19571))
;;; Generated autoloads from smart-tab/smart-tab.el

(autoload 'smart-tab "smart-tab/smart-tab" "\
Try to 'do the smart thing' when tab is pressed.
`smart-tab' attempts to expand the text before the point or
indent the current line or selection.

In a regular buffer, `smart-tab' will attempt to expand with
either `hippie-expand' or `dabbrev-expand', depending on the
value of `smart-tab-using-hippie-expand'. Alternatively, if
`auto-complete-mode' is enabled in the current buffer,
`auto-complete' will be used to attempt expansion. If the mark is
active, or PREFIX is \\[universal-argument], then `smart-tab'
will indent the region or the current line (if the mark is not
active).

\(fn &optional PREFIX)" t nil)

(autoload 'smart-tab-mode-on "smart-tab/smart-tab" "\
Turn on `smart-tab-mode'.

\(fn)" nil nil)

(autoload 'smart-tab-mode "smart-tab/smart-tab" "\
Enable `smart-tab' to be used in place of tab.

With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.

\(fn &optional ARG)" t nil)

(autoload 'global-smart-tab-mode-enable-in-buffers "smart-tab")

;;;***

;;;### (autoloads (smart-compile-select-compile-command) "smarter-compile/smarter-compile"
;;;;;;  "smarter-compile/smarter-compile.el" (20621 19579))
;;; Generated autoloads from smarter-compile/smarter-compile.el

(autoload 'smart-compile-select-compile-command "smarter-compile/smarter-compile" "\
The function that selects the `compile-command' for
a buffer, given the `smart-compile-alist'.

This is not to be called interactively.

It does not call `compile'.

\(fn)" nil nil)

;;;***

;;;### (autoloads (smex-initialize smex) "smex/smex" "smex/smex.el"
;;;;;;  (20621 19579))
;;; Generated autoloads from smex/smex.el

(autoload 'smex "smex/smex" "\


\(fn)" t nil)

(autoload 'smex-initialize "smex/smex" "\


\(fn)" t nil)

;;;***

;;;### (autoloads (global-undo-tree-mode undo-tree-mode) "undo-tree/undo-tree"
;;;;;;  "undo-tree/undo-tree.el" (20621 19573))
;;; Generated autoloads from undo-tree/undo-tree.el

(autoload 'undo-tree-mode "undo-tree/undo-tree" "\
Toggle undo-tree mode.
With no argument, this command toggles the mode.
A positive prefix argument turns the mode on.
A negative prefix argument turns it off.

Undo-tree-mode replaces Emacs' standard undo feature with a more
powerful yet easier to use version, that treats the undo history
as what it is: a tree.

The following keys are available in `undo-tree-mode':

  \\{undo-tree-map}

Within the undo-tree visualizer, the following keys are available:

  \\{undo-tree-visualizer-map}

\(fn &optional ARG)" t nil)

(defvar global-undo-tree-mode nil "\
Non-nil if Global-Undo-Tree mode is enabled.
See the command `global-undo-tree-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-undo-tree-mode'.")

(custom-autoload 'global-undo-tree-mode "undo-tree/undo-tree" nil)

(autoload 'global-undo-tree-mode "undo-tree/undo-tree" "\
Toggle Undo-Tree mode in all buffers.
With prefix ARG, enable Global-Undo-Tree mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Undo-Tree mode is enabled in all buffers where
`turn-on-undo-tree-mode' would do it.
See `undo-tree-mode' for more information on Undo-Tree mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (yas-global-mode yas-minor-mode) "yasnippet/yasnippet"
;;;;;;  "yasnippet/yasnippet.el" (20621 19573))
;;; Generated autoloads from yasnippet/yasnippet.el

(autoload 'yas-minor-mode "yasnippet/yasnippet" "\
Toggle YASnippet mode.

When YASnippet mode is enabled, the `yas-trigger-key' key expands
snippets of code depending on the major mode.

With no argument, this command toggles the mode.
positive prefix argument turns on the mode.
Negative prefix argument turns off the mode.

You can customize the key through `yas-trigger-key'.

Key bindings:
\\{yas-minor-mode-map}

\(fn &optional ARG)" t nil)

(defvar yas-global-mode nil "\
Non-nil if Yas-Global mode is enabled.
See the command `yas-global-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `yas-global-mode'.")

(custom-autoload 'yas-global-mode "yasnippet/yasnippet" nil)

(autoload 'yas-global-mode "yasnippet/yasnippet" "\
Toggle Yas minor mode in all buffers.
With prefix ARG, enable Yas-Global mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Yas minor mode is enabled in all buffers where
`yas-minor-mode-on' would do it.
See `yas-minor-mode' for more information on Yas minor mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (zencoding-preview zencoding-expand-yas zencoding-mode
;;;;;;  zencoding-expand-line) "zencoding-mode/zencoding-mode" "zencoding-mode/zencoding-mode.el"
;;;;;;  (20689 36865))
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


\(fn)" t nil)

(autoload 'zencoding-preview "zencoding-mode/zencoding-mode" "\
Expand zencode between BEG and END interactively.
This will show a preview of the expanded zen code and you can
accept it or skip it.

\(fn BEG END)" t nil)

;;;***

;;;### (autoloads nil nil ("cmake-mode/cmake-mode.el" "multi-web-mode/mweb-example-config.el"
;;;;;;  "mumamo-noweb/mumamo-noweb.el" "nxhtml/autostart.el" "nxhtml/autostart22.el"
;;;;;;  "nxhtml/nxhtml-base.el" "nxhtml/nxhtml-loaddefs.el" "nxhtml/web-autoload.el"
;;;;;;  "qml-mode/qml-mode.el") (20690 26457 354910))

;;;***

(provide '.loaddefs)
;; Local Variables:
;; version-control: never
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; .loaddefs.el ends here
