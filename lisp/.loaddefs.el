;;; .loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (set-up-slime-ac) "ac-slime/ac-slime" "ac-slime/ac-slime.el"
;;;;;;  (20440 16381))
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

;;;### (autoloads (auto-indent-global-mode auto-indent-minor-mode-on
;;;;;;  auto-indent-minor-mode auto-indent-eol-char-newline auto-indent-eol-newline)
;;;;;;  "auto-indent-mode/auto-indent-mode" "auto-indent-mode/auto-indent-mode.el"
;;;;;;  (20412 830))
;;; Generated autoloads from auto-indent-mode/auto-indent-mode.el

(autoload 'auto-indent-eol-newline "auto-indent-mode/auto-indent-mode" "\
*Auto-indent function for end-of-line and then newline.

\(fn)" t nil)

(autoload 'auto-indent-eol-char-newline "auto-indent-mode/auto-indent-mode" "\
* Auto-indent function for end-of-line, insert `auto-indent-eol-char', and then newline

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
* Turn on auto-indent minor mode.

\(fn)" nil nil)

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

;;;### (autoloads (bash-completion-reset bash-completion-dynamic-complete
;;;;;;  bash-completion-setup) "bash-completion/bash-completion"
;;;;;;  "bash-completion/bash-completion.el" (20408 63295))
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

;;;### (autoloads (c-eldoc-print-current-symbol-info c-turn-on-eldoc-mode)
;;;;;;  "c-eldoc/c-eldoc" "c-eldoc/c-eldoc.el" (20383 37768))
;;; Generated autoloads from c-eldoc/c-eldoc.el

(autoload 'c-turn-on-eldoc-mode "c-eldoc/c-eldoc" "\
Enable c-eldoc-mode

\(fn)" t nil)

(autoload 'c-eldoc-print-current-symbol-info "c-eldoc/c-eldoc" "\
Returns documentation string for the current symbol.

\(fn)" nil nil)

;;;***

;;;### (autoloads (jabber-info jabber-customize jabber-debug-keep-process-buffers
;;;;;;  jabber-debug-log-xml jabber-default-priority jabber-default-status
;;;;;;  jabber-default-show jabber-account-list) "emacs-jabber/jabber"
;;;;;;  "emacs-jabber/jabber.el" (20428 28331))
;;; Generated autoloads from emacs-jabber/jabber.el

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

(custom-autoload 'jabber-account-list "emacs-jabber/jabber" t)

(defvar jabber-default-show "" "\
default show state")

(custom-autoload 'jabber-default-show "emacs-jabber/jabber" t)

(defvar jabber-default-status "" "\
default status string")

(custom-autoload 'jabber-default-status "emacs-jabber/jabber" t)

(defvar jabber-default-priority 10 "\
default priority")

(custom-autoload 'jabber-default-priority "emacs-jabber/jabber" t)

(defvar *jabber-current-status* nil "\
the users current presence status")

(defvar *jabber-current-show* nil "\
the users current presence show")

(defvar *jabber-current-priority* nil "\
the user's current priority")

(defvar jabber-debug-log-xml nil "\
Set to non-nil to log all XML i/o in *-jabber-console-JID-* buffer. Set to string to also dump XML i/o in specified file.")

(custom-autoload 'jabber-debug-log-xml "emacs-jabber/jabber" t)

(defvar jabber-debug-keep-process-buffers nil "\
If nil, kill process buffers when the process dies.
Contents of process buffers might be useful for debugging.")

(custom-autoload 'jabber-debug-keep-process-buffers "emacs-jabber/jabber" t)

(defconst jabber-presence-faces '(("" . jabber-roster-user-online) ("away" . jabber-roster-user-away) ("xa" . jabber-roster-user-xa) ("dnd" . jabber-roster-user-dnd) ("chat" . jabber-roster-user-chatty) ("error" . jabber-roster-user-error) (nil . jabber-roster-user-offline)) "\
Mapping from presence types to faces")

(autoload 'jabber-customize "emacs-jabber/jabber" "\
customize jabber options

\(fn)" t nil)

(autoload 'jabber-info "emacs-jabber/jabber" "\
open jabber.el manual

\(fn)" t nil)

;;;***

;;;### (autoloads (jabber-activity-mode) "emacs-jabber/jabber-activity"
;;;;;;  "emacs-jabber/jabber-activity.el" (20428 28331))
;;; Generated autoloads from emacs-jabber/jabber-activity.el

(defvar jabber-activity-mode t "\
Non-nil if Jabber-Activity mode is enabled.
See the command `jabber-activity-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `jabber-activity-mode'.")

(custom-autoload 'jabber-activity-mode "emacs-jabber/jabber-activity" nil)

(autoload 'jabber-activity-mode "emacs-jabber/jabber-activity" "\
Toggle display of activity in hidden jabber buffers in the mode line.

With a numeric arg, enable this display if arg is positive.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (jabber-autoaway-start) "emacs-jabber/jabber-autoaway"
;;;;;;  "emacs-jabber/jabber-autoaway.el" (20428 28331))
;;; Generated autoloads from emacs-jabber/jabber-autoaway.el

(autoload 'jabber-autoaway-start "emacs-jabber/jabber-autoaway" "\
Start autoaway timer.
The IGNORED argument is there so you can put this function in
`jabber-post-connect-hooks'.

\(fn &optional IGNORED)" t nil)

;;;***

;;;### (autoloads (jabber-edit-bookmarks jabber-get-bookmarks-from-cache
;;;;;;  jabber-get-bookmarks jabber-parse-conference-bookmark jabber-get-conference-data)
;;;;;;  "emacs-jabber/jabber-bookmarks" "emacs-jabber/jabber-bookmarks.el"
;;;;;;  (20428 28331))
;;; Generated autoloads from emacs-jabber/jabber-bookmarks.el

(autoload 'jabber-get-conference-data "emacs-jabber/jabber-bookmarks" "\
Get bookmark data for CONFERENCE-JID.
KEY may be nil or one of :name, :autojoin, :nick and :password.
If KEY is nil, a plist containing the above keys is returned.
CONT is called when the result is available, with JC and the
result as arguments.  If CONT is nil, return the requested data
immediately, and return nil if it is not in the cache.

\(fn JC CONFERENCE-JID CONT &optional KEY)" nil nil)

(autoload 'jabber-parse-conference-bookmark "emacs-jabber/jabber-bookmarks" "\
Convert a <conference/> tag into a plist.
The plist may contain the keys :jid, :name, :autojoin,
:nick and :password.

\(fn NODE)" nil nil)

(autoload 'jabber-get-bookmarks "emacs-jabber/jabber-bookmarks" "\
Retrieve bookmarks (if needed) and call CONT.
Arguments to CONT are JC and the bookmark list.  CONT will be
called as the result of a filter function or a timer.
If REFRESH is non-nil, always fetch bookmarks.

\(fn JC CONT &optional REFRESH)" nil nil)

(autoload 'jabber-get-bookmarks-from-cache "emacs-jabber/jabber-bookmarks" "\
Return cached bookmarks for JC.
If bookmarks have not yet been fetched by `jabber-get-bookmarks',
return nil.

\(fn JC)" nil nil)

(autoload 'jabber-edit-bookmarks "emacs-jabber/jabber-bookmarks" "\
Create a buffer for editing bookmarks interactively.

\(fn JC)" t nil)

;;;***

;;;### (autoloads (jabber-chat-get-buffer) "emacs-jabber/jabber-chat"
;;;;;;  "emacs-jabber/jabber-chat.el" (20428 28331))
;;; Generated autoloads from emacs-jabber/jabber-chat.el

(defvar jabber-chatting-with nil "\
JID of the person you are chatting with")

(autoload 'jabber-chat-get-buffer "emacs-jabber/jabber-chat" "\
Return the chat buffer for chatting with CHAT-WITH (bare or full JID).
Either a string or a buffer is returned, so use `get-buffer' or
`get-buffer-create'.

\(fn CHAT-WITH)" nil nil)

;;;***

;;;### (autoloads nil "emacs-jabber/jabber-chatbuffer" "emacs-jabber/jabber-chatbuffer.el"
;;;;;;  (20428 28331))
;;; Generated autoloads from emacs-jabber/jabber-chatbuffer.el

(defvar jabber-buffer-connection nil "\
The connection used by this buffer.")

(make-variable-buffer-local 'jabber-buffer-connection)

;;;***

;;;### (autoloads (jabber-compose) "emacs-jabber/jabber-compose"
;;;;;;  "emacs-jabber/jabber-compose.el" (20428 28331))
;;; Generated autoloads from emacs-jabber/jabber-compose.el

(autoload 'jabber-compose "emacs-jabber/jabber-compose" "\
Create a buffer for composing a Jabber message.

\(fn JC &optional RECIPIENT)" t nil)

;;;***

;;;### (autoloads nil "emacs-jabber/jabber-console" "emacs-jabber/jabber-console.el"
;;;;;;  (20428 28331))
;;; Generated autoloads from emacs-jabber/jabber-console.el

(defvar jabber-buffer-connection nil "\
The connection used by this buffer.")

(make-variable-buffer-local 'jabber-buffer-connection)

;;;***

;;;### (autoloads nil "emacs-jabber/jabber-core" "emacs-jabber/jabber-core.el"
;;;;;;  (20428 28331))
;;; Generated autoloads from emacs-jabber/jabber-core.el
(autoload 'jabber-connect-all "jabber" "Connect to all configured Jabber accounts.\nSee `jabber-account-list'.\nIf no accounts are configured (or ARG supplied), call `jabber-connect' interactively." t)
(autoload 'jabber-connect "jabber" "Connect to the Jabber server and start a Jabber XML stream.\nWith prefix argument, register a new account.\nWith double prefix argument, specify more connection details." t)

;;;***

;;;### (autoloads (jabber-import-roster jabber-export-roster) "emacs-jabber/jabber-export"
;;;;;;  "emacs-jabber/jabber-export.el" (20428 28331))
;;; Generated autoloads from emacs-jabber/jabber-export.el

(autoload 'jabber-export-roster "emacs-jabber/jabber-export" "\
Export roster for connection JC.

\(fn JC)" t nil)

(autoload 'jabber-import-roster "emacs-jabber/jabber-export" "\
Create buffer for roster import for connection JC from FILE.

\(fn JC FILE)" t nil)

;;;***

;;;### (autoloads (jabber-gmail-query jabber-gmail-subscribe) "emacs-jabber/jabber-gmail"
;;;;;;  "emacs-jabber/jabber-gmail.el" (20428 28331))
;;; Generated autoloads from emacs-jabber/jabber-gmail.el

(autoload 'jabber-gmail-subscribe "emacs-jabber/jabber-gmail" "\
Subscribe to gmail notifications.
See http://code.google.com/apis/talk/jep_extensions/usersettings.html#4

\(fn JC)" t nil)

(autoload 'jabber-gmail-query "emacs-jabber/jabber-gmail" "\
Request mail information from the Google Talk server (a.k.a. one shot query).
See http://code.google.com/apis/talk/jep_extensions/gmail.html#requestmail

\(fn JC)" t nil)

;;;***

;;;### (autoloads (jabber-whitespace-ping-start jabber-whitespace-ping-interval
;;;;;;  jabber-keepalive-start jabber-keepalive-timeout jabber-keepalive-interval
;;;;;;  jabber-keepalive) "emacs-jabber/jabber-keepalive" "emacs-jabber/jabber-keepalive.el"
;;;;;;  (20428 28331))
;;; Generated autoloads from emacs-jabber/jabber-keepalive.el

(let ((loads (get 'jabber-keepalive 'custom-loads))) (if (member '"emacs-jabber/jabber-keepalive" loads) nil (put 'jabber-keepalive 'custom-loads (cons '"emacs-jabber/jabber-keepalive" loads))))

(defvar jabber-keepalive-interval 600 "\
Interval in seconds between connection checks.")

(custom-autoload 'jabber-keepalive-interval "emacs-jabber/jabber-keepalive" t)

(defvar jabber-keepalive-timeout 20 "\
Seconds to wait for response from server.")

(custom-autoload 'jabber-keepalive-timeout "emacs-jabber/jabber-keepalive" t)

(autoload 'jabber-keepalive-start "emacs-jabber/jabber-keepalive" "\
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

(custom-autoload 'jabber-whitespace-ping-interval "emacs-jabber/jabber-keepalive" t)

(autoload 'jabber-whitespace-ping-start "emacs-jabber/jabber-keepalive" "\
Start sending whitespace pings at regular intervals.
See `jabber-whitespace-ping-interval'.

The JC argument is ignored; whitespace pings are enabled for all
accounts.

\(fn &optional JC)" t nil)

;;;***

;;;### (autoloads nil "emacs-jabber/jabber-keymap" "emacs-jabber/jabber-keymap.el"
;;;;;;  (20428 28331))
;;; Generated autoloads from emacs-jabber/jabber-keymap.el

(defvar jabber-global-keymap (let ((map (make-sparse-keymap))) (define-key map "" 'jabber-connect-all) (define-key map "" 'jabber-disconnect) (define-key map "" 'jabber-switch-to-roster-buffer) (define-key map "\n" 'jabber-chat-with) (define-key map "\f" 'jabber-activity-switch-to) (define-key map "" 'jabber-send-away-presence) (define-key map "" 'jabber-send-default-presence) (define-key map "" 'jabber-send-xa-presence) (define-key map "" 'jabber-send-presence) map) "\
Global Jabber keymap (usually under C-x C-j)")

(define-key ctl-x-map "\n" jabber-global-keymap)

;;;***

;;;### (autoloads (jabber-display-menu) "emacs-jabber/jabber-menu"
;;;;;;  "emacs-jabber/jabber-menu.el" (20428 28331))
;;; Generated autoloads from emacs-jabber/jabber-menu.el

(defvar jabber-menu (let ((map (make-sparse-keymap "jabber-menu"))) (define-key map [jabber-menu-connect] '("Connect" . jabber-connect-all)) (define-key map [jabber-menu-nextmsg] '("Next unread message" . jabber-activity-switch-to)) (define-key map [jabber-menu-disconnect] '("Disconnect" . jabber-disconnect)) (define-key map [jabber-menu-roster] '("Switch to roster" . jabber-switch-to-roster-buffer)) (define-key map [jabber-menu-customize] '("Customize" . jabber-customize)) (define-key map [jabber-menu-info] '("Help" . jabber-info)) (define-key map [jabber-menu-status] (cons "Set Status" (make-sparse-keymap "set-status"))) (define-key map [jabber-menu-status jabber-menu-status-chat] '("Chatty" lambda nil (interactive) (jabber-send-presence "chat" (jabber-read-with-input-method "status message: " *jabber-current-status* '*jabber-status-history*) *jabber-current-priority*))) (define-key map [jabber-menu-status jabber-menu-status-dnd] '("Do not Disturb" lambda nil (interactive) (jabber-send-presence "dnd" (jabber-read-with-input-method "status message: " *jabber-current-status* '*jabber-status-history*) *jabber-current-priority*))) (define-key map [jabber-menu-status jabber-menu-status-xa] '("Extended Away" . jabber-send-xa-presence)) (define-key map [jabber-menu-status jabber-menu-status-away] '("Away" . jabber-send-away-presence)) (define-key map [jabber-menu-status jabber-menu-status-online] '("Online" . jabber-send-default-presence)) map))

(defvar jabber-display-menu 'maybe "\
Decide whether the \"Jabber\" menu is displayed in the menu bar.
If t, always display.
If nil, never display.
If maybe, display if any of `jabber-account-list' or `jabber-connections'
is non-nil.")

(custom-autoload 'jabber-display-menu "emacs-jabber/jabber-menu" t)

(define-key-after (lookup-key global-map [menu-bar]) [jabber-menu] (list 'menu-item "Jabber" jabber-menu :visible '(or (eq jabber-display-menu t) (and (eq jabber-display-menu 'maybe) (or jabber-account-list (bound-and-true-p jabber-connections))))))

;;;***

;;;### (autoloads (jabber-muc-private-message-p jabber-muc-sender-p
;;;;;;  jabber-muc-message-p jabber-muc-vcard-get jabber-muc-private-get-buffer
;;;;;;  jabber-muc-get-buffer jabber-muc-autojoin jabber-muc-default-nicknames)
;;;;;;  "emacs-jabber/jabber-muc" "emacs-jabber/jabber-muc.el" (20428
;;;;;;  28331))
;;; Generated autoloads from emacs-jabber/jabber-muc.el

(defvar *jabber-active-groupchats* nil "\
alist of groupchats and nicknames
Keys are strings, the bare JID of the room.
Values are strings.")

(defvar jabber-muc-default-nicknames nil "\
Default nickname for specific MUC rooms.")

(custom-autoload 'jabber-muc-default-nicknames "emacs-jabber/jabber-muc" t)

(defvar jabber-muc-autojoin nil "\
List of MUC rooms to automatically join on connection.
This list is saved in your Emacs customizations.  You can also store
such a list on the Jabber server, where it is available to every
client; see `jabber-edit-bookmarks'.")

(custom-autoload 'jabber-muc-autojoin "emacs-jabber/jabber-muc" t)

(defvar jabber-muc-printers 'nil "\
List of functions that may be able to print part of a MUC message.
This gets prepended to `jabber-chat-printers', which see.")

(autoload 'jabber-muc-get-buffer "emacs-jabber/jabber-muc" "\
Return the chat buffer for chatroom GROUP.
Either a string or a buffer is returned, so use `get-buffer' or
`get-buffer-create'.

\(fn GROUP)" nil nil)

(autoload 'jabber-muc-private-get-buffer "emacs-jabber/jabber-muc" "\
Return the chat buffer for private chat with NICKNAME in GROUP.
Either a string or a buffer is returned, so use `get-buffer' or
`get-buffer-create'.

\(fn GROUP NICKNAME)" nil nil)

(autoload 'jabber-muc-vcard-get "emacs-jabber/jabber-muc" "\
Request vcard from chat with NICKNAME in GROUP.

\(fn JC GROUP NICKNAME)" t nil)

(autoload 'jabber-muc-message-p "emacs-jabber/jabber-muc" "\
Return non-nil if MESSAGE is a groupchat message.
That does not include private messages in a groupchat, but does
include groupchat invites.

\(fn MESSAGE)" nil nil)

(autoload 'jabber-muc-sender-p "emacs-jabber/jabber-muc" "\
Return non-nil if JID is a full JID of an MUC participant.

\(fn JID)" nil nil)

(autoload 'jabber-muc-private-message-p "emacs-jabber/jabber-muc" "\
Return non-nil if MESSAGE is a private message in a groupchat.

\(fn MESSAGE)" nil nil)

;;;***

;;;### (autoloads (jabber-muc-looks-like-personal-p) "emacs-jabber/jabber-muc-nick-completion"
;;;;;;  "emacs-jabber/jabber-muc-nick-completion.el" (20428 28331))
;;; Generated autoloads from emacs-jabber/jabber-muc-nick-completion.el

(autoload 'jabber-muc-looks-like-personal-p "emacs-jabber/jabber-muc-nick-completion" "\
Return non-nil if jabber MESSAGE is addresed to me.
Optional argument GROUP to look.

\(fn MESSAGE &optional GROUP)" nil nil)

;;;***

;;;### (autoloads (jabber-send-default-presence jabber-send-presence)
;;;;;;  "emacs-jabber/jabber-presence" "emacs-jabber/jabber-presence.el"
;;;;;;  (20428 28331))
;;; Generated autoloads from emacs-jabber/jabber-presence.el

(autoload 'jabber-send-presence "emacs-jabber/jabber-presence" "\
Set presence for all accounts.

\(fn SHOW STATUS PRIORITY)" t nil)

(autoload 'jabber-send-default-presence "emacs-jabber/jabber-presence" "\
Send default presence.
Default presence is specified by `jabber-default-show',
`jabber-default-status', and `jabber-default-priority'.

\(fn &optional IGNORE)" t nil)

;;;***

;;;### (autoloads (jabber-private-set jabber-private-get) "emacs-jabber/jabber-private"
;;;;;;  "emacs-jabber/jabber-private.el" (20428 28331))
;;; Generated autoloads from emacs-jabber/jabber-private.el

(autoload 'jabber-private-get "emacs-jabber/jabber-private" "\
Retrieve an item from private XML storage.
The item to retrieve is identified by NODE-NAME (a symbol) and
NAMESPACE (a string).

On success, SUCCESS-CALLBACK is called with JC and the retrieved
XML fragment.

On error, ERROR-CALLBACK is called with JC and the entire IQ
result.

\(fn JC NODE-NAME NAMESPACE SUCCESS-CALLBACK ERROR-CALLBACK)" nil nil)

(autoload 'jabber-private-set "emacs-jabber/jabber-private" "\
Store FRAGMENT in private XML storage.
SUCCESS-CALLBACK, SUCCESS-CLOSURE-DATA, ERROR-CALLBACK and
ERROR-CLOSURE-DATA are used as in `jabber-send-iq'.

\(fn JC FRAGMENT &optional SUCCESS-CALLBACK SUCCESS-CLOSURE-DATA ERROR-CALLBACK ERROR-CLOSURE-DATA)" nil nil)

;;;***

;;;### (autoloads (jabber-roster-update jabber-switch-to-roster-buffer)
;;;;;;  "emacs-jabber/jabber-roster" "emacs-jabber/jabber-roster.el"
;;;;;;  (20428 28331))
;;; Generated autoloads from emacs-jabber/jabber-roster.el

(autoload 'jabber-switch-to-roster-buffer "emacs-jabber/jabber-roster" "\
Switch to roster buffer.
Optional JC argument is ignored; it's there so this function can
be used in `jabber-post-connection-hooks'.

\(fn &optional JC)" t nil)

(autoload 'jabber-roster-update "emacs-jabber/jabber-roster" "\
Update roster, in memory and on display.
Add NEW-ITEMS, update CHANGED-ITEMS and remove DELETED-ITEMS, all
three being lists of JID symbols.

\(fn JC NEW-ITEMS CHANGED-ITEMS DELETED-ITEMS)" nil nil)

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
;;;;;;  helm-open-last-log) "helm/helm" "helm/helm.el" (20409 7691))
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
;;;;;;  "helm/helm-adaptative.el" (20409 7691))
;;; Generated autoloads from helm/helm-adaptative.el

(autoload 'helm-c-reset-adaptative-history "helm/helm-adaptative" "\
Delete all `helm-c-adaptive-history' and his file.
Useful when you have a old or corrupted `helm-c-adaptive-history-file'.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-apt) "helm/helm-apt" "helm/helm-apt.el" (20409
;;;;;;  7691))
;;; Generated autoloads from helm/helm-apt.el

(autoload 'helm-apt "helm/helm-apt" "\
Preconfigured `helm' : frontend of APT package manager.
With a prefix arg reload cache.

\(fn ARG)" t nil)

;;;***

;;;### (autoloads (helm-bbdb) "helm/helm-bbdb" "helm/helm-bbdb.el"
;;;;;;  (20409 7691))
;;; Generated autoloads from helm/helm-bbdb.el

(autoload 'helm-bbdb "helm/helm-bbdb" "\
Preconfigured `helm' for BBDB.

Needs BBDB.

http://bbdb.sourceforge.net/

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-bookmark-ext helm-c-bmkext-run-edit) "helm/helm-bmkext"
;;;;;;  "helm/helm-bmkext.el" (20409 7691))
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
;;;;;;  "helm/helm-bookmark.el" (20409 7691))
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
;;;;;;  (20409 7691))
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
;;;;;;  (20409 7691))
;;; Generated autoloads from helm/helm-call-tree.el

(autoload 'helm-simple-call-tree "helm/helm-call-tree" "\
Preconfigured `helm' for simple-call-tree. List function relationships.

Needs simple-call-tree.el.
http://www.emacswiki.org/cgi-bin/wiki/download/simple-call-tree.el

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-colors) "helm/helm-color" "helm/helm-color.el"
;;;;;;  (20409 7691))
;;; Generated autoloads from helm/helm-color.el

(autoload 'helm-colors "helm/helm-color" "\
Preconfigured `helm' for color.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-M-x) "helm/helm-command" "helm/helm-command.el"
;;;;;;  (20409 7691))
;;; Generated autoloads from helm/helm-command.el

(autoload 'helm-M-x "helm/helm-command" "\
Preconfigured `helm' for Emacs commands.
It is `helm' replacement of regular `M-x' `execute-extended-command'.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-configuration) "helm/helm-config" "helm/helm-config.el"
;;;;;;  (20409 7691))
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
;;;;;;  (20409 7691))
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
;;;;;;  (20409 7691))
;;; Generated autoloads from helm/helm-elscreen.el

(autoload 'helm-elscreen "helm/helm-elscreen" "\
Preconfigured helm to list elscreen.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-emms) "helm/helm-emms" "helm/helm-emms.el"
;;;;;;  (20409 7691))
;;; Generated autoloads from helm/helm-emms.el

(autoload 'helm-emms "helm/helm-emms" "\
Preconfigured `helm' for emms sources.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-eshell-history helm-esh-pcomplete) "helm/helm-eshell"
;;;;;;  "helm/helm-eshell.el" (20409 7691))
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
;;;;;;  (20409 7691))
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
;;;;;;  "helm/helm-external.el" (20409 7691))
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
;;;;;;  "helm/helm-files" "helm/helm-files.el" (20409 7691))
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
;;;;;;  (20409 7691))
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
;;;;;;  (20409 7691))
;;; Generated autoloads from helm/helm-font.el

(autoload 'helm-select-xfont "helm/helm-font" "\
Preconfigured `helm' to select Xfont.

\(fn)" t nil)

(autoload 'helm-ucs "helm/helm-font" "\
Preconfigured helm for `ucs-names' math symbols.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-gentoo) "helm/helm-gentoo" "helm/helm-gentoo.el"
;;;;;;  (20409 7691))
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
;;;;;;  helm-grep-mode) "helm/helm-grep" "helm/helm-grep.el" (20409
;;;;;;  7691))
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
;;;;;;  (20409 7691))
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
;;;;;;  (20409 7691))
;;; Generated autoloads from helm/helm-imenu.el

(autoload 'helm-imenu "helm/helm-imenu" "\
Preconfigured `helm' for `imenu'.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-info-at-point) "helm/helm-info" "helm/helm-info.el"
;;;;;;  (20409 7691))
;;; Generated autoloads from helm/helm-info.el

(autoload 'helm-info-at-point "helm/helm-info" "\
Preconfigured `helm' for searching info at point.
With a prefix-arg insert symbol at point.

\(fn ARG)" t nil)

;;;***

;;;### (autoloads (helm-locate) "helm/helm-locate" "helm/helm-locate.el"
;;;;;;  (20409 7691))
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
;;;;;;  (20409 7691))
;;; Generated autoloads from helm/helm-man.el

(autoload 'helm-man-woman "helm/helm-man" "\
Preconfigured `helm' for Man and Woman pages.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-match-plugin-mode) "helm/helm-match-plugin"
;;;;;;  "helm/helm-match-plugin.el" (20409 7691))
;;; Generated autoloads from helm/helm-match-plugin.el

(defvar helm-match-plugin-mode nil "\
Non-nil if Helm-Match-Plugin mode is enabled.
See the command `helm-match-plugin-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `helm-match-plugin-mode'.")

(custom-autoload 'helm-match-plugin-mode "helm/helm-match-plugin" nil)

(autoload 'helm-match-plugin-mode "helm/helm-match-plugin" "\
Add more flexible regexp matching for helm.
See `helm-mp-matching-method' for the behavior of each method.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (helm-minibuffer-history helm-mini helm-ratpoison-commands
;;;;;;  helm-eev-anchors helm-c-insert-latex-math helm-world-time)
;;;;;;  "helm/helm-misc" "helm/helm-misc.el" (20409 7691))
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
;;;;;;  (20409 7691))
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
;;;;;;  "helm/helm-net" "helm/helm-net.el" (20409 7691))
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
;;;;;;  "helm/helm-org.el" (20409 7691))
;;; Generated autoloads from helm/helm-org.el

(autoload 'helm-org-keywords "helm/helm-org" "\
Preconfigured `helm' for org keywords.

\(fn)" t nil)

(autoload 'helm-org-headlines "helm/helm-org" "\
Preconfigured helm to show org headlines.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-browse-code helm-occur helm-regexp) "helm/helm-regexp"
;;;;;;  "helm/helm-regexp.el" (20409 7691))
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
;;;;;;  (20409 7691))
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

First call open the kill-ring browser, next calls move to next line.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-semantic-or-imenu helm-semantic) "helm/helm-semantic"
;;;;;;  "helm/helm-semantic.el" (20409 7691))
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
;;;;;;  "helm/helm-sys" "helm/helm-sys.el" (20409 7691))
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
;;;;;;  (20409 7691))
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
;;;;;;  "helm/helm-utils" "helm/helm-utils.el" (20409 7691))
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
;;;;;;  (20409 7691))
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
;;;;;;  (20409 7691))
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

;;;### (autoloads (jabber-info jabber-customize jabber-debug-keep-process-buffers
;;;;;;  jabber-debug-log-xml jabber-default-priority jabber-default-status
;;;;;;  jabber-default-show jabber-account-list) "jabber/jabber"
;;;;;;  "jabber/jabber.el" (20385 22791))
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
;;;;;;  "jabber/jabber-activity.el" (20385 22789))
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
;;;;;;  "jabber/jabber-autoaway.el" (20385 22789))
;;; Generated autoloads from jabber/jabber-autoaway.el

(autoload 'jabber-autoaway-start "jabber/jabber-autoaway" "\
Start autoaway timer.
The IGNORED argument is there so you can put this function in
`jabber-post-connect-hooks'.

\(fn &optional IGNORED)" t nil)

;;;***

;;;### (autoloads (jabber-edit-bookmarks jabber-get-bookmarks-from-cache
;;;;;;  jabber-get-bookmarks jabber-parse-conference-bookmark jabber-get-conference-data)
;;;;;;  "jabber/jabber-bookmarks" "jabber/jabber-bookmarks.el" (20385
;;;;;;  22789))
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
;;;;;;  (20385 22789))
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
;;;;;;  (20385 22789))
;;; Generated autoloads from jabber/jabber-chatbuffer.el

(defvar jabber-buffer-connection nil "\
The connection used by this buffer.")

(make-variable-buffer-local 'jabber-buffer-connection)

;;;***

;;;### (autoloads (jabber-compose) "jabber/jabber-compose" "jabber/jabber-compose.el"
;;;;;;  (20385 22789))
;;; Generated autoloads from jabber/jabber-compose.el

(autoload 'jabber-compose "jabber/jabber-compose" "\
Create a buffer for composing a Jabber message.

\(fn JC &optional RECIPIENT)" t nil)

;;;***

;;;### (autoloads nil "jabber/jabber-core" "jabber/jabber-core.el"
;;;;;;  (20385 22789))
;;; Generated autoloads from jabber/jabber-core.el
(autoload 'jabber-connect-all "jabber" "Connect to all configured Jabber accounts.\nSee `jabber-account-list'.\nIf no accounts are configured (or ARG supplied), call `jabber-connect' interactively." t)
(autoload 'jabber-connect "jabber" "Connect to the Jabber server and start a Jabber XML stream.\nWith prefix argument, register a new account.\nWith double prefix argument, specify more connection details." t)

;;;***

;;;### (autoloads (jabber-import-roster jabber-export-roster) "jabber/jabber-export"
;;;;;;  "jabber/jabber-export.el" (20385 22789))
;;; Generated autoloads from jabber/jabber-export.el

(autoload 'jabber-export-roster "jabber/jabber-export" "\
Export roster for connection JC.

\(fn JC)" t nil)

(autoload 'jabber-import-roster "jabber/jabber-export" "\
Create buffer for roster import for connection JC from FILE.

\(fn JC FILE)" t nil)

;;;***

;;;### (autoloads (jabber-gmail-query jabber-gmail-subscribe) "jabber/jabber-gmail"
;;;;;;  "jabber/jabber-gmail.el" (20385 22790))
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
;;;;;;  (20385 22790))
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
;;;;;;  (20385 22790))
;;; Generated autoloads from jabber/jabber-keymap.el

(defvar jabber-global-keymap (let ((map (make-sparse-keymap))) (define-key map "" 'jabber-connect-all) (define-key map "" 'jabber-disconnect) (define-key map "" 'jabber-switch-to-roster-buffer) (define-key map "\n" 'jabber-chat-with) (define-key map "\f" 'jabber-activity-switch-to) (define-key map "" 'jabber-send-away-presence) (define-key map "" 'jabber-send-default-presence) (define-key map "" 'jabber-send-xa-presence) (define-key map "" 'jabber-send-presence) map) "\
Global Jabber keymap (usually under C-x C-j)")

(define-key ctl-x-map "\n" jabber-global-keymap)

;;;***

;;;### (autoloads (jabber-display-menu) "jabber/jabber-menu" "jabber/jabber-menu.el"
;;;;;;  (20385 22790))
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
;;;;;;  "jabber/jabber-muc.el" (20385 22790))
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
;;;;;;  "jabber/jabber-muc-nick-completion.el" (20385 22790))
;;; Generated autoloads from jabber/jabber-muc-nick-completion.el

(autoload 'jabber-muc-looks-like-personal-p "jabber/jabber-muc-nick-completion" "\
Return non-nil if jabber MESSAGE is addresed to me.
Optional argument GROUP to look.

\(fn MESSAGE &optional GROUP)" nil nil)

;;;***

;;;### (autoloads (jabber-send-default-presence jabber-send-presence)
;;;;;;  "jabber/jabber-presence" "jabber/jabber-presence.el" (20385
;;;;;;  22790))
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
;;;;;;  "jabber/jabber-private.el" (20385 22790))
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
;;;;;;  "jabber/jabber-roster" "jabber/jabber-roster.el" (20385 22790))
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
;;;;;;  "projectile/helm-projectile.el" (20438 4067))
;;; Generated autoloads from projectile/helm-projectile.el

(autoload 'helm-projectile "projectile/helm-projectile" "\
Use projectile with Helm instead of ido.

\(fn)" t nil)

;;;***

;;;### (autoloads (projectile-mode projectile-global-mode) "projectile/projectile"
;;;;;;  "projectile/projectile.el" (20438 4067))
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

;;;### (autoloads (smallurl smallurl-replace-at-point) "smallurl/smallurl"
;;;;;;  "smallurl/smallurl.el" (20409 2403))
;;; Generated autoloads from smallurl/smallurl.el

(autoload 'smallurl-replace-at-point "smallurl/smallurl" "\
Replace the url at point with a tiny version.

\(fn)" t nil)

(autoload 'smallurl "smallurl/smallurl" "\
Print a tiny version of the url given at prompt. By defualt
will ask you for the url at point, if any.

\(fn)" t nil)

;;;***

;;;### (autoloads (yas/global-mode yas/minor-mode) "yasnippet/yasnippet"
;;;;;;  "yasnippet/yasnippet.el" (20440 12806))
;;; Generated autoloads from yasnippet/yasnippet.el

(autoload 'yas/minor-mode "yasnippet/yasnippet" "\
Toggle YASnippet mode.

When YASnippet mode is enabled, the `yas/trigger-key' key expands
snippets of code depending on the mode.

With no argument, this command toggles the mode.
positive prefix argument turns on the mode.
Negative prefix argument turns off the mode.

You can customize the key through `yas/trigger-key'.

Key bindings:
\\{yas/minor-mode-map}

\(fn &optional ARG)" t nil)

(defvar yas/global-mode nil "\
Non-nil if Yas/Global mode is enabled.
See the command `yas/global-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `yas/global-mode'.")

(custom-autoload 'yas/global-mode "yasnippet/yasnippet" nil)

(autoload 'yas/global-mode "yasnippet/yasnippet" "\
Toggle Yas/Minor mode in all buffers.
With prefix ARG, enable Yas/Global mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Yas/Minor mode is enabled in all buffers where
`yas/minor-mode-on' would do it.
See `yas/minor-mode' for more information on Yas/Minor mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("auto-complete-clang/auto-complete-clang.el"
;;;;;;  "auto-complete/auto-complete-config.el" "auto-complete/auto-complete-pkg.el"
;;;;;;  "auto-complete/auto-complete.el" "c-eldoc/c-eldoc-autoloads.el"
;;;;;;  "c-eldoc/c-eldoc-pkg.el" "cljdoc/cljdoc-autoloads.el" "cljdoc/cljdoc-pkg.el"
;;;;;;  "cljdoc/cljdoc.el" "cmake-mode/cmake-mode.el" "emacs-jabber/fsm.el"
;;;;;;  "emacs-jabber/jabber-ahc-presence.el" "emacs-jabber/jabber-ahc.el"
;;;;;;  "emacs-jabber/jabber-alert.el" "emacs-jabber/jabber-autoloads.el"
;;;;;;  "emacs-jabber/jabber-avatar.el" "emacs-jabber/jabber-awesome.el"
;;;;;;  "emacs-jabber/jabber-browse.el" "emacs-jabber/jabber-chatstates.el"
;;;;;;  "emacs-jabber/jabber-conn.el" "emacs-jabber/jabber-disco.el"
;;;;;;  "emacs-jabber/jabber-events.el" "emacs-jabber/jabber-feature-neg.el"
;;;;;;  "emacs-jabber/jabber-festival.el" "emacs-jabber/jabber-ft-client.el"
;;;;;;  "emacs-jabber/jabber-ft-common.el" "emacs-jabber/jabber-ft-server.el"
;;;;;;  "emacs-jabber/jabber-history.el" "emacs-jabber/jabber-iq.el"
;;;;;;  "emacs-jabber/jabber-libnotify.el" "emacs-jabber/jabber-logon.el"
;;;;;;  "emacs-jabber/jabber-modeline.el" "emacs-jabber/jabber-muc-nick-coloring.el"
;;;;;;  "emacs-jabber/jabber-newdisco.el" "emacs-jabber/jabber-osd.el"
;;;;;;  "emacs-jabber/jabber-ourversion.el" "emacs-jabber/jabber-ping.el"
;;;;;;  "emacs-jabber/jabber-ratpoison.el" "emacs-jabber/jabber-register.el"
;;;;;;  "emacs-jabber/jabber-sasl.el" "emacs-jabber/jabber-sawfish.el"
;;;;;;  "emacs-jabber/jabber-screen.el" "emacs-jabber/jabber-search.el"
;;;;;;  "emacs-jabber/jabber-si-client.el" "emacs-jabber/jabber-si-common.el"
;;;;;;  "emacs-jabber/jabber-si-server.el" "emacs-jabber/jabber-socks5.el"
;;;;;;  "emacs-jabber/jabber-time.el" "emacs-jabber/jabber-tmux.el"
;;;;;;  "emacs-jabber/jabber-truncate.el" "emacs-jabber/jabber-util.el"
;;;;;;  "emacs-jabber/jabber-vcard-avatars.el" "emacs-jabber/jabber-vcard.el"
;;;;;;  "emacs-jabber/jabber-version.el" "emacs-jabber/jabber-watch.el"
;;;;;;  "emacs-jabber/jabber-widget.el" "emacs-jabber/jabber-wmii.el"
;;;;;;  "emacs-jabber/jabber-xmessage.el" "emacs-jabber/jabber-xml.el"
;;;;;;  "emacs-jabber/srv.el" "guess-offset/guess-offset-autoloads.el"
;;;;;;  "guess-offset/guess-offset-pkg.el" "guess-offset/guess-offset.el"
;;;;;;  "helm/helm-pkg.el" "helm/helm-plugin.el" "hideshowvis/hideshowvis-autoloads.el"
;;;;;;  "hideshowvis/hideshowvis-pkg.el" "jabber/fsm.el" "jabber/jabber-ahc-presence.el"
;;;;;;  "jabber/jabber-ahc.el" "jabber/jabber-alert.el" "jabber/jabber-autoloads.el"
;;;;;;  "jabber/jabber-avatar.el" "jabber/jabber-awesome.el" "jabber/jabber-browse.el"
;;;;;;  "jabber/jabber-chatstates.el" "jabber/jabber-conn.el" "jabber/jabber-disco.el"
;;;;;;  "jabber/jabber-events.el" "jabber/jabber-feature-neg.el"
;;;;;;  "jabber/jabber-festival.el" "jabber/jabber-ft-client.el"
;;;;;;  "jabber/jabber-ft-common.el" "jabber/jabber-ft-server.el"
;;;;;;  "jabber/jabber-history.el" "jabber/jabber-iq.el" "jabber/jabber-logon.el"
;;;;;;  "jabber/jabber-modeline.el" "jabber/jabber-newdisco.el" "jabber/jabber-osd.el"
;;;;;;  "jabber/jabber-ourversion.el" "jabber/jabber-pkg.el" "jabber/jabber-ratpoison.el"
;;;;;;  "jabber/jabber-register.el" "jabber/jabber-sasl.el" "jabber/jabber-sawfish.el"
;;;;;;  "jabber/jabber-screen.el" "jabber/jabber-search.el" "jabber/jabber-si-client.el"
;;;;;;  "jabber/jabber-si-common.el" "jabber/jabber-si-server.el"
;;;;;;  "jabber/jabber-socks5.el" "jabber/jabber-time.el" "jabber/jabber-truncate.el"
;;;;;;  "jabber/jabber-util.el" "jabber/jabber-vcard-avatars.el"
;;;;;;  "jabber/jabber-vcard.el" "jabber/jabber-version.el" "jabber/jabber-watch.el"
;;;;;;  "jabber/jabber-widget.el" "jabber/jabber-wmii.el" "jabber/jabber-xmessage.el"
;;;;;;  "jabber/jabber-xml.el" "jabber/srv.el" "parenface/parenface.el"
;;;;;;  "switch-window/switch-window.el" "window-layout/window-layout.el"
;;;;;;  "yasnippet/dropdown-list.el" "yasnippet/yasnippet-debug.el"
;;;;;;  "yasnippet/yasnippet-tests.el") (20440 16609 798281))

;;;***

(provide '.loaddefs)
;; Local Variables:
;; version-control: never
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; .loaddefs.el ends here
