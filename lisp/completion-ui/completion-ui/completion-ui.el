
;;; completion-ui.el --- in-buffer completion user interface


;; Copyright (C) 2006-2010 Toby Cubitt

;; Author: Toby Cubitt <toby-predictive@dr-qubit.org>
;; Version: 0.11.12
;; Keywords: completion, ui, user interface
;; URL: http://www.dr-qubit.org/emacs.php


;; This file is NOT part of Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301, USA.


;;; Commentary:
;;
;; Overview
;; ========
;;
;; The goal of Completion-UI is to be the swiss-army knife of in-buffer
;; completion user-interfaces. It doesn't do the work of finding the
;; completions itself. Instead, anything that can find completions, be it a
;; built-in Emacs function or an external Elisp package, can be hooked up to
;; Completion-UI to provide a source of completions.
;;
;; Completion-UI comes with built-in support for a number of completion
;; sources: the standard Emacs dabbrevs, etags, Elisp completion and file-name
;; completion, as well as (if installed) CEDET's Semantic completion,
;; nxml-mode, and the Predictive completion package.
;;
;; Completion-UI provides the following user-interfaces and features (though
;; it is also easy to add new ones, see below):
;;
;; * Dynamic completion
;;     provisionally insert the first available completion candidate into the
;;     buffer
;;
;; * Completion hotkeys
;;     single-key selection of a completion candidate
;;
;; * Cycling
;;     cycle through completion candidates.
;;
;; * Tab-completion
;;     "traditional" expansion to longest common substring
;;
;; * Echo
;;     display a list of completion candidates in the echo-area
;;
;; * Tooltip
;;     display a list of completion candidates in a tool-tip located below the
;;     point, from which completions can be selected
;;
;; * Pop-up frame
;;     display a list of completion candidates in a pop-up frame located below
;;     the point, which can be toggled between displaying some or all
;;     completions, and from which completions can be selected
;;
;; * Completion menu
;;     allow completion candidates to be selected from a drop-down menu
;;     located below the point
;;
;; * Completion browser
;;     browse through all possible completion candidates in a hierarchical
;;     deck-of-cards menu located below the point
;;
;; * `auto-completion-mode'
;;     automatically complete words as you type
;;
;;
;;
;; For Emacs users:
;; ================
;;
;; INSTALLING
;; ----------
;; To install this package, save this and all the other accompanying
;; "completion-ui-*.el" files to a directory in your `load-path' (you can view
;; the current `load-path' using "C-h v load-path" within Emacs), then add the
;; following line to your .emacs startup file:
;;
;;    (require 'completion-ui)
;;
;;
;; USING
;; -----
;; For each source of completions, Completion-UI provides an interactive
;; command that completes the word next to the point using that source, e.g:
;; `complete-dabbrev' to complete using dabbrevs, `complete-etags' to complete
;; using etags, `complete-elisp' to complete Elisp symbols, `complete-files'
;; to complete file names, `complete-semantic' to use Semantic completion,
;; `complete-nxml' and `complete-predictive' to use nxml-mode and
;; predictive-mode completion, respectively.
;;
;; The `complete-<name>' commands are not bound to any key by default. As with
;; any Emacs command, you can run them via "M-x complete-<name>", or you can
;; bind them to keys, either globally or in a minor-mode keymap. E.g. to
;; globally bind "M-/" to `complete-dabbrev', you would put the following line
;; in your .emacs file:
;;
;;   (global-set-key [?\M-/] 'complete-dabbrev)
;;
;; To bind "M-<tab>" to `complete-elisp' in `emacs-lisp-mode', you would bind
;; the command in the `emacs-lisp-mode-map' keymap:
;;
;;   (define-key emacs-lisp-mode-map [?\M-\t] 'complete-elisp)
;;
;; You're free to bind the `complete-<name>' commands to any keys of your
;; choosing, though "M-<tab>" or "M-/" fit best with the default Completion-UI
;; key bindings that are enabled when you're completing a word. These are:
;;
;; M-<tab>  M-/
;;   Cycle through completions.
;;
;; M-S-<tab>  M-?
;;   Cycle backwards through completions.
;;
;; C-<ret>
;;   Accept the current completion.
;;
;; C-<del>
;;    Reject the current completion.
;;
;; <tab>
;;    Traditional tab-completion, i.e. insert longest common substring.
;;
;; C-<tab>
;;    Accept current completion and re-complete the resulting word.
;;
;; S-<down>
;;    Display the completion tooltip (then use <up> and <down> to cycle).
;;
;; M-<down>
;;    Display the completion menu.
;;
;; C-<down>
;;    Display the completion pop-up frame.
;;
;; S-<up> C-<up> M-<up> (in pop-up frame)
;;    Dismiss the completion pop-up frame.
;;
;; M-/ (in pop-up frame)
;;    Toggle between displaying all completions.
;;
;;
;; Completion-UI also provides a minor-mode, `auto-completion-mode', which
;; automatically completes words as you type them using any one of the
;; completion sources. You can select the source to use for
;; `auto-completion-mode' by customizing `auto-completion-source' (the default
;; is dabbrev).
;;
;; To enable and disable `auto-completion-mode', use:
;;
;;   M-x auto-completion-mode
;;
;; Note that `auto-completion-mode' is not very useful if the completion
;; source takes a long time to find completions.
;;
;; <shameless plug>
;; The Predictive completion package (available separately from the above URL)
;; is designed from the ground up to be extremely fast, even when a very large
;; number of completion candidates are available. As you type, it also learns
;; to predict which completion is the most likely. So it is particularly
;; suited to being used as the `auto-completion-mode' source.
;; </shameless plug>
;;
;;
;; CUSTOMIZING
;; -----------
;; The completion user-interfaces can be heavily customized and tweaked to
;; suit your every desire, via the `completion-ui' customization group, (and
;; subgroups thereof):
;;
;;   M-x customize-group <ret> completion-ui <ret>
;;
;; All the customization options and settings are well documented via the
;; usual built-in Emacs documentationn features.
;;
;;
;;
;; For Elisp coders:
;; =================
;;
;; In fact, Completion-UI is even better than a swiss-army knife, because it's
;; also extensible: it's easy to add new user-interfaces, as well as new
;; completion sources.
;;
;; The philosophy of Completion-UI is that customization of the user-interface
;; should be left up to USERS. They know what they want better than you do!
;; By providing a universal user-interface that can be used by all completion
;; packages, Completion-UI lets users customize their in-buffer completion
;; user interface once-and-for-all to suit their tastes, rather than having to
;; learn how to customize each new package separately.
;;
;;
;; Adding new sources
;; ------------------
;; See `completion-ui-register-source'.
;;
;; One call to `completion-ui-register-source' is all there is to it!
;; Registering a new source will define a new interactive command,
;; `complete-<name>' (where <name> is the supplied source name) which
;; completes whatever is at the point using the new completion source. It will
;; also add the new source to the list of choices for the
;; `auto-completion-source' customization option (unless this is supressed).
;;
;;
;; Adding new interfaces
;; ---------------------
;; See `completion-ui-register-interface'.
;;
;; A number of Completion-UI functions are intended for use in creating new
;; user-interfaces. These all start with the prefix `completion-ui-' (as
;; opposed to `completion-', which are user commands plus a few general
;; purpose utility functions, or `completion--', which are internal functions
;; that are NOT intended for external use).



;;; Change Log:
;;
;; Version 0.11.12
;; * modified `completion-ui-resolve-old' to take optional arguments
;;   limiting region in which to resolve completions
;; * added new `completion-resolve-before-undo' to `before-change-functions'
;;   hook in `auto-completion-mode' to resolve completions in text region
;;   modified by an undo, since undo screws up the completion overlay
;;
;; Version 0.11.11
;; * bug-fix in `completion-reject': used to run `completion-accept-functions'
;;   instead of `completion-reject-functions'!
;; * pass nil instead of empty string to `read-key-sequence' in
;;   `completion--run-if-condition'
;;
;; Version 0.11.10
;; * fall back to global `auto-completion-override-alist' if character not
;;   specified in overlay-local version, in `auto-completion-lookup-behaviour'
;; * fixed `completion-cycle' to cope gracefully with null argument
;;
;; Version 0.11.9
;; * added C-@ binding (produced by C-<space> in terminals)
;; * fixed compile warning in `completion-ui-register-source'
;; * fixed interactive spec in `completion-cycle' and
;;   `completion-cycle-backwards'
;;
;; Version 0.11.8
;; * added M-\ word-constituent binding
;;
;; Version 0.11.7
;; * bug-fixes in `auto-completion-self-insert' and
;;   `completion-backward-delete'
;;
;; Version 0.11.6
;; * added `completion-fill-paragraph', plus key bindings to override
;;   standard `fill-paragraph'
;; * bug-fix in `complete-in-buffer'
;;
;; Version 0.11.5
;; * bug-fix in `completion-ui-source-non-prefix-completion'
;; * changed `auto-completion-self-insert' to always reject if called due to
;;   `completion-auto-update' rather than `auto-completion-mode'
;; * added `completion-auto-update-self-insert' and
;;   `completion-auto-update-overlay-map', and separated
;;   `completion-auto-update' code from `auto-completion-mode' code
;;
;; Version 0.11.4
;; * added `completion-auto-update' customization option
;;
;; Version 0.11.3
;; * bug-fix to `completion-ui-register-interface' :auto-show-helper
;;
;; Version 0.11.2
;; * bug-fixes to cope with elements of completions list that are cons cells
;; * bug-fix in `completion-ui-source-word-thing'
;; * bug-fix in `completion-ui-resolve-old'
;;
;; Version 0.11.1
;; * allow indirection in source :prefix-function, :word-thing,
;;   :tooltip-function, :popup-frame-function, :menu-function and
;;   :browser-function; if they are set to a symbol, that symbol is repeatedly
;;   evaluated until we get a function (or a thing-at-point symbol in the case
;;   of :word-thing)
;; * bug-fixes to `posn-at-point' functions
;; * allow literal prefix string to be passed to `complete-in-buffer'
;;
;; Version 0.11
;; * Major rewrite: completely modularized the user-interfaces and completion
;;   sources!
;; * split `completion-resolve-behaviour' into
;;   `completion-accept-or-reject-by-default' and
;;   `completion-hot-to-resolve-old-completions'
;; * `completion-ui-register-source' settings replace the
;;   `completion-function', `completion-prefix-function' etc. variables
;; * added `auto-completion-source' customization variable
;; * Rationalised function and variable names: `completion-*' are intended for
;;   users (apart from the odd general utility function), `auto-completion-*'
;;   are related to `auto-completion-mode', `completion-ui-*' are related to
;;   user-interface implementation, `completion--*' are intended for internal
;;   use only.
;; * generalised `completion-scoot-ahead' into `completion-extend-prefix'
;;
;; Version 0.10.2
;; * bug-fixes to `completion-replaces-prefix' support (thanks once again to
;;   Henry Weller for reporting them)
;;
;; Version 0.10.1
;; * bug-fixes to `complete-dynamic' relating to prefixes whose size has
;;   changed
;; * refactored recurring code into `completion-highlight-common-substring'
;;   and `completion-hightlight-prefix-alterations' functions
;; * always bind `completion-self-insert' in `completion-map', so that old
;;   completions get resolved
;;
;; Version 0.10
;; * removed `completion-includes-prefix' flag; completion functions now have
;;   to return entire completion, including prefix
;; * prefix is now deleted along with provisional completion when a completion
;;   accepted, and the entire completion is inserted; this allows completions
;;   to modify the prefix when they're accepted
;; * completions can now include data about length of prefix, in case it is
;;   not the same length as the original
;; * `complete-in-buffer' and `complete-word-at-point' can now take arguments
;;   that override the global values for `completion-function',
;;   `completion-prefix-function' and `completion-replaces-prefix'
;; * `completion-construct-menu', `completion-construct-browser-menu' and the
;;   other browser functions now take completion-function, prefix-function and
;;   completion-replaces-prefix arguments
;; * null values for `completion-prefix-function', `completion-menu',
;;   `completion-browser-menu-function', `completion-tooltip-function' and
;;   `completion-popup-frame-function' now mean use the default
;; * only position popup frame in `completion-popup-frame' *after* setting
;;   it's size, otherwise some window managers won't position it where we want
;;
;; Version 0.9.4
;; * modified `completion-run-if-condition' and `completion-select' to get key
;;   sequence used to invoke it via `unread-command-keys' and
;;   `read-key-sequence', to ensure key sequence translation takes place
;; * added new 'pop-up setting for `completion-use-hotkeys' which only enables
;;   hotkeys when a tooltip or pop-up frame is active (thanks to Henry Weller
;;   for the suggestion)
;; * attempted to fix bug preventing default `completion-tooltip-face' being
;;   set correctly
;; * fixed bugs in `completion-browser-sub-menu' and
;;   `completion-browser-menu-iterm'
;; * made most anonymous lambda bindings into names functions (lambdas just
;;   confuse people, and make it hard to bind other keys to the same thing)
;; * bug-fix to `completion-select' which triggered infinite recursion trap
;;
;; Version 0.9.3
;; * added 'accept-common option to `completion-resolve-behaviour'
;;   (thanks to Henry Weller for the patch)
;; * other code refactorings (thanks to Henry Weller again)
;;
;; Version 0.9.2
;; * define hotkey bindings on the fly in `completion-setup-overlay', getting
;;   rid of `completion-hotkey-map' entirely
;; * `completion-hotkey-list' can revert to being a customization option
;; * remove `completion-cancel-tooltip' from `after-change-functions', since
;;   this prevents tooltip being displayed when `flyspell-mode' is enabled
;;   (it's in `before-command-hook' anyway, which should be enough)
;; * use `run-with-timer' instead of `run-with-idle-timer' in
;;   `completion-auto-show', as it seems to make more sense
;; * move backspace and delete bindings to `completion-dynamic-map' and
;;   `auto-completion-map'
;; * added `completion-browser-recurse-on-completions' variable to control
;;   whether the browser lists completions of completions (of completions
;;   of...)
;; * replace `x-popup-menu' with newer `popup-menu' in `completion-show-menu'
;;
;; Version 0.9.1
;; * use :family attribute of `completion-tooltip-face' to set tooltip font
;;   (thanks to Andy Stewart for the patch)
;;
;; Version 0.9
;; * added `completion-includes-prefix' variable to indicate that completions
;;   returned by `completion-function' include the prefix
;; * added `completion-replaces-prefix' variable to indicate that completions
;;   should replace the prefix when they're accepted, and made changes to
;;   almost all of the user-interface functions to cope with this non-prefix
;;   completion (thanks to Henry Weller for helpful discussions about this)
;; * move point to appropriate position in parent frame when cycling through
;;   completions in pop-up frame
;; * added `completion-show-browser-menu' command
;;
;; Version 0.8.2
;; * prevented `completion-show-tooltip' from moving mouse unless absolutely
;;   necessary (thanks to Martin Pohlack for reporting this)
;; * modified `completion-show-tooltip' to ensure tooltip is cancelled before
;;   (re)displaying it, otherwise `x-show-tip' "magically" moves it to the top
;;   of the frame! (thanks to Martin Pohlack for reporting this)
;; * added option to highlight longest common prefix in a dynamic completion
;;   (thanks to Vagn Johansen for the suggestion)
;; * actually make use of `completion-popup-frame-function' variable!
;; * fixed bug in some calls to `completion-reject-functions' that only passed
;;   two arguments instead of three (thanks to Vagn Johansen for reporting
;;   this)
;;
;; Version 0.8,1
;; * fix `completion-define-word-syntax-binding' so it creates key binding in
;;   `auto-completion-dynamic-map' as it should
;; * fix `completion-setup-overlay' to assign correct keymap to 'keymap
;;   property, depending on whether `auto-completion-mode' is enabled or not
;;
;; Version 0.8
;; * give completion overlay a non-nil end-advance property, because...
;; * ...keymap property works for zero-length overlays with non-nil
;;   end-advance since Emacs 22! So disable work-around from that versions on.
;;
;; Version 0.7.5
;; * added `completion-simulate-overlay-bindings' function that can
;;   automatically create key bindings to simulate overlay keymap bindings
;;   using the `completion-run-if-within-overlay' hack
;;
;; Version 0.7.4
;; * split `completion-self-insert' into two: one function, the new
;;   `completion-self-insert', deals with completion-related stuff if
;;   auto-completion-mode is disabled, the other,
;;   `auto-completion-self-insert', does auto-completion as
;;   before. This allows individual printable characters to invoke
;;   auto-completion without auto-completion-mode being enabled.
;; * `auto-completion-self-insert' is now only directly bound to
;;   printable characters in auto-completion-map
;; * overlay keymap now binds printable characters to the new
;;   `completion-self-insert' (which hands off to
;;   `auto-completion-self-insert' if auto-completion is enabled)
;;
;; Version 0.7.3
;; * fixed bug in `completion-popup-frame-toggle-show-all'
;; * fixed bug in definition of `completion-tooltip-face'
;;
;; Version 0.7.2
;; * prevent `complete-in-buffer' from auto-displaying the tooltip/menu/pop-up
;;   frame if there are no completions (otherwise Emacs CVS seems to crash!)
;; * bug fixes to key bindings and `completion-tab-complete'
;; * bug fixes to pop-up frames
;;
;; Version 0.7.1
;; * minor key binding fixes
;; * `complete-in-buffer' can now take an optional prefix argument to override
;;   automatically determined prefix
;; * `completion-self-insert' can now take an optional argument that causes
;;   `auto-completion-syntax-override-alist' to be ignored, as can
;;   `completion-define-word-constituent-binding'
;; * bug fixes to `completion-self-insert'
;; * switched ordering of `auto-completion[-override]-syntax-alist' entries
;;   back to something closer to old ordering
;;
;; Version 0.7
;; * modified core `complete-in-buffer', `complete-word-at-point',
;;   `completion-self-insert' and `completion-backward-delete' functions
;;   to allow `completion-prefix-function' to properly take over prefix
;;   finding
;; * created default `completion-prefix' function
;; * modified same core functions so that completion behaviour is more
;;   intelligent, especially when a character is inserted within a
;;   completion overlay
;; * `completion-overwrite' option now controls whether completions
;;   over-write the remainder of the word at the point or not
;; * renamed `completion-dynamic-*syntax-alist' to
;;   `auto-completion-*syntax-alist' and modified their format somewhat;
;;   behaviour now accessed through interface macros
;; * added new pop-up frame completion method (thanks to anon. on the
;;   Emacs wiki for the suggestion)
;; * auto-show can now display one out of the tooltip, completion menu,
;;   or pop-up frame
;; * `completion-tooltip-delay' and `completion-auto-show-menu' options
;;   subsumed into `completion-auto-show' and
;;   `completion-auto-show-delay'
;; * RET binding now respects customization options
;;
;; Version 0.6.5
;; * bug-fixes to interactive definitions
;; * moved modification hook setting to end of file
;;
;; Version 0.6.4
;; * defined properties to make delete-selection-mode work correctly
;;   (thanks to Sivaram for drawing my attention to this)
;; * minor improvement to text displayed in completion browser bucket
;;   menu entries
;;
;; Version 0.6.3
;; * fixed M-<space> bindings so that prefix argument is passed to
;;   `completion-reject', and fixed C-<space> bindings
;;
;; Version 0.6.2
;; * modified the default `completion-dynamic-syntax-alist' to make
;;   parentheses behave like punctuation
;; * minor bug-fix to `completion-show-menu-if-within-overlay'
;; * fixed `completion-self-insert' again so that it works if called with
;;   an explicit char (auto-fill will not work in that case)
;; * fixed `complete-dynamic' so that the completion overlay ends up in
;;   the right place even when modification hooks cause text to be
;;   inserted in the buffer during its execution
;;
;; Version 0.6.1
;; * modified define-minor-mode usage for auto-completion-mode to work in
;;   older Emacs versions
;; * fixed `completion-self-insert' so that auto-fill works again
;; * if command remapping isn't supported, attempt to simulate it more
;;   effectively for deletion commands
;;
;; Version 0.6
;; * added `completion-prefix' and `completion-tooltip' variables to
;;   allow overriding of default methods for determining prefix at point
;;   and constructing tooltip text
;; * fixed bugs related to backwards-deletion (thanks to Maciej
;;   Katafiasz for pointing some of these out)
;; * added optional arguements to `completion-self-insert' to allow
;;   automatically determined character and syntax to be overridden, and
;;   created key bindings to insert characters as word constituents
;; * modified `completion-backward-delete', created corresponding
;;   `completion-delete' function, and defined a whole host of deletion
;;   and kill commands that are substituted for the standard ones
;; * added convenience function
;;   `completion-define-word-constituent-binding' for defining bindings
;;   to insert characters as word-constituents
;;
;;
;; Version 0.5.2
;; * fixed tooltip face issues, which included defining a new
;;   `completion-tooltip-face'
;; * implemented better method of positioning tooltip, avoiding moving
;;   the mouse (thanks to Nikolaj Schumacher for this!)
;;
;; Version 0.5.1
;; * fixed small bug in `completion-self-insert' (thanks to Nikolaj
;;   Schumacher for pointing it out)
;;
;; Version 0.5
;; Modifications arising from discussions with rms:
;; * removed `completion-define-minor-mode' macro; to use completion-UI,
;;   `completion-function' should just be set appropriately
;; * auto-completion is now a separate minor mode
;; * renamed various variables and functions
;;
;; Version 0.4.1
;; * small but important bug-fix to `completion-accept'
;;
;; Version 0.4
;; * accept and reject hooks now called with two or three arguments
;;   instead of one: the prefix, the full word (this is what was passed
;;   previously) and possibly the interactive prefix argument.
;; * moved some anonymous commands into named functions to sanitize
;;   key-bindings
;;
;; Version 0.3.13
;; * Tried to work around annoying `completion-select' bug
;;
;; Version 0.3.12
;; * added `completion-backward-delete-delay' customization option
;;
;; Version 0.3.11
;; * finally figured out how to prevent list of completions displayed in
;;   echo area from being logged
;;
;; Version 0.3.10
;; * fixed start-of-word behaviour in `completion-self-insert'
;;
;; Version 0.3.9
;; * `completion-select' now uses the `completion-trap-recursion'
;;   variable, instead of testing if 'trap-recursion is bound
;;
;; Version 0.3.8
;; * fixed `completion-run-if-within-overlay' so it doesn't error if
;;   there's no "normal" binding for the key sequence used to invoke it
;; * defined a new `completion-trap-recursion' variable in case the
;;   symbol trap-recursion is bound outside
;;   `completion-run-if-within-overlay'
;;
;; Version 0.3.7
;; * fixed M-<space> binding so it's only active within an overlay
;;
;; Version 0.3.6
;; * fixed bug in `completion-define-minor-mode'
;;
;; Version 0.3.5
;; * added eval-when-compile to prevent bogus compilation errors
;;
;; Version 0.3.4
;; * added function to `after-change-functions' to hide tooltip
;; * made self-insert behaviour alists more flexible
;; * minor fix to `completion-cycle' to leave point at end of word if
;;   dynamic completion is disabled
;; * `completion-hotkey-list' no longer a customization option, since it
;;   must be set *before* completion-ui.el is loaded
;;
;; Version 0.3.3
;; * minor bug-fix to `completion-self-insert'
;; * removed cl dependency
;;
;; Version 0.3.2
;; * bug fixes
;; * incorporated compatability code
;;
;; Version 0.3.1
;; * bug fixes
;;
;; Version 0.3
;; * incorporated a lot of code from predictive.el
;; * rewrote things so that all a package needs to do is set
;;   the `completion-function' variable
;; * `completon-overlay-at-point' is kludgy no more
;;
;; Version 0.2.1
;; * added commentary
;; * prevented any attempt to display tooltips and menus when not
;;   running X
;;
;; Version 0.2
;; * bug fixes (thanks to Mark Zonzon for patch)
;; * added `completion-min-chars' and `completion-delay' options
;;   (thanks to Jin Tong for suggestions)
;; * renamed to `completion-ui.el'
;;
;; Version 0.1
;; * initial release


;;; Code:


(eval-when-compile (require 'cl))
;;(require 'auto-overlay-common nil t)


(defvar completion-ui-interface-definitions nil
  "Completion-UI user-interface definitions.")

(defvar completion-ui-source-definitions nil
  "Compltion-UI source definitions.")




;;; ============================================================
;;;                    Customization variables

(defgroup completion-ui nil
  "Completion user interface."
  :group 'convenience)


(defcustom completion-max-candidates 10
  "*Maximum number of completion candidates to offer."
  :group 'completion-ui
  :type 'integer)


(defcustom completion-accept-or-reject-by-default 'accept
  "*Default action for the current completion:

  'accept:        automatically accept the completion
  'accept-common: automatically accept the common part of the completion
  'reject:        automatically reject the completion"
  :group 'completion-ui
  :type '(choice (const :tag "accept" accept)
                 (const :tag "reject" reject)
                 (const :tag "accept-common" accept-common)))


(defcustom completion-how-to-resolve-old-completions 'accept
  "*What to do with unfinished and abandoned completions
left behind in the buffer:

  'leave:         leave the old completions pending
  'accept:        automatically accept the old completions
  'reject:        automatically reject the old completions
  'ask:           ask what to do with the old completions"
  :group 'completion-ui
  :type '(choice (const :tag "leave" leave)
                 (const :tag "accept" accept)
                 (const :tag "reject" reject)
                 (const :tag "ask" ask)))


(defcustom completion-overwrite t
  "*When non-nil, completing in the middle of a word over-writes
the rest of the word. `completion-word-thing' determines what is
considered a word."
  :group 'completion-ui
  :type 'boolean)


(defcustom completion-auto-update t
  "*When non-nil, completion candidates are updated automatically
when characters are added to the current completion prefix.
\(Effectively, it is as though `auto-completion-mode' is
temporarily enabled for the duration of the completion.\)"
  :group 'completion-ui
  :type 'boolean)


(defcustom completion-auto-show nil
  "Function to call to display a completion user-interface.
When null, nothing is auto-displayed.

The function is called after a completion command, possibly after
a delay of `completion-auto-show-delay' seconds if one is set. It
is passed one argument, a completion overlay."
  :group 'completion-ui
  :type '(choice (const nil)))


(defcustom completion-auto-show-delay 3
  "*Number of seconds to wait after completion is invoked
before the `completion-auto-show' interface is activated."
  :group 'completion-ui
  :type '(choice (const :tag "Off" nil)
                 (float :tag "On")))



(defface completion-highlight-face
  '((((class color) (background dark))
     (:background "blue" :foreground "white"))
    (((class color) (background light))
     (:background "orange" :foreground "black")))
  "*Face used to highlight completions in various user-interfaces."
  :group 'completion-ui)



;;; ===== Auto-completion customizations =====

(defgroup auto-completion-mode nil
  "auto-completion-mode"
  :group 'completion-ui)


(defcustom auto-completion-source 'dabbrev
  "*Completion source for `auto-completion-mode'."
  :group 'auto-completion-mode
  :type `(choice
	  (const nil)
	  ,@(nreverse
	     (mapcar
	      (lambda (def)
		(list 'const (car def)))  ; FIXME: breaks abstraction
	      completion-ui-source-definitions))))


(defcustom auto-completion-min-chars nil
  "*Minimum number of characters before completions are offered."
  :group 'auto-completion-mode
  :type '(choice (const :tag "Off" nil)
                 (integer :tag "On")))


(defcustom auto-completion-delay nil
  "*Number of seconds to wait before activating completion mechanisms
in auto-completion mode."
  :group 'auto-completion-mode
  :type '(choice (const :tag "Off" nil)
                 (float :tag "On")))


(defcustom auto-completion-backward-delete-delay 0.1
  "*Number of seconds to wait before activating completion mechanisms
after deleting backwards in auto-completion mode."
  :group 'auto-completion-mode
  :type 'float)


(defcustom auto-completion-syntax-alist '(reject . word)
  "*Associates character syntax with completion behaviour.
Used by the `auto-completion-self-insert' function to decide what
to do based on a typed character's syntax.

When customizing this variable, predefined choices can be used to
configure two separate syntax-dependent completion behaviours:
how provisional completions are accepted, and how the prefix is
chosen when characters are typed. The first choice is between
\"type normally\" or \"punctuation accepts\", and controls how
completions are accepted. The second is between \"word\" or
\"string\", and controls how the prefix is chosen.

If \"type normally\" is selected, the provisional completions
that appear as you type are only accepted if you call
`completion-accept' manually. You are free to ignore them
entirely and type normally. If \"punctuation accepts\" is
selected, the provisional completions are automatically accepted
whenever you type any punctuation or whitespace character (as
defined by the buffers' syntax table). For example, hitting SPC
will usually accept the current provisional completion and insert
a space after it. Once your fingers get used to it, this can
allow you to type faster as you can quickly accept a completion
and move onto the next word. However, you can no longer entirely
ignore the completions and type normally, since you may
accidentally accept a completion you didn't want.

If \"word\" is selected, typing a word-constituent character (as
defined by a buffer's syntax table) will cause the part of the
word before point to be completed. That is,
`auto-completion-mode' will first find the word at or next to the
point, and then complete that part of it that comes before the
point (`completion-word-thing' determines which characters form
part of a word). If \"string\" is selected, typing a
word-constituent character will complete the string that has been
built up by typing consecutive characters. That is, the prefix
will consist of all the characters you've typed in the buffer
since the last time you did something other than typing a
word-constituent character. Although they sound quite different,
the two behaviours usually only differ if you move the point to
the middle or end of an existing word and then start typing.


Customizing the behaviour for each syntax individually gives more
fine-grained control over the syntax-dependent completion
behaviour. In this case, the value of
`auto-completion-syntax-alist' must be an alist associating
syntax descriptors (characters) with behaviours (two-element
lists).

The first element of the behaviour list must be one of symbols
'accept, 'reject or 'add. The first two have the same meaning as
the predefined behaviours \"punctuation accepts\" and \"type
normally\", though they now apply only to one syntax
descriptor. 'add causes characters with that syntax to be added
to the current completion prefix (this is the usual setting for
word-constutuent characters).

The second element of the list must be one of the symbols 'word,
'string or 'none. 'word and 'string have the same meaning as the
predefined behaviours, though they now apply only to one syntax
descriptor, whereas 'none prevents characters with that syntax
from invoking auto-completion.


When `auto-completion-syntax-alist' is set from Lisp code, in
addition to the symbol values described above the behaviour list
entries can also be functions which return one of those
symbols. The list can also have an additional third element,
which determines whether or not the typed character is inserted
into the buffer: the character is inserted if it is non-nil, not
if it is nil. (Note that, perhaps confusingly, a non-existent
third element is equivalent to setting the third element to
t). If the third element of the list is a function, its return
value again determines the insertion behaviour. This allows a
function to take-over the job of inserting characters (e.g. in
order to make sure parentheses are inserted in pairs), and is
probably the only time it makes sense to use a null third
element."
  :group 'auto-completion-mode
  :type '(choice
          (cons :tag "Predefined"
                (choice :tag "Acceptance behaviour"
                        (const :tag "type normally" reject)
                        (const :tag "punctuation accepts" accept))
                (choice :tag "Completion behaviour"
                        (const word)
                        (const string)))
          (alist :tag "Custom"
                 :key-type character
                 :value-type (list
                              (choice (const accept)
                                      (const reject)
                                      (const add))
                              (choice (const word)
                                      (const string)
                                      (const none))))))


(defcustom auto-completion-override-syntax-alist
  '((?0 . (reject none))
    (?1 . (reject none))
    (?2 . (reject none))
    (?3 . (reject none))
    (?4 . (reject none))
    (?5 . (reject none))
    (?6 . (reject none))
    (?7 . (reject none))
    (?8 . (reject none))
    (?9 . (reject none))
    (?' . (add word)))
  "*Alist associating characters with completion behaviour.
Overrides the default behaviour defined by the character's syntax
in `auto-completion-syntax-alist'. The format is the same as for
`auto-completion-synax-alist', except that the alist keys are
characters rather than syntax descriptors."
  :group 'auto-completion-mode
  :type '(alist :key-type (choice character (const :tag "default" t))
                :value-type (list (choice (const :tag "accept" accept)
                                          (const :tag "reject" reject)
                                          (const :tag "add" add))
                                  (choice (const :tag "string" string)
                                          (const :tag "word" word)
                                          (const :tag "none" none)))))




;;; ============================================================
;;;               Other configuration variables

(defvar completion-accept-functions nil
  "Hook run after a completion is accepted.

Completions are accepted by calling `completion-accept',
selecting one with a hotkey, or selecting one from a
menu. Functions are passed three arguments: the prefix, the
complete string that was accepted (including the prefix), and any
prefix argument supplied by the user if the accept command was
called interactively.")


(defvar completion-reject-functions nil
  "Hook run after a completion is rejected.

Completions are rejected by calling
`completion-reject'. Functions are passed three arguments: the
prefix, the complete string that was rejected \(including the of
the prefix\), and any prefix argument supplied by the user if the
rejection command was called interactively.")



(defvar completion-overlay-map nil
  "Keymap active in a completion overlay.")

(defvar auto-completion-overlay-map nil
  "Keymap active in a completion overlay when
`auto-completion-mode' is enabled.")

(defvar completion-auto-update-overlay-map nil
  "Keymap active in a completion overlay when
`completion-auto-update' is enabled.")

(defvar completion-map nil
  "Keymap active when a Completion-UI is loaded.")

(defvar auto-completion-map nil
  "Keymap active when `auto-completion-mode' is enabled.")




;;; ============================================================
;;;                     Internal variables

(defvar completion-ui--activated nil
  "Non-nil when Completion-UI is activated in a buffer.

This variable enables the global `completion-map' keymap, which
contains hacks to work-around poor overlay keymap support in
older versions of Emacs.

It should only ever be disabled when debugging Completion-UI and
the `completion-map' bindings are causing problems.")

(make-variable-buffer-local 'completion-ui--activated)


(defvar completion--overlay-list nil
  "List of overlays used during completion")
(make-variable-buffer-local 'completion--overlay-list)


(defvar completion--auto-timer (timer-create)
  "Timer used to postpone auto-completion or auto-show
until there's a pause in typing.")


(defvar completion--backward-delete-timer nil
  "Timer used to postpone completion until finished deleting.")


(defvar completion--trap-recursion nil
  "Used to trap infinite recursion errors
in calls to certain completion functions, for which infinite
recursion can result from incorrectly configured key bindings set
by the Emacs user.")




;;; =================================================================
;;;            Set properties for delete-selection-mode

(put 'auto-completion-self-insert 'delete-selection t)
(put 'completion-accept-and-newline 'delete-selection t)
(put 'completion-backward-delete-char 'delete-selection 'supersede)
(put 'completion-backward-delete-char-untabify
     'delete-selection 'supersede)
(put 'completion-delete-char 'delete-selection 'supersede)




;;; ===============================================================
;;;                  Keybinding functions

(defun completion-define-word-constituent-binding
  (key char &optional syntax no-syntax-override)
  "Setup key binding for KEY so that it inserts character CHAR as
though it's syntax were SYNTAX. SYNTAX defaults to
word-constituent, ?w, hence the name of the function, but it can
be used to set up any syntax.

If NO-SYNTAX-OVERRIDE is non-nil, this binding will cause
`auto-completion-override-syntax-alist' to be ignored when this
key binding is used, so that the behaviour is determined only by
SYNTAX."

  (when (null syntax) (setq syntax ?w))
  (let ((doc (concat "Insert \"" (string char) "\" as though it were a\
 word-constituent.")))

    ;; create `auto-completion-overlay-map' binding
    (define-key auto-completion-overlay-map key
      `(lambda () ,doc
         (interactive)
         (auto-completion-self-insert ,char ,syntax
                                      ,no-syntax-override)))

    ;; if emacs version doesn't support overlay keymaps properly, create
    ;; binding in `completion-map' to simulate it via
    ;; `completion--run-if-within-overlay' hack
    (when (<= emacs-major-version 21)
      (define-key completion-map key
        `(lambda () ,doc
           (interactive)
           (completion--run-if-within-overlay
            (lambda () (interactive)
              (auto-completion-self-insert ,char ,syntax ,no-syntax-override))
            'completion-ui--activated))))))


(defun completion--remap-delete-commands (map)
  "Remap deletion commands to completion-UI versions
\(or substitute existing bindings, if remapping is not supported\)."

  ;; If command remapping is supported, remap delete commands
  (if (fboundp 'command-remapping)
      (progn
        (define-key map [remap delete-char]
          'completion-delete-char)
        (define-key map [remap backward-delete-char]
          'completion-backward-delete-char)
        (define-key map [remap delete-backward-char]
          'completion-backward-delete-char)
        (define-key map [remap backward-delete-char-untabify]
          'completion-backward-delete-char-untabify)
        (define-key map [remap kill-word]
          'completion-kill-word)
        (define-key map [remap backward-kill-word]
          'completion-backward-kill-word)
        (define-key map [remap kill-sentence]
          'completion-kill-sentence)
        (define-key map [remap backward-kill-sentence]
          'completion-backward-kill-sentence)
        (define-key map [remap kill-sexp]
          'completion-kill-sexp)
        (define-key map [remap backward-kill-sexp]
          'completion-backward-kill-sexp)
        (define-key map [remap kill-line]
          'completion-kill-line)
        (define-key map [remap kill-paragraphs]
          'completion-kill-paragraph)
        (define-key map [remap backward-kill-paragraph]
          'completion-backward-kill-paragraph))

    ;; Otherwise, can't do better than define bindings for the keys
    ;; that are currently bound to them
    (dolist (key '([delete] [deletechar] [backspace] "\d"
                   [(control delete)] [(control deletechar)]
                   [(meta delete)] [(meta deletechar)]
                   [(control backspace)] [(meta backspace)] "\M-\d"))
      (catch 'rebound
        (dolist (binding '((delete-char . completion-delete-char)
                           (kill-word . completion-kill-word)
                           (kill-sentence . completion-kill-sentence)
                           (kill-sexp . completion-kill-sexp)
			   (kill-line . completion-kill-line)
                           (kill-paragraph . completion-kill-paragraph)
                           (backward-delete-char
                            . completion-backward-delete-char)
                           (delete-backward-char
                            . completion-backward-delete-char)
                           (backward-delete-char-untabify
                            . completion-backward-delete-char-untabify)
                           (backward-kill-word
                            . completion-backward-kill-word)
                           (backward-kill-sentence
                            . completion-backward-kill-sentence)
                           (backward-kill-sexp
                            . completion-backward-kill-sexp)
                           (backward-kill-paragraph
                            . completion-backward-kill-paragraph)))
          (when (eq (key-binding key) (car binding))
            (define-key map key (cdr binding))
            (throw 'rebound t)))))))



(defun completion--bind-printable-chars (map command)
  "Manually bind printable characters to COMMAND.
Command remapping is a far better way to do this, so it should only be
used if the current Emacs version lacks command remapping support."
  (define-key map "A" command)
  (define-key map "a" command)
  (define-key map "B" command)
  (define-key map "b" command)
  (define-key map "C" command)
  (define-key map "c" command)
  (define-key map "D" command)
  (define-key map "d" command)
  (define-key map "E" command)
  (define-key map "e" command)
  (define-key map "F" command)
  (define-key map "f" command)
  (define-key map "G" command)
  (define-key map "g" command)
  (define-key map "H" command)
  (define-key map "h" command)
  (define-key map "I" command)
  (define-key map "i" command)
  (define-key map "J" command)
  (define-key map "j" command)
  (define-key map "K" command)
  (define-key map "k" command)
  (define-key map "L" command)
  (define-key map "l" command)
  (define-key map "M" command)
  (define-key map "m" command)
  (define-key map "N" command)
  (define-key map "n" command)
  (define-key map "O" command)
  (define-key map "o" command)
  (define-key map "P" command)
  (define-key map "p" command)
  (define-key map "Q" command)
  (define-key map "q" command)
  (define-key map "R" command)
  (define-key map "r" command)
  (define-key map "S" command)
  (define-key map "s" command)
  (define-key map "T" command)
  (define-key map "t" command)
  (define-key map "U" command)
  (define-key map "u" command)
  (define-key map "V" command)
  (define-key map "v" command)
  (define-key map "W" command)
  (define-key map "w" command)
  (define-key map "X" command)
  (define-key map "x" command)
  (define-key map "Y" command)
  (define-key map "y" command)
  (define-key map "Z" command)
  (define-key map "z" command)
  (define-key map "'" command)
  (define-key map "-" command)
  (define-key map "<" command)
  (define-key map ">" command)
  (define-key map " " command)
  (define-key map "." command)
  (define-key map "," command)
  (define-key map ":" command)
  (define-key map ";" command)
  (define-key map "?" command)
  (define-key map "!" command)
  (define-key map "\"" command)
  (define-key map "0" command)
  (define-key map "1" command)
  (define-key map "2" command)
  (define-key map "3" command)
  (define-key map "4" command)
  (define-key map "5" command)
  (define-key map "6" command)
  (define-key map "7" command)
  (define-key map "8" command)
  (define-key map "9" command)
  (define-key map "~" command)
  (define-key map "`" command)
  (define-key map "@" command)
  (define-key map "#" command)
  (define-key map "$" command)
  (define-key map "%" command)
  (define-key map "^" command)
  (define-key map "&" command)
  (define-key map "*" command)
  (define-key map "_" command)
  (define-key map "+" command)
  (define-key map "=" command)
  (define-key map "(" command)
  (define-key map ")" command)
  (define-key map "{" command)
  (define-key map "}" command)
  (define-key map "[" command)
  (define-key map "]" command)
  (define-key map "|" command)
  (define-key map "\\" command)
  (define-key map "/" command))



(defun completion--simulate-overlay-bindings
  (source dest variable &optional no-parent)
  ;; Simulate SOURCE overlay keybindings in DEST using the
  ;; `completion--run-if-within-overlay' hack. DEST should be a symbol whose
  ;; value is a keymap, SOURCE should be a keymap.
  ;;
  ;; VARIABLE should be a symbol that deactivates DEST when its value is
  ;; (temporarily) set to nil. Usually, DEST will be a minor-mode keymap and
  ;; VARIABLE will be the minor-mode variable with which it is associated in
  ;; `minor-mode-map-alist'.
  ;;
  ;; NO-PARENT will prevent this recursing into the parent keymap of SOURCE,
  ;; if it has one.

  ;; if NO-PARENT is specified, remove parent keymap if there is one
  (when (and no-parent (memq 'keymap (cdr source)))
    (setq source
          (completion--sublist
           source 0 (1+ (completion--position 'keymap (cdr source))))))

  ;; map over all bindings in SOURCE
  (map-keymap
   (lambda (key binding)
     ;; don't simulate remappings, and don't simulate parent keymap's bindings
     (unless (eq key 'remap)
       ;; usually need to wrap key in an array for define-key
       (unless (stringp key) (setq key (vector key)))
       ;; bind key in DEST to simulated overlay keymap binding
       (define-key dest key
         (completion--construct-simulated-overlay-binding binding variable))))
   source))



(defun completion--construct-simulated-overlay-binding (binding variable)
  ;; Return a binding that simulates assigning BINDING to KEY in an overlay
  ;; keymap, using the `completion--run-if-within-overlay' hack.
  ;;
  ;; VARIABLE should be a symbol that deactivates BINDING when its value is
  ;; (temporarily) set to nil. Typically, BINDING will be bound in a
  ;; minor-mode keymap and VARIABLE will be the minor-mode variable with which
  ;; it is associated in `minor-mode-map-alist'.
  ;;
  ;; The return value is a command if BINDING was a command, or a keymap if
  ;; BINDING was a keymap. Any other type of BINDING (e.g. a remapping)
  ;; returns nil, since there is no easy way to simulate this.

  (cond
   ;; don't simulate command remappings or keyboard macros
   ((or (eq binding 'remap) (stringp binding))
    nil)

   ;; if BINDING is a keymap, call ourselves recursively to construct a keymap
   ;; filled with bindings that simulate an overlay keymap
   ((keymapp binding)
    (let ((map (make-sparse-keymap)))
      (map-keymap
       (lambda (key bind)
         ;; usually need to wrap key in an array for define-key
         (unless (stringp key) (setq key (vector key)))
         (define-key map key
           (completion--construct-simulated-overlay-binding bind variable)))
       binding)
      map))

   ;; if BINDING is a command, construct an anonymous command that simulates
   ;; binding that command in an overlay keymap
   ((or (commandp binding) (and (symbolp binding) (symbol-function binding)))
    (let (funcdef arglist docstring interactive args)
      ;; get function definition of command
      (cond
       ((symbolp binding) (setq funcdef (symbol-function binding)))
       ((functionp binding) (setq funcdef binding)))
      ;; extract argument list
      (cond
       ;; compiled function
       ((byte-code-function-p funcdef)
	(setq arglist (aref funcdef 0)))
       ;; uncompiled function
       ((and (listp funcdef) (eq (car funcdef) 'lambda))
	(setq arglist (nth 1 funcdef))))
      ;; extract docstring and interactive definition
      (setq docstring (documentation binding))
      (setq interactive (interactive-form binding))
      ;; construct docstring for new binding
      (setq docstring
            (concat "Do different things depending on whether point is "
                    "within a provisional completion.\n\n"
                    "If point is within a provisional completion,\n"
                    (downcase (substring docstring 0 1))
                    (substring docstring 1)
                    "\n\n"
                    "If point is not within a provisional completion,\n"
                    "run whatever would normally be bound to "
                    "this key sequence."))
      ;; construct list of argument variable names, removing &optional and
      ;; &rest
      (setq args '(list))
      (mapc (lambda (a)
              (unless (or (eq a '&optional) (eq a '&rest))
                (setq args (append args (list a)))))
            arglist)
      ;; construct and return command to simulate overlay keymap binding
      `(lambda ,(copy-sequence arglist)
         "" ;,docstring
         ,(copy-sequence interactive)
         (completion--run-if-within-overlay
          (lambda ,(copy-sequence arglist) ,(copy-sequence interactive)
            (apply ',binding ,args))
          ',variable))
      ))

   ;; anything else is an error
   (t (error (concat "Unexpected binding in "
                     "`completion--construct-simulated-overlay-binding': %s")
             binding))))




;;; =================================================================
;;;                     Setup default keymaps

;; Set the default bindings for the keymap assigned to the completion
;; overlays, if it hasn't been defined already (most likely in an init file).
(unless completion-overlay-map
  ;; Note: rebinding printable characters here is redundant if
  ;;       `auto-completion-mode' is enabled, since they are also bound in
  ;;       `auto-completion-map', but we still need to ensure that provisional
  ;;       completions are correctly dealt with even if `auto-completion-mode'
  ;;       is disabled.

  ;; if we can remap commands, remap `self-insert-command' to
  ;; `completion-self-insert'
  (if (fboundp 'command-remapping)
      (progn
        (setq completion-overlay-map (make-sparse-keymap))
        (define-key completion-overlay-map [remap self-insert-command]
          'completion-self-insert))
    ;; otherwise, create a great big keymap and rebind all printable
    ;; characters to `completion-self-insert' manually
    (setq completion-overlay-map (make-keymap))
    (completion--bind-printable-chars completion-overlay-map
				     'completion-self-insert))

  ;; M-<tab> and M-/ cycle word at point
  (define-key completion-overlay-map [?\M-\t] 'completion-cycle)
  (define-key completion-overlay-map "\M-/" 'completion-cycle)
  ;; M-<shift>-<tab> and M-? (usually M-<shift>-/) cycle backwards
  (define-key completion-overlay-map "\M-?" 'completion-cycle-backwards)
  (define-key completion-overlay-map [(meta shift iso-lefttab)]
    'completion-cycle-backwards)
  ;; C-RET accepts, C-DEL rejects
  (define-key completion-overlay-map [(control return)] 'completion-accept)
  (define-key completion-overlay-map [(control backspace)] 'completion-reject)
  ;; <tab> does traditional tab-completion
  (define-key completion-overlay-map "\t" 'completion-tab-complete)
  ;; C-<tab> scoots ahead
  (define-key completion-overlay-map [(control tab)]
    'completion-extend-prefix)
  ;; C-<space> abandons (C-<space> produces C-@ in terminals)
  (define-key completion-overlay-map [?\C- ] 'completion-reject)
  (define-key completion-overlay-map "\C-@" 'completion-reject)
  ;; ;; remap the deletion commands
  (completion--remap-delete-commands completion-overlay-map)
  )


;; Set the default bindings for the keymap assigned to the completion overlays
;; in `auto-completion-mode', if it hasn't been defined already (most likely
;; in an init file).
(unless auto-completion-overlay-map
  ;; inherit all keybindings from completion-overlay-map, then add
  ;; auto-completion specific ones below
  (setq auto-completion-overlay-map (make-sparse-keymap))
  (set-keymap-parent auto-completion-overlay-map completion-overlay-map)

  ;; M-<space> abandons and inserts a space
  (define-key auto-completion-overlay-map "\M- "
    (lambda (&optional arg)
      "Reject any current provisional completion and insert a space."
      (interactive "P")
      (completion-reject arg)
      (insert " ")))

  ;; Note: the only reason we don't use
  ;;       `completion-define-word-constituent-binding' here is that the
  ;;       lambda expressions it creates wouldn't be byte-compiled. Anywhere
  ;;       else, `completion-define-word-constituent-binding' should be used.

  ;; M-S-<space> inserts a space as a word-constituent
  (define-key auto-completion-overlay-map [?\M-\S- ]
    (lambda ()
      "Insert a space as though it were a word-constituent."
      (interactive)
      (auto-completion-self-insert ?\  ?w t)))

  ;; M-. inserts "." as a word-constituent
  (define-key auto-completion-overlay-map "\M-."
    (lambda ()
      "Insert \".\" as though it were a word-constituent."
      (interactive)
      (auto-completion-self-insert ?. ?w t)))

  ;; M-- inserts "-" as a word-constituent
  (define-key auto-completion-overlay-map "\M--"
    (lambda ()
      "Insert \"-\" as though it were a word-constituent."
      (interactive)
      (auto-completion-self-insert ?- ?w t)))

  ;; M-\ inserts "\" as a word-constituent
  (define-key auto-completion-overlay-map "\M-\\"
    (lambda ()
      "Insert \"\\\" as though it were a word-constituent."
      (interactive)
      (auto-completion-self-insert ?\\ ?w t)))

;;;   ;; M-/ inserts "/" as a word-constituent
;;;   (define-key auto-completion-overlay-map "\M-/"
;;;     (lambda ()
;;;       "Insert \"/\" as though it were a word-constituent."
;;;       (interactive)
;;;       (auto-completion-self-insert ?/ ?w t)))

  ;; M-( inserts "(" as a word-constituent
  (define-key auto-completion-overlay-map "\M-("
    (lambda ()
      "Insert \"(\" as though it were a word-constituent."
      (interactive)
      (auto-completion-self-insert ?\( ?w t)))

  ;; M-( inserts ")" as a word-constituent
  (define-key auto-completion-overlay-map "\M-)"
    (lambda ()
      "Insert \")\" as though it were a word-constituent."
      (interactive)
      (auto-completion-self-insert ?\) ?w t)))

  ;; M-{ inserts "{" as a word-constituent
  (define-key auto-completion-overlay-map "\M-{"
    (lambda ()
      "Insert \"{\" as though it were a word-constituent."
      (interactive)
      (auto-completion-self-insert ?{ ?w t)))

  ;; M-} inserts "}" as a word-constituent
  (define-key auto-completion-overlay-map "\M-("
    (lambda ()
      "Insert \"}\" as though it were a word-constituent."
      (interactive)
      (auto-completion-self-insert ?} ?w t)))

  ;; if we can remap commands, remap `self-insert-command'
  (if (fboundp 'command-remapping)
      (define-key auto-completion-overlay-map
	[remap self-insert-command]
	'auto-completion-self-insert)
    ;; otherwise, rebind all printable characters to
    ;; `auto-completion-self-insert' manually
    (completion--bind-printable-chars
     auto-completion-overlay-map
     'auto-completion-self-insert))
  )



;; Set the default bindings for the keymap assigned to the completion overlays
(unless completion-auto-update-overlay-map
  ;; inherit all keybindings from completion-overlay-map, then add
  ;; completion-auto-update specific ones below
  (setq completion-auto-update-overlay-map (make-sparse-keymap))
  (set-keymap-parent completion-auto-update-overlay-map
		     completion-overlay-map)
  ;; if we can remap commands, remap `self-insert-command'
  (if (fboundp 'command-remapping)
      (define-key completion-auto-update-overlay-map
	[remap self-insert-command]
	'completion-auto-update-self-insert)
    ;; otherwise, rebind all printable characters to
    ;; `auto-completion-self-insert' manually
    (completion--bind-printable-chars
     completion-auto-update-overlay-map
     'completion-auto-update-self-insert)))



;; Set the default keymap if it hasn't been defined already (most likely in an
;; init file). This keymap is active whenever `completion-ui--activated' is
;; non-nil.
(unless completion-map
  ;; If the current Emacs version doesn't support overlay keybindings
  ;; half decently and doesn't support command remapping, we're going to
  ;; have to bind all printable characters in this keymap, so we might as
  ;; well create a full keymap
  (if (and (<= emacs-major-version 21)
           (not (fboundp 'command-remapping)))
      (setq completion-map (make-keymap))
    (setq completion-map (make-sparse-keymap)))

  ;; ;; M-<tab> and M-/ cycle or complete word at point
  ;; (define-key completion-map [?\M-\t] 'complete-or-cycle-word-at-point)
  ;; (define-key completion-map "\M-/" 'complete-or-cycle-word-at-point)
  ;; ;; M-<shift>-<tab> and M-? (usually M-<shift>-/) cycle backwards
  ;; (define-key completion-map [(meta shift iso-lefttab)]
  ;;   'complete-or-cycle-backwards-word-at-point)
  ;; (define-key completion-map "\M-?"
  ;;   'complete-or-cycle-backwards-word-at-point)

  ;; RET deals with any pending completion candidate, then runs
  ;; whatever is usually bound to RET.
  ;; Note: although this uses `completion--run-if-within-overlay', it is
  ;;       not a hack to work-around poor overlay keybinding
  ;;       support. Rather, we are using it to run
  ;;       `completion--resolve-current' and then run the normal RET
  ;;       keybinding. We bind it here instead of in the overlay keymap
  ;;       because it's easier to disable this keymap.
  (dolist (key '("\r" "\n" [return]))
    (define-key completion-map key 'completion-resolve-then-run-command))

  ;; remap the deletion commands
  (completion--remap-delete-commands completion-map)

  ;; remap `fill-paragraph', or rebind M-q if we can't remap
  (if (fboundp 'command-remapping)
      (define-key completion-map [remap fill-paragraph]
	'completion-fill-paragraph)
    (define-key completion-map "\M-q" 'completion-fill-paragraph))

  ;; if we can remap commands, remap `self-insert-command' to
  ;; `completion-self-insert'
  (if (fboundp 'command-remapping)
      (define-key completion-map [remap self-insert-command]
	'completion-self-insert)
    ;; otherwise, rebind all printable characters to
    ;; `completion-self-insert' manually
    (completion--bind-printable-chars completion-map 'completion-self-insert))
  )


;; make sure completion-map is associated with `completion-ui--activated' in
;; the minor-mode-keymap-alist, so that the bindings are enabled whenever
;; Completion-UI is loaded
(let ((existing (assq 'completion-ui--activated minor-mode-map-alist)))
  (if existing
      (setcdr existing completion-map)
    (push (cons 'completion-ui--activated completion-map)
          minor-mode-map-alist)))



;; Set the default auto-completion-mode keymap if it hasn't been defined
;; already (most likely in an init file). This keymap is active when
;; `auto-completion-mode' is enabled.
(unless auto-completion-map
  ;; if we can remap commands, remap `self-insert-command' and
  ;; `mouse-yank-at-click'
  (if (fboundp 'command-remapping)
      (progn
        (setq auto-completion-map (make-sparse-keymap))
        (define-key auto-completion-map [remap self-insert-command]
          'auto-completion-self-insert)
	(define-key auto-completion-map [remap mouse-yank-at-click]
	  'auto-completion-mouse-yank-at-click))
    ;; otherwise, create a great big keymap where all printable characters run
    ;; `auto-completion-self-insert', which decides what to do based on the
    ;; character's syntax
    (setq auto-completion-map (make-keymap))
    (completion--bind-printable-chars auto-completion-map
                                     'auto-completion-self-insert)
    (define-key auto-completion-map [mouse-2]
      'auto-completion-mouse-yank-at-click)
    (define-key auto-completion-map [left-fringe mouse-2]
      'auto-completion-mouse-yank-at-click)
    (define-key auto-completion-map [right-fringe mouse-2]
      'auto-completion-mouse-yank-at-click))
  ;; remap the deletion commands
  (completion--remap-delete-commands auto-completion-map)
  ;; remap `fill-paragraph', or rebind M-q if we can't remap
  (if (fboundp 'command-remapping)
      (define-key auto-completion-map [remap fill-paragraph]
	'completion-fill-paragraph)
    (define-key auto-completion-map "\M-q" 'completion-fill-paragraph)))




;;; ================================================================
;;;                Replacements for CL functions

(defun completion--sublist (list start &optional end)
  "Return the sub-list of LIST from START to END.
If END is omitted, it defaults to the length of the list
If START or END is negative, it counts from the end."
  (let (len)
    ;; sort out arguments
    (if end
        (when (< end 0) (setq end (+ end (setq len (length list)))))
      (setq end (or len (setq len (length list)))))
    (when (< start 0)
      (setq start (+ start (or len (length list)))))
    ;; construct sub-list
    (let (res)
      (while (< start end)
        (push (nth start list) res)
        (setq start (1+ start)))
      (nreverse res))))


(defun completion--position (item list)
  "Find the first occurrence of ITEM in LIST.
Return the index of the matching item, or nil of not found.
Comparison is done with 'equal."
  (let ((i 0))
    (catch 'found
      (while (progn
	       (when (equal item (car list)) (throw 'found i))
	       (setq i (1+ i))
	       (setq list (cdr list))))
      nil)))




;;; =======================================================
;;;         Compatibility functions and aliases

(unless (fboundp 'characterp)
  (defalias 'characterp 'char-valid-p))





;;; =======================================================
;;;         Auto-completion minor-mode definition

(define-minor-mode auto-completion-mode
  "Toggle auto-completion mode.
With no argument, this command toggles the mode.
A positive prefix argument turns the mode on.
A negative prefix argument turns it off.

In auto-completion-mode, Emacs will try to complete words as you
type, using whatever completion method has been set up (either by the
major mode, or by another minor mode).

`auto-completion-source' must be set for `auto-completion-mode'
to work."
  nil                   ; init-value
  " Complete"           ; lighter
  auto-completion-map   ; keymap

  (cond
   ;; refuse to enable if no source is defined
   ((and auto-completion-mode (null auto-completion-source))
    (setq auto-completion-mode nil)
    (message (concat "`auto-completion-source' is not set; "
		     "auto-completion-mode NOT enabled")))

   ;; run appropriate hook when `auto-completion-mode' is enabled/disabled
   (auto-completion-mode
    (add-hook 'before-change-functions 'completion-resolve-before-undo nil t)
    (run-hooks 'auto-completion-mode-enable-hook))
   ((not auto-completion-mode)
    (remove-hook 'before-change-functions 'completion-resolve-before-undo t)
    (run-hooks 'auto-completion-mode-disable-hook))))



(defun turn-on-auto-completion-mode ()
  "Turn on auto-completion mode. Useful for adding to hooks."
  (unless auto-completion-mode (auto-completion-mode)))


(defvar auto-completion-mode-enable-hook nil
  "Hook run when `auto-completion-mode' is enabled.")


(defvar auto-completion-mode-disable-hook nil
  "Hook run when `auto-completion-mode' is disabled.")




;;; =======================================================
;;;              Modularized user interfaces

(defmacro completion-ui--interface-variable (def)
  `(car ,def))

(defmacro completion-ui--interface-name (def)
  `(cadr ,def))

(defmacro completion-ui--interface-activate-function (def)
  `(nth 2 ,def))

(defmacro completion-ui--interface-deactivate-function (def)
  `(nth 3 ,def))

(defmacro completion-ui--interface-auto-show (def)
  `(plist-get ,def :auto-show))

(defmacro completion-ui--interface-auto-show-helper (def)
  `(plist-get ,def :auto-show-helper))



(defmacro* completion-ui-register-interface
    (variable &key name activate deactivate
	      update auto-show auto-show-helper after)
  "Register a Completion-UI user-interface.

VARIABLE should be the customization variable (a symbol) used to
enable and disable this interface. It should conform to the
naming convention \"completion-use-<name>\" where <name> is
the NAME of the user-interface.

The :activate keyword argument is mandatory, and should be a
function that takes one argument, a completion overlay, and does
whatever is necessary to activate the user-interface for that
completion. Its return value is ignored. :deactivate is
analogous, but should deactivate the user-interface.

The optional :name keyword argument can be used to give a
name (symbol) to the user-interface, which will appear in
customization options. The default is to construct the interface
name from VARIABLE, removing \"completion-use-\" from the
front.

The optional :after keyword causes the interface definition to be
added at the end of the list, rather than at the
beginning. Interfaces functions are called in the order they
appear in the list of definitions.

The remaining optional keyword arguments specify user-interface
functions to call in various circumstances. They are all called
with a single argument, a completion overlay.

The :update function should update the user-interface after a
change to that completion. If it is not specified, the interface
is updated by calling the :deactivate function, updating the
overlay, then calling the :activate function.

The :auto-show function activates an auto-show
user-interface. The auto-show interface is chosen by setting the
`completion-auto-show' customization option. Only one auto-show
interface can be displayed at any one time.

The :auto-show-helper function is called when an auto-show
interface is activated."

  ;; remove `quote' from arguments
  (when (and (listp variable)
	     (eq (car variable) 'quote))
    (setq variable (cadr variable)))
  (when (and (listp name)
	     (eq (car name) 'quote))
    (setq name (cadr name)))
  (when (and (listp activate)
	     (eq (car activate) 'quote))
    (setq activate (cadr activate)))
  (when (and (listp deactivate)
	     (eq (car deactivate) 'quote))
    (setq deactivate (cadr deactivate)))
  (when (and (listp update)
	     (eq (car update) 'quote))
    (setq update (cadr update)))
  (when (and (listp auto-show)
	     (eq (car auto-show) 'quote))
    (setq auto-show (cadr auto-show)))
  (when (and (listp auto-show-helper)
	     (eq (car auto-show-helper) 'quote))
    (setq auto-show-helper (cadr auto-show-helper)))

  ;; sanity-check arguments
  (unless (symbolp variable)
    (error "completion-ui-register-interface: invalid variable %s"
	   variable))
  ;; (unless (functionp activate)
  ;;   (error "completion-ui-register-interface: invalid activate function %s"
  ;; 	   activate))
  ;; (unless (functionp activate)
  ;;   (error "completion-ui-register-interface: invalid deactivate function %s"
  ;; 	   deactivate))

  ;; extract name from customization-variable if not defined explicitly
  (unless name
    (setq name (symbol-name variable))
    (when (string-match "^completion-use-" name)
      (setq name (substring name (match-end 0))))
    (setq name (intern name)))

  ;; construct interface definiton
  (let ((interface-def
	 (cons
	  variable
	  (append
	   (list name activate deactivate)
	   (when update (list :update update))
	   (when auto-show (list :auto-show auto-show))
	   (when auto-show-helper (list :auto-show-helper auto-show-helper))
	   ))))

    ;; construct code to add interface definition to list (or replace existing
    ;; definition)
    `(progn
       (let ((existing (assq ',variable completion-ui-interface-definitions)))
	 (if (not existing)
	     (add-to-list 'completion-ui-interface-definitions
			  ',interface-def ,after)
	   (message "Completion-UI interface named `%s' already registered\
 - replacing existing definition" ',name)
	   (setcdr existing ',(cdr interface-def))))

       ;; update `completion-auto-show' defcustom
       (defcustom completion-auto-show nil
	 "Function to call to display a completion user-interface.
When null, nothing is auto-displayed.

The function is called after a completion command, possibly after
a delay of `completion-auto-show-delay' seconds if one is set. It
is passed one argument, a completion overlay."
	 :group 'completion-ui
	 :type
	 '(choice
	   (const nil)
	   ,@(let (defcustom-list)
	       (mapc
		(lambda (def)
		  (when (completion-ui--interface-auto-show def)
		    (push
		     (list
		      'const
		      :tag (symbol-name (completion-ui--interface-name def))
		      (completion-ui--interface-auto-show def))
		     defcustom-list)))
		(append
		 (assq-delete-all
		  variable
		  (copy-sequence completion-ui-interface-definitions))
		 (list interface-def)))
	       defcustom-list))))
    ))



(defmacro completion-ui-interface-enabled-p (interface-def)
  ;; Returns non-nil when interface defined by INTERFACE-def is enabled,
  ;; nil otherwise
  `(eval (car ,interface-def)))


(defmacro completion-ui-interface-activate (interface-def overlay)
  ;; Activate interface defined by INTERFACE-DEF for completion OVERLAY
  `(let ((func (completion-ui--interface-activate-function ,interface-def)))
     (when (functionp func) (funcall func ,overlay))))


(defmacro completion-ui-interface-deactivate (interface-def overlay)
  ;; Activate interface defined by INTERFACE-DEF for completion OVERLAY
  `(let ((func (completion-ui--interface-deactivate-function ,interface-def)))
     (when (functionp func) (funcall func ,overlay))))


(defmacro completion-ui-activate-interfaces (overlay)
  ;; Activate all enabled user-interfaces for current completion OVERLAY
  `(dolist (interface-def completion-ui-interface-definitions)
     (when (completion-ui-interface-enabled-p interface-def)
       (completion-ui-interface-activate interface-def ,overlay))))


(defmacro completion-ui-deactivate-interfaces (overlay)
  ;; Deactivate all enabled user-interfaces for current completion OVERLAY.
  `(dolist (interface-def completion-ui-interface-definitions)
     (when (completion-ui-interface-enabled-p interface-def)
       (completion-ui-interface-deactivate interface-def ,overlay))))


(defmacro completion-ui-deactivate-interfaces-pre-update (overlay)
  ;; Deactivate all non-updatable interfaces
  `(dolist (interface-def completion-ui-interface-definitions)
     (when (and (completion-ui-interface-enabled-p interface-def)
		(not (plist-get interface-def :update)))
       (completion-ui-interface-deactivate interface-def ,overlay))))


(defmacro completion-ui-update-interfaces (overlay)
  ;; Run update interfaces, falling back to activating them
  `(let (func)
     (dolist (interface-def completion-ui-interface-definitions)
       (when (completion-ui-interface-enabled-p interface-def)
	 (if (setq func (plist-get interface-def :update))
	     (when (functionp func) (funcall func ,overlay))
	   (completion-ui-interface-activate interface-def ,overlay))))))


(defmacro completion-ui-activate-auto-show-interface (overlay)
  ;; Activate auto-show interface for current completion OVERLAY.
  `(funcall
    (completion-ui--interface-auto-show
     (assq completion-auto-show completion-ui-interface-definitions))
    overlay))


(defun completion-ui-call-auto-show-interface-helpers (overlay)
  ;; Call auto-show-helpers
  (let (func)
     (dolist (interface-def completion-ui-interface-definitions)
       (when (and (completion-ui-interface-enabled-p interface-def)
		  (setq func (completion-ui--interface-auto-show-helper
			      interface-def)))
	 (funcall func overlay)))))


(defmacro completion-ui-deactivate-auto-show-interface (overlay)
  ;; Deactivate auto-show interface for current completion OVERLAY.
  `(completion-ui-interface-deactivate
    (assq (overlay-get ,overlay 'auto-show)
	  completion-ui-interface-definitions))
  `(overlay-put ,overlay 'auto-show nil))




;;; =======================================================
;;;           Modularized source definitions

(defmacro completion-ui--source-def-name (def)
  `(car ,def))

(defmacro completion-ui--source-def-completion-function (def)
  `(cadr ,def))

(defmacro completion-ui--source-def-non-prefix-completion (def)
  `(plist-get ,def :non-prefix-completion))

(defmacro completion-ui--source-def-prefix-function (def)
  `(plist-get ,def :prefix-function))

(defmacro completion-ui--source-def-word-thing (def)
  `(plist-get ,def :word-thing))

(defmacro completion-ui--source-def-accept-function (def)
  `(plist-get ,def :accept))

(defmacro completion-ui--source-def-reject-function (def)
  `(plist-get ,def :reject))

(defmacro completion-ui--source-def-tooltip-function (def)
  `(plist-get ,def :tooltip-function))

(defmacro completion-ui--source-def-popup-frame-function (def)
  `(plist-get ,def :popup-frame))

(defmacro completion-ui--source-def-menu-function (def)
  `(plist-get ,def :menu))

(defmacro completion-ui--source-def-browser-function (def)
  `(plist-get ,def :browser))



(defmacro* completion-ui-register-source
    (completion-function
     &key name completion-args other-args
     non-prefix-completion prefix-function word-thing
     command-name no-command no-auto-completion
     accept-function reject-function
     tooltip-function popup-frame-function menu-function browser-function)
  "Register a Completion-UI source.

COMPLETION-FUNCTION should be a function that takes either zero
arguments, one argument, or one mandatory and one optional
argument.

If COMPLETION-FUNCTION takes zero arguments, it should return a
list of completion candidates for whatever is at point. In this
case, you will almost certainly need to supply a :prefix-function
or :word-thing (see below).

If COMPLETION-FUNCTION takes one argument, it is passed the
prefix to complete, and should return a list of completion
candidates for that prefix.

If COMPLETION-FUNCTION takes two arguments, the second one must
be an optional argument. It is passed the prefix to complete in
the first argument. The second argument may be used to specify
the maximum number of completion candidates to return. If it is
nil, all possible candidates should be returned.

The optional :name keyword argument can be used to give a
name (symbol) to the source, which will appear in customization
options. The default is to construct the source name from
COMPLETION-FUNCTION, removing \"completion-ui-\" or
\"completion-\" from the front.

The optional :completion-args and :other-args keyword arguments
are useful when using a COMPLETION-FUNCTION that takes additional
arguments that should be ignored by Completion-UI.

If :completion-args is 0, 1 or 2, COMPLETION-FUNCTION will be
passed that many arguments, as described above, and as many null
values will be added to the end of the argument list as necessary
to fill any remaining mandatory arguments (but see :other-args,
below).

If :completion-args is a list of integers, it specifies *which*
arguments of COMPLETION-FUNCTION to use. The argument list passed
will be padded with null values as necessary to fill the missing
arguments and any remaining mandatory arguments (but
see :other-args, below).

The optional :other-args keyword argument can be used to pass
something other than nil to the other arguments of
COMPLETION-FUNCTION not being used by Completion-UI. The values
are taken sequentially from the :other-args list.

Normally, the new completion source is made available as a
possible `auto-completion-source' choice, and a new interactive
command called `complete-<name>' is automatically defined that
completes the prefix at point using the new source. The
optional :no-auto-completion and :no-command keyword arguments
disable these features. The optional :command-name keyword
argument overrides the default command name.

The optional :prefix-function keyword argument specifies a
function to call to return the prefix to complete at point. It
defaults to `completion-prefix', which uses `thing-at-point' to
find the prefix. In this case, the \"thing\" to find is specified
by :word-thing, defaulting to `word'. (Note that we require
`forward-op' to be defined for :word-thing, which is *not* the
case for all pre-defined \"things\" in `thing-at-point'.)

The optional :non-prefix-completion keyword argument is a boolean
that should be non-null if this completion source does something
other than prefix completion \(e.g. searching for regular
expression matches\), so that the \"prefix\" is instead
interpreted as some kind of pattern used to find matches, and the
completions should *replace* that prefix when
selected. Completion-UI will then adapt the user-interface
appropriately.

The optional :accept-function and :reject-function keyword
arguments act as hook functions, called when a completion
obtained using this source is accepted or rejected,
respectively. They are passed three arguments: the prefix, the
completion candidate that was accepted or rejected, and any
prefix argument supplied by the user if the accept or reject
command was called interactively.

The remaining optional keyword arguments override the default
functions for constructing the completion tooltip, pop-up frame,
menu, and browser menu. They are passed one argument, a
completion overlay. The tooltip function should return the text
to display in the tooltip as a string. The pop-up frame function
should return a list of strings, each a line of text for the
pop-up frame. The menu functions should return menu keymaps."

  ;; remove `quote' from arguments
  (when (and (listp completion-function)
	     (eq (car completion-function) 'quote))
    (setq completion-function (cadr completion-function)))
  (when (and (listp name)
	     (eq (car name) 'quote))
    (setq name (cadr name)))
  (when (and (listp completion-args)
	     (eq (car completion-args) 'quote))
    (setq completion-args (cadr completion-args)))
  (when (and (listp other-args)
	     (eq (car other-args) 'quote))
    (setq other-args (cadr other-args)))
  (when (and (listp prefix-function)
	     (eq (car prefix-function) 'quote))
    (setq prefix-function (cadr prefix-function)))
  (when (and (listp word-thing)
	     (eq (car word-thing) 'quote))
    (setq word-thing (cadr word-thing)))
  (when (and (listp accept-function)
	     (eq (car accept-function) 'quote))
    (setq accept-function (cadr accept-function)))
  (when (and (listp reject-function)
	     (eq (car reject-function) 'quote))
    (setq reject-function (cadr reject-function)))
  (when (and (listp tooltip-function)
	     (eq (car tooltip-function) 'quote))
    (setq tooltip-function (cadr tooltip-function)))
  (when (and (listp popup-frame-function)
	     (eq (car popup-frame-function) 'quote))
    (setq popup-frame-function (cadr popup-frame-function)))
  (when (and (listp menu-function)
	     (eq (car menu-function) 'quote))
    (setq menu-function (cadr menu-function)))
  (when (and (listp browser-function)
	     (eq (car browser-function) 'quote))
    (setq browser-function (cadr browser-function)))

  ;; construct source name from completion-function if not defined explicitly
  (unless name
    (setq name (symbol-name completion-function))
    (if (string-match "^completion-ui-+" name)
	(setq name (substring name (match-end 0)))
      (when (string-match "^completion-+" name)
	(setq name (substring name (match-end 0)))))
    (setq name (intern name)))
  ;; construct command name from source name if not defined explicitly
  (unless command-name
    (setq command-name (intern (concat "complete-" (symbol-name name)))))

  ;; sort out COMPLETION-ARGS
  (cond
   ;; no COMPLETION-ARGS: infer arguments from function definition
   ((null completion-args)
    (let* ((funcdef (cond
		     ((symbolp completion-function)
		      (symbol-function completion-function))
		     ((functionp completion-function)
		      completion-function)))
 	   (arglist (cond
 		     ;; compiled function
 		     ((byte-code-function-p funcdef)
 		      (aref funcdef 0))
 		     ;; uncompiled function
 		     ((and (listp funcdef) (eq (car funcdef) 'lambda))
 		      (nth 1 funcdef))
 		     (t (error "completion-ui-register-source:\
 missing :completion-args or invalid completion function, %s"
 			       completion-function)))))
      (cond
       ((= (length arglist) 0) (setq completion-args '()))
       ((= (length arglist) 1) (setq completion-args '(0)))
       ((and (= (length arglist) 3) (eq (nth 1 arglist) '&optional))
 	(setq completion-args '(0 1)))
       (t (error "completion-ui-register-source: missing :completion-args\
 or invalid completion function, %s" completion-function)))))
   ;; numerical COMPLETION-ARGS: convert to list
   ((numberp completion-args)
    (cond
     ((= completion-args 0) (setq completion-args '()))
     ((= completion-args 1) (setq completion-args '(0)))
     ((= completion-args 2) (setq completion-args '(0 1)))))
   ;; COMPLETION-ARGS list: sanity check
   ((and (listp completion-args)
 	 (>= (length completion-args) 0)
 	 (<= (length completion-args) 2)))
   ;; anything else: we're too confused to carry on!
   (t (error "completion-ui-register-source: invalid :completion-args, %s"
 	     completion-args)))


  ;; construct argument list for COMPLETION-FUNCTION
  (let ((argnames '(prefix maxnum))
  	(cmplstack completion-args)
  	(otherstack other-args)
  	(arg 0) arglist)
    (while cmplstack
      (dotimes (i (- (car cmplstack) arg 1))
  	(push (when otherstack (pop otherstack)) arglist))
      (push (pop argnames) arglist)
      (setq arg (pop cmplstack)))
    (while otherstack (push (pop otherstack) arglist))
    (setq arglist (nreverse arglist))

    ;; construct wrapper around COMPLETION-FUNCTION if necessary
    (cond
     ;; no maxnum argument
     ((or (= (length completion-args) 0)
     	  (= (length completion-args) 1))
      (setq completion-function
     	    `(lambda (prefix &optional maxnum)
     	       (let ((completions (,completion-function ,@arglist)))
     		 (if maxnum
     		     (butlast completions (- (length completions) maxnum))
     		   completions)))))
     ;; maxnum argument with other args required
     ((and (= (length completion-args) 2)
     	   (or other-args (not (= (nth 1 completion-args) 1))))
      (setq completion-function
      	    `(lambda (prefix &optional maxnum)
      	       (let ((completions (,completion-function ,@arglist)))
      		 (if maxnum
      		     (butlast completions (- (length completions) maxnum))
      		   completions)))))
     ;; maxnum argument with no other args required - leave
     ;; COMPLETION-FUNCTION as is
     ))


  ;; construct interface definiton
  (let ((source-def
	 (cons name
	       (append
		(list completion-function)
		(when non-prefix-completion
		  (list :non-prefix-completion non-prefix-completion))
		(when prefix-function
		  (list :prefix-function prefix-function))
		(when word-thing
		  (list :word-thing word-thing))
		(when accept-function
		  (list :accept accept-function))
		(when reject-function
		  (list :reject reject-function))
		(when tooltip-function
		  (list :tooltip tooltip-function))
		(when popup-frame-function
		  (list :popup-frame popup-frame-function))
		(when menu-function
		  (list :menu menu-function))
		(when browser-function
		  (list :browser browser-function))))))


    ;; construct code to add source definition to list (or replace existing
    ;; definition)
    `(let ((existing (assq ',name completion-ui-source-definitions)))
       (if (not existing)
	   (push ',source-def completion-ui-source-definitions)
	 (message "Completion-UI source `%s' already registered\
 - replacing existing definition" ',name)
	 (setcdr existing ',(cdr source-def)))

       ;; construct code to define completion command
       ,(unless no-command
	  `(defun ,command-name
	     (&optional n)
	     ,(concat "Complete or cycle word at point using "
		      (symbol-name name) " source.")
	     (interactive "p")
	     (complete-or-cycle-word-at-point ',name n)))

       ;; update `auto-completion-source' defcustom
       ,(unless no-auto-completion
	  `(defcustom auto-completion-source nil
	     "*Completion source for `auto-completion-mode'."
	     :group 'completion-ui
	     :type
	     '(choice
	       (const nil)
	       ,@(nreverse
		  (mapcar
		   (lambda (def)
		     (list 'const (completion-ui--source-def-name def)))
		   (append
		    (assq-delete-all
		     name (copy-sequence completion-ui-source-definitions))
		    (list source-def))))))))
    ))




(defmacro completion-ui-completion-source (source &optional overlay)
  ;; return SOURCE or completion-source for OVERLAY at point
  `(or ,(when overlay
  	  `(and ,overlay (overlay-get ,overlay 'completion-source)))
       ,(when source
  	  `(and (fboundp 'auto-overlay-local-binding)
  		(let ((completion-source ,source))
  		  (auto-overlay-local-binding 'completion-source))))
       ,source))


(defmacro completion-ui-source-completion-function
  (source &optional overlay)
  ;; return completion-function for SOURCE or OVERLAY at point
  `(completion-ui--source-def-completion-function
    (assq (completion-ui-completion-source ,source ,overlay)
  	  completion-ui-source-definitions)))


(defun completion-ui-source-prefix-function (source &optional overlay)
  ;; return prefix-function for SOURCE or OVERLAY at point
  (or (and overlay (overlay-get overlay 'completion-prefix-function))
      (and source
	   (let (prefix-function)
	     ;; get overlay-local binding, falling back to SOURCE definition
	     (if (fboundp 'auto-overlay-local-binding)
		 (let ((completion-prefix-function
			(completion-ui--source-def-prefix-function
			 (assq (completion-ui-completion-source source)
			       completion-ui-source-definitions))))
		   (setq prefix-function
			 (auto-overlay-local-binding
			  'completion-prefix-function)))
	       (setq prefix-function
		     (completion-ui--source-def-prefix-function
		      (assq (completion-ui-completion-source source)
			    completion-ui-source-definitions))))
	     ;; evaluate result until we get a function
	     (while (and prefix-function
			 (not (functionp prefix-function))
			 (boundp prefix-function))
	       (setq prefix-function (eval prefix-function)))
	     prefix-function))
      ;; default fall-back
      'completion-prefix))


(defun completion-ui-source-word-thing
  (source &optional overlay)
  ;; return word-thing for SOURCE or OVERLAY at point
  (or (and overlay (overlay-get overlay 'completion-word-thing))
      (and source
	   (let (word-thing)
	     ;; get overlay-local binding, falling back to SOURCE definition
	     (if (fboundp 'auto-overlay-local-binding)
		 (let ((completion-word-thing
			(completion-ui--source-def-word-thing
			 (assq (completion-ui-completion-source source)
			       completion-ui-source-definitions))))
		   (setq word-thing (auto-overlay-local-binding
				     'completion-word-thing)))
	       (setq word-thing
		     (completion-ui--source-def-word-thing
		      (assq (completion-ui-completion-source source)
			    completion-ui-source-definitions))))
	     ;; evaluate result until we get a thing-at-point symbol
	     (while (and word-thing
			 (not (or (get word-thing 'forward-op)
				  (fboundp
				   (intern-soft
				    (format "forward-%s" word-thing)))))
			 (boundp word-thing))
	       (setq word-thing (eval word-thing)))
	     word-thing))
      ;; default fall-back
      'word))


(defun completion-ui-source-non-prefix-completion
  (source &optional overlay)
  ;; return non-prefix-completion setting for SOURCE or OVERLAY at point
  (or (and overlay (overlay-get overlay 'completion-non-prefix-completion))
      (and source
	   (or (and (fboundp 'auto-overlay-local-binding)
		    (let ((completion-non-prefix-completion
			   (completion-ui--source-def-non-prefix-completion
			    (assq (completion-ui-completion-source source)
				  completion-ui-source-definitions))))
		      (auto-overlay-local-binding
		       'completion-non-prefix-completion)))
	       (completion-ui--source-def-non-prefix-completion
		(assq (completion-ui-completion-source source)
		      completion-ui-source-definitions))))
      nil))  ; default fall-back


(defmacro completion-ui-source-accept-function
  (source &optional overlay)
  ;; return accept-function for SOURCE or OVERLAY
  `(completion-ui--source-def-accept-function
    (assq (completion-ui-completion-source ,source ,overlay)
  	  completion-ui-source-definitions)))


(defmacro completion-ui-source-reject-function
  (source &optional overlay)
  ;; return reject-function for SOURCE or OVERLAY
  `(completion-ui--source-def-reject-function
    (assq (completion-ui-completion-source ,source ,overlay)
  	  completion-ui-source-definitions)))


(defun completion-ui-source-popup-frame-function
  (source &optional overlay)
  ;; return popup-frame-function for SOURCE of OVERLAY at point
  (or (let (popup-frame-function)
	;; get overlay-local binding, falling back to SOURCE definition
	(if (fboundp 'auto-overlay-local-binding)
	    (let ((completion-popup-frame-function
  		   (completion-ui--source-def-popup-frame-function
  		    (assq (completion-ui-completion-source source overlay)
  			  completion-ui-source-definitions))))
  	      (setq popup-frame-function
		    (auto-overlay-local-binding
		     'completion-popup-frame-function)))
	  (setq popup-frame-function
		(completion-ui--source-def-popup-frame-function
		 (assq (completion-ui-completion-source source overlay)
		       completion-ui-source-definitions))))
	;; evaluate result until we get a function
	(while (and popup-frame-function
		    (not (functionp popup-frame-function))
		    (boundp popup-frame-function))
	  (setq popup-frame-function (eval popup-frame-function)))
	popup-frame-function)
      ;; default fall-back
      'completion-construct-popup-frame-text))


(defun completion-ui-source-tooltip-function
  (source &optional overlay)
  ;; return tooltip-function for SOURCE of OVERLAY at point
  (or (let (tooltip-function)
	;; get overlay-local binding, falling back to SOURCE definition
	(if (fboundp 'auto-overlay-local-binding)
	    (let ((completion-tooltip-function
		   (completion-ui--source-def-tooltip-function
		    (assq (completion-ui-completion-source source overlay)
			  completion-ui-source-definitions))))
	      (setq tooltip-function
		    (auto-overlay-local-binding
		     'completion-tooltip-function)))
	  (setq tooltip-function
		(completion-ui--source-def-tooltip-function
		 (assq (completion-ui-completion-source source overlay)
		       completion-ui-source-definitions))))
	;; evaluate result until we get a function
	(while (and tooltip-function
		    (not (functionp tooltip-function))
		    (boundp tooltip-function))
	  (setq tooltip-function (eval tooltip-function)))
	tooltip-function)
      ;; default fall-back
      'completion-construct-tooltip-text))


(defun completion-ui-source-menu-function
  (source &optional overlay)
  ;; return popup-frame-function for SOURCE of OVERLAY at point
  (or (let (menu-function)
	;; get overlay-local binding, falling back to SOURCE definition
	(if (fboundp 'auto-overlay-local-binding)
  	    (let ((completion-menu-function
  		   (completion-ui--source-def-menu-function
  		    (assq (completion-ui-completion-source source overlay)
  			  completion-ui-source-definitions))))
  	      (setq menu-function
		    (auto-overlay-local-binding 'completion-menu-function)))
	  (setq menu-function
		(completion-ui--source-def-menu-function
		 (assq (completion-ui-completion-source source overlay)
		       completion-ui-source-definitions))))
	;; evaluate result until we get a function
	(while (and menu-function
		    (not (functionp menu-function))
		    (boundp menu-function))
	  (setq menu-function (eval menu-function)))
	menu-function)
      ;; default fall-back
      'completion-construct-menu))


(defun completion-ui-source-browser-function
  (source &optional overlay)
  ;; return popup-frame-function for SOURCE of OVERLAY at point
  (or (let (browser-function)
	;; get overlay-local binding, falling back to SOURCE definition
	(if (fboundp 'auto-overlay-local-binding)
  	    (let ((completion-browser-function
  		   (completion-ui--source-def-browser-function
  		    (assq (completion-ui-completion-source source overlay)
  			  completion-ui-source-definitions))))
  	      (setq browser-function
		    (auto-overlay-local-binding
		     'completion-browser-function)))
	  (setq browser-function
		(completion-ui--source-def-browser-function
		 (assq (completion-ui-completion-source source overlay)
		       completion-ui-source-definitions))))
	;; evaluate result until we get a function
	(while (and browser-function
		    (not (functionp browser-function))
		    (boundp browser-function))
	  (setq browser-function (eval browser-function)))
	browser-function)
      ;; default fall-back
      'completion-construct-browser-menu))



(defmacro completion-ui-source-run-accept-function
  (overlay prefix completion &optional arg)
  ;; run accept function for completion OVERLAY, passing the rejected
  ;; COMPLETION of PREFIX and any user-supplied ARG
  `(let ((func (completion-ui-source-accept-function
  		(overlay-get ,overlay 'completion-source)
  		overlay)))
     (when (functionp func) (funcall func ,prefix ,completion ,arg))))


(defmacro completion-ui-source-run-reject-function
  (overlay prefix completion &optional arg)
  ;; run reject function for completion OVERLAY, passing the rejected
  ;; COMPLETION of PREFIX and any user-supplied ARG
  `(let ((func (completion-ui-source-reject-function
  		(overlay-get ,overlay 'completion-source)
  		overlay)))
     (when (functionp func) (funcall func ,prefix ,completion ,arg))))




;;; =======================================================
;;;             The core completion functions

(defun* complete-in-buffer
    (completion-source
     &optional prefix-function (non-prefix-completion nil s-npcmpl)
     auto update pos)
  "Complete in-buffer using COMPLETION-SOURCE.

COMPLETION-SOURCE must either be a function, or the name of a
registered Completion-UI source (a symbol; see
`completion-ui-register-source'). If a function, it must take one
mandatory and one optional argument, the first being the prefix
to complete, the second specifying the maximum number of
completion candidates to return. Note that a completion source
whose name is the same as that of a function will take precedence
over that function.

If no other arguments are supplied, the remaining settings are
determined automatically from the Completion-UI source definition
corresponding to COMPLETION-SOURCE (see
`completion-ui-register-source'), falling back to default values
if the COMPLETION-SOURCE isn't a registered Completion-UI
source. The remaining optional arguments can be used to override
these defaults.

PREFIX-FUNCTION can either be a function that takes zero
arguments (called to find the prefix at the point), a
`thing-at-point' symbol (used to find the prefix at point), a
string (complete that string), or nil (complete the word at or
next to the point). Note that in the case of a `thing-at-point'
symbol, we require `forward-op' to be defined for the \"thing\",
which is *not* the case for all pre-defined \"things\" in
`thing-at-point'.

If NON-PREFIX-COMPLETION is non-null, it indicates that
COMPLETION-SOURCE does something other than prefix completion,
and instead treats the PREFIX as a pattern to search for, which
should be replaced by the completion.

The remaining arguments are for internal use only."
  ;; If AUTO is non-nil, assume we're auto-completing and respect settings of
  ;; `auto-completion-min-chars' and (unless AUTO is 'timer)
  ;; `auto-completion-delay'. If UPDATE is an overlay, update the
  ;; user-interfaces for that overlay rather than activating them from
  ;; scratch. If POS is non-nil, only complete if point is at POS. To specify
  ;; any of these without setting NON-PRFIX-COMPLETION, use any value for the
  ;; latter that is neither null nor t.

  ;; cancel any timer so that we don't have two running at once
  (cancel-timer completion--auto-timer)

  ;; only complete if point is at POS (only used when called from timer)
  (unless (and pos (/= (point) pos))

    ;; if we're auto-completing and `auto-completion-delay' is set,
    ;; delay completing by setting a timer to call ourselves later
    (if (and auto auto-completion-delay (not (eq auto 'timer)))
        (setq completion--auto-timer
              (run-with-idle-timer
	       auto-completion-delay nil
	       'complete-in-buffer
	       completion-source prefix-function
	       (if s-npcmpl non-prefix-completion 'not-set)
	       'timer update (point)))


      ;; otherwise...
      (let (completion-function word-thing prefix completions)
	;; resolve any provisional completions
	(completion-ui-resolve-old update)


	;; --- get completion properties ---

	;; if updating a completion overlay, use it's properties
	(if update
	    (setq completion-function
		    (completion-ui-source-completion-function nil update)
		  prefix
		    (overlay-get update 'prefix)
		  non-prefix-completion
		    (overlay-get update 'non-prefix-completion))

	  ;; otherwise, sort out arguments...
	  ;; get completion-function
	  (setq completion-function
		(completion-ui-source-completion-function completion-source))
	  ;; literal PREFIX-FUNCTION takes precedence
	  (if (stringp prefix-function)
	      (setq prefix prefix-function)
	    ;; sort out PREFIX-FUNCTION and word-thing
	    (cond
	     ((functionp prefix-function))  ; PREFIX-FUNCTION is function
	     ((symbolp prefix-function)     ; PREFIX-FUNCTION is word-thing
	      (setq word-thing prefix-function
		    prefix-function nil)))
	    ;; get prefix function unless already specified in arguments
	    (unless prefix-function
	      (setq prefix-function
		    (completion-ui-source-prefix-function completion-source)))
	    ;; get prefix word-thing unless specified in arguments
	    (unless word-thing
	      (setq word-thing
		    (completion-ui-source-word-thing completion-source)))
	    ;; --- get prefix ---
	    (let ((completion-word-thing word-thing))
	      (setq prefix (funcall prefix-function))))

	  ;; get non-prefix-completion unless specified in arguments
	  (unless (and s-npcmpl
		       (or (null non-prefix-completion)
			   (eq non-prefix-completion t)))
	    (setq non-prefix-completion
		  (completion-ui-source-non-prefix-completion
		   completion-source))))


	;; if auto-completing, only do so if prefix if it has requisite number
        ;; of characters
        (if (and auto
		 auto-completion-min-chars
		 (< (length prefix) auto-completion-min-chars))
	    (if update (completion-ui-deactivate-interfaces update))


          ;; --- get completions ---
          (setq completions
		(funcall completion-function
			 prefix completion-max-candidates))

          (let ((overlay
		 (completion-ui-setup-overlay
		  prefix 'unchanged completions nil
		  completion-source prefix-function non-prefix-completion
		  update)))
	    (move-overlay overlay (point) (point))

	    ;; --- activate completion user-interfaces ---
	    (if update
		(completion-ui-update-interfaces overlay)
	      (completion-ui-activate-interfaces overlay))
	    (when completion-auto-show (completion-ui-auto-show overlay)))

	  ;; set `completion-ui--activated' (buffer-locally) to enable
	  ;; `completion-map' work-around hacks
	  (setq completion-ui--activated t))
	))))



(defun completion-ui-auto-show (&optional overlay point)
  "Display list of completions for OVERLAY in auto-show interface.
The point had better be within OVERLAY or your hair will fall
out.

Which one is shown depends on the setting of
`completion-auto-show'. If `completion-auto-show-delay' is
non-nil, the auto-show interface will only be displayed after a
delay.

If OVERLAY is not supplied, tries to find one at point.

If POINT is supplied, the auto-show interface will be displayed
immediately, but only if point is at POINT (used internally when
called from timer)."
  (interactive)

  ;; if no overlay supplied, try to find one at point
  (unless overlay (setq overlay (completion-ui-overlay-at-point)))
  ;; cancel any running timer so we don't end up being called twice
  (cancel-timer completion--auto-timer)

  ;; make sure things are still in a sensible state (might not be if
  ;; displaying after a delay)
  (when (and completion-auto-show
	     overlay
	     (overlay-buffer overlay)
             (or (null point) (= (point) point)))
    (cond

     ;; ;; if auto-show interface is already active for overlay, update it
     ;; ((overlay-get overlay 'auto-show)
     ;;  (completion-ui-call-auto-show-interface-helpers overlay)
     ;;  (funcall (overlay-get overlay 'auto-show) overlay))

     ;; if delaying, setup timer to call ourselves later
     ((and completion-auto-show-delay (null point))
      (setq completion--auto-timer
	    (run-with-timer completion-auto-show-delay nil
			    'completion-ui-auto-show
			    overlay (point))))

     ;; otherwise, display whatever we're displaying
     (t
      (overlay-put overlay 'auto-show completion-auto-show)
      (completion-ui-call-auto-show-interface-helpers overlay)
      (funcall completion-auto-show overlay)))))



(defmacro completion-ui-cancel-auto-show ()
  "Cancel any pending auto-show."
  `(when (timerp completion--auto-timer)
     (cancel-timer completion--auto-timer)))



;;; ============================================================
;;;                  Interactive commands

(defun* complete-word-at-point
  (&optional completion-source prefix-function
	     (non-prefix-completion nil s-npcmpl))
  "Complete the word at or next to point.

COMPLETION-SOURCE, PREFIX-FUNCTION, and NON-PREFIX-COMPLETION are
passed to `complete-in-buffer' (which see)."
  (interactive)

  ;; get completion overlay at point
  (let* ((overlay (completion-ui-overlay-at-point))
	 (word-thing
	  (or (and overlay (overlay-get overlay 'prefix-word-thing))
	      (and (fboundp 'auto-overlay-local-binding)
		   (let ((completion-word-thing
			  (and (symbolp prefix-function)
			       (not (functionp prefix-function))
			       prefix-function)))
		     (auto-overlay-local-binding 'completion-word-thing)))
	      (and (symbolp prefix-function) (not (functionp prefix-function))
		   prefix-function)
	      'word)))

    ;; if point is at start of an existing overlay, delete old completion
    ;; before completing, preserving overlay so its prefix can be reused
    (if (and overlay (= (point) (overlay-start overlay)))
        (delete-region (overlay-start overlay) (overlay-end overlay))

      ;; if there's a completion at point but point is not at start,
      ;; delete overlay (effectively accepting old completion) and behave
      ;; as if no completion was in progress
      (when overlay
	(completion-ui-delete-overlay overlay)
	(setq overlay nil))

      ;; if point is in middle of a word and `completion-overwrite' is
      ;; enabled, delete rest of word before completing
      (when (and completion-overwrite (completion-within-word-p word-thing))
	(completion-overwrite-word-at-point word-thing)
        ;; if there is now a completion overlay at point, delete it
        (when (setq overlay (completion-ui-overlay-at-point))
          (completion-ui-delete-overlay overlay)
	  (setq overlay nil))))

    ;; do completion
    (complete-in-buffer completion-source prefix-function
			(if s-npcmpl non-prefix-completion 'not-set)
			nil overlay)))



(defun* complete-or-cycle-word-at-point
  (completion-source &optional n prefix-function
		     (non-prefix-completion nil s-npcmpl))
  "Cycle through available completions if there are any,
otherwise complete the word at point.

When completing, COMPLETION-SOURCE, PREFIX-FUNCTION, and
NON-PREFIX-COMPLETION are passed to `complete-in-buffer' (which
see)."
  (interactive "p")
  (if (completion-ui-overlay-at-point)
      (completion-cycle n)
    (complete-word-at-point
     completion-source prefix-function
     (if s-npcmpl non-prefix-completion 'not-set))))


(defun* complete-or-cycle-backwards-word-at-point
  (completion-source &optional n prefix-function
		     (non-prefix-completion nil s-npcmpl))
  "Cycle backwards through available completions if there are any,
otherwise complete the word at point.

When completing, COMPLETION-SOURCE, PREFIX-FUNCTION, and
NON-PREFIX-COMPLETION are passed to `complete-in-buffer' (which
see)."
  (interactive "p")
  (if (completion-ui-overlay-at-point)
      (completion-cycle (- n))
    (complete-word-at-point
     completion-source prefix-function
     (if s-npcmpl non-prefix-completion 'not-set))))



(defun completion-accept (&optional arg overlay)
  "Accept current provisional completion.

The value of ARG is passed as the third argument to any functions
called from the `completion-accept-functions' hook. Interactively,
ARG is the prefix argument.

If optional argument OVERLAY is supplied, it is used instead of
looking for an overlay at the point. The point had better be
within OVERLAY or else your hair will fall out.

If a completion was accepted, returns a cons cell containing the
prefix and the entire accepted completion \(the concatenation of
the prefix and the completion string\). Otherwise returns nil."
  (interactive "P")

  ;; if we haven't been passed one, get completion overlay at point
  (unless overlay (setq overlay (completion-ui-overlay-at-point)))

  ;; ;; resolve any other old provisional completions
  ;; (completion-ui-resolve-old overlay)

  ;; if we ain't found an overlay, can't accept nuffink!
  (when overlay
    (let ((prefix (overlay-get overlay 'prefix))
	  cmpl)
      ;; if there's no selected completion, then we're effectively accepting
      ;; the prefix as a completion
      (if (null (overlay-get overlay 'completion-num))
	  (progn
	    ;; delete the overlay, effectively accepting the prefix
	    (completion-ui-delete-overlay overlay)
	    (completion-ui-deactivate-interfaces overlay)
	    ;; run accept hooks
	    (completion-ui-source-run-accept-function
	     overlay prefix prefix arg)
	    (run-hook-with-args
	     'completion-accept-functions prefix prefix arg)
	    ;; return prefix and accepted completion (the prefix itself)
	    (cons prefix prefix))

	;; otherwise, deactivate the interfaces and accept the completion
	;; deactivate the interfaces
	(setq cmpl (nth (overlay-get overlay 'completion-num)
			(overlay-get overlay 'completions)))
	(unless (stringp cmpl) (setq cmpl (car cmpl)))
	(completion-ui-deactivate-interfaces overlay)
	;; delete the original prefix and insert the completion
	(delete-region (- (point) (length prefix)) (point))
	(let ((overwrite-mode nil)) (insert cmpl))
	;; run accept hooks
	(completion-ui-source-run-accept-function overlay prefix cmpl arg)
	(run-hook-with-args 'completion-accept-functions prefix cmpl arg)
	;; delete overlay
	(completion-ui-delete-overlay overlay)
	;; return prefix and accepted completion
	(cons prefix cmpl)))))



(defun completion-reject (&optional arg overlay)
  "Reject current provisional completion.

The value of ARG is passed as the third argument to any functions
called from the `completion-reject-functions' hook. Interactively,
ARG is the prefix argument.

If optional argument OVERLAY is supplied, it is used instead of
looking for an overlay at the point. The point had better be
within OVERLAY or else your hair will fall out.

If a completion was rejected, returns a cons cell containing the
prefix and the entire rejected completion \(the concatenation of
the prefix and the completion string\). Otherwise returns nil."
  (interactive "P")

  ;; if we haven't been passed one, get completion overlay at point
  (unless overlay (setq overlay (completion-ui-overlay-at-point)))

  ;; ;; resolve any other old provisional completions
  ;; (completion-ui-resolve-old overlay)

  ;; if we ain't found no overlay, we can't reject nuffink!
  (unless (or (null overlay)
	      (and (null (overlay-get overlay 'completion-num))
		   (progn (completion-ui-delete-overlay overlay) t)))
    ;; otherwise, deactivate the interfaces and reject the completion
    (let ((prefix (overlay-get overlay 'prefix))
	  (cmpl (nth (overlay-get overlay 'completion-num)
		     (overlay-get overlay 'completions))))
      ;; deactivate the interfaces
      (completion-ui-deactivate-interfaces overlay)
      ;; run reject hooks
      (completion-ui-source-run-reject-function overlay prefix cmpl arg)
      (run-hook-with-args 'completion-reject-functions prefix cmpl arg)
      ;; delete overlay
      (completion-ui-delete-overlay overlay)
      ;; return prefix and rejected completion
      (cons prefix cmpl))))



(defun completion-resolve-then-run-command ()
 "Resolve current completion, then run command
that would normally be bound to this key."
 (interactive)
 (completion--run-if-within-overlay
  (lambda () (interactive) (completion--resolve-current nil nil ? ))
  'completion-ui--activated 'before))



(defun completion-select (n &optional overlay)
  "Select N'th completion candidate for current completion.

Interactively, N is the prefix argument.

If OVERLAY is supplied, use that instead of finding one at
point. The point had better be within OVERLAY or a meteorite will
crash through your ceiling."
  (interactive "P")

  ;; resolve any other old provisional completions
  (completion-ui-resolve-old overlay)
  ;; if we haven't been passed one, get completion overlay at point
  (unless overlay (setq overlay (completion-ui-overlay-at-point)))

  ;; if we ain't found an overlay, can't select nuffink!
  ;; if we ain't found an overlay, can't accept nuffink!
  (if (null overlay)
      (message "No completion to select")

    (let ((completions (overlay-get overlay 'completions))
	  (non-prefix-completion
	   (overlay-get overlay 'non-prefix-completion))
	  prefix cmpl len)

      ;; if there are too few completions, display message
      (if (>= n (length completions))
	  (progn
	    (beep)
	    (if (= (length completions) 1)
		(message "Only 1 completion available")
	      (message "Only %d completions available" (length completions))))

	;; otherwise, deactivate interfaces and select completion
	(setq prefix (overlay-get overlay 'prefix)
	      cmpl (nth n (overlay-get overlay 'completions))
	      len (length (overlay-get overlay 'prefix)))
	(unless (stringp cmpl) (setq cmpl (car cmpl)))
	;; deactivate the interfaces
	(completion-ui-deactivate-interfaces overlay)
	;; delete the original prefix and insert the completion
	(delete-region (- (point) len) (point))
	(let ((overwrite-mode nil)) (insert cmpl))
	;; run accept hooks
	(completion-ui-source-run-accept-function overlay prefix cmpl)
	(run-hook-with-args 'completion-accept-functions prefix cmpl)
	;; delete overlay
	(completion-ui-delete-overlay overlay)
	;; return prefix and accepted completion
	(cons prefix cmpl)))))



(defun completion-extend-prefix (&optional n overlay)
  "Extend the current prefix by N characters from the current completion,
and recomplete the resulting string.

If N is null, extend the prefix to the entire current
completion. If N is negative, remove that many characters from
the current prefix, and recomplete the resulting string.

When called from Lisp programs, OVERLAY is used if supplied
instead of finding one. The point had better be within OVERLAY or
the oceans will boil away."
  (interactive "P")

  ;; look for completion overlay at point if none was specified
  (unless overlay (setq overlay (completion-ui-overlay-at-point)))

  ;; if there are no characters to accept, do nothing
  (if (or (null overlay) (null (overlay-get overlay 'completion-num)))
      (message "No characters with which to extend prefix")

    ;; otherwise
    (let ((prefix (overlay-get overlay 'prefix))
	  (len (overlay-get overlay 'prefix-length))
	  (cmpl (nth (overlay-get overlay 'completion-num)
		     (overlay-get overlay 'completions)))
	  str)
      (unless (stringp cmpl) (setq cmpl (car cmpl)))
      (setq str (if (and n (< (+ len n) (length cmpl)) (> (+ len n) 0))
		    (substring cmpl 0 (+ len n))
		  cmpl))
      ;; deactivate interfaces pending update
      (completion-ui-deactivate-interfaces-pre-update overlay)
      ;; delete the original prefix
      (delete-region (- (point) (length prefix)) (point))

      ;; if prefix has been contracted down to nothing, stop completing
      (if (and n (<= (+ len n) 0))
	  (progn
	    (completion-ui-delete-overlay overlay)
	    (completion-ui-deactivate-interfaces overlay))
	;; otherwise, insert the characters, update the overlay, and
	;; recomplete
	(let ((overwrite-mode nil)) (insert str))
	(move-overlay overlay (point) (point))
	(completion-ui-setup-overlay str nil nil nil nil nil nil overlay)
	(complete-in-buffer nil nil 'not-set nil overlay)
	;; reposition point at start of completion, so that user can continue
	;; extending or contracting the prefix
	(goto-char (overlay-start overlay))))))



(defun completion-cycle (&optional n overlay)
  "Cycle through available completions.

Optional argument N specifies the number of completions to cycle
forwards \(backwards if negative\). Default is 1. Interactively,
N is the prefix argument.

If OVERLAY is supplied, use that instead of finding one. The
point had better be within OVERLAY or you'll be struck by
lightening."
  (interactive "p")
  (unless (numberp n) (setq n 1))

  ;; if we haven't been passed one, get completion overlay at point
  (unless overlay (setq overlay (completion-ui-overlay-at-point)))

  ;; if there are no completions, can't cycle
  (if (or (null overlay) (null (overlay-get overlay 'completions)))
      (message "No completions to cycle")

    ;; otherwise, cycle away!
    (let* ((completions (overlay-get overlay 'completions))
	   (prefix (overlay-get overlay 'prefix))
	   (non-prefix-completion
	    (overlay-get overlay 'non-prefix-completion))
	   (i (mod (+ (or (overlay-get overlay 'completion-num) -1) n)
		   (length completions)))
	   (cmpl (nth i completions))
	   (len (if (stringp cmpl)
		    (length prefix)
		  (prog1 (cdr cmpl) (setq cmpl (car cmpl))))))
      ;; run pre-cycle interface functions
      (completion-ui-deactivate-interfaces-pre-update overlay)
      ;; update overlay properties
      (overlay-put overlay 'prefix-length len)
      (overlay-put overlay 'completion-num i)
      ;; run post-cycle interface functions
      (completion-ui-update-interfaces overlay))))



(defun completion-cycle-backwards (&optional n)
  "Cycle backwards through available completions.

Optional argument N specifies the number of completions to cycle
backwards \(forwards if negative\). Default is 1. Interactively,
N is the prefix argument."
  (interactive "p")
  (completion-cycle (- n)))



(defun completion-tab-complete (&optional overlay)
  "Tab-complete completion at point
\(i.e. insert longest common prefix of all the completions\).

If OVERLAY is supplied, use that instead of finding one. The
point had better be within OVERLAY or your teeth will turn bright
green over night."
  (interactive)

  ;; look for completion overlay at point if none was specified
  (unless overlay (setq overlay (completion-ui-overlay-at-point)))

  ;; if within a completion overlay
  (when overlay
    (let* ((prefix (overlay-get overlay 'prefix))
	   (completions (overlay-get overlay 'completions))
	   (str (try-completion
		 "" (mapcar
		     (lambda (cmpl)
		       (if (stringp cmpl)
			   (substring cmpl (length prefix))
			 (substring (car cmpl) (cdr cmpl))))
		     completions))))
      ;; try-completion returns t if there's only one completion
      (when (eq str t)
	(setq str (car (overlay-get overlay 'completions)))
	(if (stringp str)
	    (setq str (substring str (length prefix)))
	  (setq str (substring (car str) (cdr str)))))

      ;; do tab-completion
      (unless (or (null str) (string= str ""))
	;; add prefix (which might have been modified) to front of
	;; tab-completion
	(setq str
	      (concat (buffer-substring-no-properties
		       (- (overlay-start overlay)
			  (overlay-get overlay 'prefix-length))
		       (overlay-start overlay))
		      str))
	;; run pre-tab-complete interface functions
	(completion-ui-deactivate-interfaces-pre-update overlay)
	;; delete the original prefix and insert the tab-completion
	(delete-region (- (point) (length prefix)) (point))
        (let ((overwrite-mode nil)) (insert str))
	;; update overlay properties
        (move-overlay overlay (point) (point))
	(completion-ui-setup-overlay
	 str nil nil nil nil nil 'unchanged overlay)
        ;; when auto-completing, do so
        (if (or completion-auto-update
		(and auto-completion-mode
		     (eq (overlay-get overlay 'completion-source)
			 auto-completion-source)))
            (complete-in-buffer
	     (overlay-get overlay 'completion-source)
	     nil 'not-set 'auto overlay)
          ;; otherwise, update completion interfaces
	  (completion-ui-update-interfaces overlay))))))




;;; ===============================================================
;;;                Self-insert functions

(defun auto-completion-lookup-behaviour (&optional char syntax no-overlay)
  "Return syntax-dependent behaviour
of character CHAR and/or syntax-class SYNTAX. At least one of
these must be supplied. If both are supplied, SYNTAX overrides the
syntax-class of CHAR.

Returns a three-element list:

  (RESOLVE COMPLETE INSERT)

containing the resolve-behaviour, completion-behaviour and
insert-behaviour.

The \"overlay-local\" bindings of `auto-completion-syntax-alist'
and `auto-completion-override-syntax-alist' are used when they
exist, unless NO-OVERLAY is non-nil."

  ;; SYNTAX defaults to syntax-class of CHAR
  (when (and char (not syntax)) (setq syntax (char-syntax char)))

  ;; get syntax alists
  (let ((syntax-alist
         (if (and (fboundp 'auto-overlay-local-binding)
		  (not no-overlay))
             (auto-overlay-local-binding 'auto-completion-syntax-alist)
           auto-completion-syntax-alist))
        (override-alist
         (if (and (fboundp 'auto-overlay-local-binding)
		  (not no-overlay))
             (auto-overlay-local-binding
              'auto-completion-override-syntax-alist)
           auto-completion-override-syntax-alist))
	(global-syntax-alist auto-completion-syntax-alist)
	behaviour)

    ;; if `auto-completion-syntax-alist' is a predefined behaviour (a
    ;; cons cell), convert it to an alist
    (dolist (alist '(syntax-alist global-syntax-alist))
      (unless (listp (car (eval alist)))
	(set alist
	     `(;; word constituents add to current completion and complete
	       ;; word or string, depending on VALUE
	       (?w . (add ,(cdr (eval alist))))
	       ;; symbol constituents, whitespace and punctuation characters
	       ;; either accept or reject, depending on VALUE, and don't
	       ;; complete
	       (?_ .  (,(car (eval alist)) none))
	       (?  .  (,(car (eval alist)) none))
	       (?. .  (,(car (eval alist)) none))
	       (?\( . (,(car (eval alist)) none))
	       (?\) . (,(car (eval alist)) none))
	       ;; anything else rejects and does't complete
	       (t . (reject none)))
	     )))

    ;; extract behaviours from syntax alists
    (setq behaviour
	  (or
	   ;; if char is specified, check override-alist
	   (and char
		(or (cdr (assq char override-alist))
		    ;; fall back to global override-alist
		    (cdr (assq char auto-completion-override-syntax-alist))))
	   ;; check syntax-alist
	   (cdr (assq syntax syntax-alist))
	   (cdr (assq t syntax-alist))
	   ;; fall back to global syntax-alist
	   (cdr (assq syntax global-syntax-alist))
	   (cdr (assq t global-syntax-alist))))
    (when (= (length behaviour) 2) (setq behaviour (append behaviour '(t))))
    ;; return behaviour
    behaviour))



;; (defmacro completion-get-resolve-behaviour (behaviour)
;;   "Extract syntax-dependent resolve behaviour from BEHAVIOUR.
;; BEHAVIOUR should be the return value of a call to
;; `auto-completion-lookup-behaviour'."
;;   `(nth 0 ,behaviour))


;; (defmacro completion-get-completion-behaviour (behaviour)
;;   "Extract syntax-dependent completion behaviour from BEHAVIOUR.
;; BEHAVIOUR should be the return value of a call to
;; `auto-completion-lookup-behaviour'."
;;   `(nth 1 ,behaviour))


;; (defmacro completion-get-insertion-behaviour (behaviour)
;;   "Extract syntax-dependent insertion behaviour from BEHAVIOUR.
;; BEHAVIOUR should be the return value of a call to
;; `auto-completion-lookup-behaviour'."
;;   `(nth 2 ,behaviour))



(defun completion-self-insert ()
  "Deal with completion-related stuff, then insert last input event."
  (interactive)
  ;; FIXME: whether to keep or delete provisional completion should
  ;;        depend on point's location relative to it

  ;; if we're auto-completing, hand over to `auto-completion-self-insert'
  (if auto-completion-mode
      (auto-completion-self-insert)
    ;; otherwise, resolve old and current completions, and insert last input
    ;; event
    (let ((overlay (completion-ui-overlay-at-point)))
      (completion-ui-resolve-old overlay)
      (completion--resolve-current overlay))
    (self-insert-command 1)))



(defun auto-completion-self-insert
  (&optional char syntax no-syntax-override)
  "Execute a completion function based on syntax of the character
to be inserted.

Decide what completion function to execute by looking up the
syntax of the character corresponding to the last input event in
`auto-completion-syntax-alist'. The syntax-derived function can
be overridden for individual characters by
`auto-completion-override-syntax-alist'.

If CHAR is supplied, it is used instead of the last input event
to determine the character typed. If SYNTAX is supplied, it
overrides the character's syntax, and is used instead to lookup
the behaviour in the alists. If NO-SYNTAX-OVERRIDE is non-nil,
the behaviour is determined only by syntax, even if it is
overridden for the character in question
\(i.e. `auto-completion-override-syntax-alist' is ignored\).

The default actions in `completion-dymamic-syntax-alist' all
insert the last input event, in addition to taking any completion
related action \(hence the name,
`auto-completion-self-insert'\). Therefore, unless you know what
you are doing, only bind `auto-completion-self-insert' to
printable characters.

The Emacs `self-insert-command' is remapped to this in completion
overlays."
  (interactive)

  ;; if CHAR or SYNTAX were supplied, use them; otherwise get character
  ;; and syntax from last input event (which relies on sensible key
  ;; bindings being used for this command)
  (when (null char)
    (if (characterp last-input-event)
	(setq char last-input-event)
      ;; if `last-input-event' is not a character, look at
      ;; `this-single-command-keys' instead in case we get a character after
      ;; translation (as e.g. for "\S- ")
      (setq char (this-single-command-keys))
      (if (= (length char) 1) (setq char (aref char 0))
	(error "`auto-completion-self-insert'\
 bound to non-printable character"))))
  (when (null syntax) (setq syntax (char-syntax char)))

  (destructuring-bind (resolve-behaviour complete-behaviour insert-behaviour
		       overlay word-thing wordstart prefix)
      (append
       (if no-syntax-override
	   (auto-completion-lookup-behaviour nil syntax)
	 (auto-completion-lookup-behaviour char syntax))
       (list (completion-ui-overlay-at-point) nil nil nil))
    (setq word-thing
	  (completion-ui-source-word-thing auto-completion-source overlay))

    ;; ----- resolve behaviour -----
    (completion-ui-resolve-old overlay)

    ;; if behaviour alist entry is a function, call it
    (when (functionp resolve-behaviour)
      (setq resolve-behaviour (funcall resolve-behaviour)))

    ;; do whatever action was specified in alists
    (cond
     ;; no-op
     ((null resolve-behaviour))

     ;; accept
     ((eq resolve-behaviour 'accept)
      ;; CHAR might not be a character if `last-input-event' included a
      ;; modifier (e.g. S-<space>)
      (setq prefix (string char))
      (setq wordstart t)
      ;; if there is a completion at point...
      (when overlay
	;; if point is at start of overlay, accept completion
	(if (= (point) (overlay-start overlay))
	    (completion-accept nil overlay)
	  ;; otherwise, delete overlay (effectively accepting old completion
	  ;; but without running hooks)
	  (completion-ui-delete-overlay overlay)
	  (completion-ui-deactivate-interfaces overlay))
	(setq overlay nil)))

     ;; reject
     ((eq resolve-behaviour 'reject)
      ;; CHAR might not be a character if `last-input-event' included a
      ;; modifier (e.g. S-<space>)
      (setq prefix (string char))
      (setq wordstart t)
      ;; if there is a completion at point...
      (when overlay
	;; if point is at start of overlay, reject completion
	(if (= (point) (overlay-start overlay))
	    (completion-reject nil overlay)
	  ;; otherwise, delete everything following point, and delete overlay
	  (delete-region (point) (overlay-end overlay))
	  (completion-ui-delete-overlay overlay)
	  (completion-ui-deactivate-interfaces overlay))
	(setq overlay nil)))

     ;; add to prefix
     ((eq resolve-behaviour 'add)
      ;; if we're at the start of a word, prevent adjacent word from being
      ;; deleted below if `completion-overwrite' is non-nil
      (when (completion-beginning-of-word-p word-thing) (setq wordstart t))
      ;; if point is within a completion overlay...
      (when overlay
	;; if point is at start of overlay, update prefix and prevent adjacent
	;; words being deleted
	(if (= (point) (overlay-start overlay))
	    (progn
	      (setq prefix (concat (overlay-get overlay 'prefix)
				   (string char))
		    wordstart t)
	      (completion-ui-deactivate-interfaces-pre-update overlay)
	      (completion-ui-setup-overlay
	       prefix (when overlay (1+ (overlay-get overlay 'prefix-length)))
	       nil nil nil nil nil overlay))
	  ;; otherwise, delete overlay (effectively accepting the old
	  ;; completion) and behave as if no completion was in progress
	  (completion-ui-delete-overlay overlay)
	  (completion-ui-deactivate-interfaces overlay)
	  (setq overlay nil))))

     ;; error
     (t (error "Invalid entry in `auto-completion-syntax-alist'\
 or `auto-completion-override-syntax-alist', %s"
	       (prin1-to-string resolve-behaviour))))


    ;; ----- insersion behaviour -----
    ;; if behaviour alist entry is a function, call it
    (when (functionp insert-behaviour)
      (setq insert-behaviour (funcall insert-behaviour)))

    ;; if we're inserting...
    (when insert-behaviour
      ;; use `self-insert-command' if possible, since `auto-fill-mode' depends
      ;; on it
      (if (eq char last-input-event)
	  (self-insert-command 1)
	(insert char))
      (when overlay (move-overlay overlay (point) (point))))


    ;; ----- completion behaviour -----
    ;; if behaviour alist entry is a function, call it
    (when (functionp complete-behaviour)
      (setq complete-behaviour (funcall complete-behaviour)))

    (cond
     ;; no-op
     ((null complete-behaviour))

     ;; if not completing, clear up any overlay left lying around
     ((eq complete-behaviour 'none)
      (when overlay
	(completion-ui-deactivate-interfaces overlay)
	(completion-ui-delete-overlay overlay)))

     ;; if completing...
     ((or (eq complete-behaviour 'word)
	  (eq complete-behaviour 'string))
      ;; if point is in middle of a word, `completion-overwrite' is set, and
      ;; overwriting hasn't been disabled, delete rest of word prior to
      ;; completing
      (when (and completion-overwrite
		 (completion-within-word-p word-thing)
		 (null wordstart))
	(completion-overwrite-word-at-point word-thing))

      (cond
       ;; if a prefix has been set, do completion
       (prefix
	(complete-in-buffer
	 auto-completion-source nil 'not-set 'auto overlay))

       ;; if doing basic completion, let prefix be found normally
       ((eq complete-behaviour 'string)
	(complete-in-buffer
	 auto-completion-source nil 'not-set 'auto overlay))

       ;; if completing word at point, delete any overlay at point to ensure
       ;; prefix is found anew, and do completion
       (t
	(when (setq overlay (completion-ui-overlay-at-point))
	  (completion-ui-delete-overlay overlay))
	(complete-in-buffer
	 auto-completion-source nil 'not-set 'auto overlay))))

     ;; error
     (t (error "Invalid entry in `auto-completion-syntax-alist'\
 or `auto-completion-override-syntax-alist', %s"
	       (prin1-to-string complete-behaviour))))
    ))



(defun completion-auto-update-self-insert ()
  "Add character to current prefix and recomplete
based on current syntax table."
  (interactive)

  (let ((char last-input-event)
	(syntax (char-syntax last-input-event))
	(overlay (completion-ui-overlay-at-point))
	prefix dont-complete)
    (when overlay
      (cond

       ;; word- or symbol-constituent: add to prefix
       ;; note: disabled for non-prefix-completion, unless
       ;;       `completion-accept-or-reject-by-default' is set to 'reject,
       ;;       because otherwise the prefix being added to is hidden
       ((and (or (eq syntax ?w) (eq syntax ?_))
	     (or (not (overlay-get overlay 'non-prefix-completion))
		 (eq completion-accept-or-reject-by-default 'reject)))
	;; add characters up to point and new character to prefix
	(setq prefix
	      (concat (buffer-substring-no-properties
		       (- (overlay-start overlay)
			  (if (overlay-get overlay 'prefix-replaced)
			      0 (overlay-get overlay 'prefix-length)))
		       (point))
		      (string char)))
	(delete-region (point) (overlay-end overlay))
	(overlay-put overlay 'prefix-replaced nil)
	(completion-ui-delete-overlay overlay)
	(completion-ui-deactivate-interfaces-pre-update overlay)
	;; insert character
	(if (eq char last-input-event)
	    (self-insert-command 1)
	  (insert char))
	;; re-complete new prefix
	(unless dont-complete
	  (move-overlay overlay (point) (point))
	  (completion-ui-setup-overlay prefix nil nil nil nil nil nil overlay)
	  (complete-in-buffer
	   (overlay-get overlay 'completion-source)
	   nil 'not-set 'auto overlay)))


       ;; anything else: accept up to point, reject rest
       (t
	;; if point is at start of overlay, reject completion
	(if (= (point) (overlay-start overlay))
	    (completion-reject nil overlay)
	  ;; otherwise, delete everything following point, and delete overlay
	  (delete-region (point) (overlay-end overlay))
	  (completion-ui-delete-overlay overlay)
	  (completion-ui-deactivate-interfaces overlay))
	;; insert character
	(if (eq char last-input-event)
	    (self-insert-command 1)
	  (insert char)))))))





;;; ============================================================
;;;                          Undo

(defun completion-resolve-before-undo (beg end)
  "Resolve completions betweeh BEG and END before undoing.
Added to `before-change-functions' hook."
  ;; check if current command is an undo
  (when undo-in-progress
    ;; replace 'leave behaviour by 'accept, since we have to get rid of the
    ;; completion overlay before the undo
    (let ((completion-how-to-resolve-old-completions
	   (if (eq completion-how-to-resolve-old-completions 'leave)
	       'accept
	     completion-how-to-resolve-old-completions)))
      (completion-ui-resolve-old nil beg end))))





;;; ============================================================
;;;                      Fill commands

(defun completion-fill-paragraph (&optional justify region)
  "Fill paragraph at or after point.
This command first sorts out any provisional completions, before
calling `fill-paragraph', passing any argument straight through."
  ;; interactive spec copied from `fill-paragraph'
  (interactive (progn
		 (barf-if-buffer-read-only)
		 (list (if current-prefix-arg 'full) t)))
  (completion-ui-resolve-old)
  (fill-paragraph justify region))





;;; ============================================================
;;;                      Yank Commands

(defun auto-completion-mouse-yank-at-click (click arg)
  "Insert the last stretch of killed text at the position clicked on.
Temporarily disables `auto-completion-mode', then calls
`mouse-yank-at-click'."
  (interactive "e\nP")
  (let ((auto-completion-mode nil))
    (mouse-yank-at-click click arg)))





;;; ============================================================
;;;                    Deletion commands

(defun completion-backward-delete (command &rest args)
  "Call backward-delete COMMAND, passing it ARGS.
Any provisional completion at the point is first rejected. If
COMMAND deletes into a word and `auto-completion-mode' is
enabled, complete what remains of that word."

  (let* ((overlay (completion-ui-overlay-at-point))
         pos)
    ;(combine-after-change-calls

      ;; ----- not auto-completing -----
      (if (and (not auto-completion-mode) (not completion-auto-update))

          (progn
            ;; if within a completion...
            (when overlay
              (cond
	       ;; if rejecting old completions, delete everything after the
	       ;; point
	       ((eq completion-accept-or-reject-by-default 'reject)
                (delete-region (point) (overlay-end overlay)))

	       ;; if accepting longest common substring...
	       ((eq completion-accept-or-reject-by-default 'accept-common)
		;; find the end of the longest common substring, using
		;; common-substring overlay if it exists, otherwise searching
		;; for it ourselves
		(if (overlayp (overlay-get overlay 'common-substring))
		    (setq pos (overlay-end
			       (overlay-get overlay 'common-substring)))
		  (let* ((prefix (overlay-get overlay 'prefix))
			 (completions
			  (mapcar
			   (lambda (c)
			     (if (stringp c)
				 (substring c (length prefix))
			       (substring (car c) (cdr c))))
			   (overlay-get overlay 'completions)))
			 (str (try-completion "" completions)))
		    ;; (try-completion returns t if there's only one completion)
		    (when (eq str t) (setq str (car completions)))
		    (setq pos (+ (overlay-start overlay) (length str)))))
		;; if point is before end of common substring, delete
		;; everthing after common substring, otherwise delete
		;; everything after point
		(delete-region (if (< (point) pos) pos (point))
			       (overlay-end overlay))))

	      ;; delete overlay, effectively accepting (rest of) the
              ;; completion at point
              (completion-ui-delete-overlay overlay)
	      (completion-ui-deactivate-interfaces overlay))

            ;; resolve old provisional completions and delete backwards
            (completion-ui-resolve-old)
            (apply command args))


        ;; ----- auto-completing -----
	(let* ((word-thing
		(completion-ui-source-word-thing auto-completion-source))
	       (wordstart (or overlay
			      (completion-beginning-of-word-p word-thing))))

	  ;; resolve any old provisional completions
	  (completion-ui-resolve-old overlay)

	  ;; if point is in a completion...
	  (when overlay
	    ;; delete provisional completion characters after point
	    ;; FIXME: should this depend on
	    ;;        `completion-accept-or-reject-by-default'?
	    (delete-region (point) (overlay-end overlay))
	    ;; store position of beginning of prefix
	    (setq pos (- (overlay-start overlay)
			 (overlay-get overlay 'prefix-length)))
	    ;; delete the overlay, effectively accepting (rest of) completion
	    (completion-ui-delete-overlay overlay)
	    ;; deactivate the interfaces pending update
	    (completion-ui-deactivate-interfaces-pre-update overlay))


	  ;; delete backwards
	  (apply command args)


	  (cond
	   ;; if we're not in or at the end of a word, or we're auto-updating
	   ;; rather than auto-completing and we've deleted beyond current
	   ;; completion, deactivate any user-interfaces and cancel any timer
	   ;; that's been set up
	   ((or (and (not auto-completion-mode)
		     (or (not overlay)
			 (<= (point)
			    (- pos (overlay-get overlay 'prefix-length)))))
		(and (not (completion-within-word-p word-thing))
		     (not (completion-end-of-word-p word-thing))))
	    (when (timerp completion--backward-delete-timer)
	      (cancel-timer completion--backward-delete-timer))
	    (setq completion--backward-delete-timer nil)
	    (when overlay (completion-ui-deactivate-interfaces overlay)))


	   ;; otherwise, we're in or at the end of a word, so complete the
	   ;; word at point
	   (t
	    ;; if point was at start of completion or start of word before
	    ;; deleting, and we're now within or at end of a word, setup
	    ;; overlay to prevent word after point being deleted
	    (when (or overlay
		      (and wordstart
			   (or (completion-within-word-p word-thing)
			       (completion-end-of-word-p word-thing))))
	      (let* ((prefix-fun
		      (completion-ui-source-prefix-function
		       auto-completion-source overlay))
		     (prefix (let ((completion-word-thing word-thing))
			       (funcall prefix-fun))))
		(setq overlay
		      (completion-ui-setup-overlay
		       prefix nil nil nil auto-completion-source
		       nil nil overlay))
		(move-overlay overlay (point) (point))))

	    ;; if there's no existing timer, set one up to complete remainder
	    ;; of word after some idle time
	    (when (timerp completion--backward-delete-timer)
	      (cancel-timer completion--backward-delete-timer))
	    (if auto-completion-backward-delete-delay
		(setq completion--backward-delete-timer
		      (run-with-idle-timer
		       auto-completion-backward-delete-delay nil
		       `(lambda ()
			  (setq completion--backward-delete-timer nil)
			  (complete-in-buffer
			   ',auto-completion-source nil 'not-set
			   'auto ,overlay ,(point)))))
	      ;; if completing with no delay, do so
	      (complete-in-buffer auto-completion-source nil 'not-set
				  'auto overlay (point)))
	    )))));)
  )



(defun completion-delete (command &rest args)
  "Call forward-delete COMMAND, passing it ARGS.
If there is a provisional completion at point after deleting, reject
it."
  ;; delete any completion overlay at point (effectively accepting it, but
  ;; without running any hook functions)
  (let ((overlay (completion-ui-overlay-at-point)))
    (when overlay
      (completion-ui-delete-overlay overlay)
      (completion-ui-deactivate-interfaces overlay)))
  ;; now resolve any old completions
  (completion-ui-resolve-old)
  ;; call the deletion command
  (apply command args)
  ;; if there's a completion overlay at point after deleting, delete it
  ;; (effectively rejecting it, but without calling any hook functions)
  (let ((overlay (completion-ui-overlay-at-point)))
    (when overlay
      (delete-region (overlay-start overlay) (overlay-end overlay))
      (completion-ui-delete-overlay overlay)
      (completion-ui-deactivate-interfaces overlay))))


(defun completion-delete-char (n &optional killflag)
  "Delete the following N characters (previous if N is negative).

Non-nil optional second arg KILLFLAG means kill instead (save in
kill ring). Interactively, N is the prefix arg (default 1), and
KILLFLAG is set if n was explicitly specified. \(If N is
negative, behaviour is instead as for
`completion-backward-delete-char'.\)

If there is a provisional completion at point after deleting, it
is rejected. "
  (interactive "P")
  (when (and (interactive-p) n) (setq killflag t))
  (setq n (prefix-numeric-value n))
  ;; if deleting backwards, call `completion-backward-delete' instead
  (if (< n 0)
      (completion-backward-delete 'backward-delete-char (- n) killflag)
    (completion-delete 'delete-char n killflag)))


(defun completion-backward-delete-char (n &optional killflag)
  "Delete the previous N characters.

Optional second arg KILLFLAG non-nil means kill instead (save in
kill ring). Interactively, N is the prefix arg (default 1), and
KILLFLAG is set if N was explicitly specified. \(If N is
negative, behaviour is instead as for `completion-delete-char'.\)

Any provisional completion at point is first rejected. If
deleting backwards into a word, and `auto-completion-mode' is
enabled, complete what remains of that word."
  (interactive "P")
  (when (and (interactive-p) n) (setq killflag t))
  (setq n (prefix-numeric-value n))
  ;; if deleting forwards, call `completion-delete' instead
  (if (< n 0)
      (completion-delete 'delete-char (- n) killflag)
    (completion-backward-delete 'backward-delete-char n killflag)))


(defun completion-backward-delete-char-untabify (n &optional killflag)
  "Delete N characters backward, changing tabs into spaces.

Optional second arg KILLFLAG non-nil means kill instead (save in
kill ring). Interactively, N is the prefix arg (default 1), and
KILLFLAG is set if N was explicitly specified. \(If N is
negative, behaviour is instead as for `completion-delete-char'.\)

The exact behavior depends on `backward-delete-char-untabify-method'.

Any provisional completion at point is first rejected. If
deleting backwards into a word, and `auto-completion-mode' is
enabled, complete what remains of that word."
  (interactive "P")
  (when (and (interactive-p) n) (setq killflag t))
  (setq n (prefix-numeric-value n))
  ;; if deleting forwards, call `completion-delete' instead
  (if (< n 0)
      (completion-delete 'delete-char (- n) killflag)
    (completion-backward-delete 'backward-delete-char-untabify n killflag)))


(defun completion-kill-word (&optional n)
  "Kill characters forward until encountering the end of a word.
With argument, do this that many times. \(If N is negative,
behaviour is instead as for `completion-backward-kill-word'.\)

If there is a provisional completion at point after deleting, it
is rejected."
  (interactive "p")
  ;; if deleting backwards, call `completion-backward-delete' instead
  (if (< n 0)
      (completion-backward-delete 'backward-kill-word (- n))
    (completion-delete 'kill-word n)))


(defun completion-backward-kill-word (&optional n)
  "Kill characters backward until encountering the end of a word.
With argument, do this that many times. \(If N is negative,
behaviour is instead as for `completion-kill-word'.\)

Any provisional completion at point is first rejected. If
deleting backwards into a word, and `auto-completion-mode' is
enabled, complete what remains of that word."
  (interactive "p")
  ;; if deleting forwards, call `completion-delete' instead
  (if (< n 0)
      (completion-delete 'kill-word (- n))
    (completion-backward-delete 'backward-kill-word n)))


(defun completion-kill-sentence (&optional n)
  "Kill from point to end of sentence.
With argument, do this that many times. \(If N is negative,
behaviour is instead as for
`completion-backward-kill-sentence'.\)

If there is a provisional completion at point after deleting, it
is rejected."
  (interactive "p")
  ;; if deleting backwards, call `completion-backward-delete' instead
  (if (< n 0)
      (completion-backward-delete 'backward-kill-sentence (- n))
    (completion-delete 'kill-sentence n)))


(defun completion-backward-kill-sentence (&optional n)
  "Kill back from point to start of sentence.
With argument, do this that many times. \(If N is negative,
behaviour is instead as for `completion-kill-sentence'.\)

Any provisional completion at point is first rejected. If
deleting backwards into a word, and `auto-completion-mode' is
enabled, complete what remains of that word."
  (interactive "p")
  ;; if deleting forwards, call `completion-delete' instead
  (if (< n 0)
      (completion-delete 'kill-sentence (- n))
    (completion-backward-delete 'backward-kill-sentence n)))


(defun completion-kill-sexp (&optional n)
  "Kill the sexp (balanced expression) following point.
With argument, do this that many times. \(If N is negative,
behaviour is instead as for `completion-backward-kill-sexp'.\)

If there is a provisional completion at point after deleting, it
is rejected."
  (interactive "p")
  ;; if deleting backwards, call `completion-backward-delete' instead
  (if (< n 0)
      (completion-backward-delete 'backward-kill-sexp (- n))
    (completion-delete 'kill-sexp n)))


(defun completion-backward-kill-sexp (&optional n)
  "Kill the sexp (balanced expression) before point.
With argument, do this that many times. \(If N is negative,
behaviour is instead as for `completion-kill-sexp'.\)

Any provisional completion at point is first rejected. If
deleting backwards into a word, and `auto-completion-mode' is
enabled, complete what remains of that word."
  (interactive "p")
  ;; if deleting forwards, call `completion-delete' instead
  (if (< n 0)
      (completion-delete 'kill-sexp (- n))
    (completion-backward-delete 'backward-kill-sexp n)))



(defun completion-kill-line (&optional n)
  "Kill the line following point.
With argument, do this that many times. \(If N is negative,
behaviour is instead as for `completion-backward-kill-line'.\)

If there is a provisional completion at point after deleting, it
is rejected."
  (interactive "P")
  ;; if deleting backwards, call `completion-backward-delete' instead
  (if (and (integerp n) (< n 0))
      (completion-backward-delete 'kill-line (- n))
    (completion-delete 'kill-line n)))



(defun completion-backward-kill-line (&optional n)
  "Kill the line before point.
With argument, do this that many times. \(If N is negative,
behaviour is instead as for `completion-kill-line'.\)

Any provisional completion at point is first rejected. If
deleting backwards into a word, and `auto-completion-mode' is
enabled, complete what remains of that word."
  (interactive "p")
  ;; if deleting forwards, call `completion-delete' instead
  (if (< n 0)
      (completion-delete 'kill-line (- n))
    (completion-backward-delete 'kill-line n)))


(defun completion-kill-paragraph (&optional n)
  "Kill forward to end of paragraph.
With argument, do this that many times. \(If N is negative,
behaviour is instead as for
`completion-backward-kill-paragraph'.\)

If there is a provisional completion at point after deleting,
reject it."
  (interactive "p")
  ;; if deleting backwards, call `completion-backward-delete' instead
  (if (< n 0)
      (completion-backward-delete 'backward-kill-paragraph (- n))
    (completion-delete 'kill-paragraph n)))


(defun completion-backward-kill-paragraph (&optional n)
  "Kill backward to start of paragraph.
With argument, do this that many times. \(If N is negative,
behaviour is instead as for `completion-kill-paragraph'.\)

Any provisional completion at point is first rejected. If
deleting backwards into a word, and `auto-completion-mode' is
enabled, complete what remains of that word."
  (interactive "p")
  ;; if deleting forwards, call `completion-delete' instead
  (if (< n 0)
      (completion-delete 'kill-paragraph (- n))
    (completion-backward-delete 'backward-kill-paragraph n)))





;;; ==============================================================
;;;                    Internal functions

(defun completion-prefix ()
  "Return the completion prefix at point.
This is the default `completion-prefix-function'."
  (declare (special completion-word-thing))
  (let ((overlay (completion-ui-overlay-at-point))
	(pos (point)))
    ;; if point is within existing completion overlay, return its prefix
    (if overlay
        (overlay-get overlay 'prefix)
      ;; otherwise, prefix is the word before point
      (save-excursion
        (forward-thing completion-word-thing -1)
        (buffer-substring-no-properties (point) pos)))))



(defun completion-ui-setup-overlay
    (prefix &optional prefix-length completions num
	    completion-source prefix-function
	    non-prefix-completion overlay)
  "Set completion overlay properties according to the arguments.

If OVERLAY isn't supplied, look for one at point, and create a
new one if none exists.

If PREFIX-LENGTH is null, it defaults to the length of PREFIX. If
NUM is null, it defaults to 1. To leave these properties
unchanged, use any non-numeric and non-null value.

The COMPLETION-SOURCE, COMPLETION-PREFIX-FUNCTION and
NON-PREFIX-COMPLETION properties are ignored and the
corresponding properties left unchanged if an existing overlay's
properties (either a supplied OVERLAY or an existing overlay at
point) are being set."

  ;; look for completion overlay at point
  (unless overlay (setq overlay (completion-ui-overlay-at-point)))

  ;; if overlay does not already exists, create one
  (unless overlay
    (setq overlay (make-overlay (point) (point) nil nil t))
    ;; set permanent overlay properties
    (overlay-put overlay 'completion-overlay t)
    (overlay-put overlay 'non-prefix-completion non-prefix-completion)
    (overlay-put overlay 'face 'completion-highlight-face)
    (overlay-put overlay 'priority 100)
    (overlay-put overlay 'completion-source completion-source)
    (overlay-put overlay 'completion-prefix-function prefix-function)
    ;; set overlay keymap
    (let ((map (make-sparse-keymap)))
      (overlay-put overlay 'keymap map)
      (set-keymap-parent
       map
       (cond
	((and auto-completion-mode
	      (eq (overlay-get overlay 'completion-source)
		  auto-completion-source))
	 auto-completion-overlay-map)
	(completion-auto-update completion-auto-update-overlay-map)
	(t completion-overlay-map))))
    ;; add overlay to list
    (push overlay completion--overlay-list))

  ;; update modifiable overlay properties
  (overlay-put overlay 'prefix prefix)
  (cond
   ((numberp prefix-length)
    (overlay-put overlay 'prefix-length prefix-length))
   ((and prefix-length (overlay-get overlay 'prefix-length)))
   (t (overlay-put overlay 'prefix-length (length prefix))))
  (overlay-put overlay 'completions completions)
  (cond
   ((null num) (overlay-put overlay 'completion-num (if completions 0 nil)))
   ((numberp num) (overlay-put overlay 'completion-num num)))

  ;; return the new overlay
  overlay)



(defun completion-ui-delete-overlay (overlay &optional keep-popup)
  "Delete completion overlay, and clean up after it.
If KEEP-POPUP is non-nil, prevent deletion of any pop-up frame
associated with OVERLAY."
  (when (overlayp (overlay-get overlay 'common-substring))
    (delete-overlay (overlay-get overlay 'common-substring)))
  (delete-overlay overlay)
  (setq completion--overlay-list (delq overlay completion--overlay-list)))



(defun completion-ui-overlay-at-point (&optional point)
  "Return completion overlay overlapping point.
\(There should only be one; if not, one is returned at random\)"
  (setq point (or point (point)))

  ;; and overlays starting at POINT
  (let (overlay-list)
    (catch 'found
      ;; check overlays overlapping POINT (including zero-length)
      (setq overlay-list (overlays-in point point))
      (dolist (o overlay-list)
        (when (overlay-get o 'completion-overlay)
          (throw 'found o)))

      ;; check overlays ending at POINT
      (setq overlay-list (overlays-in (1- point) point))
      (dolist (o overlay-list)
        (when (and (overlay-get o 'completion-overlay)
                   (= (overlay-end o) point))
          (throw 'found o)))

      ;; check overlays starting at POINT
      (setq overlay-list (overlays-in point (1+ point)))
      (dolist (o overlay-list)
        (when (and (overlay-get o 'completion-overlay)
                   (= (overlay-start o) point))
          (throw 'found o)))
      )))



(defun completion-overlays-in (start end)
  "Return list of completion overlays between START and END."
  ;; get overlays between START and END
  (let ((o-list (overlays-in start end))
        overlay-list)
    ;; filter overlay list
    (dolist (o o-list)
      (when (overlay-get o 'completion-overlay)
        (push o overlay-list)))
    ;; return the overlay list
    overlay-list))



(defun completion-ui-resolve-old (&optional overlay beg end)
  "Resolve old completions according to the setting of
`completion-how-to-resolve-old-completions'.

Any completion overlay specified by OVERLAY will be left alone,
and completions near the point are dealt with specially, so as
not to modify characters around the point.

If BEG and END are specified, only completions between BEG and
END are resolved. If only one of BEG or END is specified, the
other defaults to `point-max' or `point-min', respectively."
  (cond
   ((and beg (not end)) (setq end (point-max)))
   ((and (not beg) end) (setq beg (point-min))))

  (let (overlay-list)
    ;; ignore OVERLAY and any overlays not between BEG and END (if specified)
    (if beg
	(dolist (o completion--overlay-list)
	  (unless (or (eq o overlay)
		      (< (overlay-end o) beg)
		      (> (overlay-start o) end))
	    (push o overlay-list)))
      (setq overlay-list (delq overlay completion--overlay-list)))


    (save-excursion
      (cond
       ;; leave old completions (but accept zero-length ones)
       ((eq completion-how-to-resolve-old-completions 'leave)
	(mapc (lambda (o)
		(when (= (overlay-start o) (overlay-end o))
		  (completion-accept nil o)))
	      overlay-list))

       ;; accept old completions
       ((eq completion-how-to-resolve-old-completions 'accept)
	(mapc (lambda (o)
		;; if completion is nowhere near point, accept it
		(if (or (> (point) (overlay-end o))
			(< (point)
			   (- (overlay-start o)
			      (if (and (overlay-get o 'non-prefix-completion)
				       (overlay-get o 'prefix-replaced))
				  0 (overlay-get o 'prefix-length)))))
		    (completion-accept nil o)
		  ;; otherwise, completion overlaps point, so just delete
		  ;; overlay, effectively accepting whatever is there
		  (completion-ui-delete-overlay o)
		  (completion-ui-deactivate-interfaces o)))
	      overlay-list))

       ;; reject old completions
       ((eq completion-how-to-resolve-old-completions 'reject)
	(mapc (lambda (o)
		;; if completion is nowhere near point, reject it
		(if (or (> (point) (overlay-end o))
			(< (point)
			   (- (overlay-start o)
			      (if (and (overlay-get o 'non-prefix-completion)
				       (overlay-get o 'prefix-replaced))
				  0 (overlay-get o 'prefix-length)))))
		    (completion-reject nil o)
		  ;; otherwise, completion overlaps point, so just delete
		  ;; provisional completion characters and overlay
		  (delete-region (overlay-start o) (overlay-end o))
		  (completion-ui-delete-overlay o)
		  (completion-ui-deactivate-interfaces o)))
	      overlay-list))

       ;; ask 'em
       ((eq completion-how-to-resolve-old-completions 'ask)
	(save-excursion
	  (mapc (lambda (o)
		  (goto-char (overlay-end o))
		  ;; FIXME: remove hard-coded face
		  (overlay-put o 'face '(background-color . "red"))
		  (if (y-or-n-p "Accept completion? ")
		      (completion-accept nil o)
		    (completion-reject nil o)))
		overlay-list))))))

  (when (null completion--overlay-list)
    (setq completion-ui--activated nil)))



(defun completion--resolve-current (&optional overlay char syntax)
  "Resolve current completion according to customization settings.

If OVERLAY is supplied, use that instead of trying to find one at
point. The point had better be within OVERLAY or your pet
mosquito will suffer an untimely death.

If CHAR and/or SYNTAX are supplied and `auto-completion-mode' is
enabled, resolve current completion as though the character CHAR
with syntax class SYNTAX was inserted at point (without actually
inserting anything)."

  ;; if no overlay was supplied, try to find one at point
  (unless overlay (setq overlay (completion-ui-overlay-at-point)))
  ;; resolve provisional completions not at point
  (completion-ui-resolve-old overlay)

  ;; if there's a completion at point...
  (when overlay
    (let (resolve)
      ;; if `auto-completion-mode' is enabled, and at least one of CHAR or
      ;; SYNTAX was supplied, lookup behaviour for CHAR and SYNTAX
      (if (and auto-completion-mode
	       (eq (overlay-get overlay 'completion-source)
		   auto-completion-source)
	       (or char syntax))
	  (setq resolve (car (auto-completion-lookup-behaviour char syntax)))

	;; otherwise, `completion-accept-or-reject-by-default' determines the
	;; behaviour
	;; note: setting resolve to 'reject causes characters between point
	;;       and end of overlay to be deleted, which accepts longest
	;;       common substring in the case of 'accept-common because of
	;;       where point is positioned
	(cond
	 ((or (eq completion-accept-or-reject-by-default 'reject)
	      (eq completion-accept-or-reject-by-default 'accept-common))
	  (setq resolve 'reject))
	 (t (setq resolve 'accept))))


      (cond
       ;; if rejecting...
       ((eq resolve 'reject)
        ;; if point is at the start of a completion, reject normally
        (if (= (point) (overlay-start overlay))
            (completion-reject nil overlay)
          ;; otherwise, delete everything after point but keep whatever
          ;; comes before it
          (delete-region (point) (overlay-end overlay))
          (completion-ui-delete-overlay overlay)))

       ;; if accepting, do so
       ((eq resolve 'accept)
        (completion-accept nil overlay))

       ;; anything else effectively accepts the completion but without
       ;; running accept hooks
       (t (completion-ui-delete-overlay overlay)))
      )))



(defun completion--run-if-condition
  (command variable condition &optional when)
  "Run COMMAND if CONDITION is non-nil.

If WHEN is null or 'instead, run whatever would normally be bound
to the key sequence used to invoke this command if not within a
completion overlay. If WHEN is 'before or 'after, run the normal
binding before or after COMMAND.

VARIABLE should be a symbol that deactivates the keymap in which
COMMAND is bound when its value is set to nil. It is reset at the
end of this function.

Intended to be invoked (directly or indirectly) via a key
sequence in a keymap."

  ;; throw and error if executing recursively
  (when completion--trap-recursion
    (error "Recursive call to `completion--run-if-condition';\
 supplied variable probably doesn't disable keymap"))

  ;; run command if running before, or if running instead and CONDITION
  ;; is non-nil
  (when (or (eq when 'before)
            (and (or (null when) (eq when 'instead))
                 condition))
    (command-execute command))

  ;; run whatever would normally be bound to the key sequence,
  ;; unless running instead and CONDITION is non-nil
  (unless (and (or (null when) (eq when 'instead)) condition)
    (let ((completion--trap-recursion t)
          (restore (eval variable))
          command)
      (set variable nil)
      ;; we add (this-command-keys) to `unread-command-events' and then
      ;; re-read it in order to ensure key sequence translation takes place
      (setq unread-command-events (listify-key-sequence (this-command-keys)))
      (setq command (key-binding (read-key-sequence nil) t))
      (unwind-protect
          (when (commandp command)
            (command-execute command)
            (setq last-command command))  ; doesn't work - clobbered later :(
        (set variable restore))))

  ;; run command if running after
  (when (eq when 'after) (command-execute command)))



(defun completion--run-if-within-overlay
  (command variable &optional when)
  "Run COMMAND if within a completion overlay.

If WHEN is null or 'instead, run whatever would normally be bound
to the key sequence used to invoke this command if not within a
completion overlay. If WHEN is 'before or 'after, run the normal
binding before or after COMMAND.

VARIABLE should be a symbol that deactivates the keymap in which
COMMAND is bound when its value is set to nil. It is reset at the
end of this function.

Intended to be (invoked directly or indirectly) via a key
sequence in a keymap."
  (completion--run-if-condition
   command variable (completion-ui-overlay-at-point) when))



(defun completion-overwrite-word-at-point (word-thing)
  "Delete remainder of completion-word-thing at point."
  (let ((pos (point)))
    (save-excursion
      (forward-thing word-thing)
      (delete-region pos (point)))))



(defun completion-beginning-of-word-p (thing &optional point)
  "Return non-nil if POINT is at beginning of a THING.
\(POINT defaults to the point\)."
  (unless point (setq point (point)))
  (save-excursion
    (goto-char point)
    (let (bounds)
      (and (< point (point-max))
           (setq bounds (bounds-of-thing-at-point thing))
           (= point (car bounds))))))



(defun completion-within-word-p (thing &optional point)
  "Return non-nil if POINT is within or at end of a THING.
\(POINT defaults to the point\)."
  (unless point (setq point (point)))
  (save-excursion
    (goto-char point)
    (let (bounds)
      (and (setq bounds (bounds-of-thing-at-point thing))
           (> point (car bounds))
           (< point (cdr bounds))))))



(defun completion-end-of-word-p (thing &optional point)
  "Return non-nil if POINT is at end of a THING.
\(POINT defaults to the point\)"
  (unless point (setq point (point)))
  (save-excursion
    (goto-char point)
    (let (bounds)
      (and (> point (point-min))
           (setq bounds (bounds-of-thing-at-point thing))
           (= point (cdr bounds))))))



(defun completion-posn-at-point-as-event
  (&optional position window dx dy)
  "Return pixel position of top left corner of glyph at POSITION,
relative to top left corner of WINDOW, as a mouse-1 click
event (identical to the event that would be triggered by clicking
mouse button 1 at the top left corner of the glyph).

POSITION and WINDOW default to the position of point in the
selected window.

DX and DY specify optional offsets from the top left of the glyph."

  (unless window (setq window (selected-window)))
  (unless position (setq position (window-point window)))

  (let* ((pos (posn-at-point position window))
         (x-y (posn-x-y pos))
         (edges (window-inside-pixel-edges window))
         (win-x-y (window-pixel-edges window)))
    ;; adjust for window edges
    (setcar (nthcdr 2 pos)
            (cons (+ (car x-y) (car  edges) (- (car win-x-y))  (or dx 0))
                  (+ (cdr x-y) (cadr edges) (- (cadr win-x-y)) (or dy 0))))
    (list 'mouse-1 pos 1)))



(defun completion-window-posn-at-point
  (&optional position window dx dy)
  "Return pixel position of top left of corner glyph at POSITION,
relative to top left corner of WINDOW. Defaults to the position
of point in the selected window.

DX and DY specify optional offsets from the top left of the glyph.

See also `completion-window-inside-posn-at-point' and
`completion-frame-posn-at-point'."

  (unless window (setq window (selected-window)))
  (unless position (setq position (window-point window)))

  (let ((x-y (posn-x-y (posn-at-point position window)))
        (edges (window-inside-pixel-edges window))
        (win-x-y (window-pixel-edges window)))
    (cons (+ (car x-y) (car  edges) (- (car win-x-y)) (or dx 0))
          (+ (cdr x-y) (cadr edges) (- (cadr win-x-y)) (or dy 0)))))



(defun completion-window-inside-posn-at-point
  (&optional position window dx dy)
  "Return pixel position of top left corner of glyph at POSITION,
relative to top left corner of the text area in WINDOW. Defaults
to the position of point in the selected window.

DX and DY specify optional offsets from the top left of the glyph.

See also `completion-window-posn-at-point' and
`completion-frame-posn-at-point'.."

  (unless window (setq window (selected-window)))
  (unless position (setq position (window-point window)))
  (let ((x-y (posn-x-y (posn-at-point position window))))
    (cons (+ (car x-y) (or dx 0)) (+ (cdr x-y) (or dy 0)))))



(defun completion-frame-posn-at-point
  (&optional position window dx dy)
  "Return pixel position of top left corner of glyph at POSITION,
relative to top left corner of frame containing WINDOW. Defaults
to the position of point in the selected window.

DX and DY specify optional offsets from the top left of the glyph.

See also `completion-window-posn-at-point' and
`completion-window-inside-posn-at-point'."

  (unless window (setq window (selected-window)))
  (unless position (setq position (window-point window)))

  (let ((x-y (posn-x-y (posn-at-point position window)))
        (edges (window-inside-pixel-edges window)))
    (cons (+ (car x-y) (car  edges) (or dx 0))
          (+ (cdr x-y) (cadr edges) (or dy 0)))))




;;; ===============================================================
;;;                     Compatibility Stuff

;; prevent bogus compiler warnings
(eval-when-compile
  (defun completion--compat-window-offsets (dummy))
  (defun completion--compat-frame-posn-at-point
    (&optional arg1 arg2 arg3 arg4)))



(unless (fboundp 'posn-at-point)

  (defun completion--compat-frame-posn-at-point
    (&optional position window dx dy)
    "Return pixel position of top left corner of glyph at POSITION,
relative to top left corner of frame containing WINDOW. Defaults
to the position of point in the selected window.

DX and DY specify optional offsets from the top left of the
glyph."
    (unless window (setq window (selected-window)))
    (unless position (setq position (window-point window)))

    ;; get window-relative position in units of characters
    (let* ((x-y (compute-motion (window-start) '(0 . 0)
                                position
                                (cons (window-width) (window-height))
                                (window-width)
                                ; prob. shouldn't be 0
                                (cons (window-hscroll) 0)
                                window))
           (x (nth 1 x-y))
           (y (nth 2 x-y))
           (offset (completion--compat-window-offsets window))
           (restore (mouse-pixel-position))
           pixel-pos)

      ;; move and restore mouse position using position in units of
      ;; characters to get position in pixels
      (set-mouse-position (window-frame window)
                          (+ x (car offset)) (+ y (cdr offset)))
      (setq pixel-pos (cdr (mouse-pixel-position)))
      (set-mouse-pixel-position (car restore) (cadr restore)
                                (cddr restore))

      ;; return pixel position
      (setf (car pixel-pos) (+ (car pixel-pos) (or dx 0))
	    (cdr pixel-pos)
	    (+ (- (cdr pixel-pos)
		  (/ (frame-char-height (window-frame window)) 2))
	       (or dy 0)))
      pixel-pos))



  (defun completion--compat-posn-at-point-as-event
    (&optional position window dx dy)
    "Return pixel position of top left corner of glyph at POSITION,
relative to top left corner of WINDOW, as a mouse-1 click
event (identical to the event that would be triggered by clicking
mouse button 1 at the top left corner of the glyph).

POSITION and WINDOW default to the position of point in the
selected window.

DX and DY specify optional offsets from the top left of the
glyph."

    (unless window (setq window (selected-window)))
    (unless position (setq position (window-point window)))

    ;; get window-relative position in units of characters
    (let* ((x-y (compute-motion (window-start) '(0 . 0)
                                position
                                (cons (window-width) (window-height))
                                (window-width)
                                        ; prob. shouldn't be 0
                                (cons (window-hscroll) 0)
                                window))
           (x (nth 1 x-y))
           (y (nth 2 x-y))
           (offset (completion--compat-window-offsets window))
           (restore (mouse-pixel-position))
           (frame (window-frame window))
           (edges (window-edges window))
           pixel-pos)

      ;; move and restore mouse position using position in units of
      ;; characters to get position in pixels
      (set-mouse-position (window-frame window)
                          (+ x (car offset)) (+ y (cdr offset)))
      (setq pixel-pos (cdr (mouse-pixel-position)))
      (set-mouse-pixel-position (car restore) (cadr restore)
                                (cddr restore))

      ;; convert pixel position from frame-relative to window-relative
      ;; (this is crude and will fail e.g. if using different sized
      ;; fonts)
      (setcar pixel-pos (- (car pixel-pos) 1
                           (* (frame-char-width frame) (car edges))))
      (setcdr pixel-pos (- (cdr pixel-pos) 1
                           (* (frame-char-height frame) (nth 1 edges))
                           (/ (frame-char-height frame) 2)))

      ;; return a fake event containing the position
      (setcar pixel-pos (+ (car pixel-pos) (or dx 0)))
      (setcdr pixel-pos (+ (cdr pixel-pos) (or dy 0)))
      (list 'mouse-1 (list window position pixel-pos))))



  (defun completion--compat-window-posn-at-point
    (&optional position window dx dy)
    "Return pixel position of top left corner of glyph at POSITION,
relative to top left corner of WINDOW. Defaults to the position
of point in the selected window.

DX and DY specify optional offsets from the top left of the
glyph."
    (let ((x-y (completion--compat-frame-posn-at-point
		position window dx dy))
	  (win-x-y (completion--compat-window-offsets window)))
      (cons (+ (car x-y) (car win-x-y))
	    (+ (cdr x-y) (cdr win-x-y)))))



;;; Borrowed from senator.el (I promise I'll give it back when I'm
;;; finished...)

  (defun completion--compat-window-offsets (&optional window)
    "Return offsets of WINDOW relative to WINDOW's frame.
Return a cons cell (XOFFSET . YOFFSET) so the position (X . Y) in
WINDOW is equal to the position ((+ X XOFFSET) .  (+ Y YOFFSET))
in WINDOW'S frame."
    (let* ((window  (or window (selected-window)))
           (e       (window-edges window))
           (left    (nth 0 e))
           (top     (nth 1 e))
           (right   (nth 2 e))
           (bottom  (nth 3 e))
           (x       (+ left (/ (- right left) 2)))
           (y       (+ top  (/ (- bottom top) 2)))
           (wpos    (coordinates-in-window-p (cons x y) window))
           (xoffset 0)
           (yoffset 0))
      (if (consp wpos)
          (let* ((f  (window-frame window))
                 (cy (/ 1.0 (float (frame-char-height f)))))
            (setq xoffset (- x (car wpos))
                  yoffset (float (- y (cdr wpos))))
            ;; If Emacs 21 add to:
            ;; - XOFFSET the WINDOW left margin width.
            ;; - YOFFSET the height of header lines above WINDOW.
            (if (> emacs-major-version 20)
                (progn
                  (setq wpos    (cons (+ left xoffset) 0.0)
                        bottom  (float bottom))
                  (while (< (cdr wpos) bottom)
                    (if (eq (coordinates-in-window-p wpos window)
                            'header-line)
                        (setq yoffset (+ yoffset cy)))
                    (setcdr wpos (+ (cdr wpos) cy)))
                  (setq xoffset
                        (floor (+ xoffset
                                  (or (car (window-margins window))
                                      0))))))
            (setq yoffset (floor yoffset))))
      (cons xoffset yoffset)))


  (defun completion--compat-line-number-at-pos (pos)
    "Return (narrowed) buffer line number at position POS.
\(Defaults to the point.\)"
    (1+ (count-lines (point-min) pos)))


  (defalias 'completion-posn-at-point-as-event
    'completion--compat-posn-at-point-as-event)
  (defalias 'completion-frame-posn-at-point
    'completion--compat-frame-posn-at-point)
  (defalias 'completion-window-posn-at-point
    'completion--compat-window-posn-at-point)
)



;;; =================================================================
;;;                      Compatibility hacks

;; If the current Emacs version doesn't support overlay keybindings half
;; decently, have to simulate them using the
;; `completion--run-if-within-overlay' hack. So far, no Emacs version supports
;; things properly for zero-length overlays, so we always have to do this!

(when (<= emacs-major-version 21)
  (completion--simulate-overlay-bindings completion-overlay-map completion-map
                                        'completion-ui)
  (completion--simulate-overlay-bindings auto-completion-overlay-map
                                        auto-completion-map
                                        'auto-completion-mode t))




;;; ================================================================
;;;                Load user-interface modules

(provide 'completion-ui)
(require 'completion-ui-popup-frame)
(require 'completion-ui-dynamic)
(require 'completion-ui-hotkeys)
(require 'completion-ui-echo)
(require 'completion-ui-tooltip)
(require 'completion-ui-menu)
(require 'completion-ui-sources)



;;; completion-ui.el ends here
