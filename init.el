;; init.el --- coldnew's Emacs Configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2010-2026 Yen-Chin, Lee <coldnew.tw@gmail.com>

;;             DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
;;                    Version 2, December 2024
;;
;; Everyone is permitted to copy and distribute verbatim or modified
;; copies of this license document, and changing it is allowed as long
;; as the name is changed.
;;
;;            DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
;;   TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION
;;
;;  0. You just DO WHAT THE FUCK YOU WANT TO.

;;; Commentary:

;;
;; My Emacs configuration migrated from org-mode to outshine format.
;; Use C-c @ to toggle outline-minor-mode for navigation.
;; All org-mode outline navigation keys work (C-c n/p/f/b).

;; This file is NOT part of GNU Emacs.

;;; Code:

;;
;; * Core Configuration
;;
;;   Foundation settings for Emacs behavior including environment setup,
;;   paths, personal information, and customization handling.
;;   All other sections depend on this configuration.
;;

;; ** Emacs Compatibility and Core Libraries

;;
;; *** Use Common Lisp Extension
;;
;;   Some of my function may need the Common Lisp Extension, let's
;;   import libraries first.

(require 'cl-lib)                       ; built-in

;; *** Load extra builtin library
;;
;;   Add some extra buildin library I will use in my config file.

(require 'find-lisp)

;; ** Directory Variables Setup
;;
;;   In this configuration, =user-emacs-directory= always refer to the
;;   emacs configuration's init.el parent directory.
;;
;;   We set `user-emacs-directory' here so we can use command-line
;;   switch different emacs configuration like following:
;;
;;      emacs -q -l ~/coldnew-spacemacs/init.el

(defconst user-emacs-directory
  (file-name-directory (or load-file-name (buffer-file-name)))
  "My Emacs config directory.")

;;   Setup the cache directory to store some cache content.

(defconst user-cache-directory
  (file-name-as-directory (concat user-emacs-directory ".cache"))
  "My Emacs storage area for persistent files.")
;; create the `user-cache-directory' if not exists
(make-directory user-cache-directory t)

;;
;;   I specify a ramdisk path to make my emacs can save some temporary
;;   file to it. The ramdisk path should be =~/ramdisk=, if the
;;   directory not found, use =/tmp= as fallback.

(defconst user-ramdisk-directory
  (let ( (user-ramdisk                   ; ~/ramdisk/
          (concat (getenv "HOME") "/ramdisk/")))
    ;; if ~/ramdisk/ exist, use it
    (if (file-exists-p user-ramdisk)
        user-ramdisk
      ;; fallback to system default ramdisk dir
      temporary-file-directory))
  "My ramdisk path in system.")

;; ** Personal Information

(setq user-full-name "Yen-Chin, Lee")
(setq user-mail-address "coldnew.tw@gmail.com")

;; ** Load Path Setup
;;
;;   The variable =load-path= lists all the directories where Emacs
;;   should look for emacs-lisp files.
;;

(eval-and-compile
  (dolist (dir '("local-lisp" "styles"))
    (let ((full-dir (expand-file-name dir user-emacs-directory)))
      (when (and full-dir (file-exists-p full-dir))
        (add-to-list 'load-path full-dir)))))


;; ** Environment Setup (macOS)

;; Under Mac OSX use Command key as ALT
;;
;;   Under Mac OSX, I always bind =Caps lock= as Control key, and make
;;   the =Command= key as =ALT= key like I done in Linux.
;;
;;   The =Option= key will be setup as =Super=.
;;
;;   I also disable some keys like =⌘-h= bypass to system in emacs-mac
;;   port.

(when (eq system-type 'darwin)
  (setq-default mac-option-modifier 'super)
  (setq-default mac-command-modifier 'meta))

;; ** Customization and Backup
;;
;; Save custom-file as cache
;;
;;   Most of my config is written in this file, it's no need to
;;   tracking the emacs's custom-setting.
;;
;;   I move the file to cache-dir and make git ignore it.

(setq-default custom-file (concat user-cache-directory "custom.el"))
;; load custom-file only when file exist
(when (file-exists-p custom-file)
  (load-file custom-file))

;; Setup backup files
;;
;;   By default Emacs saves =BackupFiles= under the original name with
;;   a tilde =~= appended. Example: Editing README will result in
;;   README and README~ in the same directory.
;;
;;   This is primitive and boring.
;;
;;   I save my backup files to =~/.emacs.d/.cache/backup= and since is
;;   always ignore by version control system, it's a nice place to
;;   store backup files.

(let ((backup-dir (concat user-cache-directory "backup")))
  ;; Move backup file to `~/.emacs.d/.cache/backup'
  (setq backup-directory-alist `(("." . ,backup-dir)))
  ;; Make sure backup directory exist
  (when (not (file-exists-p backup-dir))
    (make-directory backup-dir t)))

(setq delete-by-moving-to-trash nil)
(setq version-control t)
(setq kept-old-versions 10)
(setq kept-new-versions 20)
(setq delete-old-versions t)
(setq backup-by-copying t)

;; ** Startup Configuration

;; Startup emacs server
;;
;;   Only start server mode if I'm not root

(unless (string-equal "root" (getenv "USER"))
  (require 'server)
  (when (fboundp 'server-running-p)
    (unless (server-running-p) (server-start))))

;; ** Disable Default Stuff

;; Turn-off Alarm Bell

(setq ring-bell-function #'ignore)

;; Clean scratch buffer messages
;;
;;   Leave me a clean **scratch** buffer and I'll be more happy :)

(setq initial-scratch-message "")

;; Use visible bell instead of buzzer

(setq visible-bell t)

;; Ask for y or n, not yes or no
;;
;;   Emacs starts out asking for you to type yes or no with most
;;   important questions. Just let me use =y= or =n= with no =RET=
;;   required and I'm quite happy.

(defalias 'yes-or-no-p 'y-or-n-p)

;; Maximized window after emacs start

(modify-all-frames-parameters '((fullscreen . maximized)))

;; ** Built-in Package Configuration

;; Languages and Encodings
;;
;;   Add UTF8 at the front of the priority list for automatic detection.

(prefer-coding-system 'utf-8)

;; Set up multilingual environment to use UTF-8.

(set-language-environment "UTF-8")

;; Set default value of various coding systems to UTF-8.

(set-default-coding-systems 'utf-8)

;; Use =C= as locale for display time info (Actually it will display
;; English).

(setq system-time-locale "C")

;; ** Built-in Packages Cache Redirection

;; abbrev

(eval-after-load 'bookmark
  '(progn
     (setq abbrev-file-name
           (concat user-cache-directory "abbrev_defs"))))

;; eshell
;;
;;   Move eshell cache dir to =~/.emacs.d/.cache/eshell=

(eval-when-compile (defvar eshell-directory-name)) ; defined in esh-mode.el

(with-eval-after-load 'esh-mode
  (setq-default eshell-directory-name
                (concat user-cache-directory "eshell")))

(with-eval-after-load 'em-hist
  (setq-default eshell-history-file-name
                (expand-file-name "history" eshell-directory-name)))

;; bookmark

(eval-after-load 'bookmark
  '(progn
     (setq-default bookmark-default-file
                   (concat user-cache-directory "bookmarks"))))

;; idlwave
;;
;;   Major mode for editing IDL source files.

(eval-after-load 'idlwave
  '(progn
     (setq-default idlwave-config-directory
		   (concat user-cache-directory "idlwave"))))

;; srecode

;; change srecode cache file path
(eval-after-load 'srecode
  '(progn
     (setq-default srecode-map-save-file
                   (concat user-cache-directory "srecode-map.el"))))

;; request

(eval-after-load 'request
  '(progn
     (setq-default request-storage-directory
                   (concat user-cache-directory "request"))))

;; nsm

(eval-after-load 'nsm
  '(progn
     (setq-default nsm-settings-file
                   (concat user-cache-directory "network-security.data"))))

;; url

(eval-after-load 'url
  '(progn
     (setq url-configuration-directory
           (file-name-as-directory
            (concat user-cache-directory "url")))))

;; startup
;;
;;   NOTE: `auto-save-list-file-prefix' defined in startup.el, but
;;   startup.el doesn't have provide pacage name (provide 'startup)

(setq-default auto-save-list-file-prefix
              (cond ((eq system-type 'ms-dos)
                     ;; MS-DOS cannot have initial dot, and allows only 8.3 names
                     (file-name-as-directory
                      (concat user-cache-directory "auto-save.list/_saves-")))
                    (t
                     (file-name-as-directory
                      (concat user-cache-directory "auto-save-list/.saves-")))))

;; * Package Management
;;
;;   Sets up the package ecosystem using Elpaca (modern package manager)
;;   integrated with use-package for declarative package configuration.
;;   This section establishes the foundation for all external packages.
;;

;; ** Elpaca setup
;;
;;   Elpaca is an elisp package manager. It clones and manages packages
;;   from their repositories. We integrate it with use-package for
;;   seamless package configuration.
;;
;;   GitHub: https://github.com/progfolio/elpaca

;; Setup elpaca-use-package-mode for use-package integration
(elpaca elpaca-use-package
  (elpaca-use-package-mode))

;; Set use-package to always ensure packages are installed
(setq use-package-always-ensure t)

(eval-when-compile (require 'use-package))

;; ** Global use-package settings
;;    Defer all packages by default for faster startup.
;;    Use :demand t for packages needed immediately.

(setq use-package-always-defer t)

;; ** Package reporting settings

(setq use-package-verbose t)


;; ** Common Utility Libraries
;;
;;   Some common libraries I'll use in my personal's command or anything else.

;; ** Install some common libraries
;;
;;   Some common libraries I'll use in my personal's command or anything else.

;; *** f
;;
;;   Modern API for working with files and paths in Emacs Lisp.
;;
;;   Key features:
;;   - Path manipulation functions (join, expand, dirname, basename)
;;   - File operations (read, write, copy, delete)
;;   - Directory operations (mkdir, rmdir, ls)
;;   - Path predicates (file?, dir?, absolute?, relative?)
;;   - Cross-platform path handling
;;
;;   Why I use it:
;;   Provides a clean, consistent API for file operations that works
;;   across different Emacs versions and platforms. Much better than
;;   built-in functions for complex path manipulations.
;;
;;   GitHub: https://github.com/rejeep/f.el
;;
;;   Configuration notes:
;;   Pure library, no configuration needed.

(use-package f :ensure t)
;; *** s
;;
;;   String manipulation library inspired by Clojure's string functions.
;;
;;   Key features:
;;   - String transformations (upcase, downcase, capitalize)
;;   - String predicates (blank?, empty?, present?)
;;   - String splitting and joining
;;   - String trimming and cleaning
;;   - Regex operations
;;
;;   Why I use it:
;;   Consistent and readable string operations. Avoids verbose
;;   built-in string functions and regexps for common tasks.
;;
;;   GitHub: https://github.com/magnars/s.el
;;
;;   Configuration notes:
;;   Pure library, no configuration needed.

(use-package s :ensure t)
;; *** dash
;;
;;   List manipulation library providing functional programming utilities.
;;
;;   Key features:
;;   - List operations (--map, --filter, --reduce)
;;   - List transformations (--flatten, --distinct, --sort)
;;   - List queries (--first, --last, --find)
;;   - Threading macros (->, ->>)
;;   - Destructuring
;;
;;   Why I use it:
;;   Makes list processing more readable and functional. Essential
;;   for complex data transformations in Emacs Lisp.
;;
;;   GitHub: https://github.com/magnars/dash.el
;;
;;   Configuration notes:
;;   Pure library, no configuration needed.

(use-package dash :ensure t)
;; *** htmlize
;;
;;   Convert buffer text and faces to HTML.
;;
;;   Key features:
;;   - Export buffers to HTML with syntax highlighting
;;   - Preserve faces and colors
;;   - Support for custom CSS
;;   - Batch processing
;;
;;   Why I use it:
;;   Essential for exporting code with syntax highlighting to HTML.
;;   Used by various export and blogging tools.
;;
;;   GitHub: https://github.com/hniksic/emacs-htmlize
;;
;;   Configuration notes:
;;   Used by org-mode and other export tools.

(use-package htmlize :ensure t)
;; *** async
;;
;;   Asynchronous processing in Emacs Lisp.
;;
;;   Key features:
;;   - Run functions asynchronously
;;   - Start external processes without blocking
;;   - Parallel execution
;;   - Callback handling
;;
;;   Why I use it:
;;   Prevents UI freezing during long operations. Improves
;;   responsiveness for tasks like package installation or
;;   heavy computations.
;;
;;   GitHub: https://github.com/jwiegley/emacs-async
;;
;;   Configuration notes:
;;   Pure library, integrates with various packages.

(use-package async :ensure t)

;; * Theme & Fonts
;;
;;   User interface customization including themes, fonts, colors,
;;   and visual appearance settings.
;;
;;
;; ** Theme
;;
;;   Before use emacs's =load-theme= function, I advise it to make it
;;   fully unload previous theme before loading a new one.

;; Make `load-theme' fully unload previous theme before loading a new one.
(defadvice load-theme
    (before theme-dont-propagate activate)
  (mapc #'disable-theme custom-enabled-themes))

(require 'day-coldnew-theme)
(require 'night-coldnew-theme)
(load-theme 'night-coldnew t nil)  ; default use `night-coldnew-theme'

;; * Fonts
;;
;;   Font configuration for English and CJK characters.

(defvar my/emacs-english-font "Monaco"
  "The font name of English.")

(defvar my/emacs-cjk-font "Hiragino Sans GB"
  "The font name for CJK.")

(defvar my/emacs-font-size-pair '(13 . 16)
  "Default font size pair for (english . chinese)")

(defun my/font-exist-p (fontname)
  "Test if this font is exist or not.
This function only work on GUI mode, on terminal it just
return nil since you can't set font for emacs on it."
  (if (or (not fontname) (string= fontname "") (not (display-graphic-p)))
      nil
    (if (not (x-list-fonts fontname))
        nil t)))

(defun my/set-font (english chinese size-pair)
  "Setup emacs English and Chinese font on x window-system."

  (if (my/font-exist-p english)
      (set-frame-font (format "%s:pixelsize=%d" english (car size-pair)) t))

  (if (my/font-exist-p chinese)
      (dolist (charset '(kana han cjk-misc bopomofo))
        (set-fontset-font (frame-parameter nil 'font) charset
                          (font-spec :family chinese :size (cdr size-pair))))))

;; Setup font size based on my/emacs-font-size-pair
(my/set-font my/emacs-english-font my/emacs-cjk-font my/emacs-font-size-pair)

(defvar my/emacs-font-size-pair-list
  '(( 5 .  6) (10 . 12)
    (13 . 16) (15 . 18) (17 . 20)
    (19 . 22) (20 . 24) (21 . 26)
    (24 . 28) (26 . 32) (28 . 34)
    (30 . 36) (34 . 40) (36 . 44))
  "This list is used to store matching (english . chinese) font-size.")

(defun my/emacs-step-font-size (step)
  "Increase/Decrease emacs's font size."
  (let ((scale-steps my/emacs-font-size-pair-list))
    (if (< step 0) (setq scale-steps (reverse scale-steps)))
    (setq my/emacs-font-size-pair
          (or (cadr (member my/emacs-font-size-pair scale-steps))
              my/emacs-font-size-pair))
    (when my/emacs-font-size-pair
      (message "emacs font size set to %.1f" (car my/emacs-font-size-pair))
      (my/set-font my/emacs-english-font my/emacs-cjk-font my/emacs-font-size-pair))))

(defun my/increase-emacs-font-size ()
  "Increase emacs's font-size according emacs-font-size-pair-list."
  (interactive) (my/emacs-step-font-size 1))

(defun my/decrease-emacs-font-size ()
  "Decrease emacs's font-size according emacs-font-size-pair-list."
  (interactive) (my/emacs-step-font-size -1))

;; ** Setup Keybinds

(bind-keys :map global-map
           ("C-=" . my/increase-emacs-font-size)
           ("C--" . my/decrease-emacs-font-size))

;; ** Minibuffer

(when (require 'minibuffer nil t)  ; built-in, so optional require
  ;; Use bar cursor in minibuffer
  (add-hook 'minibuffer-setup-hook (lambda () (setq cursor-type 'bar)))

  ;; Helper to clean minibuffer and insert new content
  (defun my/minibuffer-insert (str)
    "Clean minibuffer and insert STR as new content."
    (when (minibufferp)
      (delete-minibuffer-contents)
      (insert str)))

  ;; Keybindings with inline lambdas (no separate interactive functions needed)
  (bind-keys :map minibuffer-local-map
             ("C-w" . backward-kill-word)
             ("M-p" . previous-history-element)
             ("M-n" . next-history-element)
             ("C-g" . my/minibuffer-keyboard-quit)
             ("M-t" . (lambda () (interactive) (my/minibuffer-insert user-ramdisk-directory)))
             ("M-h" . (lambda () (interactive) (my/minibuffer-insert (expand-file-name "~/"))))
             ("M-/" . (lambda () (interactive) (my/minibuffer-insert "/")))
             ("M-s" . (lambda () (interactive) (my/minibuffer-insert "/ssh:")))))

;; *** savehist
;;
;;   Save minibuffer history across Emacs sessions. Automatically saves
;;   your command history, search history, and other variables.
;;
;;   Key features:
;;   - Save minibuffer history across sessions
;;   - Configurable save interval
;;   - Save additional variables beyond minibuffer
;;   - Merge history from multiple sessions
;;   - Built-in Emacs feature
;;
;;   Why I use it:
;;   Maintains context between Emacs sessions. Don't lose your
;;   frequently-used commands, search terms, and completion history.
;;
;;   Built-in since Emacs 21.
;;
;;   Configuration notes:
;;   History saved to ~/.emacs.d/.cache/savehist.dat.
;;   Also saves corfu completion history.

(use-package savehist
  :ensure nil                           ; built-in
  :commands (savehist-mode)
  :hook (after-init . savehist-mode)
  :config
  (setq savehist-file (expand-file-name "savehist.dat" user-cache-directory))
  (add-to-list 'savehist-additional-variables 'corfu-history))

;;
;; * Editing and Development Tools
;;
;;   Editor enhancements that work across all modes: text editing utilities,
;;   navigation helpers, visual aids, undo management, and general
;;   editor behaviors that improve the editing experience regardless of language.
;;

;; ** Editor Behaviors

;; *** exec-path-from-shell
;;
;;   exec-path-from-shell is A GNU Emacs library to ensure environment
;;   variables inside Emacs look the same as in the user's shell.
;;
;;   Ever find that a command works in your shell, but not in Emacs?
;;
;;   This happens a lot on OS X, where an Emacs instance started from
;;   the GUI inherits a default set of environment variables.
;;
;;   This library works solves this problem by copying important
;;   environment variables from the user's shell: it works by asking
;;   your shell to print out the variables of interest, then copying
;;   them into the Emacs environment.
;;
;;   GitHub: https://github.com/purcell/exec-path-from-shell

(use-package exec-path-from-shell
  :ensure t
  :commands (exec-path-from-shell-initialize)
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; *** which-key
;;
;;   which-key is a minor mode for Emacs that displays the key bindings
;;   following your currently entered incomplete command (a prefix) in
;;   a popup.
;;
;;   GitHub: https://github.com/justbur/emacs-which-key

(use-package which-key
  :ensure t
  :demand t  ; Needed for keybinding display at startup
  :commands (which-key-mode)
  :config
  (which-key-mode)
  ;; Reset to the default or customized value before adding our values in order
  ;; to make this initialization code idempotent.
  (custom-reevaluate-setting 'which-key-replacement-alist)
  ;; Use my own rules for better naming of functions
  (let ((desc
         ;; being higher in this list means the replacement is applied later
         '(("er/expand-region" . "expand region")
           ("evil-lisp-state-\\(.+\\)" . "\\1")
           ;; my own commands prefix with `my/'
           ("my/\\(.+\\)" . "\\1")
           )))
    (dolist (nd desc)
      ;; ensure the target matches the whole string
      (push (cons (cons nil (concat "\\`" (car nd) "\\'")) (cons nil (cdr nd)))
            which-key-replacement-alist))))

;; *** manage-minor-mode
;;
;;   manage-minor-mode provides an interactive interface to manage all
;;   your minor modes from a dedicated buffer.
;;
;;   Key features:
;;   - List all active and inactive minor modes
;;   - Toggle minor modes on/off with keybindings
;;   - Organized by buffer or globally
;;   - Visual indicators for mode status
;;
;;   Why I use it:
;;   Emacs accumulates many minor modes; this makes it easy to
;;   understand and control what's active without digging through
;;   mode-line.
;;
;;   GitHub: https://github.com/ShingoFukuyama/manage-minor-mode
;;
;;   Configuration notes:
;;   Use `M-x manage-minor-mode` to open the management interface.

(use-package manage-minor-mode
  :ensure t
  :commands (manage-minor-mode))

;; ** Text Editing and Manipulation

;; *** expand-region
;;
;;   Expand region increases the selected region by semantic units.
;;   Just keep pressing the key until it selects what you want.
;;
;;   Key features:
;;   - Expands selection by semantic units (word, sentence, paragraph, etc.)
;;   - Supports programming structures (functions, blocks, statements)
;;   - Smart expansion based on context
;;   - Works with all major modes
;;
;;   Why I use it:
;;   Much faster than manual selection, especially for code blocks or
;;   complex text structures. One keypress repeatedly expands to
;;   desired scope.
;;
;;   GitHub: https://github.com/magnars/expand-region.el
;;
;;   Configuration notes:
;;   Bound to M-v in my config. Press repeatedly to expand.

(use-package expand-region
  :ensure t
  :commands (er/expand-region er/contract-region)
  :bind (("M-v" . er/expand-region)))

;; *** iedit
;;
;;   iedit lets you edit multiple regions in the same way simultaneously.
;;   Like multiple-cursors but uses Emacs overlays.
;;
;;   Key features:
;;   - Edit all occurrences of a symbol/word at once
;;   - Live editing with immediate feedback
;;   - Toggle between occurrences
;;   - Case-sensitive matching option
;;
;;   Why I use it:
;;   Powerful for renaming variables or refactoring code. Much
;;   lighter than multiple-cursors package.
;;
;;   GitHub: https://github.com/victorhge/iedit
;;
;;   Configuration notes:
;;   Bound to C-;. Activate by selecting a word/region.

(use-package iedit
  :ensure t
  :commands (iedit-mode)
  :bind (("C-;" . iedit-mode)))

;; *** smartparens
;;
;;   Smartparens is a minor mode for dealing with pairs in Emacs.
;;   Provides automatic insertion, wrapping, and navigation of brackets.
;;
;;   Key features:
;;   - Auto-close brackets, quotes, and pairs
;;   - Wrap regions with pairs
;;   - Navigate between pairs (C-M-f, C-M-b)
;;   - Delete balanced pairs
;;   - Lisp-aware (works with S-expressions)
;;   - Extensive configuration options
;;
;;   Why I use it:
;;   Eliminates need to manually type closing brackets. Makes
;;   Lisp coding much faster with proper pair handling.
;;
;;   GitHub: https://github.com/Fuco1/smartparens
;;
;;   Configuration notes:
;;   Enabled globally. See smartparens docs for keybindings.

(use-package smartparens
  :ensure t
  :commands (smartparens-mode show-smartparens-global-mode)
  :hook (prog-mode . smartparens-mode)
  :config
  (require 'smartparens-config))

;; *** hungry-delete
;;
;;   hungry-delete borrows hungry deletion from =cc-mode=, which causes
;;   deletion to delete all whitespace in the direction you are deleting.
;;   Similar to Vim's "hungry delete" behavior.
;;
;;   Key features:
;;   - Delete all whitespace to next non-whitespace character
;;   - Works in both directions
;;   - Configurable characters to treat as whitespace
;;   - Can be toggled per-buffer
;;
;;   Why I use it:
;;   Saves time by deleting multiple spaces/tabs with one keystroke.
;;   Essential for cleaning up indented code.
;;
;;   GitHub: https://github.com/pcolby/hungry-delete
;;
;;   Configuration notes:
;;   Enabled globally. Bound to C-d and C-l in evil insert state.

(use-package hungry-delete
  :ensure t
  :commands (global-hungry-delete-mode hungry-delete-mode)
  :hook (after-init . global-hungry-delete-mode))

;; *** mwim
;;
;;   mwim (Move Where I Mean) provides smart commands for moving to
;;   beginning/end of code or line. Toggles between two positions.
;;
;;   Key features:
;;   - First press: move to code beginning/end
;;   - Second press: move to line beginning/end
;;   - Respects indentation and whitespace
;;   - Works in all modes
;;
;;   Why I use it:
;;   Solves the frustration of C-a/C-e not going where you want.
;;   Toggle between code and line position with same key.
;;
;;   GitHub: https://github.com/alezost/mwim.el
;;
;;   Configuration notes:
;;   Bound to C-a (beginning) and C-e (end). Press twice for line.

(use-package mwim
  :ensure t
  :bind
  (("C-a" . mwim-beginning-of-code-or-line)
   ("C-e" . mwim-end-of-code-or-line)))

;; *** visual-regexp
;;
;;   visual-regexp for Emacs is like replace-regexp, but with live
;;   visual feedback directly in the buffer. Shows matches as you type.
;;
;;   Key features:
;;   - Real-time highlighting of regex matches
;;   - Visual feedback for replacement strings
;;   - Supports both replace-regexp and replace-string
;;   - Can use python-mode regex syntax
;;   - Preview replacements before applying
;;
;;   Why I use it:
;;   Eliminates regex anxiety by showing matches immediately. No more
;;   guessing if your regex is correct.
;;
;;   GitHub: https://github.com/benma/visual-regexp.el
;;
;;   Configuration notes:
;;   Use `M-x vr/replace` for visual replacement. Install
;;   visual-regexp-steroids for additional features.

(use-package visual-regexp
  :ensure t
  :commands (vr/replace vr/query-replace))

;; *** fancy-narrow
;;
;;   Fancy narrow enhances Emacs' built-in narrow-to-region with
;;   visual feedback and improved UX. Shows what's narrowed.
;;
;;   Key features:
;;   - Visual indicator of narrowed region
;;   - Highlighting of active content
;;   - Easier to widen/narrow
;;   - Maintains undo history
;;   - Better than built-in narrow commands
;;
;;   Why I use it:
;;   Makes narrow-to-region more discoverable. Visual feedback
;;   prevents confusion about what's currently focused.
;;
;;   GitHub: https://github.com/Malabarba/fancy-narrow
;;
;;   Configuration notes:
;;   Use `M-x fancy-narrow-to-region` to narrow. Use `C-x n w` to widen.

(use-package fancy-narrow
  :ensure t
  :commands (fancy-narrow-to-region fancy-widen)
  :hook (prog-mode . fancy-narrow-mode))

;; ** Navigation and Movement

;; *** ace-jump-mode
;;
;;   Ace-jump allows you to jump to any visible character on screen quickly.
;;   Type the key sequence and then the character you want to jump to.
;;   Like Vim's easymotion but for Emacs.
;;
;;   Key features:
;;   - Jump to any character in 2-3 keystrokes
;;   - Shows hints on all jump targets
;;   - Works across all windows
;;   - Supports word-line-char modes
;;   - No mouse needed
;;
;;   Why I use it:
;;   Faster than searching or scrolling. Essential for navigating
;;   within visible area quickly.
;;
;;   GitHub: https://github.com/winterTTr/ace-jump-mode
;;
;;   Configuration notes:
;;   Bound to C-c SPC. Type prefix, then hint key to jump.

(use-package ace-jump-mode
  :ensure t
  :commands (ace-jump-mode ace-jump-char-mode ace-jump-line-mode)
  :bind (("C-c SPC" . ace-jump-mode)))

;; *** goto-last-change
;;
;;   Move point through buffer-undo-list positions. Allows you to
;;   jump to previous edit locations in the buffer.
;;
;;   Key features:
;;   - Jump to last edit position
;;   - Cycle through edit history
;;   - Works across undo operations
;;   - Shows where you were editing
;;   - Similar to Vim's `` mark
;;
;;   Why I use it:
;;   After undo or context switch, quickly return to where you
;;   were editing. Essential for refactoring workflows.
;;
;;   GitHub: https://github.com/camdez/goto-last-change.el
;;
;;   Configuration notes:
;;   No default binding. Consider binding to C-o or similar.

(use-package goto-last-change
  :ensure t
  :commands (goto-last-change goto-last-change-reverse))

;; ** Visual Enhancements

;; *** rainbow-mode
;;
;;   rainbow-mode is a minor mode for Emacs which displays strings
;;   representing colors with the color they represent as background.
;;   Great for CSS, HTML, and color-coded configurations.
;;
;;   Key features:
;;   - Color strings show as their actual color
;;   - Supports hex (#fff), rgb, hsl, and more
;;   - Works in CSS, HTML, Lisp, and other modes
;;   - Toggle on/off per buffer
;;   - Live updates as you edit
;;
;;   Why I use it:
;;   Makes color selection intuitive. No need to remember hex codes
;;   or preview externally.
;;
;;   GitHub: https://github.com/emacsmirror/rainbow-mode
;;
;;   Configuration notes:
;;   Enable with `M-x rainbow-mode`. Auto-enable for CSS/HTML recommended.

(use-package rainbow-mode
  :ensure t
  :commands (rainbow-mode)
  :hook (css-mode html-mode web-mode sass-mode less-css-mode scss-mode))

;; *** focus
;;
;;   Focus provides `focus-mode` that dims the text of surrounding
;;   sections, similar to iA Writer's Focus Mode. Highlights
;;   only the paragraph or section you're working on.
;;
;;   Key features:
;;   - Dim inactive text for distraction-free writing
;;   - Auto-expand current section
;;   - Configurable focus depth
;;   - Works in all text modes
;;   - Great for long documents
;;
;;   Why I use it:
;;   Helps maintain focus by visually isolating current section.
;;   Excellent for writing and documentation tasks.
;;
;;   GitHub: https://github.com/larstvei/Focus
;;
;;   Configuration notes:
;;   Use `M-x focus-mode` to toggle. Use `M-x focus-pin` to
;;   pin current region permanently.

(use-package focus
  :ensure t
  :commands (focus-mode focus-pin))

;; ** Undo and History Management

;; *** undo-tree
;;
;;   undo-tree provides a tree-like visualization of Emacs undo history.
;;   Unlike linear undo, you can branch off and explore alternate edits.
;;
;;   Key features:
;;   - Tree visualization of undo history
;;   - Branch to previous states after multiple undos
;;   - Timestamp and diff visualization
;;   - Persistent history across Emacs sessions
;;   - Navigate with undo/redo without losing states
;;
;;   Why I use it:
;;   Solves the "undo too far, lost work" problem. Can explore
;;   alternate edit paths and recover any previous state.
;;
;;   GitHub: https://github.com/emacsmirror/undo-tree
;;
;;   Configuration notes:
;;   History saved to ~/.emacs.d/.cache/undo-tree/.
;;   Use `C-x u` to visualize tree, `q` to exit.

(use-package undo-tree
  :ensure t
  :commands (global-undo-tree-mode undo-tree-visualize)
  :config
  ;; Persistent undo-tree history across emacs sessions
  (let ((dir
         (file-name-as-directory (concat user-cache-directory "undo-tree"))))
    (setq undo-tree-history-directory-alist `(("." . ,dir))))
  ;; Make undo-tree save history
  (setq undo-tree-auto-save-history t)
  ;; global enable undo-tree
  (global-undo-tree-mode))


;; ** General Editor Configuration
;;
;;   General editor configuration that applies across all modes:
;;   file synchronization, locking, encryption, remote editing, and
;;   editor-wide behavior settings.
;;
;; *** Keeping files in sync
;;
;;   By default, Emacs will not update the contents of open buffers
;;   when a file changes on disk. This is inconvenient when switching
;;   branches in Git - as you'd risk editing stale buffers.

(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)
(setq revert-without-query '(".*")) ;; disable revert query

;; *** Disable lock file
;;
;;   I don't want emacs create some temporary file like =.#-emacs-a08196=,
;;   disable it.

;; https://www.emacswiki.org/emacs/LockFiles
(when (version<= "24.3" emacs-version)
  (setq create-lockfiles nil))

;; *** En/Decrypt files by EasyPG

(require 'epa-file)			; part of emacs
;; Enable epa, so I can use gnupg in emacs to en/decrypt file
(epa-file-enable)
;; Control whether or not to pop up the key selection dialog.
(setq epa-file-select-keys 0)
;; Cache passphrase for symmetric encryption.
(setq epa-file-cache-passphrase-for-symmetric-encryption t)

(require 'epa)				; built-in
(setq epg-pinentry-mode 'loopback)

(use-package pinentry
  :ensure t
  :commands (pinentry-start)
  :config
  ;; Start the Pinentry service
  (pinentry-start))

;;
;; * Completion Framework
;;
;;   Modern completion stack: Vertico (minibuffer), Corfu (in-buffer),
;;   Consult (search), Marginalia (annotations), and Orderless (flexible
;;   matching). Provides fast, intuitive completion throughout Emacs.
;;

;; ** Vertico Stack (Minibuffer Completion)

;; *** vertico
;;
;;   Vertico provides a vertical completion UI for the minibuffer.
;;   It's minimalistic, fast, and highly composable with other
;;   completion packages in the ecosystem.
;;
;;   Key features:
;;   - Vertical display of candidates (like helm, but simpler)
;;   - Fuzzy matching with orderless
;;   - Candidate preview with consult
;;   - Compatible with savehist and corfu
;;
;;   Why I use it:
;;   Replaces helm with a more modular, faster alternative that
;;   integrates seamlessly with consult and corfu.
;;
;;   GitHub: https://github.com/minad/vertico

(use-package vertico
  :ensure t
  :demand t
  :config
  (vertico-mode 1)
  ;; Show more candidates (default is 10)
  (setq vertico-count 15)
  ;; Resize vertico window to fit content
  (setq vertico-resize t))

;; *** marginalia
;;
;;   Marginalia enriches the minibuffer completion by adding helpful
;;   annotations to the side of completion candidates (like descriptions,
;;   file sizes, function signatures, etc.).
;;
;;   Key features:
;;   - Annotates file completions with size, mode
;;   - Shows function signatures for completions
;;   - Displays command descriptions
;;   - Works with vertico and consult
;;
;;   Why I use it:
;;   Provides context that makes completion decisions faster without
;;   opening documentation.
;;
;;   GitHub: https://github.com/minad/marginalia

(use-package marginalia
  :ensure t
  :demand t
  :config
  (marginalia-mode 1))

;; *** orderless
;;
;;   Orderless provides a powerful completion style that matches
;;   components in any order (space-separated), making fuzzy search
;;   much more intuitive.
;;
;;   Key features:
;;   - Space-separated matching (e.g., "ff p" matches "preffile")
;;   - Literal strings with quotes
;;   - Multiple styles can be chained
;;   - Fast regex-based implementation
;;
;;   Why I use it:
;;   Makes completion much more flexible without needing complex
;;   configuration.
;;
;;   GitHub: https://github.com/oantolin/orderless

(use-package orderless
  :ensure t
  :demand t
  :config
  (setq completion-styles '(orderless basic)))

;; *** consult
;;
;;   Consult provides a suite of useful search and navigation commands
;;   built on top of Emacs' completion infrastructure. It's the
;;   modern successor to swiper/ivy.
;;
;;   Key features:
;;   - `consult-line`: Jump to line in buffer (like swiper)
;;   - `consult-find`: Find files async
;;   - `consult-ripgrep`: Search with ripgrep
;;   - `consult-yank-pop`: Browse kill ring
;;   - `consult-grep`: Grep through files
;;   - Integration with vertico for completion UI
;;
;;   Why I use it:
;;   Replaces ivy/swiper with faster, composable alternatives that
;;   leverage native Emacs completion.
;;
;;   GitHub: https://github.com/minad/consult

(use-package consult
  :ensure t
  :demand t
  :bind (("C-x C-f" . find-file)
         ("C-x b" . switch-to-buffer)
         ("M-s" . consult-line)
         ("M-y" . consult-yank-pop)
         ("C-x C-r" . consult-recent-file)
         ("C-c g" . consult-gnus)
         ("C-c h" . consult-history))
  :config
  (setq consult-preview-key '(:debounce 0.2 any))
  ;; Use consult for grep/ag search
  (setq consult-async-refresh-debounce 0.2)
  (setq consult-async-input-throttle 0.1)
  ;; Customize consult-line for better display
  (consult-customize consult-line
                     :preview-key '(:debounce 0.3 any)
                     ;; Show more context in preview (lines before/after match)
                     (setq consult-line-numbers-widen t)))

;; *** rg
;;
;;   rg is the ripgrep search tool integration for Emacs. Ripgrep
;;   is an extremely fast text search tool that respects .gitignore.
;;
;;   Key features:
;;   - Async search with ripgrep
;;   - Live filtering of results
;;   - Jump to results easily
;;   - Works with consult
;;
;;   Why I use it:
;;   Faster than grep, respects gitignore, and integrates perfectly
;;   with consult for async search.
;;
;;   GitHub: https://github.com/dajva/rg.el
;;   Requires: ripgrep (install from package manager)

(use-package rg
  :ensure t
  :if (executable-find "rg")
  :commands (rg rg-project rg-literal))

;; ** Corfu (In-Buffer Completion)

;; corfu
;;
;;   Corfu is a minimalistic completion UI for in-buffer completion.
;;   It provides completion-at-point with a popup interface similar to
;;   company, but with less overhead and better integration with
;;   standard Emacs completion.
;;
;;   Key features:
;;   - Auto popup after delay
;;   - TAB/S-TAB for navigation
;;   - Works with standard completion-at-point
;;   - Integration with orderless for fuzzy matching
;;   - Lightweight and fast
;;
;;   Why I use it:
;;   Provides modern in-buffer completion with minimal configuration,
;;   works seamlessly with LSP and built-in completion.
;;
;;   GitHub: https://github.com/minad/corfu

(use-package corfu
  :ensure t
  :demand t
  :custom
  ;; Lower idle delay for faster completion (default is 0.5s)
  (corfu-auto-delay 0.2)
  ;; Show candidates after 2 characters
  (corfu-minimum-prefix-length 2)
  ;; Number of candidates to show
  (corfu-count 15)
  ;; Show quick-access numbers
  (corfu-quick nil)
  ;; Don't use corfu for minibuffer (vertico handles that)
  (corfu-global-modes '(not minibuffer-mode))
  ;; Enable auto completion with manual TAB trigger
  (corfu-auto t)
  ;; Preselect first candidate
  (corfu-preselect 'first)
  ;; Cycle through candidates
  (corfu-cycle t)
  :config
  ;; Enable Corfu globally
  (global-corfu-mode)

  ;; Corfu keybindings (work in all states)
  (define-key corfu-map (kbd "TAB") 'corfu-insert)
  (define-key corfu-map [tab] 'corfu-insert)
  (define-key corfu-map (kbd "RET") 'corfu-insert)
  (define-key corfu-map (kbd "ESC") 'corfu-reset))  ;; End of use-package corfu

;; Corfu + Eglot integration for LSP completion (C/C++, Python, etc.)
;; Eglot provides completion-at-point, corfu enhances it with UI
(with-eval-after-load 'eglot
  (setq-local completion-in-region-function #'corfu-complete-in-region))

;; kind-icon
;;
;;   kind-icon adds icons to completion candidates in Corfu.
;;   It displays colorful icons for different completion kinds
;;   (functions, variables, methods, files, etc.).
;;
;;   Key features:
;;   - LSP completion icons (function, variable, class, etc.)
;;   - File completion icons
;;   - Configurable colors
;;   - Works with corfu, vertico, etc.
;;
;;   Why I use it:
;;   Makes completion visually richer and faster to scan by type.
;;
;;   GitHub: https://github.com/jdtsmith/kind-icon

(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  ;; Use icons instead of text labels
  (kind-icon-use-icons t)
  ;; Match corfu background face
  (kind-icon-default-face 'corfu-default)
  :config
  ;; Add kind-icon to Corfu margin formatters
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; ** Xref (Cross-Referencing)

;; xref
;;
;;   xref provides a generic framework for cross-referencing commands.
;;   It provides a way to find definitions, references, and declarations
;;   across different programming languages and tools.
;;
;;   Key features:
;;   - `xref-find-definitions`: Jump to definition
;;   - `xref-find-references`: Find all usages
;;   - `xref-apropos`: Search symbol across project
;;   - Backend-agnostic (works with etags, gtags, LSP, etc.)
;;
;;   Why I use it:
;;   Unified interface for all symbol search operations, works with LSP
;;   and traditional tag systems.
;;
;;   Built-in since Emacs 25, enhanced in Emacs 28+.

(use-package xref
  :ensure nil
  :hook ((xref-after-return xref-after-jump) . recenter)
  :custom
  ;; Emacs 28+
  ;;
  ;; `project-find-regexp' can be faster when setting `xref-search-program' to
  ;;  `ripgrep'.
  (xref-search-program (cond ((executable-find "rg") 'ripgrep)
                             ((executable-find "ugrep") 'ugrep)
                             (t 'grep)))
  (xref-history-storage 'xref-window-local-history)
  (xref-show-xrefs-function #'xref-show-definitions-completing-read)
  (xref-show-definitions-function #'xref-show-definitions-completing-read))


;;
;; * Vim Emulation
;;
;;   Vim keybindings via Evil mode with extensions for surround, leader key,
;;   quickscope navigation, and terminal cursor changes. Provides Vim
;;   editing experience in Emacs.
;;

;; ** Evil Mode

;; *** evil
;;
;;   Evil is an extensible Vim layer for Emacs. It provides Vim
;;   modal editing with states (normal, insert, visual, etc.) and
;;   comprehensive Vim emulation.
;;
;;   Key features:
;;   - Modal editing with states
;;   - Vim keybindings
;;   - Text objects
;;   - Extensible with packages
;;   - Works with Emacs features
;;
;;   Why I use it:
;;   Familiar Vim keybindings with Emacs extensibility, best of both
;;   worlds.
;;
;;   GitHub: https://github.com/emacs-evil/evil

(use-package evil
  :ensure t
  :demand t		       ; Needed for vim keybindings at startup
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)	; for `evil-collection'
  :config
  ;; enable evil-mode globally
  (evil-mode t)
  ;; some configs setup later
  ;; default state set to insert-state
  (setq evil-default-state 'insert)

  (setcdr evil-insert-state-map nil)
  (define-key evil-insert-state-map
	      (read-kbd-macro evil-toggle-key) 'evil-emacs-state)
  (define-key evil-insert-state-map [escape] 'evil-normal-state)
  (dolist (m evil-emacs-state-modes)
    (add-to-list 'evil-insert-state-modes m))

  ;; extra keybindings defined in `Keybinding' section
  (evil-define-key 'normal 'global
    (kbd "C-x C-f") 'find-file
    (kbd "C-x C-q") 'read-only-mode
    (kbd "C-x M-1") 'deft-or-close
    (kbd "C-x M-2") 'eshell
    (kbd "C-x M-3") 'mu4e
    (kbd "C-x M-4") 'erc-start-or-switch
    (kbd "C-x vl") 'magit-log
    (kbd "C-x vp") 'magit-push
    (kbd "C-x vs") 'magit-status
    (kbd "C-x b") 'switch-to-buffer
    (kbd "M-[") 'winner-undo
    (kbd "M-]") 'winner-redo
    (kbd "M-x") 'execute-extended-command
    (kbd "M-s") 'consult-line
    (kbd "C-x C-o") 'other-frame
    (kbd "M-o") 'other-window)
  (evil-define-key 'insert 'global
    (kbd "<delete>") 'hungry-delete-backward
    (kbd "C-;") 'iedit-mode
    (kbd "C-d") 'hungry-delete-forward
    (kbd "C-l") 'hungry-delete-backward
    (kbd "C-o") 'evil-execute-in-normal-state
    (kbd "C-n") 'evil-next-line
    (kbd "C-p") 'evil-previous-line
    (kbd "C-v") 'set-mark-mode/rectangle-mark-mode
    (kbd "C-w") 'backward-kill-word
    (kbd "C-x C-f") 'find-file
    (kbd "C-x C-n") 'corfu-complete
    (kbd "C-x C-o") 'other-frame
    (kbd "C-x C-q") 'read-only-mode
    (kbd "C-x M-1") 'deft-or-close
    (kbd "C-x M-2") 'eshell
    (kbd "C-x M-3") 'mu4e
    (kbd "C-x M-4") 'erc-start-or-switch
    (kbd "C-x T") 'sane-term
    (kbd "C-x b") 'switch-to-buffer
    (kbd "C-x t") 'sane-term
    (kbd "C-x vl") 'magit-log
    (kbd "C-x vp") 'magit-push
    (kbd "C-x vs") 'magit-status
    (kbd "M-<SPC>") 'insert-U200B-char
    (kbd "M-[") 'winner-undo
    (kbd "M-]") 'winner-redo
    (kbd "M-s") 'consult-line
    (kbd "M-v") 'er/expand-region
    (kbd "M-x") 'execute-extended-command
    (kbd "M-y") 'consult-yank-pop
    (kbd "M-z")   'zzz-to-char
    (kbd "s-<RET>") 'insert-empty-line
    (kbd "s-<SPC>") 'insert-U200B-char
    (kbd "C-x C-d") 'dired)
  (evil-ex-define-cmd "ag" 'consult-ag)
  (evil-ex-define-cmd "agi[nteractive]" 'consult-ag)
  (evil-ex-define-cmd "google" 'consult-google)
  (evil-ex-define-cmd "google-suggest" 'consult-google-suggest)
  (evil-ex-define-cmd "gtag" 'ggtags-create-tags))

;; *** evil-collection
;;
;;   Evil collection provides Evil keybindings for various packages that
;;   don't have them by default. Unifies keybindings across Emacs.
;;
;;   Key features:
;;   - Evil keybindings for many packages (100+)
;;   - Consistent Vim-like experience
;;   - Easy to enable per-package or all
;;   - Customizable keymap overrides
;;   - Active development and maintenance
;;
;;   Why I use it:
;;   Makes all packages feel native to Vim workflow. Don't need
;;   to learn different keybindings for dired, dashboard, etc.
;;
;;   GitHub: https://github.com/emacs-evil/evil-collection
;;
;;   Configuration notes:
;;   Enabled for: corfu, dashboard, diff-hl, dired, eldoc,
;;   elpaca, lsp-ui-imenu, which-key.

(use-package evil-collection
  :ensure t
  :demand t
  :after evil
  :config
  (evil-collection-init '(corfu dashboard diff-hl dired eldoc elpaca lsp-ui-imenu which-key)))

;; ** Evil Extensions

;; *** evil-leader
;;
;;   Evil Leader provides `<leader>` feature from Vim that provides
;;   an easy way to bind keys under a variable prefix key.
;;   Leader keys reduce conflict with existing bindings.
;;
;;   Key features:
;;   - Prefix key for custom bindings
;;   - Global and buffer-local keybindings
;;   - Works in all evil states
;;   - Easy to define custom commands
;;   - Compatible with evil-mode
;;
;;   Why I use it:
;;   Allows organizing personal keybindings under SPC prefix,
;;   similar to Vim's leader key approach.
;;
;;   GitHub: https://github.com/cofi/evil-leader
;;
;;   Configuration notes:
;;   Leader key set to <SPC>. Window selection: SPC 1-9.

(use-package evil-leader
  :ensure t
  :after evil
  :commands (global-evil-leader-mode evil-leader/set-leader evil-leader/set-key)
  :config
  ;; enable evil-leader globally
  (global-evil-leader-mode)
  ;; extra keybindings defined in `Keybinding' section
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
   "1" 'select-window-1
   "2" 'select-window-2
   "3" 'select-window-3
   "4" 'select-window-4
   "5" 'select-window-5
   "6" 'select-window-6
   "7" 'select-window-7
   "8" 'select-window-8
   "9" 'select-window-9
   "0" 'select-window-0))

;; *** evil-surround
;;
;;   Evil surround emulates surround.vim by Tim Pope. The functionality
;;   is wrapped into a minor mode. Edit surrounding pairs easily.
;;
;;   Key features:
;;   - Add surrounding characters (cs"' -> changes " to ')
;;   - Delete surrounding characters (ds")
;;   - Change surrounding characters (cs"[)
;;   - Works with tags, quotes, brackets, etc.
;;   - Supports visual mode selections
;;
;;   Why I use it:
;;   Essential for quickly changing or removing surrounding quotes,
;;   brackets, or tags. Saves many keystrokes.
;;
;;   GitHub: https://github.com/timcharper/evil-surround
;;
;;   Configuration notes:
;;   Enabled globally. Keybindings: cs (change), ds (delete), ys (add).

(use-package evil-surround
  :ensure t
  :after evil
  :commands (global-evil-surround-mode)
  :config
  (global-evil-surround-mode 1))

;; *** evil-quickscope
;;
;;   Evil quickscope emulates quick_scope.vim by Brian Le. It highlights
;;   targets for evil-mode's f,F,t,T keys, allowing for quick
;;   navigation within a line with no additional mappings.
;;   Highlighted characters show their jump direction.
;;
;;   Key features:
;;   - Visual highlighting of f/t targets
;;   - Color-coded by distance/position
;;   - Reduces need to remember positions
;;   - Faster character-based navigation
;;   - Configurable highlights
;;
;;   Why I use it:
;;   Makes f/F/t/T operations more intuitive. Visual cues
;;   show exact character positions on the line.
;;
;;   GitHub: https://github.com/blorbx/evil-quickscope
;;
;;   Configuration notes:
;;   Enabled for all programming modes.

(use-package evil-quickscope
  :ensure t
  :after evil
  :commands (turn-on-evil-quickscope-always-mode)
  :config
  (add-hook 'prog-mode-hook 'turn-on-evil-quickscope-always-mode))

(use-package evil-quickscope
  :ensure t
  :after evil
  :config
  (add-hook 'prog-mode-hook 'turn-on-evil-quickscope-always-mode))

;; *** vi-tilde-fringe
;;
;;   Displays tildes in fringe on empty lines a la Vi.
;;   Visual indicator of where file content ends, like Vi/Vim.
;;
;;   Key features:
;;   - Shows ~ in fringe for empty lines
;;   - Visual distinction between content and blank space
;;   - Consistent with Vi/Vim display
;;   - Works in all modes
;;   - Lightweight
;;
;;   Why I use it:
;;   Visual clarity: see where actual content ends. Familiar
;;   Vi-style indicator for file boundaries.
;;
;;   GitHub: https://github.com/syohex/vi-tilde-fringe
;;
;;   Configuration notes:
;;   Only enabled in graphical Emacs (window-system).

(use-package vi-tilde-fringe
  :ensure t
  :if window-system
  :commands (global-vi-tilde-fringe-mode)
  :config
  (global-vi-tilde-fringe-mode))


;; *** evil-terminal-cursor-changer
;;
;;   Make terminal support evil's cursor shape change. This package
;;   changing cursor shape and color by evil state for evil-mode.
;;   Works in terminal Emacs where cursor shaping isn't native.
;;
;;   Key features:
;;   - Different cursor per evil state
;;   - Visual feedback for state changes
;;   - Terminal-only (GUI has native support)
;;   - Supports xterm, gnome-terminal, iTerm, konsole
;;   - Configurable shapes: box, bar, hbar
;;
;;   Why I use it:
;;   In terminal, shows current evil mode via cursor shape.
;;   Essential for avoiding mode confusion.
;;
;;   GitHub: https://github.com/4DA/evil-terminal-cursor-changer
;;
;;   Configuration notes:
;;   Terminal-only (not GUI). Shapes: normal/motion/visual=box,
;;   insert=bar, emacs=hbar.

(use-package evil-terminal-cursor-changer
  :ensure t
  :if (not (display-graphic-p))		; Only use this package when in terminal
  :config
  ;; cursor shape setting
  (setq evil-motion-state-cursor 'box)  ; █
  (setq evil-visual-state-cursor 'box)  ; █
  (setq evil-normal-state-cursor 'box)  ; █
  (setq evil-insert-state-cursor 'bar)  ; ⎸
  (setq evil-emacs-state-cursor  'hbar) ; _
  ;; enable this package
  (evil-terminal-cursor-changer-activate))



;; * Language and Documentation Modes
;;
;;   Language-specific modes and documentation tools. Packages that provide
;;   syntax highlighting, formatting, and specialized editing features for
;;   specific programming languages and markup formats.
;;

;; ** doxymacs
;;
;;   Doxygen is a system for extracting documentation from source code.
;;   It supports a variety of programming languages, human languages
;;   and output formats. You can find it at http://www.doxygen.org.
;;
;;   doxymacs is emacs's wrapper for Doxygen.

(when (require 'doxymacs nil 'noerror)
  (add-hook 'prog-mode-hook #'(lambda () (doxymacs-mode))))

;; ** esup
;;
;;   Benchmark Emacs Startup time without ever leaving your Emacs.
;;
;;   GitHub: https://github.com/jschaf/esup

(use-package esup
  :ensure t
  :commands (esup)
  :init
  ;; Work around a bug where esup tries to step into the byte-compiled
  ;; version of `cl-lib', and fails horribly.
  ;; see:
  ;; https://github.com/jschaf/esup/issues/54#issuecomment-651247749
  (setq esup-depth 0))

;; ** ggtags
;;
;;   Emacs frontend to GNU Global source code tagging system.
;;   Provides fast code navigation, definition lookup, and reference finding.
;;
;;   Requires GNU Global to be installed on your system.
;;

(use-package ggtags
  :ensure t
  :config
  ;; Enable ggtags mode for supported languages
  (add-hook 'c-mode-common-hook 'ggtags-mode)
  (add-hook 'c++-mode-common-hook 'ggtags-mode)
  (add-hook 'java-mode-hook 'ggtags-mode)
  (add-hook 'python-mode-hook 'ggtags-mode)

  ;; Keybindings for ggtags navigation
  (local-set-key (kbd "M-.") 'ggtags-find-tag-dwim)
  (local-set-key (kbd "M-*") 'ggtags-find-reference)
  (local-set-key (kbd "M-,") 'ggtags-pop-stack)
  (local-set-key (kbd "C-c <") 'ggtags-prev-file)
  (local-set-key (kbd "C-c >") 'ggtags-next-file)
  (local-set-key (kbd "C-c C-s") 'ggtags-find-with-grep)
  (local-set-key (kbd "C-c C-r") 'ggtags-find-with-rgrep)
  (local-set-key (kbd "C-c C-f") 'ggtags-find-file)
  (local-set-key (kbd "C-c C-g") 'ggtags-update-tags))

;; ** rmsbolt
;;
;;   RMSbolt is a supercharged implementation of the Godbolt compiler-explorer for Emacs.
;;   It shows you the assembly or bytecode output of your source code files with
;;   bidirectional highlighting between source and compiled output.
;;
;;   GitHub: https://gitlab.com/jgkamat/rmsbolt

(use-package rmsbolt
  :ensure t
  :commands (rmsbolt rmsbolt-compile)
  :custom
  ;; Use tool defaults for assembly format instead of imposing one
  (rmsbolt-asm-format nil)
  ;; Use temporary directory for compilation
  (rmsbolt-default-directory temporary-file-directory)
  ;; Enable demangling for C++ symbols
  (rmsbolt-demangle t)
  ;; Filter assembly directives and unused labels
  (rmsbolt-filter-directives t)
  (rmsbolt-filter-labels t))

;; ** dirvish
;;
;;   A modern file manager based on dired mode with enhanced features.
;;   Provides visual file management, preview capabilities, and extensions.
;;
;;   Integrates well with ggtags for code navigation.
;;
(use-package dirvish
  :ensure t
  :config
  ;; Enable dirvish globally
  (dirvish-override-dired-mode)

  ;; Integration with ggtags for code navigation
  (add-hook 'dirvish-mode-hook
            (lambda ()
              (when (and (featurep 'ggtags)
                         (ggtags-root-directory))
                (local-set-key (kbd "M-.") 'ggtags-find-tag-dwim)
                (local-set-key (kbd "M-*") 'ggtags-find-reference)
                (local-set-key (kbd "M-,") 'ggtags-pop-stack))))

  ;; Additional dirvish keybindings
  (local-set-key (kbd "C-c C-o") 'dirvish-omit-details-toggle)
  (local-set-key (kbd "C-c C-d") 'dirvish-details-toggle)
  (local-set-key (kbd "C-c C-l") 'dirvish-go-up-directory)
  (local-set-key (kbd "C-c C-r") 'dirvish-go-down-directory)
  (local-set-key (kbd "C-c C-f") 'dirvish-find-file)
  (local-set-key (kbd "C-c C-n") 'dirvish-narrow)
  (local-set-key (kbd "C-c C-w") 'dirvish-widen))

;; ** fontawesome
;;
;;   Emacs fontawesome utility.
;;
;;   GitHub: https://github.com/syohex/emacs-fontawesome

(use-package fontawesome :ensure t)

;; ** google-translate
;;
;;   This package allows to translate the strings using Google Translate
;;   service directly from GNU Emacs.
;;
;;   GitHub: https://github.com/atykhonov/google-translate

(use-package google-translate :ensure t)

;; ** pangu-spacing
;;
;;   pangu-spacing is an minor-mode to auto add =space= between Chinese
;;   and English characters. Note that these white-space characters are
;;   not really added to the contents, it just like to do so.
;;
;;   GitHub: https://github.com/coldnew/pangu-spacing

(use-package pangu-spacing
  :ensure t
  :commands (global-pangu-spacing-mode)
  :config
  ;; start pangu-spacing globally
  (global-pangu-spacing-mode 1)
  ;; Always insert `real' space in org-mode.
  (add-hook 'org-mode-hook
            (lambda ()
              (set (make-local-variable 'pangu-spacing-real-insert-separtor) t))))

;; ** password-generator
;;
;;   password-generator provides simple functions to create passwords
;;   and insert them inside buffer immediately.
;;
;;   GitHub: https://github.com/zargener/emacs-password-genarator

(use-package password-generator :ensure t)

;; ** url-shortener
;;
;;   This package can convert long url to tiny url and expand tiny url
;;   to long url, support: bit.ly, goo.gl, dwz.cn, 126.am
;;
;;   GitHub: https://github.com/yuyang0/url-shortener

(use-package url-shortener :ensure t)

;; ** verify-url
;;
;;   verify-url is a little tool that used to find out invalid urls in
;;   the buffer or region.
;;
;;   Use =M-x verify-url= to find invalid urls in current buffer.
;;
;;   After executed command, you can use =verify-url/next-invalid-url=
;;   to goto next invalid-url or =verify-url/previous-invalid-url= to
;;   goto previous one.
;;
;;   GitHub: https://github.com/lujun9972/verify-url

(use-package verify-url
  :defer 2
  :commands (verify-url))

;; ** webpaste
;;
;;   webpaste.el allows to paste whole buffers or parts of buffers to
;;   pastebin-like services. It supports more than one service and
;;   will failover if one service fails.
;;
;;   Supported platform: ix.io, dpaste.com, sprunge.us, dpaste.de
;;
;;   GitHub: https://github.com/etu/webpaste.el

(use-package webpaste
  :ensure t)


;; * AI Integration

(use-package llm
  :ensure t :defer t
  :config
  ;; Should not throw any warning message on non-free LLM
  (setq llm-warn-on-nonfree nil)
  ;; Require specific provider modules for their constructors
  (require 'llm-openai)
  (require 'llm-ollama)

  ;; OpenCode
  (when (boundp 'opencode-api-key)
    (let ((opencode-api-url "https://opencode.ai/zen/v1"))

      ;; Big Pickle
      (defvar my/llm-provider-opencode-bigpickle
        (make-llm-openai-compatible
         :key opencode-api-key
         :chat-model "big-pickle"
         :url opencode-api-url))

      ;; Grok
      (defvar my/llm-provider-opencode-grok
        (make-llm-openai-compatible
         :key opencode-api-key
         :chat-model "grok-code"
         :url opencode-api-url))

      ;; GLM 4.7
      (defvar my/llm-provider-opencode-glm4.7
        (make-llm-openai-compatible
         :key opencode-api-key
         :chat-model "glm-4.7-free"
         :url opencode-api-url))

      ;; MiniMax M2.1
      (defvar my/llm-provider-opencode-minimax-m2.1
        (make-llm-openai-compatible
         :key opencode-api-key
         :chat-model "minimax-m2.1-free"
         :url opencode-api-url)
        )
      ))

  ;; ollama
  (defvar my/llm-provider-ollama-gpt-oss-2ob
    (make-llm-ollama
     :chat-model "gpt-oss:20b"
     :host "127.0.0.1"
     :port 11434))

  ;; openai
  (when-let (openai-api-key (getenv "OPENAI_API_KEY"))
    (defvar my/llm-provider-openai
      (make-llm-openai :key openai-api-key)))

  ;; anthropic
  (when-let (anthropic-api-key (getenv "ANTHROPIC_API_KEY"))
    (require 'llm-anthropic)
    (defvar my/llm-provider-anthropic
      (make-llm-anthropic :key anthropic-api-key)))

  ;; openrouter
  (when-let (openrouter-api-key (getenv "OPENROUTER_API_KEY"))
    (let ((openrouter-url "https://openrouter.ai/api/v1"))
      (defvar my/llm-provider-openrouter
        (make-llm-openai-compatible
         :key openrouter-api-key
         :chat-model "openrouter/auto"
         :url openrouter-url))))
  )

;; ** Default LLM Provider Selection
;;
;;   Customize your preferred LLM provider. API keys should be set via
;;   environment variables:
;;   - OPENAI_API_KEY for OpenAI
;;   - ANTHROPIC_API_KEY for Anthropic (Claude)
;;   - OPENROUTER_API_KEY for OpenRouter
;;   - OPENCODE_API_KEY for OpenCode
;;   Ollama runs locally and doesn't require an API key.

(defcustom my/llm-default-provider 'openai
  "Default LLM provider to use when only one is needed."
  :type '(choice
          (const :tag "OpenAI (uses OPENAI_API_KEY)" openai)
          (const :tag "Anthropic Claude (uses ANTHROPIC_API_KEY)" anthropic)
          (const :tag "OpenRouter (uses OPENROUTER_API_KEY)" openrouter)
          (const :tag "OpenCode Big Pickle (uses OPENCODE_API_KEY)" opencode-bigpickle)
          (const :tag "OpenCode Grok" opencode-grok)
          (const :tag "OpenCode GLM 4.7" opencode-glm4.7)
          (const :tag "OpenCode MiniMax" opencode-minimax)
          (const :tag "Ollama (local)" ollama))
  :group 'my-llm)

(defun my/llm-get-provider (provider)
  "Return the provider instance for PROVIDER symbol.
PROVIDER should be one of: openai, anthropic, openrouter, opencode-bigpickle,
opencode-grok, opencode-glm4.7, opencode-minimax, ollama."
  (pcase provider
    ('openai
     (when (boundp 'my/llm-provider-openai)
       (symbol-value 'my/llm-provider-openai)))
    ('anthropic
     (when (boundp 'my/llm-provider-anthropic)
       (symbol-value 'my/llm-provider-anthropic)))
    ('openrouter
     (when (boundp 'my/llm-provider-openrouter)
       (symbol-value 'my/llm-provider-openrouter)))
    ('opencode-bigpickle
     (when (boundp 'my/llm-provider-opencode-bigpickle)
       (symbol-value 'my/llm-provider-opencode-bigpickle)))
    ('opencode-grok
     (when (boundp 'my/llm-provider-opencode-grok)
       (symbol-value 'my/llm-provider-opencode-grok)))
    ('opencode-glm4.7
     (when (boundp 'my/llm-provider-opencode-glm4.7)
       (symbol-value 'my/llm-provider-opencode-glm4.7)))
    ('opencode-minimax
     (when (boundp 'my/llm-provider-opencode-minimax-m2.1)
       (symbol-value 'my/llm-provider-opencode-minimax-m2.1)))
    ('ollama
     (when (boundp 'my/llm-provider-ollama-gpt-oss-2ob)
       (symbol-value 'my/llm-provider-ollama-gpt-oss-2ob)))
    (_ nil)))

;; ** aidermacs
;;
;;   Aidermacs is an Emacs package that integrates Aider, an AI pair
;;   programming tool. It provides Cursor-like AI coding capabilities while
;;   staying in Emacs.
;;
;;   GitHub: https://github.com/MatthewZMD/aidermacs
;;
;;   Features:
;;   - AI pair programming with Claude, GPT-4o, DeepSeek, Gemini, etc.
;;   - Built-in Ediff integration for change review
;;   - Intelligent model selection
;;   - Architect mode (separate reasoning/editing models)
;;   - Multiple terminal backends (comint/vterm)

(use-package aidermacs
  :ensure t
  :after vterm
  :demand t
  :bind (("C-c a" . aidermacs-transient-menu))
  :config
  ;; API keys should be set in shell environment or via pre-run hook
  (add-hook 'aidermacs-before-run-backend-hook
            (lambda ()
              ;; API keys from environment (set in .bashrc/.zshrc)
              ;; or use password-store for secure management
              (when-let (openai-key (getenv "OPENAI_API_KEY"))
                (setenv "OPENAI_API_KEY" openai-key))
              (when-let (anthropic-key (getenv "ANTHROPIC_API_KEY"))
                (setenv "ANTHROPIC_API_KEY" anthropic-key))
              (when-let (openrouter-key (getenv "OPENROUTER_API_KEY"))
                (setenv "OPENROUTER_API_KEY" openrouter-key))))

  ;; Buffer display settings
  (add-to-list 'display-buffer-alist
               `("\\*aidermacs.*\\*"
                 (display-buffer-pop-up-window)
                 (inhibit-same-window . t)))
  :custom
  ;; Use vterm backend for better terminal compatibility
  (aidermacs-backend 'vterm)

  ;; Default chat mode (code, ask, architect, help)
  (aidermacs-default-chat-mode 'architect)

  ;; Default model based on provider selection
  (aidermacs-default-model
   (pcase my/llm-default-provider
     ('anthropic "claude-sonnet-4-20250514")
     ('openrouter "openrouter/auto")
     (_ "gpt-4o")))

  ;; Architect mode: separate reasoning and editing models
  (aidermacs-architect-model
   (pcase my/llm-default-provider
     ('anthropic "claude-opus-4-20250514")
     ('openrouter "openrouter/o1")
     (_ "o1")))
  (aidermacs-editor-model
   (pcase my/llm-default-provider
     ('anthropic "claude-sonnet-4-20250514")
     ('openrouter "openrouter/gpt-4o")
     (_ "gpt-4o")))

  ;; Behavior settings
  (aidermacs-auto-commits nil)           ; Let user control Git workflow
  (aidermacs-show-diff-after-change t)  ; Show diffs for review
  (aidermacs-exit-kills-buffer nil)     ; Keep buffer after exit

  ;; Read-only files (context only, won't be modified)
  (aidermacs-global-read-only-files '("~/.aider/AI_RULES.md"))
  (aidermacs-project-read-only-files '("README.md" "CONVENTIONS.md"))

  ;; Extra arguments passed to aider CLI
  (aidermacs-extra-args '("--chat-language" "en")))

;; ** ellama
;;
;;   Ellama is a tool for interacting with large language models from Emacs.
;;   Unlike aidermacs (which focuses on pair programming), ellama is a general
;;   LLM assistant for chat, translation, summarization, Q&A, and more.
;;
;;   GitHub: https://github.com/s-kostyaev/ellama
;;
;;   Use cases:
;;   - Chat conversations with LLMs
;;   - Translation and proofreading
;;   - Code review and explanation
;;   - Summarization of code/text
;;   - Context-aware Q&A about your codebase

(use-package ellama
  :ensure t
  :demand t
  :bind (("C-c e" . ellama-transient-menu))
  :init
  ;; Auto-scroll during streaming output
  (setq-default ellama-auto-scroll t)
  ;; User and assistant nicks for chat display
  (setq-default ellama-user-nick "You")
  (setq-default ellama-assistant-nick "Ellama")
  :config
  ;; Set up provider using custom default or fallback to available providers
  (setq-default ellama-provider
		(or (my/llm-get-provider my/llm-default-provider)
		    ;; Fall back to OpenAI if available
		    (when (boundp 'my/llm-provider-openai)
                      (symbol-value 'my/llm-provider-openai))
		    ;; Fall back to OpenCode bigpickle
		    (when (boundp 'my/llm-provider-opencode-bigpickle)
                      (symbol-value 'my/llm-provider-opencode-bigpickle))
		    ;; Fall back to Ollama
		    (when (boundp 'my/llm-provider-ollama-gpt-oss-2ob)
                      (symbol-value 'my/llm-provider-ollama-gpt-oss-2ob))
		    ;; Default to ollama localhost
		    (make-llm-ollama
		     :chat-model "llama3.2"
		     :host "127.0.0.1"
		     :port 11434)))

  ;; Enable global header line for context visibility
  (ellama-context-header-line-global-mode 1)
  (ellama-session-header-line-global-mode 1)

  ;; Configure buffer display
  (setq-default ellama-chat-display-action-function #'display-buffer-pop-up-window)
  (setq-default ellama-instant-display-action-function #'display-buffer-at-bottom)

  ;; Disable reasoning model cleanup for transparency
  (setq-default ellama-session-remove-reasoning nil)

  ;; Register our configured providers for interactive switching
  (when (boundp 'my/llm-provider-opencode-bigpickle)
    (setq-default ellama-providers
		  (append
		   (when (boundp 'my/llm-provider-anthropic)
                     '(("anthropic" . ,(symbol-value 'my/llm-provider-anthropic))))
		   (when (boundp 'my/llm-provider-openrouter)
                     '(("openrouter" . ,(symbol-value 'my/llm-provider-openrouter))))
		   (when (symbol-value 'my/llm-provider-opencode-bigpickle)
                     '(("bigpickle" . ,(symbol-value 'my/llm-provider-opencode-bigpickle))))
		   (when (boundp 'my/llm-provider-opencode-grok)
                     '(("grok" . ,(symbol-value 'my/llm-provider-opencode-grok))))
		   (when (boundp 'my/llm-provider-opencode-glm4.7)
                     '(("glm4.7" . ,(symbol-value 'my/llm-provider-opencode-glm4.7))))
		   (when (boundp 'my/llm-provider-openai)
                     '(("openai" . ,(symbol-value 'my/llm-provider-openai))))
		   (when (boundp 'my/llm-provider-ollama-gpt-oss-2ob)
                     '(("ollama" . ,(symbol-value 'my/llm-provider-ollama-gpt-oss-2ob))))
		   ellama-providers)))

  ;; Enable keymap with prefix
  (setq-default ellama-enable-keymap t)
  (setq-default ellama-keymap-prefix "C-c e"))

;; * Interactive Commands
;;
;;   Custom interactive commands and utility functions that enhance
;;   productivity. Includes buffer management, text manipulation,
;;   file operations, and debugging helpers.
;;
;;   All custom commands start with =my/= prefix.
;;
;; ** Buffers
;;
;; *** Kill all buffers except *scratch* buffer

(defun my/nuke-all-buffers ()
  "Kill all buffers, leaving *scratch* only."
  (interactive)
  (mapcar (lambda (x) (kill-buffer x)) (buffer-list))
  (delete-other-windows))

;; *** Make emacs can always save buffers (even if file is not modified)

(defun my/save-buffer-always ()
  "Save the buffer even if it is not modified."
  (interactive)
  (set-buffer-modified-p t)
  (save-buffer))

;; *** Abort minibuffer recursive edit

(defun my/minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

;; *** Make buffer untabify

(defun my/untabify-buffer ()
  "Untabify all tabs in the current buffer."
  (interactive)
  (save-excursion
    (untabify (point-min) (point-max))))

;; *** Indent whole buffer

(defun my/indent-whole-buffer ()
  "Indent whole buffer."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max))))

;; *** Remove buffers trailing whitespace and untabify

(defun my/cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (save-excursion
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max))
    (untabify (point-min) (point-max))))

;; *** Replace the preceding sexp with its value

(defun my/eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

;; *** Quick folding source block

(defun my/quick-folding-source ()
  "Use emacs buildin easy to folding code."
  (interactive)
  (set-selective-display
   (if selective-display nil 1)))

;; *** Convert file format between DOS and UNIX

(defun my/dos2unix ()
  "Convert buffer file from dos file to unix file."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix 't) )

(defun my/unix2dos ()
  "Convert buffer file from unix file to dos file."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos 't))

;; ** Edit (Insert/Remove)
;;
;; *** Insert U200B char
;;
;;   =<U200B>= character is a =zero width space character= which is
;;   nice to use under org-mode.

(defun my/insert-U200B-char ()
  "Insert <U200B> char, this character is nice use in org-mode."
  (interactive)
  (insert "\ufeff"))

;; *** Insert empty line after current line

(defun my/insert-empty-line ()
  "Insert an empty line after current line and position cursor on newline."
  (interactive)
  (move-end-of-line nil)
  (open-line 1)
  (forward-line 1))

;; *** Delete word

(defun my/delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

;; *** Set mark or expand region

(defun my/set-mark-mode/rectangle-mark-mode ()
  "toggle between set-mark-command or rectangle-mark-mode"
  (interactive)
  (if (not mark-active)
      (call-interactively 'set-mark-command)
    (call-interactively 'rectangle-mark-mode)))

;; *** Copy and comments

(defun my/copy-and-comment ()
  "Copy region and comment it."
  (interactive)
  (kill-ring-save (region-beginning) (region-end))
  (comment-dwim nil))

;; ** File Handle
;;
;; *** Reopen file as root

(defun my/file-reopen-as-root ()
  (interactive)
  (when buffer-file-name
    (find-alternate-file
     (concat "/sudo:root@localhost:"
             buffer-file-name))))

;; *** Delete current buffer file

(defun my/delete-current-buffer-file ()
  "Remove file connected to current buffer and kill buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

;; *** Rename current Buffer and file

(defun my/rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

;; *** Add executable attribute to file

(defun my/set-file-executable()
  "Add executable permissions on current file."
  (interactive)
  (when (buffer-file-name)
    (set-file-modes buffer-file-name
                    (logior (file-modes buffer-file-name) #o100))
    (message (concat "Made " buffer-file-name " executable"))))

;; *** Clone current file to new one

(defun my/clone-file-and-open (filename)
  "Clone the current buffer writing it into FILENAME and open it"
  (interactive "FClone to file: ")
  (save-restriction
    (widen)
    (write-region (point-min) (point-max) filename nil nil nil 'confirm))
  (find-file filename))

;; *** Show current buffer-file information

(defun my/file-info ()
  "Show current buffer information."
  (interactive)
  (if (buffer-file-name (current-buffer))
      (progn
        (let* ((file-name (buffer-file-name (current-buffer)))
               (f-attr (file-attributes file-name))
               (f-size (nth 7 f-attr))  ; ファイルサイズ
               (f-mode (nth 8 f-attr))  ; ファイル属性
               (mes1 (format "file path: %s\n" file-name))
               (mes2 (format "file size: %s byte\n" f-size))
               (mes3 (format "file type: %s" f-mode))
               (mess (concat mes1 mes2 mes3)))
          (message "%s" mess)))
    nil))

;; ** Debug
;;
;; *** Eval emacs buffer until error

(defun my/eval-buffer-until-error ()
  "Evaluate emacs buffer until error occured."
  (interactive)
  (goto-char (point-min))
  (while t (eval (read (current-buffer)))))

;; *** Display face found at the current point

(defun my/what-face (pos)
  "Display face found at the current point."
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

;; *** Reload emacs init config

(defun my/reload-init ()
  "Reload init.el file"
  (interactive)
  (load-file user-init-file))

;; ** Window
;;
;; *** Switch to other window or split it

(defun my/other-window-or-split ()
  "Switch to other window or split it."
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1))

;; *** Swap left/right windows

(defun my/swap-window-positions ()
  "*Swap the positions of this window and the next one."
  (interactive)
  (let ((other-window (next-window (selected-window) 'no-minibuf)))
    (let ((other-window-buffer (window-buffer other-window))
          (other-window-hscroll (window-hscroll other-window))
          (other-window-point (window-point other-window))
          (other-window-start (window-start other-window)))
      (set-window-buffer other-window (current-buffer))
      (set-window-hscroll other-window (window-hscroll (selected-window)))
      (set-window-point other-window (point))
      (set-window-start other-window (window-start (selected-window)))
      (set-window-buffer (selected-window) other-window-buffer)
      (set-window-hscroll (selected-window) other-window-hscroll)
      (set-window-point (selected-window) other-window-point)
      (set-window-start (selected-window) other-window-start))
    (select-window other-window)))










;; * Programming Mode Enhancements
;;
;; ** Line Numbers

;; display-line-numbers-mode was introduced in Emacs 26.1
;; Enable it for Emacs 26.1 and later versions
(when (version<= "26.1" emacs-version)
  (require 'display-line-numbers)
  ;; Only use line number in `prog-mode-hook'
  (add-hook 'prog-mode-hook #'display-line-numbers-mode)
  ;; Customize display-line-numbers--turn-on to skip certain modes
  (defun display-line-numbers--turn-on ()
    "Turn on `display-line-numbers-mode'."
    (unless (or (minibufferp)
                ;; Skip line numbers in these modes
                (memq major-mode '(eshell-mode shell-mode term-mode erc-mode
                                               compilation-mode woman-mode w3m-mode
                                               calendar-mode org-mode)))
      (display-line-numbers-mode))))

;; ** rainbow-delimiters

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; ** recentf

(use-package recentf
  :ensure nil                             ; built-in
  :init (setq recentf-save-file (expand-file-name "recentf" user-cache-directory))
  :config
  (recentf-mode 1))

;; ** highlight-numbers

(use-package highlight-numbers
  :ensure t
  :config
  ;; json-mode has it's own highlight numbers method
  (add-hook 'prog-mode-hook #'(lambda()
				(if (not (derived-mode-p 'json-mode))
                                    (highlight-numbers-mode)))))

;; ** highlight-escape-sequences

(use-package highlight-escape-sequences
  :ensure t
  :config
  ;; Make face the same as builtin face
  (put 'font-lock-regexp-grouping-backslash 'face-alias 'font-lock-builtin-face)
  ;; Enable globally
  (hes-mode))

;; ** font-lock-comment-annotations

(defun my/font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations.
This functions should be added to the hooks of major modes for programming."
  (font-lock-add-keywords
   nil
   '(("\\<\\(FIX\\(ME\\)?\\|BUG\\|HACK\\):" 1 font-lock-warning-face t)
     ("\\<\\(NOTE\\):" 1 'org-level-2 t)
     ("\\<\\(TODO\\):" 1 'org-todo t)
     ("\\<\\(DONE\\):" 1 'org-done t))
   ))

(add-hook 'prog-mode-hook 'my/font-lock-comment-annotations)

;; ** indent-guide

(use-package indent-guide
  :ensure t
  :commands (indent-guide-mode)
  :config
  ;; Only show indent-guide in idle-time.
  (setq indent-guide-delay 0.1)
  (add-hook 'emacs-lisp-mode-hook #'indent-guide-mode)
  (add-hook 'lisp-interaction-mode-hook #'indent-guide-mode)
  (add-hook 'clojure-mode-hook #'indent-guide-mode)
  (add-hook 'scheme-mode-hook #'indent-guide-mode)
  (add-hook 'lisp-mode-hook #'indent-guide-mode))

(show-paren-mode 1)
(setq show-paren-delay 0)               ; no delay

;; ** dtrt-indent

(use-package dtrt-indent
  :ensure t
  :commands (dtrt-indent-mode)
  :config
  ;; enable dtrt-indent-mode globally
  (dtrt-indent-mode 1))

;; ** whitespace-cleanup-mode

(use-package whitespace-cleanup-mode
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'whitespace-cleanup-mode))

;; ** edit-server

(use-package edit-server
  :ensure t
  :commands (edit-server-start)
  :config
  (edit-server-start))

;; ** Tab and completion

(setq tab-always-indent 'complete)

;; ** symbol-overlay

(use-package symbol-overlay
  :ensure t
  :commands (symbol-overlay-mode)
  :config
  (add-hook 'prog-mode-hook #'symbol-overlay-mode)
  (define-key symbol-overlay-map (kbd "p") 'symbol-overlay-jump-prev) ;; 次のシンボルへ
  (define-key symbol-overlay-map (kbd "n") 'symbol-overlay-jump-next) ;; 前のシンボルへ
  (define-key symbol-overlay-map (kbd "C-g") 'symbol-overlay-remove-all) ;; ハイライトキャンセル
  )

;; ** Mouse wheel

(setq mouse-wheel-scroll-amount '(1)) ; Distance in pixel-resolution to scroll each mouse wheel event.
(setq mouse-wheel-progressive-speed nil) ; Progressive speed is too fast for me.

;; ** Create *scratch* automatically

(run-with-idle-timer 1 t
                     #'(lambda ()
                         (unless (get-buffer "*scratch*")
                           (with-current-buffer (get-buffer-create "*scratch*")
                             (lisp-interaction-mode)))))

;; ** uniquify

(use-package uniquify
  :ensure nil                           ; built-in
  :config
  ;; starting separator for buffer name components
  (setq uniquify-separator " • ")
  ;; rationalize buffer names after a buffer has been killed.
  (setq uniquify-after-kill-buffer-p t)
  ;; ignore non file buffers
  (setq uniquify-ignore-buffers-re "^\\*"))

;; ** Register configurations
;; Only set org-related registers if ~/Org/tasks exists
(let ((org-tasks-exists (file-directory-p "~/Org/tasks")))
  (dolist
      (r `(
           ;; emacs's config.el
           (?e (file . "~/.emacs.d/init.el"))
           ;; tasks: todo (only if directory exists)
           ,@(when org-tasks-exists
               '((?t (file . "~/Org/tasks/todo.org"))
                 ;; tasks: personal
                 (?p (file . "~/Org/tasks/personal.org"))
                 ;; tasks: work
                 (?w (file . "~/Org/tasks/work.org"))))
           ;; Office docs
           (?W (file . "~/Org/Weintek/index.org"))
           ;; My personal note
           (?n (file . "~/Org/Note.org"))
           ;; blogging ideas
           (?b (file . "~/Org/blog.org"))
           ;; Finance
           (?f (file . "~/Org/finance/personal.org"))
           ))
    (set-register (car r) (cadr r))))

;; * Version Control
;;
;; ** magit

(use-package magit
  :ensure t
  :config
  (setq magit-commit-arguments '("--verbose" "--signoff")))

(setq magit-diff-refine-hunk 'all)

(use-package git-modes :ensure t)

(use-package git-gutter-fringe
  :ensure t
  :if window-system                     ; git-gutter-fringe only work on GUI
  :commands (git-gutter-mode)
  :config
  ;; enable globally
  (git-gutter-mode))

;; ** magit-gptcommit (AI-assisted commits)

(use-package magit-gptcommit
  :ensure t
  :demand t
  :after magit llm
  :config

  (setq magit-gptcommit-llm-provider
        (or (my/llm-get-provider my/llm-default-provider)
            (when (boundp 'my/llm-provider-openai)
              (symbol-value 'my/llm-provider-openai))
            (when (boundp 'my/llm-provider-ollama-gpt-oss-2ob)
              (symbol-value 'my/llm-provider-ollama-gpt-oss-2ob))))

  ;; add to magit's transit buffer - defer to avoid conflicts
  (with-eval-after-load 'magit
    (magit-gptcommit-status-buffer-setup))
  :bind (:map git-commit-mode-map
              ("C-c C-g" . magit-gptcommit-commit-accept)))

;; * Terminal & Shell
;;
;; ** vterm
;;
;;   vterm is a terminal emulator based on libvterm, which provides
;;   a fast and feature-rich alternative to term-mode.
;;
;;   GitHub: https://github.com/akermu/emacs-libvterm

(use-package transient :ensure t)

(use-package vterm
  :ensure t
  :if (executable-find "cmake")
  :commands vterm
  :config
  (setq vterm-always-prompt-on-exit t
        vterm-shell (or (getenv "SHELL") "/bin/bash")
        vterm-max-scrollback 10000
        vterm-always-compile-module t))

;; ** term handling

(defadvice term-handle-exit (after kill-buffer-after-exit activate)
  "Kill term buffer if process finished."
  (kill-buffer (current-buffer)))

;; ** Eshell Configuration
;;
;;   Eshell is a command shell written in Emacs Lisp.

(use-package eshell
  :ensure nil				; built-in
  :config
  ;; extra eshell configs
  ;; Make eshell prompt look likes default bash prompt
  (require 'cl-lib)
  (setq eshell-prompt-function
        '(lambda ()
           (concat
            user-login-name "@" system-name " "
            (if (cl-search (directory-file-name (expand-file-name (getenv "HOME"))) (eshell/pwd))
            	(replace-regexp-in-string (expand-file-name (getenv "HOME")) "~" (eshell/pwd))
              (eshell/pwd))
            (if (= (user-uid) 0) " # " " $ "))))
  ;; Add color for eshell prompt like Gentoo does
  (defun colorfy-eshell-prompt ()
    (let* ((mpoint)
           (user-string-regexp (concat "^" user-login-name "@" (system-name))))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward (concat user-string-regexp ".*[$#]") (point-max) t)
          (setq mpoint (point))
          (overlay-put (make-overlay (pos-bol) mpoint) 'face '(:foreground "dodger blue")))
        (goto-char (point-min))
        (while (re-search-forward user-string-regexp (point-max) t)
          (setq mpoint (point))
          (overlay-put (make-overlay (pos-bol) mpoint) 'face '(:foreground "green3"))))))
  ;; Make eshell prompt more colorful
  (add-hook 'eshell-output-filter-functions 'colorfy-eshell-prompt)
  (setq eshell-visual-commands
        '("less" "tmux" "htop" "top" "bash" "zsh" "fish" "ssh" "tail"
          "vi" "vim" "screen" "less" "more" "lynx" "ncftp" "pine" "tin"
          "nmtui" "alsamixer"))

  (setq eshell-visual-subcommands
        '(("git" "log" "diff" "show")))
  (add-hook 'eshell-mode-hook
            (lambda ()
              (define-key eshell-mode-map (kbd "C-u") 'eshell-kill-input))))

(use-package eshell-autojump :ensure t)

(use-package esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode)
  :ensure t)

;; ** Eshell commands

(with-eval-after-load 'eshell
  (defun eshell/.. (&optional level)
    "Go up LEVEL directories"
    (interactive)
    (let ((level (or level 1)))
      (eshell/cd (make-string (1+ level) ?.))
      (eshell/ls))))

(defun eshell/clear ()
  "Clears shell buffer ala Unix's clear or DOS' cls"
  ;; shell prompts are read-only, so clear that for the duration
  (let ((inhibit-read-only t))
    ;; simply delete the region
    (delete-region (point-min) (point-max))))

(defun eshell/emacs (&rest args)
  "Open a file in emacs. Some habits die hard."
  (if (null args)
      ;; If I just ran "emacs", I probably expect to be launching
      ;; Emacs, which is rather silly since I'm already in Emacs.
      ;; So just pretend to do what I ask.
      (bury-buffer)
    ;; We have to expand the file names or else naming a directory in a
    ;; argument causes later arguments to be looked for in that directory,
    ;; not the starting directory
    (mapc #'find-file (mapcar #'expand-file-name (flatten-tree (reverse args))))))

(defalias 'eshell/e 'eshell/emacs)

(defun eshell/unpack (file)
  (let ((command (cl-some (lambda (x)
                            (if (string-match-p (car x) file)
				(cadr x)))
                          '((".*\.tar.bz2" "tar xjf")
                            (".*\.tar.gz" "tar xzf")
                            (".*\.bz2" "bunzip2")
                            (".*\.rar" "unrar x")
                            (".*\.gz" "gunzip")
                            (".*\.tar" "tar xf")
                            (".*\.tbz2" "tar xjf")
                            (".*\.tgz" "tar xzf")
                            (".*\.zip" "unzip")
                            (".*\.Z" "uncompress")
                            (".*" "echo 'Could not unpack file:'")))))
    (eshell-command-result (concat command " " file))))

;; * File Management
;;
;;   Tools for managing files, remote access, and project-wide settings.
;;
;; ** editorconfig
;;
;;   EditorConfig helps developers define and maintain consistent
;;   coding styles between different editors and IDEs.
;;
;;   Key features:
;;   - Project-specific coding style configuration
;;   - Automatic indentation and formatting settings
;;   - Cross-editor compatibility
;;   - .editorconfig file support
;;
;;   Why I use it:
;;   Ensures consistent coding styles across different editors and team members.
;;   Eliminates style debates by defining standards per project.
;;
;;   GitHub: https://github.com/editorconfig/editorconfig-emacs
;;
;;   Configuration notes:
;;   Requires EditorConfig core to be installed.

(use-package editorconfig
  :ensure t
  :if (executable-find "editorconfig")
  :mode ("\\.editorconfig\\'" . conf-unix-mode)
  :commands editorconfig-mode
  :init
  (add-hook 'prog-mode-hook #'editorconfig-mode))

;; ** tramp
;;
;;   Transparent Remote (file) Access, Multiple Protocol.
;;
;;   Key features:
;;   - Edit remote files transparently
;;   - Support for multiple protocols (ssh, rsync, ftp, etc.)
;;   - Connection caching and persistence
;;   - Remote shell execution
;;   - File transfer capabilities
;;
;;   Why I use it:
;;   Enables seamless editing of remote files as if they were local.
;;   Essential for working with remote servers and cloud environments.
;;
;;   Built-in Emacs feature.
;;
;;   Configuration notes:
;;   Uses rsync for faster file transfers. Cache files in user-cache-directory.

(use-package tramp
  :ensure nil  ; built-in
  :init
  (setq tramp-persistency-file-name (concat user-cache-directory "tramp"))
  :config
  (setq tramp-default-method "rsync"))

;; * Org Mode
;;
;;   Org-mode is for keeping notes, maintaining TODO lists, and
;;   project planning with a fast and effective plain-markup system.

(use-package org
  :ensure nil				; built-in
  :mode (("\\.org\\'" . org-mode)
         ("\\.org_archive\\'" . org-mode))
  :config
  (setq org-url-hexify-p nil)
  ;; fontify source code
  (setq org-src-fontify-natively t)
  ;; Use current window when switch to source block
  (setq org-src-window-setup 'current-window)
  ;; Disable prompting to evaluate babel blocks
  (setq org-confirm-babel-evaluate nil)
  ;; Disable add validation link when export to HTML
  (setq org-html-validation-link nil)
  (setq org-todo-keywords '((sequence "TODO(t)" "|" "DONE(d)")
                            (sequence "WAITING(w)" "|")
                            (sequence "|" "CANCELED(c)")))
  (add-hook 'org-mode-hook #'visual-line-mode)
  (add-hook 'org-mode-hook #'(lambda () (electric-pair-local-mode -1)))
  ;; NOTE:
  ;; After org-mode 9.2, we need to require `org-tempo' module
  ;; to make easy-template work
  (when (not (version< (org-version) "9.2"))
    (require 'org-tempo))

  (add-to-list 'org-structure-template-alist
  	       '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist
  	       '("S" . "src sh"))
  (add-to-list 'org-structure-template-alist
  	       '("p" . "src plantuml :file uml.png"))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (C . t)
     (ditaa . t)
     (dot . t)
     (js . t)
     (latex . t)
     (perl . t)
     (python . t)
     (ruby . t)
     ;; (sh . t)
     (shell . t)
     (plantuml . t)
     (R . t)
     (clojure . t)))
  ;; make dot work as graphviz-dot
  (add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))
  (setq org-link-abbrev-alist
        '(("google" . "http://www.google.com/search?q=") ; ex: [[google:hi emacs]]
          ("google-map" . "http://maps.google.com/maps?q=%s") ; ex: [[google-map:taiwan]]
          ("wiki" . "https://en.wikipedia.org/wiki/%s")	; ex: [[wiki:emacs]]
          ))
  ;; make agenda show on current window
  (setq org-agenda-window-setup 'current-window)
  ;; highlight current in agenda
  (add-hook 'org-agenda-mode-hook 'hl-line-mode)

  ;; Setup files for agenda - only configure if directory exists
  (let ((org-task-dir (expand-file-name "~/Org/tasks")))
    (when (file-directory-p org-task-dir)
      (setq org-directory org-task-dir)
      (setq org-agenda-files nil) ; Lazy loaded in my/org-setup-agenda-files
      (defun my/org-setup-agenda-files ()
        "Setup org-agenda-files lazily."
        (unless org-agenda-files
          (setq org-agenda-files
                (find-lisp-find-files org-directory "\.org$"))))
      (add-hook 'org-mode-hook #'my/org-setup-agenda-files)
      ;;
      (setq org-default-notes-file (file-name-concat org-directory "tasks" "TODO.org"))))

  ;; Always use `C-g' to exit agenda
  (add-hook 'org-agenda-mode-hook
            #'(lambda ()
		(local-set-key (kbd "C-g") 'org-agenda-exit)))

  ;; Use speed command to quick navigating
  (setq org-use-speed-commands t)
  ;; Log timestamp when done
  (setq org-log-done 'time)
  (setq org-capture-templates
        '(("t" "TODO"     entry (file+headline "" "Tasks") "* TODO %?\n %i\n")
          ("n" "NOTE"     entry (file+headline "" "Tasks") "* NOTE %?\n %i\n %a")
          ("l" "Links"    entry (file+headline "" "Links") "* TODO %? :link:\nSCHEDULED: <%<%Y-%m-%d %a>>\n %i\n %a")
          ("j" "Journal"  entry (file+datetree "" "Journal") "* %?\nEntered on %U\n  %i\n  %a")
          ))
  (eval-after-load 'ispell
    '(progn
       (add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
       (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))
       ))
  (with-eval-after-load 'org
    (bind-keys :map org-mode-map
               ("M-p"   . org-previous-visible-heading)
               ("M-n"   . org-next-visible-heading)
               ("C-c a" . org-agenda)
               ("C-c c" . org-capture)
               ("C-c l" . org-store-link)
               ("C-c b" . org-metaleft)
               ("C-c f" . org-metaright)
               ("C-c p" . org-metaup)
               ("C-c n" . org-metadown)
               ("C-c i" . org-insert-link)
               ("C-c I" . org-toggle-inline-images)
               ("C-c %" . org-mark-ring-push)
               ("C-c &" . org-mark-ring-goto)
               ("C-c C-." . org-babel-remove-result-one-or-many))
    (bind-keys :map org-src-mode-map
               ("C-c C-c" . org-edit-src-exit))))

;; ** org-indent

(use-package org-indent
  :ensure nil				; build-in
  :after (org)
  :config
  ;; Enable `org-indent-mode' by default
  (with-eval-after-load 'org-indent
    (add-hook 'org-mode-hook #'(lambda () (org-indent-mode t)))))

;; ** org-bullets

(use-package org-bullets
  :ensure t
  :after (org)
  :config
  (with-eval-after-load 'org-bullets
    (add-hook 'org-mode-hook #'(lambda () (org-bullets-mode 1)))))

;; ** org-crypt

(use-package org-crypt
  :ensure nil
  :after (org)
  :config
  ;; Disable `auto-save-mode' for org-mode buffer prior to decrypting an entry.
  (setq org-crypt-disable-auto-save t)
  ;; Auto encrypt when save file
  (org-crypt-use-before-save-magic)
  ;; Encrypt with tagname: `secret'
  (setq org-crypt-tag-matcher "secret")
  ;; Prevent the `secret' tag inherit by child
  ;; (The child item still will be encrypt)
  (setq org-tags-exclude-from-inheritance (quote ("secret")))
  ;; Use my own password to encrypt
  (setq org-crypt-key nil))

;; ** org-download

(use-package org-download
  :ensure t
  :config
  ;; add support to dired
  (add-hook 'dired-mode-hook 'org-download-enable))

;; * Language Support - Documentation & Markup
;;
;; ** plantuml-mode

(use-package plantuml-mode
  :ensure t
  :mode ("\\.plantuml\\'")
  :init
  ;; FIXME: setup plantuml jar path
  ;; FIXME: add org-mode support
  )

;; ** bison-mode

(use-package bison-mode
  :ensure t
  :mode ("\\.y\\'" "\\.l\\'" "\\.jison\\'"))

;; ** gn-mode

(use-package gn-mode
  :ensure t
  :mode ("BUILD.gn" "\\.gni?\\'"))

;; ** nasm-mode

(use-package nasm-mode :ensure t)

;; ** ess (R mode)

(use-package ess
  :ensure t
  :mode ("\\.[rR]\\'" . R-mode)
  :config
  )

;; ** qml-mode

(use-package qml-mode
  :ensure t
  :mode "\\.qml$"
  :hook (qml-mode . indent-guide-mode))

;; ** vala-mode

(use-package vala-mode
  :ensure t
  :mode ("\\.vala\\'" "\\.vapi\\'")
  :config
  )

;; ** verilog-mode

(use-package verilog-mode
  :ensure t
  :mode (("\\.v\\'" . verilog-mode)
         ("\\.vh\\'" . verilog-mode)
         ("\\.sv\\'" . verilog-mode)
         ("\\.svh\\'" . verilog-mode))
  :config
  ;; Linting and compilation
  (setq verilog-linter "verilator --lint-only --Wall")
  (setq verilog-compiler "verilator --cc --exe --build")
  (setq verilog-simulator "verilator --cc --exe --build --run")

  ;; Flycheck integration
  ;; https://github.com/flycheck/flycheck/issues/1250
  (setq flycheck-verilog-verilator-executable "/usr/bin/verilator_bin")

  ;; Indentation and style
  (setq verilog-indent-level 3)
  (setq verilog-indent-level-module 3)
  (setq verilog-indent-level-declaration 3)
  (setq verilog-case-indent 2)
  (setq verilog-auto-newline t)
  (setq verilog-auto-indent-on-newline t)

  ;; AUTO features (powerful code generation)
  (setq verilog-auto-lineup 'declarations)
  (setq verilog-auto-endcomments t)
  (setq verilog-auto-wire-comment t)

  ;; Project settings
  (setq verilog-library-directories '("."))
  (setq verilog-library-extensions '(".v" ".vh" ".sv" ".svh"))

  ;; Hooks
  (add-hook 'verilog-mode-hook #'flycheck-mode)
  (add-hook 'verilog-mode-hook #'hs-minor-mode)  ; Code folding
  (add-hook 'verilog-mode-hook #'electric-pair-mode)
  (add-hook 'verilog-mode-hook #'verilog-set-compile-command)

  ;; Keybindings
  (define-key verilog-mode-map (kbd "C-c C-l") #'verilog-lint)
  (define-key verilog-mode-map (kbd "C-c C-c") #'verilog-compile)
  (define-key verilog-mode-map (kbd "C-c C-s") #'verilog-simulate)
  (define-key verilog-mode-map (kbd "C-c C-a") #'verilog-auto)
  (define-key verilog-mode-map (kbd "C-c C-d") #'verilog-delete-auto)
  (define-key verilog-mode-map (kbd "C-c C-p") #'verilog-preprocess)
  (define-key verilog-mode-map (kbd "C-c C-t") #'my/verilog-run-testbench)
  (define-key verilog-mode-map (kbd "C-c C-w") #'my/verilog-wave-view))

;; ** Custom Verilog functions

(defun my/verilog-run-testbench ()
  "Run the current Verilog testbench with Verilator."
  (interactive)
  (let ((file (buffer-file-name)))
    (compile (format "verilator --cc --exe --build --run %s" file))))

(defun my/verilog-wave-view ()
  "Generate and view waveforms (requires gtkwave)."
  (interactive)
  (compile "make sim && gtkwave dump.vcd"))

;; ** Verilog LSP completion with Eglot + Corfu

;; Configure Eglot for Verilog using svlangserver (SystemVerilog/Verilog language server)
;; Install svlangserver: npm install -g svlangserver
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(verilog-mode . ("svlangserver"))))

;; Enable Eglot in verilog-mode for LSP-powered completion
(add-hook 'verilog-mode-hook #'eglot-ensure)

;; Alternative: Use hdl_checker if svlangserver is not available
;; (with-eval-after-load 'eglot
;;   (add-to-list 'eglot-server-programs
;;                '(verilog-mode . ("hdl_checker" "--lsp"))))

;; ** groovy-mode

(use-package groovy-mode
  :mode (("\\.groovy" . groovy-mode)
         ("/Jenkinsfile" . groovy-mode))
  :ensure t)

;; ** svelte-mode

(use-package svelte-mode
  :ensure t
  :mode ("\\.svelte\\'" . svelte-mode))

;; ** ssh-config-mode

(use-package ssh-config-mode
  :ensure t
  :mode ((".ssh/config\\'"       . ssh-config-mode)
         ("sshd?_config\\'"      . ssh-config-mode)
         ("known_hosts\\'"       . ssh-known-hosts-mode)
         ("authorized_keys\\'"   . ssh-authorized-keys-mode)))

;; ** systemd

(use-package systemd
  :ensure t)

;; ** cmake-mode

(use-package cmake-mode
  :ensure t
  :mode (("CMakeLists\\.txt\\'" . cmake-mode) ("\\.cmake\\'" . cmake-mode)))

;; ** cmake-font-lock

;; cmake-font-lock: emacs font lock rules for CMake
;; https://github.com/Lindydancer/cmake-font-lock
(use-package cmake-font-lock
  :ensure t
  :config
  (autoload 'cmake-font-lock-activate "cmake-font-lock" nil t)
  (add-hook 'cmake-mode-hook 'cmake-font-lock-activate))

;; ** meson-mode

(use-package meson-mode
  :ensure t
  :mode (("meson\\.build\\'" . meson-mode)))

;; * Language Support - C/C++
;;
;; ** cc-mode configuration

(use-package cc-mode
  :ensure nil				; built-in
  :mode
  (("\\.h\\'" . c++-mode)
   ("\\.c\\'" . c-mode)
   ("\\.hpp\\'" . c++-mode)
   ("\\.cpp\\'" . c++-mode)
   ("\\.cc\\'" . c++-mode))
  :config
  ;; subword-mode, e.g., someThing is treated as two words
  (add-hook 'c-mode-common-hook #'(lambda () (subword-mode 1)))
  (dolist (m '(c-mode c++-mode))
    (font-lock-add-keywords
     m
     '(("\\<\\(int8_t\\|int16_t\\|int32_t\\|int64_t\\|uint8_t\\|uint16_t\\|uint32_t\\|uint64_t\\)\\>" . font-lock-keyword-face))))
  (add-hook 'c-mode-hook
            #'(lambda ()
		(c-set-style "linux")
		(setq c-basic-offset 8)
		;; Make TAB equivalent to 8 spaces
		(setq tab-width 8)))
  (defun c-lineup-arglist-tabs-only (ignored)
    "Line up argument lists by tabs, not spaces"
    (let* ((anchor (c-langelem-pos c-syntactic-element))
           (column (c-langelem-2nd-pos c-syntactic-element))
           (offset (- (1+ column) anchor))
           (steps (floor offset c-basic-offset)))
      (* (max steps 1)
         c-basic-offset)))

  ;; Add Linux kernel style
  (add-hook 'c-mode-common-hook
            (lambda ()
              (c-add-style "linux-kernel"
                           '("linux" (c-offsets-alist
                                      (arglist-cont-nonempty
                                       c-lineup-gcc-asm-reg
                                       c-lineup-arglist-tabs-only))))))

  (defun linux-kernel-development-setup ()
    (let ((filename (buffer-file-name)))
      ;; Enable kernel mode for the appropriate files
      (when (and filename
                 (or (locate-dominating-file filename "Kbuild")
                     (locate-dominating-file filename "Kconfig")
                     (save-excursion (goto-char 0)
                                     (search-forward-regexp "^#include <linux/\\(module\\|kernel\\)\\.h>$" nil t))))
        (setq indent-tabs-mode t)
        (setq tab-width 8)
        (setq c-basic-offset 8)
        (c-set-style "linux-kernel")
        (message "Setting up indentation for the linux kernel"))))

  (add-hook 'c-mode-hook 'linux-kernel-development-setup)
  (add-hook 'c++-mode-hook
            #'(lambda ()

		;; Use stroustrup style
		(c-set-style "stroustrup")

		;; Setting indentation level
		(setq c-basic-offset 4)

		;; Make TAB equivalent to 4 spaces
		(setq tab-width 4)

		;; Use spaces to indent instead of tabs.
		(setq indent-tabs-mode nil)

		;; Indent the continuation by 2
		(setq c-continued-statement-offset 2)

		;; Brackets should be at same indentation level as the statements they open
		;; for example:
		;;                 if (0)        becomes        if (0)
		;;                     {                        {
		;;                        ;                         ;
		;;                     }                        }
		(c-set-offset 'substatement-open 0)

		;; make open-braces after a case
		(c-set-offset 'case-label '+)

		;; Not indent code inside a namespace
		;; for example:
		;;                namespace A {
		;;
		;;                int namespace_global_variable;
		;;
		;;                class Class {
		;;
		;;                Class();
		;;                //...
		;;                };
		;;
		;;                }
		(c-set-offset 'innamespace 0)
		))
  (bind-keys :map c-mode-base-map
             ;;("C-c '" . my/narrow-or-widen-dwim)
             ("C-c C-c" . compile)
             ("C-c C-g" . gdb)
             ("C-c C-o" . cff-find-other-file))

  ;; Some keys may override global map add here
  (bind-keys :map c-mode-base-map
             ("M-." . ggtags-find-tag-dwim)
             ("M-," . ggtags-find-tag-return)))

;; ** c-eldoc
;;
;;   c-eldoc provides eldoc support for C/C++ code. Shows
;;   function signatures, macro expansions, and include paths in
;;   echo area or tooltip as you type.
;;
;;   Key features:
;;   - Display function signatures at point
;;   - Show macro expansions
;;   - Display include file paths
;;   - Works with standard eldoc
;;   - Customizable include paths
;;
;;   Why I use it:
;;   Quick reference to function signatures while coding in C/C++.
;;   Reduces need to look up API documentation.
;;
;;   GitHub: https://github.com/lewisjr/c-eldoc
;;
;;   Configuration notes:
;;   Enabled for all c/c++ modes. Includes pkg-config paths.

(use-package c-eldoc
  :ensure t
  :commands (c-turn-on-eldoc-mode)
  :config
  (add-hook 'c-mode-common-hook
            #'(lambda ()
		(setq c-eldoc-includes "`pkg-config --cflags --libs` -I./ -I../")
		(c-turn-on-eldoc-mode))))

;; ** cwarn
;;
;;   cwarn highlights suspicious and error-prone C/C++ constructs.
;;   Detects common mistakes that compilers sometimes miss.
;;
;;   Key features:
;;   - Suspicious semicolon warnings
;;   - Assignment in condition warnings
;;   - Wrong type for printf warnings
;;   - Missing header guards warnings
;;   - Customizable warning levels
;;   - Highlights potential errors
;;
;;   Why I use it:
;;   Early detection of C/C++ bugs and suspicious code patterns.
;;   Catches issues before compile time.
;;
;;   Built-in Emacs feature (no package needed).
;;
;;   Configuration notes:
;;   Enabled globally for all c-mode-family modes.

(use-package cwarn
  :ensure nil				; built-in
  :commands (cwarn-mode global-cwarn-mode)
  :config
  (add-hook 'c-mode-common-hook #'(lambda () (cwarn-mode 1))))

;; ** cc-mode utilities

(defun my/cc-mode/highlight-if-0 ()
  "highlight c/c++ #if 0 #endif macros"
  (defvar cpp-known-face)
  (defvar cpp-unknown-face)
  (defvar cpp-known-writable)
  (defvar cpp-unknown-writable)
  (defvar cpp-edit-list)
  (setq cpp-known-face 'default)
  (setq cpp-unknown-face 'default)
  (setq cpp-known-writable 't)
  (setq cpp-unknown-writable 't)
  (setq cpp-edit-list '(("0" '(foreground-color . "gray")  default both)
                        ("1" default font-lock-comment-face both)))
  (cpp-highlight-buffer t))

;; Add to c/c++ mode
(defun my/cc-mode/highlight-if-0-hook ()
  (when (or (eq major-mode 'c++-mode) (eq major-mode 'c-mode)
            (eq major-mode 'c++-ts-mode) (eq major-mode 'c-ts-mode))
    (my/cc-mode/highlight-if-0)))
(add-hook 'after-save-hook #'my/cc-mode/highlight-if-0-hook)

(add-hook 'c-mode-common-hook 'electric-pair-mode)

;; ** srefactor
;;
;;   srefactor is a C/C++ source code refactoring tool for Emacs.
;;   Provides semantic-aware refactoring operations on code.
;;
;;   Key features:
;;   - Extract function/method
;;   - Inline function/method
;;   - Rename variables and functions
;;   - Convert between local/global variables
;;   - Generate getter/setter methods
;;   - Intelligent code transformation
;;
;;   Why I use it:
;;   Semantic refactoring without regex or manual edits.
;;   Saves time on common C/C++ refactoring tasks.
;;
;;   GitHub: https://github.com/tuhdo/srefactor
;;
;;   Configuration notes:
;;   Loaded after cc-mode. Use M-x srefactor-* for commands.

(use-package srefactor
  :ensure t
  :defer t
  :commands (srefactor-menu srefactor-extract-function srefactor-inline-function)
  :after (cc-mode))

;; ** cff
;;
;;   cff (C/C++ Find File) helps navigate between header
;;   and source files in C/C++ projects.
;;
;;   Key features:
;;   - Jump between .h and .c/.cpp files
;;   - Smart file detection and matching
;;   - Multiple projects support
;;   - Works with cc-mode
;;   - Toggle between header/implementation
;;
;;   Why I use it:
;;   Quick navigation between C/C++ header and source files.
;;   Essential for large projects with many files.
;;
;;   GitHub: https://github.com/larstvei/cff
;;
;;   Configuration notes:
;;   Keybinding: C-c C-o to find corresponding file.

(use-package cff
  :ensure t
  :commands (cff-find-other-file)
  :after (cc-mode))

;; ** modern-cpp-font-lock
;;
;;   modern-cpp-font-lock adds syntax highlighting for modern C/C++ features.
;;   Supports C++11 through C++20 keywords and concepts.
;;
;;   Key features:
;;   - Modern C++ keywords (auto, constexpr, noexcept, etc.)
;;   - C++11/14/17/20 support
;;   - Standard library highlighting
;;   - Improved type recognition
;;   - Template syntax highlighting
;;
;;   Why I use it:
;;   Better syntax highlighting for modern C++ code. Default Emacs
;;   highlighting lacks support for recent C++ features.
;;
;;   GitHub: https://github.com/ludwigpacifici/modern-cpp-font-lock
;;
;;   Configuration notes:
;;   Enabled for all c++-mode and c++-ts-mode buffers.

(use-package modern-cpp-font-lock
  :ensure t
  :commands (modern-c++-font-lock-mode)
  :hook (c++-mode . modern-c++-font-lock-mode)
  :after (cc-mode))

;; ** C utilities

(defun my/c-kill-defun ()
  "Move backward to the beginning of top level declaration and save
this declaration to the kill-ring."
  (interactive)
  (save-excursion
    (kill-region
     (progn (c-beginning-of-defun) (point))
     (progn (c-end-of-defun)       (point)))))

;; ** clang-format
;;
;;   clang-format formats C/C++/Objective-C buffers using clang-format.
;;   Provides automatic code formatting with LLVM standards.
;;
;;   Key features:
;;   - Format entire buffer or region
;;   - Read .clang-format config from project
;;   - Fallback style support (Chromium, Google, etc.)
;;   - Fast formatting with clang
;;   - Preserves cursor position
;;   - Supports multiple languages
;;
;;   Why I use it:
;;   Consistent code formatting across projects. Integrates with
;;   LLVM toolchain which many projects already use.
;;
;;   GitHub: https://github.com/emacsorphanage/clang-format
;;
;;   Configuration notes:
;;   Commands: clang-format-buffer, clang-format-region.
;;   Fallback style: Chromium. Respects project .clang-format.

(use-package clang-format
  :ensure t
  :commands (clang-format clang-format-buffer clang-format-region)
  :config
  ;; Style to use when calling `clang-format-buffer' unless   project has a
  ;; .clang-format file.
  (setq clang-format-fallback-style "Chromium"))

;; * Emacs Lisp Development

;;
;; ** elisp-mode configuration

;; Helper functions for elisp-mode
(defun my/elisp/check-parens-on-save ()
  "Run `check-parens' when the current buffer is saved."
  (add-hook 'after-save-hook #'check-parens nil 'make-it-local))

(defun my/remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))))

;; eldoc - built-in, configure for elisp-mode
(use-package eldoc
  :ensure nil            ; built-in
  :hook (emacs-lisp-mode . turn-on-eldoc-mode)
  :config
  ;; Fix for paredit if it exists
  (eval-after-load 'paredit
    '(progn
       (eldoc-add-command 'paredit-backward-delete
                          'paredit-close-round))))

;; ** macrostep - expand Emacs Lisp macros
;;
;;   macrostep allows stepping through and expanding Emacs Lisp
;;   macros with visual highlighting of each expansion step.
;;
;;   Key features:
;;   - Visual macro expansion (M-x macrostep-expand)
;;   - Step through macro expansions
;;   - Highlight current step
;;   - Expand nested macros
;;   - Works with elisp-mode and lispy
;;
;;   Why I use it:
;;   Understand complex macro expansions in Emacs Lisp code.
;;   Essential for debugging macros and learning elisp.
;;
;;   GitHub: https://github.com/joddie/macrostep
;;
;;   Configuration notes:
;;   Use M-x macrostep-expand to expand macro at point.

(use-package macrostep
  :ensure t
  :commands (macrostep-expand))

;; ** el-spice - interactive elisp evaluation
;;
;;   el-spice provides interactive Emacs Lisp evaluation with
;;   colored output and improved error reporting.
;;
;;   Key features:
;;   - Evaluate last s-expression with keybinding
;;   - Evaluate current region
;;   - Colored output for better readability
;;   - Enhanced error messages
;;   - Preserve point position after eval
;;
;;   Why I use it:
;;   Quick interactive elisp evaluation during development.
;;   Easier than C-x C-e for common eval operations.
;;
;;   GitHub: https://github.com/emacsorphanage/el-spice
;;
;;   Configuration notes:
;;   Commands: el-spice-eval-buffer, el-spice-eval-last-sexp.

(use-package el-spice
  :ensure t
  :commands (el-spice-eval-buffer el-spice-eval-last-sexp))

;; ** litable - display lists in tables
;;
;;   litable displays Emacs Lisp lists (association lists, property lists)
;;   in tabular form for better readability.
;;
;;   Key features:
;;   - Tabular list display (M-x litable-mode)
;;   - Read/write lists as tables
;;   - Edit list elements in table
;;   - Sortable columns
;;   - Works with association and property lists
;;
;;   Why I use it:
;;   Better visualization of list data structures in elisp.
;;   Makes editing complex lists more intuitive.
;;
;;   GitHub: https://github.com/Fuco1/litable
;;
;;   Configuration notes:
;;   Lists saved to ~/.emacs.d/.cache/litable-lists.el.

(use-package litable
  :ensure t
  :commands (litable-mode)
  :config
  ;; Save cache file to `user-cache-directory'
  (setq litable-list-file (concat user-cache-directory ".litable-lists.el")))

;; ** page-break-lines - display lines as decorative breaks
;;
;;   page-break-lines displays form feed characters (^L) as
;;   decorative lines, similar to page breaks in word processors.
;;
;;   Key features:
;;   - Decorative horizontal lines for page breaks
;;   - Configurable line character and width
;;   - Global or buffer-specific mode
;;   - Customizable faces (colors, etc.)
;;   - Works with any mode that uses ^L
;;
;;   Why I use it:
;;   Visual separation of logical sections in files.
;;   Makes long files with manual page breaks easier to read.
;;
;;   GitHub: https://github.com/purcell/page-break-lines
;;
;;   Configuration notes:
;;   Use M-x global-page-break-lines-mode to enable globally.

(use-package page-break-lines
  :ensure t
  :commands (global-page-break-lines-mode))

;; ** outshine-mode - org-mode-like navigation in elisp files
;;
;;   outshine provides org-mode-like navigation and folding for
;;   Emacs Lisp source files using outline headings.
;;
;;   Key features:
;;   - Outline navigation (C-c n/p/f/b keys)
;;   - Fold/unfold sections (TAB/S-TAB)
;;   - Org-mode heading style (;; *, ;; ***)
;;   - Works with elisp-mode
;;   - Compatible with other outline modes
;;
;;   Why I use it:
;;   Makes Emacs Lisp files easier to navigate.
;;   Provides familiar org-mode workflow for elisp development.
;;
;;   GitHub: https://github.com/alphapapa/outshine
;;
;;   Configuration notes:
;;   Outline prefix: C-c @. Start with headings hidden (level 1).

(use-package outshine
  :ensure t
  :diminish outshine-mode
  :hook (emacs-lisp-mode . outshine-mode)
  :config
  ;; Set outline-minor-mode prefix to C-c @ (same as outshine default)
  (setq outline-minor-mode-prefix (kbd "C-c @"))
  ;; Start with bodies hidden (show only headings)
  (setq outshine-start-level 1))

;; elisp-mode - built-in, configure hooks and keybindings
(use-package elisp-mode
  :ensure nil				; built-in
  :hook
  ((emacs-lisp-mode . my/elisp/check-parens-on-save)
   (emacs-lisp-mode . my/remove-elc-on-save))
  :config
  ;; Enable litable-mode globally after litable loads
  (with-eval-after-load 'litable
    (litable-mode))
  ;; Enable page-break-lines globally
  (with-eval-after-load 'page-break-lines
    (global-page-break-lines-mode 1))
  :bind
  (:map emacs-lisp-mode-map
        ;; ("C-c '" . my/narrow-or-widen-dwim)
        ))
;; * Syntax Checking and Linting
;;
;; ** flycheck
;;
;;   Flycheck provides on-the-fly syntax checking for many languages.
;;   Supports emacs-lisp, c/c++ (via clang), python, javascript and more.
;;
;;   Key features:
;;   - Real-time syntax checking as you type
;;   - Supports 50+ languages and checkers
;;   - Error navigation with flycheck-next-error/flycheck-previous-error
;;   - Multiple error display modes (list, fringe, tooltips)
;;   - Automatic syntax checker detection
;;
;;   Why I use it:
;;   Immediate feedback on code errors saves debugging time. Works
;;   seamlessly with flycheck-pos-tip for visual error display.
;;
;;   GitHub: https://github.com/flycheck/flycheck
;;
;;   Configuration notes:
;;   Enabled for emacs-lisp, c/c++, dart modes. Uses
;;   flycheck-pos-tip for error tooltips.

(use-package flycheck
  :ensure t
  :commands (flycheck-mode global-flycheck-mode)
  :hook
  ;; Enable flycheck for these modes
  ((emacs-lisp-mode . flycheck-mode)
   (c-mode . flycheck-mode)
   (c++-mode . flycheck-mode)
   (dart-mode . flycheck-mode))
  :config
  ;; Enable flycheck globally (for modes with built-in checkers)
  (global-flycheck-mode 1))

;; Show flycheck errors in tooltip
;;
;; *** flycheck-pos-tip
;;
;;   Flycheck-pos-tip displays Flycheck errors in popup tooltips.
;;   Provides a cleaner, more discoverable error display interface.
;;
;;   Key features:
;;   - Popup tooltips for error messages
;;   - Show full error/warning text on hover
;;   - Multiple tooltip position options
;;   - Works with all Flycheck checkers
;;   - Integrates with company/other completion
;;
;;   Why I use it:
;;   Makes Flycheck errors easier to read without leaving context.
;;   Hover over error to see full message.
;;
;;   GitHub: https://github.com/flycheck/flycheck-pos-tip
;;
;;   Configuration notes:
;;   Used as flycheck-display-errors-function for tooltips.

(use-package flycheck-pos-tip
  :ensure t
  :after flycheck
  :config
  (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))
(setq vc-handled-backends nil)

;; * LSP & Code Intelligence
;;
;;   Language Server Protocol and code intelligence tools providing
;;   IDE-like features: auto-completion, go-to-definition, find-references,
;;   refactoring, and diagnostics across multiple programming languages.

(use-package eglot
  :ensure nil		 ; Built-in since Emacs 29
  :defer t		 ; Load only when editing code, not at startup
  :config
  ;; Reduce event buffer size for performance
  (setq-default eglot-events-buffer-size 0)

  ;; Configure language servers
  ;; Configure language servers (direct configuration, no with-eval-after-load needed)
  ;; Python: prefer ty-mode server, fall back to pylsp
  (add-to-list 'eglot-server-programs
	       '((python-mode python-ts-mode)
		 . ,(eglot-alternatives
		     '("ty" "server" "pylsp" ("basedpyright-langserver" "--stdio")
		       ("pyright-langserver" "--stdio")))))
  ;; JavaScript/TypeScript: tsserver
  (add-to-list 'eglot-server-programs
	       '((js-mode js-ts-mode tsx-ts-mode typescript-mode typescript-ts-mode) . ("typescript-language-server" "--stdio")))
  ;; Rust: rust-analyzer
  ;; See https://rust-analyzer.github.io/manual.html#emacs
  (add-to-list 'eglot-server-programs '((rust-mode rust-ts-mode) . ("rust-analyzer" :initializationOptions (:check (:command "clippy")))))
  ;; Go: gopls
  (add-to-list 'eglot-server-programs
	       '((go-mode go-ts-mode) . ("gopls")))
  ;; C/C++: clangd
  (add-to-list 'eglot-server-programs
	       '((c-mode c++-mode c-ts-mode c++-ts-mode) . ("clangd")))
  ;; CMake
  (add-to-list 'eglot-server-programs '((cmake-mode) . ("cmake-language-server")))
  ;; Add hooks for all languages
  (dolist (hook '(python-mode-hook python-ts-mode-hook
				   rust-mode-hook rust-ts-mode-hook
				   go-mode-hook go-ts-mode-hook
				   typescript-mode-hook js-mode-hook tsx-mode-hook
				   sh-mode-hook bash-ts-mode-hook
				   c-mode-hook c++-mode-hook c-ts-mode-hook c++-ts-mode-hook
				   c-or-c++-mode-hook c-or-c++-ts-mode-hook
				   ;; Add Emacs Lisp for completion
				   emacs-lisp-mode-hook
				   ;; Add corfu for better completions with flycheck
				   c-mode-hook c++-mode-hook c-or-c++-ts-mode-hook))
    (add-hook hook 'eglot-ensure))

  ;; If `xref-find-definitions' lands in a file outside the project, momentarily
  ;; consider that file managed by the same language server. This avoids
  ;; starting a new language server for such external files (startup cost).
  (setq eglot-extend-to-xref t)

  ;; Add keybindings for eglot (after eglot is loaded)
  (with-eval-after-load 'eglot
    (define-key eglot-mode-map (kbd "M-.") 'xref-find-definitions)
    (define-key eglot-mode-map (kbd "M-?") 'xref-find-references)
    (define-key eglot-mode-map (kbd "K") 'eldoc)
    (define-key eglot-mode-map (kbd "C-c l r") 'eglot-rename)
    (define-key eglot-mode-map (kbd "C-c l f") 'eglot-format)
    (define-key eglot-mode-map (kbd "C-c l a") 'eglot-code-actions)
    (define-key eglot-mode-map (kbd "C-c l d") 'xref-find-declarations)
    (define-key eglot-mode-map (kbd "C-c l t") 'xref-find-type-definitions)
    (define-key eglot-mode-map (kbd "C-c l i") 'xref-find-implementation)
    (define-key eglot-mode-map (kbd "C-c l q") 'eglot-shutdown-all)
    (define-key eglot-mode-map (kbd "C-c l s") 'eglot-reconnect)))

;; ** citre
;;
;;   Citre is a code completion and navigation framework for Emacs.
;;   It works with various backends like ctags, gtags, and other tag systems.
;;   Provides fast completion, jump-to-definition, and find-references.
;;
;;   Key features:
;;   - Fast code completion from tags
;;   - Jump-to-definition (M-.)
;;   - Find references and usages
;;   - Multiple tag backends (ctags, gtags, global)
;;   - Integrated with completion frameworks (corfu)
;;   - Auto-generate tags when missing
;;
;;   Why I use it:
;;   Fast, offline code navigation without needing full LSP setup.
;;   Works well with legacy projects that use ctags.
;;
;;   GitHub: https://github.com/universal-ctags/citre
;;
;;   Configuration notes:
;;   Keybindings: M-. (jump), M-, (back), C-c c t (update tags).
;;   Integrates with corfu for completion UI.

(use-package citre
  :ensure t
  :commands (citre-jump citre-jump-back citre-ace-jump citre-update-tags citre-peek citre-find-file)
  :defer t
  :config
  ;; Enable citre-mode globally
  (global-citre-mode 1)
  ;; Set default backend to use ctags
  (setq citre-default-project 'ctags)
  ;; Enable completion at point
  (setq citre-completion-enable t)
  ;; Enable jump-to-definition
  (setq citre-jump-enable t)
  ;; Set cache directory for tags
  (setq citre-tags-file-name "TAGS")
  ;; Auto-create tags when none found
  (setq citre-auto-update-tags t)
  ;; Use corfu for completion UI integration
  (with-eval-after-load 'corfu
    (setq-local completion-at-point-functions
                (list (cape-capf-super
                       #'citre-completion-at-point
                       #'cape-file))))

  ;; Keybindings for citre (following eglot pattern)
  (with-eval-after-load 'citre
    (define-key citre-mode-map (kbd "M-.") 'citre-jump)
    (define-key citre-mode-map (kbd "M-,") 'citre-jump-back)
    (define-key citre-mode-map (kbd "M-*") 'citre-ace-jump)
    (define-key citre-mode-map (kbd "C-c c t") 'citre-update-tags)
    (define-key citre-mode-map (kbd "C-c c s") 'citre-peek)
    (define-key citre-mode-map (kbd "C-c c f") 'citre-find-file)))

;; ** mason
;;
;;   Mason.el provides integration with mason, a tool for managing LSP servers,
;;   DAP servers, linters, and formatters. It allows easy installation and
;;   management of language tools without manual setup.
;;
;;   Key features:
;;   - Easy package installation from registry
;;   - List available/manage installed packages
;;   - Automatic LSP server setup
;;   - DAP adapter support for debugging
;;   - Per-project package management
;;   - No manual PATH configuration needed
;;
;;   Why I use it:
;;   One-command installation of language servers and tools.
;;   Eliminates manual setup and PATH management for LSP/DAP.
;;
;;   GitHub: https://github.com/hlissner/emacs-mason
;;
;;   Configuration notes:
;;   Auto-installs: typescript, python (pyright), rust (rust-analyzer),
;;   go (gopls), c (clangd), cmake, bash, lua, json, yaml servers.

(use-package mason
  :ensure t
  :defer t
  :commands (mason-install mason-uninstall mason-list mason-info)
  :config
  ;; Set mason directory to cache
  (when (boundp 'user-cache-directory)
    (setq mason-cache-directory (expand-file-name "mason" user-cache-directory)))

  ;; Install commonly used servers automatically
  ;; LSP servers
  (dolist (pkg '("typescript-language-server"
                 "pyright"
                 "rust-analyzer"
                 "gopls"
                 "clangd"
                 "cmake-language-server"
                 "bash-language-server"
                 "lua-language-server"
                 "json-lsp"
                 "yaml-language-server"))
    (when (not (mason-installed-p pkg))
      (message "Mason: Installing LSP server %s..." pkg)
      (mason-install pkg))))

;; DAP servers for debugging (will be installed on-demand by dape-maybe-install-adapter)
;; These are installed when first needed to avoid startup delays

;; ** dape
;;
;;   DAPE (Debug Adapter Protocol for Emacs) provides debugging support
;;   for multiple languages using the Debug Adapter Protocol (DAP).
;;   It supports breakpoints, stepping, variable inspection, and more.
;;
;;   GitHub: https://github.com/svaante/dape

(use-package dape
  :ensure t
  :defer t
  :config
  ;; Save breakpoints between sessions
  (add-hook 'kill-emacs-hook #'dape-breakpoint-save)
  (add-hook 'after-init-hook #'dape-breakpoint-load)

  ;; Add keybindings for dape
  (with-eval-after-load 'dape
    ;; Add dape keymap to prog-mode
    (add-hook 'prog-mode-hook
	      (lambda ()
                (define-key prog-mode-map (kbd "C-c d") dape-global-map)))

    ;; Evil keybindings
    (with-eval-after-load 'evil
      (evil-define-key 'normal dape-info-mode-map
        (kbd "RET") #'dape-info-select)
      (evil-define-key 'normal dape-info-mode-map
        (kbd "TAB") #'dape-info-expand)
      (evil-define-key 'normal dape-info-mode-map
        (kbd "S-TAB") #'dape-info-collapse))

    ;; Helper function to ensure mason adapter is installed
    (defun dape-maybe-install-adapter (config)
      "Ensure the mason adapter specified in CONFIG is installed."
      (when-let ((adapter-id (plist-get config 'adapter-id)))
        (unless (mason-installed-p adapter-id)
          (message "Installing DAP adapter %s via mason..." adapter-id)
          (mason-install adapter-id))))

    ;; Configure adapters for various languages using mason-managed servers
    ;; Python
    (add-to-list 'dape-configs
                 `(debugpy
                   modes (python-mode python-ts-mode)
                   ensure dape-maybe-install-adapter
                   adapter-id "debugpy"
                   fn dape-config-autoport
                   command "python"
                   command-args ("-m" "debugpy.adapter")
                   :type "python"
                   :request "launch"
                   :cwd dape-cwd-fn
                   :program dape-find-file-buffer-default))

    ;; JavaScript/Node.js
    (add-to-list 'dape-configs
                 `(js-debug
                   modes (js-mode js-ts-mode typescript-mode typescript-ts-mode tsx-ts-mode)
                   ensure dape-maybe-install-adapter
                   adapter-id "js-debug-adapter"
                   fn dape-config-autoport
                   command "node"
                   command-args ,(lambda ()
                                   (let ((adapter-path (mason-get-path "js-debug-adapter")))
                                     (list "--loader" "tsx" (expand-file-name "js-debug/src/dapDebugServer.js" adapter-path))))
                   :type "pwa-node"
                   :request "launch"
                   :cwd dape-cwd-fn
                   :program dape-find-file-buffer-default))

    ;; Go
    (add-to-list 'dape-configs
                 `(delve
                   modes (go-mode go-ts-mode)
                   ensure dape-maybe-install-adapter
                   adapter-id "delve"
                   fn dape-config-autoport
                   command ,(lambda () (mason-get-path "delve"))
                   command-args ("dap")
                   :type "go"
                   :request "launch"
                   :cwd dape-cwd-fn
                   :program dape-find-file-buffer-default))

    ;; C/C++
    (add-to-list 'dape-configs
                 `(cpptools
                   modes (c-mode c++-mode c-ts-mode c++-ts-mode)
                   ensure dape-maybe-install-adapter
                   adapter-id "cpptools"
                   fn dape-config-autoport
                   command-cwd dape-command-cwd
                   command ,(lambda ()
			      (let ((adapter-path (mason-get-path "cpptools")))
                                (expand-file-name "extension/debugAdapters/bin/OpenDebugAD7" adapter-path)))
                   :type "cppdbg"
                   :request "launch"
                   :cwd dape-cwd-fn
                   :program dape-find-file-buffer-default
                   :MIMode "gdb"))

    ;; Rust
    (add-to-list 'dape-configs
                 `(codelldb
                   modes (rust-mode rust-ts-mode)
                   ensure dape-maybe-install-adapter
                   adapter-id "codelldb"
                   fn dape-config-autoport
                   command ,(lambda ()
			      (let ((adapter-path (mason-get-path "codelldb")))
                                (expand-file-name "extension/adapter/codelldb" adapter-path)))
                   :type "lldb"
                   :request "launch"
                   :cwd dape-cwd-fn
                   :program dape-find-file-buffer-default))))

;; ** realgud
;;
;;   RealGUD is a modular, extensible GNU Emacs front-end for interacting
;;   with external debuggers. It provides a consistent interface for
;;   debugging across multiple languages and debugger backends.
;;
;;   Key features:
;;   - Modular backend support (GDB, PDB, JDB, and more)
;;   - Consistent interface across debuggers
;;   - Works with GUD (Grand Unified Debugger)
;;   - Extensible for new backends
;;   - Better error handling and output
;;
;;   Why I use it:
;;   Unified debugging experience. Same commands work across different
;;   languages (C/C++, Python, Java, Ruby).
;;
;;   GitHub: https://github.com/realgud/realgud
;;
;;   Configuration notes:
;;   Keybindings: C-c d g (GDB), C-c d p (PDB), C-c d j (JDB), C-c d t (trepan).

(use-package realgud
  :ensure t
  :defer t
  :commands (realgud:gdb realgud:pdb realgud:jdb realgud:trepan)
  :config
  ;; Configure realgud for various languages
  ;; C/C++ with gdb
  (add-hook 'c-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c d g") 'realgud:gdb)))
  (add-hook 'c++-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c d g") 'realgud:gdb)))

  ;; Python with pdb
  (add-hook 'python-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c d p") 'realgud:pdb)))

  ;; Java with jdb
  (add-hook 'java-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c d j") 'realgud:jdb)))

  ;; Ruby with trepan
  (add-hook 'ruby-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c d t") 'realgud:trepan))))


;; * Debugging & Testing
;;
;;   Testing frameworks and debugging tools for various programming languages
;;   and Emacs Lisp development.
;;
;; ** ert
;;
;;   ERT (Emacs Lisp Regression Testing) is Emacs's built-in testing framework.
;;   Use `M-x ert' to run tests interactively.
;;
;;   Key commands:
;;   - M-x ert - Run tests interactively
;;   - M-x ert-run-tests-interactively - Run with UI
;;   - M-x ert-run-tests-batch-and-exit - Run all tests and exit
;;   - M-x ert-delete-testcover-data - Clear coverage data
;;
;;   Test files should be named *-test.el or end with test.el

(use-package ert
  :ensure nil				; built-in since Emacs 24
  :commands (ert-run-tests-interactively ert-run-tests-batch-and-exit)
  :config
  ;; Enable colors in batch mode
  (setq ert-font-lock-syntactic-face-function
        (lambda (char) (ert--face-for-mode char 'font-lock-comment-face)))

  ;; Use backtrace line limit for cleaner output
  (setq ert-backtrace-max-indent 40)

  ;; Show test duration
  (setq ert-show-duration t)

  ;; Load test files on demand
  (autoload 'my/load-test-file "ert-helper" "Load test file for current buffer." t)
  :bind
  ("C-c t" . my/ert-run-tests-for-current-file))

(defun my/ert-run-tests-for-current-file ()
  "Run ERT tests for the current buffer's corresponding test file."
  (interactive)
  (let* ((file-name (buffer-file-name))
         (test-file (if file-name
                        (replace-regexp-in-string "\\.el\\'" "-test.el" file-name)
                      nil)))
    (if (and test-file (file-exists-p test-file))
        (progn
          (load-file test-file)
          (ert-run-tests-interactively (file-name-base test-file)))
      (user-error "No test file found for %s" (buffer-name)))))

(defun my/ert-run-all-tests ()
  "Run all tests matching *-test.el pattern in the project."
  (interactive)
  (let ((test-files (directory-files-recursively
                     default-directory
                     "-test\\.el\\'")))
    (dolist (file test-files)
      (load-file file))
    (ert-run-tests-batch-and-exit)))

;; * Tree-sitter Support
;;
;;   Tree-sitter is an incremental parsing library. It provides a
;;   faster and more powerful alternative to built-in Emacs parsing.

(use-package treesit
  :ensure nil 				; build-in since emacs-29.1
  :when (and (fboundp 'treesit-available-p)
             (treesit-available-p))
  :config (setq treesit-font-lock-level 4)
  :init
  ;; install source parser for tree-sitter
  (setq treesit-language-source-alist
        '(
          (bash       . ("https://github.com/tree-sitter/tree-sitter-bash"))
          (c          . ("https://github.com/tree-sitter/tree-sitter-c"))
          (cmake      . ("https://github.com/uyha/tree-sitter-cmake"))
          (cpp        . ("https://github.com/tree-sitter/tree-sitter-cpp"))
          (csharp     . ("https://github.com/tree-sitter/tree-sitter-c-sharp.git"))
          (css        . ("https://github.com/tree-sitter/tree-sitter-css"))
          (devicetree . ("https://github.com/joelspadin/tree-sitter-devicetree"))
          (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
          (elisp      . ("https://github.com/Wilfred/tree-sitter-elisp"))
          (go         . ("https://github.com/tree-sitter/tree-sitter-go"))
          (gomod      . ("https://github.com/camdencheek/tree-sitter-go-mod.git"))
          (html       . ("https://github.com/tree-sitter/tree-sitter-html"))
          (java       . ("https://github.com/tree-sitter/tree-sitter-java.git"))
          (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
          (json       . ("https://github.com/tree-sitter/tree-sitter-json"))
          (lua        . ("https://github.com/Azganoth/tree-sitter-lua"))
          (make       . ("https://github.com/alemuller/tree-sitter-make"))
          (markdown   . ("https://github.com/MDeiml/tree-sitter-markdown" nil "tree-sitter-markdown/src"))
          (ocaml      . ("https://github.com/tree-sitter/tree-sitter-ocaml" nil "ocaml/src"))
          (org        . ("https://github.com/milisims/tree-sitter-org"))
          (php        . ("https://github.com/tree-sitter/tree-sitter-php"))
          (python     . ("https://github.com/tree-sitter/tree-sitter-python"))
          (qmljs      . ("https://github.com/yuja/tree-sitter-qmljs"))
          (ruby       . ("https://github.com/tree-sitter/tree-sitter-ruby"))
          (rust       . ("https://github.com/tree-sitter/tree-sitter-rust"))
          (sql        . ("https://github.com/m-novikov/tree-sitter-sql"))
          (toml       . ("https://github.com/BurntSushi/tree-sitter-toml"))
          (tsx        . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src"))
          (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src"))
          (vue        . ("https://github.com/merico-dev/tree-sitter-vue"))
          (yaml       . ("https://github.com/ikatyang/tree-sitter-yaml"))
          (zig        . ("https://github.com/GrayJack/tree-sitter-zig")))
        )

  ;; Auto-install tree-sitter grammars when needed
  ;; Generate mode-to-language mapping from treesit-language-source-alist
  (setq my/treesit-mode-to-language-alist
        (mapcar (lambda (lang-entry)
                  (cons (intern (format "%s-ts-mode" (car lang-entry)))
                        (car lang-entry)))
                treesit-language-source-alist))

  ;; Handle special case: c++-ts-mode uses "cpp" grammar
  (push '(c++-ts-mode . cpp) my/treesit-mode-to-language-alist)

  ;; Auto-install tree-sitter grammar when entering a tree-sitter mode
  (defun my/treesit-maybe-install-grammar ()
    "Auto-install tree-sitter grammar for current mode if not available."
    (when (and (boundp 'major-mode) major-mode)
      (let* ((lang (cdr (assoc major-mode my/treesit-mode-to-language-alist)))
             (parser-path (when lang (treesit-parser-create-path lang))))
        ;; Install if grammar doesn't exist
        (when (and lang (not (file-directory-p parser-path)))
          (message "[treesit] Auto-installing grammar for %s..." lang)
          (condition-case err
              (treesit-install-language-grammar lang)
            (error
             (message "[treesit] Failed to install %s: %s" lang err)))))))

  ;; Run auto-install hook for all tree-sitter modes
  (dolist (mode (mapcar #'car my/treesit-mode-to-language-alist))
    (add-hook (intern (format "%s-hook" mode)) #'my/treesit-maybe-install-grammar))

  ;; treesit mode will create lang-ts-mode, take python for example, we
  ;; will have python-mode (native) and python-ts-mode (tree-sitter)
  ;; setup the major-modes we want to override here
  (add-to-list 'auto-mode-alist '("/go\\.mod\\'" . go-mod-ts-mode))
  (add-to-list 'auto-mode-alist '("\\(?:Dockerfile\\(?:\\..*\\)?\\|\\.[Dd]ockerfile\\)\\'" . dockerfile-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.y[a]?ml\\'" . yaml-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c++-mode        . c++-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c-mode          . c-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c-or-c++-mode   . c-or-c++-ts-mode))
  (add-to-list 'major-mode-remap-alist '(conf-toml-mode  . toml-ts-mode))
  (add-to-list 'major-mode-remap-alist '(css-mode        . css-ts-mode))
  (add-to-list 'major-mode-remap-alist '(java-mode       . java-ts-mode))
  (add-to-list 'major-mode-remap-alist '(js-json-mode    . json-ts-mode))
  (add-to-list 'major-mode-remap-alist '(js-mode         . js-ts-mode))
  (add-to-list 'major-mode-remap-alist '(makefile-mode   . cmake-ts-mode))
  (add-to-list 'major-mode-remap-alist '(python-mode     . python-ts-mode))
  (add-to-list 'major-mode-remap-alist '(ruby-mode       . ruby-ts-mode))
  (add-to-list 'major-mode-remap-alist '(sh-mode         . bash-ts-mode))
  ;;(add-to-list 'major-mode-remap-alist '(qml-mode       . qmljs-ts-mode))
  )

;; * Snippets and Templates
;;
;; ** yasnippet

(use-package yasnippet
  :ensure t
  :mode ("emacs.+/snippets/" . snippet-mode)
  :commands (yas-global-mode yas-minor-mode)
  :config
  ;; enable yasnippet globally
  (yas-global-mode 1)
  ;; extra yasnipet configs
  (setq yas-prompt-functions '(yas-dropdown-prompt
                               yas-completing-prompt
                               yas-ido-prompt))
  (let ((my-snippet-dir (concat user-emacs-directory "snippets")))
    (if (and (file-exists-p my-snippet-dir)
    	     (not (member my-snippet-dir yas/snippet-dirs)))
       	(add-to-list 'yas-snippet-dirs my-snippet-dir)))
  (add-hook 'term-mode-hook (lambda() (yas-minor-mode -1)))
  (defadvice yas-expand (around major-mode-expand activate)
    "Try to complete a structure template before point like org-mode does.
  This looks for strings like \"<e\" on an otherwise empty line and
  expands them.
  Before use this function, you must setup `major-mode-name'-expand-alist variable.

   Take emacs-lisp-mode as example, if you want to use <r to expand your snippet `require'
   in yasnippet, you must setup the emacs-lisp-mode-expand-alist variable.

    (setq emacs-lisp-expand-alist '((\"r\" . \"require\")))"
    (let* ((l (buffer-substring (pos-bol) (point)))
           (expand-symbol (intern (concat (symbol-name major-mode) "-expand-alist")))
           (expand-alist (if (boundp expand-symbol) (symbol-value expand-symbol) nil))
           a)
      (when (and (looking-at "[ \t]*$")
                 (string-match "^[ \t]*<\\([a-zA-Z]+\\)$" l)
                 (setq a (assoc (match-string 1 l) expand-alist)))
        (delete-char (1+ (length (car-safe a))))
        (if (symbolp (cdr-safe a))
            (funcall (cdr-safe a))
          (insert (cdr-safe a)))
        t)
      ad-do-it)))

;; * Shell Scripting Support
;;
;; ** flymake-shell
;;
;;   flymake-shell provides on-the-fly syntax checking for shell scripts.
;;   Uses shellcheck or bash -n to validate shell syntax.
;;
;;   Key features:
;;   - Real-time shell syntax checking
;;   - Supports multiple shells (bash, sh, zsh)
;;   - Integration with flymake framework
;;   - Detects common shell script errors
;;   - Shows errors in flymake UI
;;
;;   Why I use it:
;;   Immediate feedback on shell script syntax errors before
;;   running. Prevents bugs from reaching production.
;;
;;   GitHub: https://github.com/purcell/flymake-shell
;;
;;   Configuration notes:
;;   Enabled for all shell modes via sh-set-shell-hook.

(use-package flymake-shell
  :ensure t
  :commands (flymake-shell-load)
  :config (add-hook 'sh-set-shell-hook 'flymake-shell-load))

;; * Language Support - Hardware/Embedded
;;
;; ** bitbake

(use-package bitbake
  :ensure t
  :mode ("\\.bb\\'" "\\.bbappend\\'"))

;; ** devicetree-ts-mode

(use-package devicetree-ts-mode
  :ensure t
  :mode ("\\.dts\\'" "\\.dtsi\\'")
  :init
  (with-eval-after-load 'treesit
    (add-to-list 'treesit-language-source-alist
                 '(devicetree "https://github.com/joelspadin/tree-sitter-devicetree"))))

;; ** dart-mode

(use-package dart-mode
  :ensure t
  :mode ("\\.dart\\'")
  :config
  ;; enable analyzer support
  (setq dart-enable-analysis-server t))

;; * Language Support - Graphics & Visualization
;;
;; ** gnuplot

(use-package gnuplot :ensure t
  :commands gnuplot-mode
  :mode "\\.gp$")

;; ** graphviz-dot-mode

(use-package graphviz-dot-mode :ensure t
  :mode "\\.dot\\'"
  :config
  ;; alias `dot-mode' to graphviz-dot-mode
  (defalias 'dot-mode 'graphviz-dot-mode))

;; ** glsl-mode

(use-package glsl-mode :ensure t
  :mode (("\\.vs\\'" . glsl-mode)
         ("\\.fs\\'" . glsl-mode)
         ("\\.gs\\'" . glsl-mode))
  :config
  (setq glsl-other-file-alist '(("\\.fs$" (".vs")) ("\\.vs$" (".fs")))))

;; * Language Support - JavaScript/TypeScript
;;
;; ** js2-mode

(use-package js2-mode
  :ensure t
  :mode "\\.js\\'")

;; ** js2-refactor
;;
;;   js2-refactor provides semantic refactoring operations for JavaScript.
;;   Works on JavaScript/TypeScript code with js2-mode parsing.
;;
;;   Key features:
;;   - Extract function/method (M-x js2r-extract-function)
;;   - Wrap in try/catch (M-x js2r-wrap-in-try-catch)
;;   - Convert var/let/const (M-x js2r-var-to-let)
;;   - Inline function (M-x js2r-inline-function)
;;   - Toggle between arrow functions and regular functions
;;   - Rename variables intelligently
;;
;;   Why I use it:
;;   Semantic-aware refactoring for JavaScript/TypeScript code.
;;   Reduces manual edits for common refactoring patterns.
;;
;;   GitHub: https://github.com/mooz/js2-refactor.el
;;
;;   Configuration notes:
;;   Works with js2-mode. Requires js2-mode for parsing.

(use-package js2-refactor
  :ensure t
  :defer t
  :after js2-mode)

;; ** nvm
;;
;;   nvm.el integrates Node Version Manager (nvm) with Emacs.
;;   Allows switching Node.js versions per project or globally.
;;
;;   Key features:
;;   - Auto-detect project .nvmrc files
;;   - Switch Node.js versions
;;   - Display current Node version in mode-line
;;   - Project-specific Node versions
;;   - Works with Node, npm, yarn commands
;;
;;   Why I use it:
;;   Project-specific Node.js versions. Different projects use
;;   different Node versions without manual switching.
;;
;;   GitHub: https://github.com/emacsorphanage/nvm.el
;;
;;   Configuration notes:
;;   Use .nvmrc files in project directories for version control.

(use-package nvm
  :ensure t
  :commands (nvm-use nvm-switch-version nvm-which))

;; ** import-js
;;
;;   import-js automatically adds import statements for JavaScript modules.
;;   Scans buffer for require() calls and adds ES6 import statements.
;;
;;   Key features:
;;   - Auto-generate import statements
;;   - Sort imports by module
;;   - Remove duplicate imports
;;   - Convert to ES6 import syntax
;;   - Works with CommonJS modules
;;
;;   Why I use it:
;;   Automates import management in legacy JavaScript code.
;;   Reduces manual import editing when refactoring modules.
;;
;;   GitHub: https://github.com/GregCoke/js-import-import
;;
;;   Configuration notes:
;;   Use M-x import-js-add-import to add imports manually.

(use-package import-js
  :ensure t
  :commands (import-js-add-import import-js-switch-to-es6))

;; * Internationalization
;;
;; ** po-mode

(use-package po-mode :ensure t
  :mode "\\.po\\'\\|\\.po\\."
  :config

  ;; To use the right coding system automatically under Emacs 20 or newer,
  ;; also add:
  (when (require 'po nil 'noerror)
    (modify-coding-system-alist 'file "\\.po\\'\\|\\.po\\."
                                'po-find-file-coding-system))
  )

;; * Language Support - Python
;;
;;   Comprehensive Python development environment with virtual environment
;;   management, code formatting, linting, import sorting, and testing support.
;;
;; ** python-mode
;;
;;   Built-in Python major mode for Emacs.
;;
;;   Key features:
;;   - Syntax highlighting for Python code
;;   - Indentation support
;;   - Integration with inferior Python process
;;   - Support for different Python versions
;;
;;   Why I use it:
;;   Foundation for Python development in Emacs. Provides basic
;;   editing capabilities and REPL integration.
;;
;;   Built-in since Emacs 24.
;;
;;   Configuration notes:
;;   Extended mode associations for SCons and DEPS files.

(use-package python
  :ensure nil                          ; built-in
  :mode (("SCons\\(truct\\|cript\\)\\'" . python-mode)
         ("DEPS" . python-mode))
  :config
  ;; Use python3 by default
  (setq python-shell-interpreter "python3")
  ;; Enable eldoc for Python
  (add-hook 'python-mode-hook 'eldoc-mode)
  ;; Enable electric-pair-mode for automatic bracket pairing
  (add-hook 'python-mode-hook 'electric-pair-mode))

;; ** pyvenv
;;
;;   Virtual environment management for Python projects.
;;
;;   Key features:
;;   - Activate/deactivate virtual environments
;;   - Auto-activate based on .venv directory or pyvenv.cfg
;;   - Support for conda environments
;;   - Environment switching
;;   - Integration with projectile
;;
;;   Why I use it:
;;   Essential for managing project-specific Python environments.
;;   Prevents dependency conflicts between projects.
;;
;;   GitHub: https://github.com/jorgenschaefer/pyvenv
;;
;;   Configuration notes:
;;   Automatically activates virtual environments when entering Python projects.

(use-package pyvenv
  :ensure t
  :commands (pyvenv-activate pyvenv-deactivate)
  :config
  ;; Auto-activate virtual environments
  (pyvenv-mode t)
  ;; Set default virtual environment location
  (setq pyvenv-default-virtual-env-name "venv"))

;; ** python-black
;;
;;   Black code formatter integration for Emacs.
;;
;;   Key features:
;;   - Format Python code with Black
;;   - Asynchronous formatting
;;   - Format buffer or region
;;   - Configurable Black executable path
;;   - Error highlighting
;;
;;   Why I use it:
;;   Consistent code formatting following Python community standards.
;;   Reduces time spent on style arguments and manual formatting.
;;
;;   GitHub: https://github.com/psf/black
;;
;;   Configuration notes:
;;   Requires Black to be installed (pip install black).
;;   Formats on save if enabled.

(use-package python-black
  :ensure t
  :commands (python-black-buffer python-black-region)
  :config
  ;; Enable formatting on save
  (add-hook 'python-mode-hook 'python-black-on-save-mode))

;; ** python-isort
;;
;;   Import sorting with isort for Python.
;;
;;   Key features:
;;   - Sort Python imports automatically
;;   - Group imports by type (stdlib, third-party, local)
;;   - Configurable import sections
;;   - Integration with python-mode
;;
;;   Why I use it:
;;   Keeps imports organized and consistent across the codebase.
;;   Follows PEP8 import ordering standards.
;;
;;   GitHub: https://github.com/pycqa/isort
;;
;;   Configuration notes:
;;   Requires isort to be installed (pip install isort).
;;   Can be configured via .isort.cfg or pyproject.toml.

(use-package python-isort
  :ensure t
  :commands (python-isort-buffer python-isort-region)
  :config
  ;; Sort imports on save
  (add-hook 'python-mode-hook 'python-isort-on-save-mode))

;; ** pytest
;;
;;   Pytest integration for running Python tests in Emacs.
;;
;;   Key features:
;;   - Run pytest on current buffer or function
;;   - Test discovery and execution
;;   - Result display in compilation buffer
;;   - Support for pytest fixtures and parametrization
;;   - Integration with projectile
;;
;;   Why I use it:
;;   Essential for test-driven development. Allows running tests
;;   directly from Emacs without switching to terminal.
;;
;;   GitHub: https://github.com/wbolster/emacs-python-pytest
;;
;;   Configuration notes:
;;   Requires pytest to be installed (pip install pytest).
;;   Keybindings: C-c C-t (run tests for current buffer).

(use-package python-pytest
  :ensure t
  :commands (python-pytest-run-def
             python-pytest-run-file
             python-pytest-run-directory
             python-pytest-popup)
  :bind (:map python-mode-map
              ("C-c C-t" . python-pytest-popup)))

;; * Language Support - Other Languages
;;
;;   Support for XML and various configuration file formats used in
;;   development, deployment, and system administration.
;;
;; ** nxml-mode
;;
;;   Built-in XML editing mode for Emacs.
;;
;;   Key features:
;;   - Syntax highlighting for XML
;;   - Automatic indentation
;;   - Tag completion and validation
;;   - Support for XSLT, XSD, and other XML formats
;;   - Folding support
;;
;;   Why I use it:
;;   Comprehensive XML editing with built-in validation and formatting.
;;   Handles complex XML documents better than generic modes.
;;
;;   Built-in since Emacs 23.
;;
;;   Configuration notes:
;;   Auto-detects XML files by content. Replaces older xml-mode and sgml-mode.

(use-package nxml-mode
  :ensure nil                  ; emacs built-in
  :mode (("\\.plist\\'" . nxml-mode)
         ("\\.rss\\'"   . nxml-mode)
         ("\\.svg\\'"   . nxml-mode)
         ("\\.xml\\'"   . nxml-mode)
         ("\\.xsd\\'"   . nxml-mode)
         ("\\.xslt\\'"  . nxml-mode)
         ("\\.pom$"     . nxml-mode))
  :config
  ;; Any file start with xml will be treat as nxml-mode
  (add-to-list 'magic-mode-alist '("<\\?xml" . nxml-mode))
  ;; Use nxml-mode instead of sgml, xml or html mode.
  (mapc
   (lambda (pair)
     (if (or (eq (cdr pair) 'xml-mode)
             (eq (cdr pair) 'sgml-mode))
         (setcdr pair 'nxml-mode)))
   auto-mode-alist))

;; ** yaml-mode
;;
;;   Major mode for editing YAML files.
;;
;;   Key features:
;;   - Syntax highlighting for YAML
;;   - Indentation support
;;   - Comment/uncomment support
;;   - Outline mode integration
;;
;;   Why I use it:
;;   Essential for editing configuration files in YAML format,
;;   commonly used in Kubernetes, Docker Compose, and CI/CD pipelines.
;;
;;   GitHub: https://github.com/yoshiki/yaml-mode
;;
;;   Configuration notes:
;;   Used for Kubernetes manifests, Docker Compose, and configuration files.

(use-package yaml-mode
  :ensure t
  :mode ("\\.yml$"
         "\\.yaml$"))

;; ** toml-mode
;;
;;   Major mode for editing TOML files.
;;
;;   Key features:
;;   - Syntax highlighting for TOML
;;   - Indentation and formatting
;;   - Comment support
;;
;;   Why I use it:
;;   TOML is widely used for configuration in Rust projects (Cargo.toml),
;;   Python projects (pyproject.toml), and various tools.
;;
;;   GitHub: https://github.com/dryman/toml-mode.el
;;
;;   Configuration notes:
;;   Supports TOML v1.0 specification.

(use-package toml-mode
  :ensure t
  :mode "\\.toml$")

;; ** ini-mode
;;
;;   Major mode for editing INI-style configuration files.
;;
;;   Key features:
;;   - Syntax highlighting for INI files
;;   - Section and key highlighting
;;   - Comment support
;;
;;   Why I use it:
;;   INI files are used in many applications for configuration,
;;   especially on Windows and in legacy systems.
;;
;;   GitHub: https://github.com/Lindydancer/ini-mode
;;
;;   Configuration notes:
;;   Supports standard INI format with sections and key-value pairs.

(use-package ini-mode
  :ensure t
  :mode ("\\.ini\\'" "\\.cfg\\'" "\\.conf\\'"))

;; ** conf-mode
;;
;;   Generic mode for configuration files.
;;
;;   Key features:
;;   - Basic syntax highlighting for config files
;;   - Comment support (# and ;)
;;   - Key-value pair recognition
;;
;;   Why I use it:
;;   Fallback mode for configuration files that don't have specific modes.
;;   Provides basic editing support for various config formats.
;;
;;   Built-in Emacs feature.
;;
;;   Configuration notes:
;;   Used for generic configuration files like .gitconfig, .npmrc, etc.

(use-package conf-mode
  :ensure nil  ; built-in
  :mode (("\\.gitconfig\\'" . conf-mode)
         ("\\.npmrc\\'" . conf-mode)
         ("\\.env\\'" . conf-mode)
         ("\\.envrc\\'" . conf-mode)))

;; ** nginx-mode
;;
;;   Major mode for editing nginx configuration files.
;;
;;   Key features:
;;   - Syntax highlighting for nginx directives
;;   - Context-aware indentation
;;   - Block structure recognition
;;
;;   Why I use it:
;;   Essential for editing nginx web server configurations.
;;   Provides proper syntax highlighting and structure understanding.
;;
;;   GitHub: https://github.com/ajc/nginx-mode
;;
;;   Configuration notes:
;;   Recognizes nginx.conf and sites-enabled files.

(use-package nginx-mode
  :ensure t
  :mode ("nginx\\.conf\\'" "/nginx/.+\\.conf\\'"))

;; ** apache-mode
;;
;;   Major mode for editing Apache configuration files.
;;
;;   Key features:
;;   - Syntax highlighting for Apache directives
;;   - Comment support
;;   - Section recognition
;;
;;   Why I use it:
;;   Useful for editing Apache web server configurations.
;;   Supports .htaccess and httpd.conf files.
;;
;;   GitHub: https://github.com/emacs-php/apache-mode
;;
;;   Configuration notes:
;;   Recognizes Apache configuration files and .htaccess.

(use-package apache-mode
  :ensure t
  :mode (("\\.htaccess\\'" . apache-mode)
         ("httpd\\.conf\\'" . apache-mode)
         ("apache2\\.conf\\'" . apache-mode)))

;; ** dockerfile-mode
;;
;;   Major mode for editing Dockerfiles.
;;
;;   Key features:
;;   - Syntax highlighting for Docker instructions
;;   - Indentation support
;;   - Build command integration
;;
;;   Why I use it:
;;   Essential for creating and editing Docker container definitions.
;;   Provides proper syntax highlighting for Docker commands.
;;
;;   GitHub: https://github.com/spotify/dockerfile-mode
;;
;;   Configuration notes:
;;   Recognizes Dockerfile and .dockerfile extensions.

(use-package dockerfile-mode
  :ensure t
  :mode ("Dockerfile\\'" "\\.dockerfile\\'"))

;; ** docker-compose-mode
;;
;;   Major mode for editing Docker Compose files.
;;
;;   Key features:
;;   - Syntax highlighting for docker-compose.yml
;;   - YAML-based structure recognition
;;   - Service and network highlighting
;;
;;   Why I use it:
;;   Specialized support for Docker Compose configuration files.
;;   Builds on yaml-mode with Docker-specific highlighting.
;;
;;   GitHub: https://github.com/meqif/docker-compose-mode
;;
;;   Configuration notes:
;;   Works with docker-compose.yml and docker-compose.*.yml files.

(use-package docker-compose-mode
  :ensure t
  :mode ("docker-compose\\.yml\\'" "docker-compose\\..*\\.yml\\'"))

;; ** JSON Support
;;
;;   JSON (JavaScript Object Notation) is a lightweight data interchange format
;;   widely used for configuration files, API communication, and data storage.
;;   This subsection provides comprehensive tools for editing, formatting, and
;;   validating JSON files in Emacs.
;;
;; *** json-mode
;;
;;   Major mode for editing JSON files.
;;
;;   Key features:
;;   - Syntax highlighting for JSON
;;   - Automatic indentation
;;   - Validation
;;   - Pretty printing
;;
;;   Why I use it:
;;   Essential for editing JSON configuration files, API responses,
;;   and data interchange formats.
;;
;;   GitHub: https://github.com/joshwnj/json-mode
;;
;;   Configuration notes:
;;   Integrates with json-reformat for pretty printing.

(use-package json-mode
  :ensure t
  :mode "\\.json$")

;; ** json-reformat
;;
;;   Reformat JSON with proper indentation and formatting.
;;
;;   Key features:
;;   - Pretty-print JSON with consistent formatting
;;   - Command-line tool integration
;;   - Region or buffer formatting
;;
;;   Why I use it:
;;   Makes JSON files readable and consistently formatted.
;;   Essential for working with minified or poorly formatted JSON.
;;
;;   GitHub: https://github.com/gongo/json-reformat
;;
;;   Configuration notes:
;;   Use M-x json-reformat-region on selected JSON text.

(use-package json-reformat
  :ensure t
  :commands json-reformat-region)

;; ** flymake-json
;;
;;   Flymake backend for JSON syntax checking.
;;
;;   Key features:
;;   - Real-time JSON validation
;;   - Parses JSON to detect syntax errors
;;   - Shows line and column numbers
;;   - Integrates with flymake UI
;;   - Works with json-mode and other JSON modes
;;
;;   Why I use it:
;;   Early detection of JSON syntax errors. Essential for config
;;   files and API responses.
;;
;;   GitHub: https://github.com/oantolin/flymake-json
;;
;;   Configuration notes:
;;   Enabled for all json-mode buffers.

(use-package flymake-json
  :ensure t
  :commands (flymake-json-load)
  :config
  (add-hook 'json-mode-hook (lambda () (flymake-json-load))))

;; * Language Support - Lisp Development
;;
;;   Comprehensive Lisp development environment supporting multiple Lisp dialects
;;   with structural editing, debugging, and interactive development tools.
;;
;; ** Structural Editing and Visualization
;;
;; *** lispy with lispyville integration
;;
;;   Lispy provides structural editing for lisp-like languages.
;;   lispyville provides comprehensive evil integration with safe operations.
;;
;;   Key features:
;;   - Navigate S-expressions with arrow keys
;;   - Jump between delimiters with `(` and `)`
;;   - Safe delete/copy/kill operations that preserve structure
;;   - Additional movement and prettify commands
;;   - Works seamlessly with evil-mode
;;
;;   Why I use it:
;;   Advanced structural editing that understands Lisp syntax.
;;   Makes editing complex nested expressions much easier.
;;
;;   GitHub: https://github.com/abo-abo/lispy
;;
;;   Configuration notes:
;;   Enabled for all Lisp modes. Evil integration via lispyville.

(use-package lispy
  :ensure t
  :config
  ;; Compatibility with other modes
  (setq lispy-compat '(god-mode edebug cider))
  ;; Enable lispy for lisp modes
  :hook ((emacs-lisp-mode clojure-mode lisp-mode scheme-mode) . lispy-mode))

(use-package lispyville
  :ensure t
  :after (lispy evil)
  :commands (lispyville-set-key-theme)
  :hook (lispy-mode . lispyville-mode)
  :config
  ;; Configure key theme with safe operators
  (lispyville-set-key-theme
   '(operators
     c-w
     (prettify)
     (additional-movement
      normal
      visual
      motion))))

;; *** rainbow-delimiters
;;
;;   Color-code parentheses and other delimiters by nesting level.
;;
;;   Key features:
;;   - Color-code delimiters by depth
;;   - Customizable color schemes
;;   - Works with all programming modes
;;   - Improves readability of nested expressions
;;
;;   Why I use it:
;;   Essential for reading and editing deeply nested Lisp code.
;;   Makes it easy to spot mismatched parentheses and understand structure.
;;
;;   GitHub: https://github.com/Fanael/rainbow-delimiters
;;
;;   Configuration notes:
;;   Enabled globally for all Lisp modes.

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; *** eros
;;
;;   Evaluation Result Overlays for Emacs Lisp.
;;
;;   Key features:
;;   - Show evaluation results inline as overlays
;;   - Display results near the evaluated expression
;;   - Customizable display format
;;   - Works with eval-last-sexp and other eval commands
;;
;;   Why I use it:
;;   Makes interactive Lisp development much more convenient.
;;   See results immediately without checking the minibuffer.
;;
;;   GitHub: https://github.com/xiongtx/eros
;;
;;   Configuration notes:
;;   Enabled for Emacs Lisp mode. Integrates with eval commands.

(use-package eros
  :ensure t
  :hook (emacs-lisp-mode . eros-mode))

;; *** prettify-symbols-mode
;;
;;   Display lambda as λ, function as ƒ, etc.
;;
;;   Key features:
;;   - Replace text with Unicode symbols
;;   - Customizable symbol mappings
;;   - Improves code readability
;;
;;   Why I use it:
;;   Makes Lisp code more readable by using mathematical symbols
;;   for common functions and operators.
;;
;;   Built-in Emacs feature.
;;
;;   Configuration notes:
;;   Enabled globally.

(global-prettify-symbols-mode 1)

;; ** Emacs Lisp Development
;;
;; *** Enhanced Emacs Lisp mode
;;
;;   Built-in Emacs Lisp major mode with additional enhancements.
;;
;;   Key features:
;;   - Syntax highlighting for Elisp
;;   - Integration with eval commands
;;   - Documentation lookup
;;   - Symbol completion
;;
;;   Why I use it:
;;   Essential for Emacs configuration and package development.
;;   Provides all necessary tools for Elisp programming.
;;
;;   Built-in since Emacs 24.
;;
;;   Configuration notes:
;;   Enhanced with eros, lispy, and other tools above.

;; ** Clojure Development
;;
;; *** clojure-mode
;;
;;   Major mode for editing Clojure code.
;;
;;   Key features:
;;   - Syntax highlighting for Clojure
;;   - Indentation rules for Clojure
;;   - Font-locking for special forms
;;   - Integration with CIDER
;;
;;   Why I use it:
;;   Essential foundation for Clojure development in Emacs.
;;   Provides basic editing capabilities for Clojure code.
;;
;;   GitHub: https://github.com/clojure-emacs/clojure-mode
;;
;;   Configuration notes:
;;   Works with lispy for structural editing.

(use-package clojure-mode
  :ensure t
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.cljs\\'" . clojure-mode)
         ("\\.cljc\\'" . clojure-mode)
         ("\\.edn\\'" . clojure-mode))
  :hook (clojure-mode . lispy-mode))

;; *** cider
;;
;;   Clojure Interactive Development Environment that Rocks.
;;
;;   Key features:
;;   - Interactive REPL with completion
;;   - Code evaluation and debugging
;;   - Test runner integration
;;   - Namespace management
;;   - Documentation lookup
;;   - Refactoring tools
;;
;;   Why I use it:
;;   The premier Clojure development environment for Emacs.
;;   Provides rich interactive development experience.
;;
;;   GitHub: https://github.com/clojure-emacs/cider
;;
;;   Configuration notes:
;;   Requires Leiningen or Boot for project management.

(use-package cider
  :ensure t
  :commands (cider-jack-in cider-connect)
  :config
  ;; Enhanced REPL experience
  (setq cider-repl-display-help-banner nil)
  ;; Enable cider-mode in clojure buffers
  (add-hook 'clojure-mode-hook 'cider-mode))



;; * Language Support - Web Development
;;
;; ** web-mode

(use-package web-mode
  :ensure t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.ejs?\\'" . web-mode)))

;; ** css-mode

(use-package css-mode
  :ensure nil				; built-in
  :mode "\\.css\\'")

;; ** css-eldoc

(use-package css-eldoc
  :ensure t
  :config
  (add-hook 'css-mode-hook 'turn-on-css-eldoc)
  (add-hook 'scss-mode-hook 'turn-on-css-eldoc)
  (add-hook 'less-css-mode-hook 'turn-on-css-eldoc))

;; ** less-css-mode

(use-package less-css-mode
  :ensure t
  :mode ("\\.less$" . less-css-mode))

;; ** scss-mode

(use-package scss-mode
  :ensure t
  :mode "\\.scss\\'"
  :config
  ;; dont' build scss to css after save file
  (setq scss-compile-at-save nil))

(use-package mustache-mode :mode "\\.mustache$" :ensure t)

;; * Window Management
;;
;; ** winner-mode

(use-package winner                     ; builtin
  :ensure nil				; built-in
  :demand t  ; Needed for window navigation at startup
  :commands (winner-undo winner-redo)
  :config
  ;; I use my own keymap for winner-mode
  (setq winner-dont-bind-my-keys t)
  ;; Start winner-mode globally
  (winner-mode t))

;; ** eyebrowse

(use-package eyebrowse
  :ensure t
  :demand t  ; Needed for workspace management at startup
  :config
  ;; enable eyebrowse globally
  (eyebrowse-mode t))

;; ** window-numbering

(use-package window-numbering
  :ensure t)

;; ** Keybindings

(bind-keys :map global-map
           ("C-x C-s" . my/save-buffer-always))

;; * Personal Configuration
;;
;;   Load personal settings if available.

(let ((secret "~/.personal.el"))
  (when (file-exists-p secret) (load-file secret)))

;; * Final sections

(provide 'init.el)
;;; init.el ends here
