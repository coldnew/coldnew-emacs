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
;; * Emacs Compatibility and Core Setup
;;
;; Use Common Lisp Extension
;;
;;   Some of my function may need the Common Lisp Extension, let's
;;   import libraries first.

(require 'cl-lib)                       ; built-in

;; Load extra builtin library
;;
;;   Add some extra buildin library I will use in my config file.

(require 'find-lisp)

;; * Directory Variables Setup
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
  "My emacs config directory.")

;;   Setup the cache directory to store some cache content.

(defconst user-cache-directory
  (file-name-as-directory (concat user-emacs-directory ".cache"))
  "My emacs storage area for persistent files.")
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

;; * Load Path Setup
;;
;;   The variable =load-path= lists all the directories where Emacs
;;   should look for emacs-lisp files.
;;

(eval-and-compile
  (dolist (dir '("local-lisp" "styles"))
    (let ((full-dir (expand-file-name dir user-emacs-directory)))
      (when (and full-dir (file-exists-p full-dir))
        (add-to-list 'load-path full-dir)))))

;; * Under Mac OSX use Command key as ALT
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

;; * Customization and Backup
;;
;; ** Save custom-file as cache
;;
;;   Most of my config is written in this file, it's no need to
;;   tracking the emacs's custom-setting.
;;
;;   I move the file to cache-dir and make git ignore it.

(setq-default custom-file (concat user-cache-directory "custom.el"))
;; load custom-file only when file exist
(when (file-exists-p custom-file)
  (load-file custom-file))

;; ** Setup backup files
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

;; * Startup emacs server
;;
;;   Only start server mode if I'm not root

(unless (string-equal "root" (getenv "USER"))
  (require 'server)
  (when (fboundp 'server-running-p)
    (unless (server-running-p) (server-start))))

;; * Disable Default Stuff
;;
;; ** Turn-off Alarm Bell

(setq ring-bell-function #'ignore)

;; ** Clean scratch buffer messages
;;
;;   Leave me a clean **scratch** buffer and I'll be more happy :)

(setq initial-scratch-message "")

;; ** Use visible bell instead of buzzer

(setq visible-bell t)

;; ** Ask for y or n, not yes or no
;;
;;   Emacs starts out asking for you to type yes or no with most
;;   important questions. Just let me use =y= or =n= with no =RET=
;;   required and I'm quite happy.

(defalias 'yes-or-no-p 'y-or-n-p)

;; ** Maximized window after emacs start

(modify-all-frames-parameters '((fullscreen . maximized)))

;; * Personal Information

(setq user-full-name "Yen-Chin, Lee")
(setq user-mail-address "coldnew.tw@gmail.com")

;; * Package Management
;;
;; ** Add my extra package list
;;
;;   melpa contains many community packages that not contribute to
;;   emacs's elpa, we add here.

(eval-and-compile
  (require 'package)			; built-in

  ;; melpa
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/") t)

  ;; For important compatibility libraries like cl-lib
  (when (< emacs-major-version 24)
    (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))))

 ;; ** Initialize package.el
 ;;
 ;;   Before we start to use =package.el=, we need to initialize it
 ;;   first.
 ;;
 ;;   This must come before configurations of installed packages.
 ;;   Don't delete this line. If you don't want it, just comment it out
 ;;   by adding a semicolon to the start of the line. You may delete
 ;;   these explanatory comments.

 (eval-and-compile
   (package-initialize))

;; Upgrade built-in packages to latest version
(setq package-install-upgrade-built-in t)

;; ** Install use-package
;;
;;   The =use-package= macro allows you to isolate package configuration
;;   in your =.emacs= file in a way that is both performance-oriented
;;   and, well, tidy. I created it because I have over 80 packages that
;;   I use in Emacs, and things were getting difficult to manage. Yet
;;   with this utility my total load time is around 2 seconds, with no
;;   loss of functionality!
;;
;;   GitHub: https://github.com/jwiegley/use-package
;;
;;   NOTE: use-package is integrated in emacs directly since emacs
;;   29.1, it's no need to install it manually anymore.

(eval-and-compile
  (unless (package-installed-p 'use-package)
    (package-install 'use-package))) ; Only install if missing, don't refresh

(eval-when-compile (require 'use-package))

;; *** Global use-package settings
;;    Defer all packages by default for faster startup.
;;    Use :demand t for packages needed immediately.
(setq use-package-always-defer t)

;; ** Install paradox
;;
;;   Project for modernizing Emacs' Package Menu. With improved
;;   appearance, mode-line information. Github integration,
;;   customizability, asynchronous upgrading, and more.
;;
;;   GitHub: https://github.com/Malabarba/paradox

(use-package paradox
  :ensure t
  :commands (paradox-enable)
  :hook (after-init . paradox-enable)
  :init
  (setq paradox-execute-asynchronously t
        paradox-spinner-type 'progress-bar
        paradox-github-token t
        paradox-display-star-count nil))

;; ** Install some common libraries
;;
;;   Some common libraries I'll use in my personal's command or anything else.

(use-package f :ensure t)
(use-package s :ensure t)
(use-package dash :ensure t)
(use-package htmlize :ensure t)
(use-package async :ensure t)

;; ** Package reporting settings

(setq use-package-verbose t)

;; * Languages and Encodings
;;
;;   Add UTF8 at the front of the priority list for automatic detection.

(prefer-coding-system 'utf-8)

;;   Set up multilingual environment to use UTF-8.

(set-language-environment "UTF-8")

;;   Set default value of various coding systems to UTF-8.

(set-default-coding-systems 'utf-8)

;;   Use =C= as locale for display time info (Actually it will display
;;   English).

(setq system-time-locale "C")

;; * Built-in Packages Cache Redirection
;;
;;   Some buildin packages need to add extra setups for my emacs
;;   setting. Most of them are the cache file, I'll move them to
;;   =~/.emacs.d/.cache= directory.
;;
;; ** abbrev

(eval-after-load 'bookmark
  '(progn
     (setq abbrev-file-name
           (concat user-cache-directory "abbrev_defs"))))

;; ** eshell
;;
;;   Move eshell cache dir to =~/.emacs.d/.cache/eshell=

(eval-when-compile (defvar eshell-directory-name)) ; defined in esh-mode.el

(with-eval-after-load 'esh-mode
  (setq-default eshell-directory-name
                (concat user-cache-directory "eshell")))

(with-eval-after-load 'em-hist
  (setq-default eshell-history-file-name
                (expand-file-name "history" eshell-directory-name)))

;; ** bookmark

(eval-after-load 'bookmark
  '(progn
     (setq-default bookmark-default-file
                   (concat user-cache-directory "bookmarks"))))

;; ** idlwave
;;
;;   Major mode for editing IDL source files.

(eval-after-load 'idlwave
  '(progn
     (setq-default idlwave-config-directory
           (concat user-cache-directory "idlwave"))))

;; ** srecode

;; change srecode cache file path
(eval-after-load 'srecode
  '(progn
     (setq-default srecode-map-save-file
                   (concat user-cache-directory "srecode-map.el"))))

;; ** request

(eval-after-load 'request
  '(progn
     (setq-default request-storage-directory
                   (concat user-cache-directory "request"))))

;; ** nsm

(eval-after-load 'nsm
  '(progn
     (setq-default nsm-settings-file
                   (concat user-cache-directory "network-security.data"))))

;; ** url

(eval-after-load 'url
  '(progn
     (setq url-configuration-directory
           (file-name-as-directory
            (concat user-cache-directory "url")))))

;; ** startup
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

;; * External Packages
;;
;;   Most of emacs packages do not need many configs or just provide
;;   commands/functions to use, I put them here.
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

;; ** exec-path-from-shell
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

;; ** vterm
;;
;;   vterm is a terminal emulator based on libvterm, which provides
;;   a fast and feature-rich alternative to term-mode.
;;
;;   GitHub: https://github.com/akermu/emacs-libvterm

(use-package vterm
  :ensure t
  :if (executable-find "cmake")
  :commands vterm
  :config
  (setq vterm-always-prompt-on-exit t
        vterm-shell (or (getenv "SHELL") "/bin/zsh")
        vterm-max-scrollback 10000))

;; ** expand-region
;;
;;   Expand region increases the selected region by semantic units.
;;   Just keep pressing the key until it selects what you want.
;;
;;   GitHub: https://github.com/magnars/expand-region.el

(use-package expand-region
  :ensure t
  :bind (("M-v" . er/expand-region)))

;; ** fancy-narrow
;;
;;   Emacs package to immitate narrow-to-region with more eye-candy.
;;
;;   GitHub: https://github.com/Malabarba/fancy-narrow

(use-package fancy-narrow :ensure t)

;; ** focus
;;
;;   Focus provides =focus-mode= that dims the text of surrounding
;;   sections, similar to iA Writer's Focus Mode.
;;
;;   GitHub: https://github.com/larstvei/Focus

(use-package focus :ensure t)

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

;; ** goto-last-change
;;
;;   Move point through buffer-undo-list positions.
;;
;;   GitHub: https://github.com/camdez/goto-last-change.el

(use-package goto-last-change
  :ensure t)

;; ** hungry-delete
;;
;;   hungry-delete borrows hungry deletion from =cc-mode=, which will
;;   causes deletion to delete all whitespace in the direction you are
;;   deleting.

(use-package hungry-delete
  :ensure t
  :demand t
  :config
  (global-hungry-delete-mode))

;; ** iedit
;;
;;   iedit let you edit multiple regions in the same way simultaneously.
;;
;;   GitHub: https://github.com/victorhge/iedit

(use-package iedit
  :ensure t
  :bind (("C-;" . iedit-mode)))

;; ** manage-minor-mode
;;
;;   manage-minor-mode let you manage your minor-mode on the dedicated
;;   interface buffer.
;;
;;   GitHub: https://github.com/ShingoFukuyama/manage-minor-mode

(use-package manage-minor-mode :ensure t)

;; ** mwim
;;
;;   This Emacs package provides commands for moving to the
;;   beginning/end of code or line.
;;
;;   GitHub: https://github.com/alezost/mwim.el

(use-package mwim
  :ensure t
  :bind
  (("C-a" . mwim-beginning-of-code-or-line)
   ("C-e" . mwim-end-of-code-or-line)))

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

;; ** rainbow-mode
;;
;;   rainbow-mode is a minor mode for Emacs which displays strings
;;   representing colors with the color they represent as background.

(use-package rainbow-mode :ensure t)

;; ** smartparens
;;
;;   Smartparens is a minor mode for dealing with pairs in Emacs.
;;
;;   GitHub: https://github.com/Fuco1/smartparens

(use-package smartparens
  :ensure t
  :commands (smartparens-mode)
  :config
  (smartparens-mode 1))

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

;; ** visual-regexp
;;
;;   visual-regexp for Emacs is like replace-regexp, but with live
;;   visual feedback directly in the buffer.
;;
;;   GitHub: https://github.com/benma/visual-regexp.el

(use-package visual-regexp :ensure t)

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

;; ** which-key
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

;; * Artifical Intellengence

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
  (setopt ellama-auto-scroll t)
  ;; User and assistant nicks for chat display
   (setopt ellama-user-nick "You")
   (setopt ellama-assistant-nick "Ellama")
   :config
   ;; Set up provider using custom default or fallback to available providers
   (setopt ellama-provider
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
  (setopt ellama-chat-display-action-function #'display-buffer-pop-up-window)
  (setopt ellama-instant-display-action-function #'display-buffer-at-bottom)

  ;; Disable reasoning model cleanup for transparency
  (setopt ellama-session-remove-reasoning nil)

   ;; Register our configured providers for interactive switching
   (when (boundp 'my/llm-provider-opencode-bigpickle)
     (setopt ellama-providers
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
  (setopt ellama-enable-keymap t)
  (setopt ellama-keymap-prefix "C-c e"))

;; * Interactive Commands
;;
;;   In emacs, we can use =M-x= to execute interactive commands, I
;;   implement some of them to make my emacs more easy to use.
;;
;;   All my =commands= starts with =my/= prefix.
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

;; *** Insert lorem ipsum

(defun my/insert-lorem ()
  "Insert a lorem ipsum."
  (interactive)
  (insert "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do "
          "eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim"
          "ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut "
          "aliquip ex ea commodo consequat. Duis aute irure dolor in "
          "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla "
          "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in "
          "culpa qui officia deserunt mollit anim id est laborum."))

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

;; * Styles
;;
;;   My own emacs, my own style :)
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

;; * Minibutter

(when (require 'minibuffer nil t)  ; built-in, so optional require
  ;; Use bar cursor in minibuffer
  (add-hook 'minibuffer-setup-hook (lambda () (setq cursor-type 'bar))))

;; Helper to clean minibuffer and insert new content
(defun my/minibuffer-insert (str)
  "Clean minibuffer and insert STR as new content."
  (when (minibufferp)
    (delete-minibuffer-contents)
    (insert str)))

(defun my/minibuffer-switch-to-ramdisk ()
  (interactive)
  (my/minibuffer-insert user-ramdisk-directory))

(defun my/minibuffer-switch-to-home ()
  (interactive)
  (my/minibuffer-insert (expand-file-name "~/")))

(defun my/minibuffer-switch-to-rootdir ()
  (interactive)
  (my/minibuffer-insert "/"))

(defun my/minibuffer-switch-to-tramp ()
  (interactive)
  (my/minibuffer-insert "/ssh:"))

;; Save minibuffer history
(use-package savehist
  :demand t
  :config
  (setq savehist-file (expand-file-name "savehist.dat" user-cache-directory))
  (add-to-list 'savehist-additional-variables 'corfu-history)
  (savehist-mode 1))

;; Keybindings
(bind-keys :map minibuffer-local-map
           ("C-w" . backward-kill-word)
           ("M-p" . previous-history-element)
           ("M-n" . next-history-element)
           ("C-g" . my/minibuffer-keyboard-quit)  ; built-in, better than abort-recursive-edit alone
           ("M-t" . my/minibuffer-switch-to-ramdisk)
           ("M-h" . my/minibuffer-switch-to-home)
           ("M-/" . my/minibuffer-switch-to-rootdir)
           ("M-s" . my/minibuffer-switch-to-tramp))

;; * Vim Emulation
;;
;;   Though I am really familiar with emacs, I still like some vim command.


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

;; Evil Collection - Evil keybindings for various packages
(use-package evil-collection
  :ensure t
  :demand t
  :after evil
  :config
  (evil-collection-init '(corfu dashboard diff-hl dired eldoc elpaca lsp-ui-imenu which-key)))

;; ** evil-leader
;;
;;   Evil Leader provides the =<leader>= feature from Vim that provides
;;   an easy way to bind keys under a variable prefix key.
;;
;;   GitHub: https://github.com/cofi/evil-leader

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
   "0" 'select-window-0) )

;; ** evil-surround
;;
;;   This package emulates surround.vim by Tim Pope. The functionality
;;   is wrapped into a minor mode.
;;
;;   GitHub: https://github.com/timcharper/evil-surround

(use-package evil-surround
  :ensure t
  :after evil
  :commands (global-evil-surround-mode)
  :config
  (global-evil-surround-mode 1))

;; ** evil-quickscope
;;
;;   This package emulates quick_scope.vim by Brian Le. It highlights
;;   targets for evil-mode's f,F,t,T keys, allowing for quick
;;   navigation within a line with no additional mappings.
;;
;;   GitHub: https://github.com/blorbx/evil-quickscope

(use-package evil-quickscope
  :ensure t
  :after evil
  :config
  (add-hook 'prog-mode-hook 'turn-on-evil-quickscope-always-mode))

;; ** evil-terminal-cursor-changer
;;
;;   evil-terminal-cursor-changer is changing cursor shape and color
;;   by evil state for evil-mode. When running in terminal, It's
;;   especially helpful to recognize evil's state.
;;
;;   GitHub: https://github.com/7696122/evil-terminal-cursor-changer

(use-package evil-terminal-cursor-changer
  :ensure t
  :after evil
  :commands (evil-terminal-cursor-changer-activate)
  :config (evil-terminal-cursor-changer-activate))

;; ** vi-tilde-fringe
;;
;;   Displays tildes in the fringe on empty lines a la Vi
;;
;;   GitHub: https://github.com/syohex/vi-tilde-fringe

(use-package vi-tilde-fringe
  :ensure t
  :if window-system
  :commands (global-vi-tilde-fringe-mode)
  :config
  (global-vi-tilde-fringe-mode))

;; ** evil-terminal-cursor-changer (terminal)
;;
;;   Make terminal support evil's cursor shape change. This package
;;   changing cursor shape and color by evil state for evil-mode.
;;
;;   Supported terminal: xterm, gnome-terminal, iTerm, konsole.

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

;; * Editor
;;
;;   Why emacs config has an editor section, doesn't means emacs is not
;;   an editor? Yes, Emacs is an OS :)
;;
;;   I put some editor/IDE relative functions and packages here.
 ;;
 ;; ** Keeping files in sync
;;
;;   By default, Emacs will not update the contents of open buffers
;;   when a file changes on disk. This is inconvenient when switching
;;   branches in Git - as you'd risk editing stale buffers.

(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)
(setq revert-without-query '(".*")) ;; disable revert query

;; ** Disable lock file
;;
;;   I don't want emacs create some temporary file like =.#-emacs-a08196=,
;;   disable it.

;; https://www.emacswiki.org/emacs/LockFiles
(when (version<= "24.3" emacs-version)
  (setq create-lockfiles nil))

;; ** Add support for editorconfig
;;
;;   EditorConfig helps developers define and maintain consistent
;;   coding styles between different editors and IDEs.
;;
;;   GitHub: https://github.com/editorconfig/editorconfig-emacs

(use-package editorconfig
  :ensure t
  :if (executable-find "editorconfig")
  :mode ("\\.editorconfig\\'" . conf-unix-mode)
  :commands editorconfig-mode
  :init
  (add-hook 'prog-mode-hook #'editorconfig-mode))

;; ** En/Decrypt files by EasyPG

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

;; ** Remote file editing

(use-package tramp
  :ensure t
  :init
  (setq tramp-persistency-file-name (concat user-cache-directory "tramp"))
  :config
  (setq tramp-default-method "rsync"))

;; * Line Numbers

(if (version<= "26.1" emacs-version)
    ;; emacs 26.1 has display-line-number-mode, which is written in C
    (progn
      (require 'display-line-numbers)
      ;; Only use line number in `prog-mode-hook'
      (add-hook 'prog-mode-hook #'display-line-numbers-mode))
    ;; for emacs version less than 26, use linum instead
    (use-package linum :ensure t :init (global-linum-mode 1)))

;; disable some mode with linum
(use-package linum-off
  :ensure t
  :config
  (setq linum-disabled-mode-list
        '(eshell-mode shell-mode term-mode erc-mode compilation-mode
                      woman-mode w3m-mode calendar-mode org-mode)))

;; for emacs 26.1 or above, we use `display-line-number-mode' instead
(when (version<= "26.1" emacs-version)
  ;; NOTE: overwrite display-line-numbers--turn-on
  (defun display-line-numbers--turn-on ()
    "Turn on `display-line-numbers-mode'."
    (unless (or (minibufferp)
                ;; taken from linum.el
                (and (daemonp) (null (frame-parameter nil 'client)))
                ;; take code from `linum-off'
                (member major-mode linum-disabled-modes-list))
      (display-line-numbers-mode))))

;; * Programming Mode Enhancements
;;
;; ** rainbow-delimiters

(use-package rainbow-delimiters
  :ensure t
  :demand t
  :config
  ;; Enable for all programming modes
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; ** recentf

(use-package recentf
  :ensure t                             ; built-in
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
  :config
  ;; Only show indent-guide in idle-time.
  (setq indent-guide-delay 0.1))

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

;; ** undo-tree

(use-package undo-tree
  :ensure t
  :commands (global-undo-tree-mode)
  :config
  ;; Persistent undo-tree history across emacs sessions
  (let ((dir
         (file-name-as-directory (concat user-cache-directory "undo-tree"))))
    (setq undo-tree-history-directory-alist `(("." . ,dir))))
  ;; Make undo-tree save history
  (setq undo-tree-auto-save-history t)
  ;; global enable undo-tree
  (global-undo-tree-mode))

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
 
 ;; * Completion
;;
;;   Modern completion framework with Vertico, Consult, Marginalia, and Orderless.
;;   Much faster and more modular than helm.

(use-package vertico
  :ensure t
  :demand t
  :config
  (vertico-mode 1)
  ;; Show more candidates (default is 10)
  (setq vertico-count 15)
  ;; Resize vertico window to fit content
  (setq vertico-resize t))

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

(use-package marginalia
  :ensure t
  :demand t
  :config
  (marginalia-mode 1))

(use-package orderless
  :ensure t
  :demand t
  :config
  (setq completion-styles '(orderless basic)))

;; ** corfu
;;
;;   Corfu is a minimalistic completion UI for in-buffer completion.
;;   It works as the in-buffer completion counterpart to Vertico's minibuffer UI.
;;
;;   Corfu complements the Vertico stack:
;;   - Vertico/Consult: minibuffer completion (M-x, find-file, etc.)
;;   - Corfu: in-buffer completion (code completion, symbols, etc.)

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
   ;; Quit at completion boundary
   (corfu-quit-at-boundary 'separator)
  :config
  ;; Enable Corfu globally
  (global-corfu-mode)

  ;; Corfu keybindings (work in all states)
  (define-key corfu-map (kbd "TAB") 'corfu-insert)
  (define-key corfu-map [tab] 'corfu-insert)
  (define-key corfu-map (kbd "RET") 'corfu-insert)
  (define-key corfu-map [return] 'corfu-insert)
  (define-key corfu-map (kbd "ESC") 'corfu-reset))  ;; End of use-package corfu

 ;; Enable corfu extensions and Evil keybindings after package loads
(with-eval-after-load 'corfu
   ;; Load extensions
   (require 'corfu-history)
   (require 'corfu-popupinfo)
   (corfu-history-mode 1)
   (corfu-popupinfo-mode 1)

   ;; Evil Collection corfu bindings - load main package first
   (with-eval-after-load 'evil-collection
      (require 'evil-collection-corfu)))

  ;; ** kind-icon
  ;;
  ;;   kind-icon adds icons to completion candidates in Corfu.
  ;;   It displays colorful icons for different completion kinds
  ;;   (functions, variables, methods, files, etc.).

  ;; Install kind-icon if not present
  (unless (package-installed-p 'kind-icon)
    (package-install 'kind-icon))

  ;; Load and configure kind-icon
  (require 'kind-icon)

  ;; Use icons instead of text labels
  (setq kind-icon-use-icons t)

  ;; Match corfu background face
  (setq kind-icon-default-face 'corfu-default)

  ;; Add kind-icon to Corfu margin formatters
  (with-eval-after-load 'corfu
    (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

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

;; * Documentation and Markup Modes
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

;; Declare functions from mmm-mode to suppress compiler warnings
(declare-function mmm-add-classes "mmm-mode")
(declare-function mmm-add-mode-ext-class "mmm-mode")
(declare-function markdown-insert-link "markdown-mode")

;; ** markdown-mode

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :config
  ;; http://jblevins.org/log/mmm
  (use-package mmm-mode
    :ensure t
    :config
    (setq mmm-global-mode 'maybe)
    (setq mmm-parse-when-idle 't)
    (defun my/mmm-markdown-auto-class (lang &optional submode)
      "Define a mmm-mode class for LANG in `markdown-mode' using SUBMODE.
  If SUBMODE is not provided, use `LANG-mode' by default."
      (let ((class (intern (concat "markdown-" lang)))
            (submode (or submode (intern (concat lang "-mode"))))
            (front (concat "^```" lang "[\n\r]+"))
            (back "^```"))
        (mmm-add-classes (list (list class :submode submode :front front :back back)))
        (mmm-add-mode-ext-class 'markdown-mode nil class)))

    ;; Mode names that derive directly from the language name
    (mapc 'my/mmm-markdown-auto-class
          '("awk" "bibtex" "c" "cpp" "css" "html" "latex" "lisp" "makefile"
            "markdown" "python" "r" "ruby" "sql" "stata" "xml" "js")))
  (bind-keys :map markdown-mode-map
             ("C-c i" . markdown-insert-link))

  (bind-keys :map gfm-mode-map
             ("C-c i" . markdown-insert-link)))

;; ** nasm-mode

(use-package nasm-mode :ensure t)

;; ** toml-mode

(use-package toml-mode
  :ensure t
  :mode "\\.toml$")

;; ** yaml-mode

(use-package yaml-mode
  :ensure t
  :mode "\\.yml$")

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
  :config
  (use-package indent-guide
    :ensure t
    :commands (indent-guide-mode)
    :config
    (add-hook 'qml-mode-hook #'indent-guide-mode)))

;; ** vala-mode

(use-package vala-mode
  :ensure t
  :mode ("\\.vala\\'" "\\.vapi\\'")
  :config
  )

;; ** verilog-mode

(use-package verilog-mode
  :mode ("\\.v\\'")
  :config
  (setq verilog-linter "verilator --lint-only")
  ;; https://github.com/flycheck/flycheck/issues/1250
  (setq flycheck-verilog-verilator-executable "/usr/bin/verilator_bin"))

;; ** groovy-mode

(use-package groovy-mode
  :mode (("\\.groovy" . groovy-mode)
         ("/Jenkinsfile" . groovy-mode))
  :ensure t)

;; ** svelte-mode

(use-package svelte-mode
  :ensure t
  :mode ("\\.svelte\\'" . svelte-mode))

;; ** dockerfile-mode

(use-package dockerfile-mode
  :ensure t
  :mode "Dockerfile\\'")

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

;; * C/C++ Programming
;;
;; ** cc-mode configuration

(use-package cc-mode
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

(use-package c-eldoc
  :ensure t
  :config
  (add-hook 'c-mode-common-hook
            #'(lambda ()
               (setq c-eldoc-includes "`pkg-config --cflags --libs` -I./ -I../")
               (c-turn-on-eldoc-mode))))

;; ** cwarn

(use-package cwarn
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

(use-package srefactor
  :ensure t
  :defer t
  :after (cc-mode))

;; ** cff

(use-package cff
  :ensure t
  :after (cc-mode))

;; ** modern-cpp-font-lock

;; adds font-lock highlighting for modern C++ upto C++17
;; https://github.com/ludwigpacifici/modern-cpp-font-lock
(use-package modern-cpp-font-lock
  :ensure t
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

;; clang-format: format C/C++ buffers using clang-format
;; https://github.com/emacsorphanage/clang-format
(use-package clang-format
  :ensure t)

;; * Emacs Lisp Development
;;
;; ** elisp-mode configuration

(use-package elisp-mode
  :ensure nil            ; built-in
  :config
  (use-package macrostep
    :ensure t)
  (use-package el-spice
    :ensure t)
  (use-package eldoc
    :ensure t
    :config
    (add-hook 'emacs-lisp-mode-hook
              #'(lambda ()
                 ;; enable eldoc
                 (turn-on-eldoc-mode)
                 ;; fix for paredit if exist
                 (eval-after-load 'paredit
                   '(progn
                      (eldoc-add-command 'paredit-backward-delete
                                         'paredit-close-round))))))
  (defun my/elisp/check-parens-on-save ()
    "Run `check-parens' when the current buffer is saved."
    (add-hook 'after-save-hook #'check-parens nil 'make-it-local))

  (add-hook 'emacs-lisp-mode-hook
            (lambda () (my/elisp/check-parens-on-save)))
    (use-package litable
     :ensure t
     :commands (litable-mode)
     :config
     ;; Save cache file to `user-cache-directory'
     (setq litable-list-file (concat user-cache-directory ".litable-lists.el"))
     ;; Enable litable-mode globally
     (litable-mode))
  (use-package page-break-lines
    :ensure t
    :commands (global-page-break-lines-mode)
    :config
    ;; enable globally
    (global-page-break-lines-mode 1))
  (defun my/remove-elc-on-save ()
    "If you're saving an elisp file, likely the .elc is no longer valid."
    (make-local-variable 'after-save-hook)
    (add-hook 'after-save-hook
              (lambda ()
                (if (file-exists-p (concat buffer-file-name "c"))
                    (delete-file (concat buffer-file-name "c"))))))

    (add-hook 'emacs-lisp-mode-hook 'my/remove-elc-on-save)
   (bind-keys :map emacs-lisp-mode-map
              ;; ("C-c '" . my/narrow-or-widen-dwim)
              )

  ;; *** outshine-mode
  ;;    Enable outshine-mode for org-mode-like navigation in elisp files.
  ;;    Use C-c @ to toggle visibility, C-c n/p/f/b to navigate headings.
  (use-package outshine
    :ensure t
    :diminish outshine-mode
    :config
    ;; Enable outshine-mode for emacs-lisp-mode
    (add-hook 'emacs-lisp-mode-hook 'outshine-mode)
    ;; Set outline-minor-mode prefix to C-c @ (same as outshine default)
    (setq outline-minor-mode-prefix (kbd "C-c @"))
    ;; Start with bodies hidden (show only headings)
    (setq outshine-start-level 1)))
;; * Syntax Checking and Linting
;;
;; ** flycheck

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode 1))

(use-package flycheck-pos-tip
  :ensure t
  :after flycheck
  :config
  (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))

;; ** magit-gptcommit

(setq vc-handled-backends nil)

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

;; ** flycheck global mode

(use-package flycheck
  :ensure t
  :config
  ;; enable globally
  (global-flycheck-mode))

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
  (add-hook 'treesit-after-major-mode-change-hook #'my/treesit-maybe-install-grammar)

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

;; * LSP Support
;;
;;   Eglot provides LSP (Language Server Protocol) support for Emacs.
;;   It enables IDE features like auto-completion, go-to-definition,
;;   find-references, and code actions across many languages.

(use-package eglot
  :ensure nil  ; Built-in since Emacs 29
  :defer t  ; Load only when editing code, not at startup
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
    '((js-mode js-ts-mode tsx-ts-mode typescript-mode typescript-ts-mode)
      . ("typescript-language-server" "--stdio")))
  ;; Rust: rust-analyzer
  (add-to-list 'eglot-server-programs
    '((rust-mode rust-ts-mode) . "rust-analyzer"))
  ;; Go: gopls
  (add-to-list 'eglot-server-programs
    '((go-mode go-ts-mode) . "gopls"))
  ;; C/C++: clangd
  (add-to-list 'eglot-server-programs
    '((c-mode c++-mode c-ts-mode c++-ts-mode) . "clangd"))
  ;; Shell: bash-language-server
  (add-to-list 'eglot-server-programs
    '((sh-mode bash-ts-mode) . ("bash-language-server" "start")))

  ;; Add hooks for all languages
  (dolist (hook '(python-mode-hook python-ts-mode-hook
                  rust-mode-hook rust-ts-mode-hook
                  go-mode-hook go-ts-mode-hook
                  typescript-mode-hook js-mode-hook tsx-mode-hook
                  sh-mode-hook bash-ts-mode-hook
                  c-mode-hook c++-mode-hook c-ts-mode-hook c++-ts-mode-hook))
    (add-hook hook 'eglot-ensure))

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

(use-package flymake-shell
  :ensure t
  :config (add-hook 'sh-set-shell-hook 'flymake-shell-load))

;; * Embedded Systems Development
;;
;; ** bitbake

(use-package bitbake
  :ensure t
  :mode ("\\.bb\\'" "\\.bbappend\\'"))

;; ** dts-mode

(use-package dts-mode :ensure t
  :mode ("\\.dts\\'" "\\.dtsi\\'"))

;; * Dart/Flutter Development
;;
;; ** dart-mode

(use-package dart-mode
  :ensure t
  :mode ("\\.dart\\'")
  :config
  ;; enable analyzer support
  (setq dart-enable-analysis-server t)
  ;; add flycheck support
  (add-hook 'dart-mode-hook 'flycheck-mode))

;; * Plotting and Diagram Modes
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

;; * Shader Programming
;;
;; ** glsl-mode

(use-package glsl-mode :ensure t
  :mode (("\\.vs\\'" . glsl-mode)
         ("\\.fs\\'" . glsl-mode)
         ("\\.gs\\'" . glsl-mode))
  :config
  (setq glsl-other-file-alist '(("\\.fs$" (".vs")) ("\\.vs$" (".fs")))))

;; * JavaScript/TypeScript Development
;;
;; ** js2-mode

(use-package js2-mode
  :ensure t
  :mode "\\.js\\'")

;; ** js2-refactor

(use-package js2-refactor
  :ensure t)

;; ** nvm

(use-package nvm :ensure t)

;; ** import-js

(use-package import-js :ensure t)

;; ** json-mode

(use-package json-mode :ensure t
  :mode "\\.json\\'")

;; ** json-reformat

(use-package json-reformat :ensure t :commands json-reformat-region)

;; ** flymake-json

(use-package flymake-json
  :ensure t
  :commands (flymake-json-load)
  :config
  (add-hook 'json-mode-hook (lambda () (flymake-json-load))))

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

;; * Python Development
;;
;; ** python-mode

(use-package python
  :mode (("SCons\\(truct\\|cript\\)\\'" . python-mode)
         ("DEPS" . python-mode)))

;; * XML/Configuration Files
;;
;; ** nxml-mode

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

;; * Lisp Development
;;
;; ** lispy with lispyville integration
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

(use-package indent-guide
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook #'indent-guide-mode)
  (add-hook 'lisp-interaction-mode-hook #'indent-guide-mode)
  (add-hook 'clojure-mode-hook #'indent-guide-mode)
  (add-hook 'scheme-mode-hook #'indent-guide-mode)
  (add-hook 'lisp-mode-hook #'indent-guide-mode))

(global-prettify-symbols-mode 1)

;; * Testing with ERT
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
  :ensure nil  ; built-in since Emacs 24
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

;; * Web Development
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

;; * Terminal Support
;;
;; ** term handling

(defadvice term-handle-exit (after kill-buffer-after-exit activate)
  "Kill the term buffer if the process finished."
  (kill-buffer (current-buffer)))

;; * Eshell Configuration
;;
;;   Eshell is a command shell written in Emacs Lisp.

(use-package eshell
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
  ;; FIXME: why this will still global-map ?
  ;; (bind-keys :map eshell-mode-map
  ;;            ("C-u" . eshell-kill-input))

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
  "Clears the shell buffer ala Unix's clear or DOS' cls"
  ;; the shell prompts are read-only, so clear that for the duration
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
                          (".*" "echo 'Could not unpack the file:'")))))
    (eshell-command-result (concat command " " file))))

;; * Window Management
;;
;; ** winner-mode

(use-package winner                     ; builtin
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

;; * Local Personal Configuration
;;
;;   Load personal settings if available.

(let ((secret "~/.personal.el"))
  (when (file-exists-p secret) (load-file secret)))

;; * Final sections

(provide 'init.el)
;;; init.el ends here
