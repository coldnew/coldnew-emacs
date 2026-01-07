;; -*- lexical-binding: t -*-

(setq byte-compile-cond-use-jump-table nil)

(require 'cl-lib)                       ; built-in

(require 'find-lisp)

(setq load-prefer-newer t)

;; We set `user-emacs-directory' here so we can use command-line
;; switch different emacs configuration like following:
;;
;;    emacs -q -l ~/coldnew-spacemacs/init.el
(defconst user-emacs-directory
  (file-name-directory (or load-file-name (buffer-file-name)))
  "My emacs config directory.")

(defconst user-cache-directory
  (file-name-as-directory (concat user-emacs-directory ".cache"))
  "My emacs storage area for persistent files.")
;; create the `user-cache-directory' if not exists
(make-directory user-cache-directory t)

(defconst user-modules-directory
  (file-name-as-directory (concat user-emacs-directory "modules"))
  "My emacs storage area for modules.")

(defconst user-ramdisk-directory
  (let ( (user-ramdisk                   ; ~/ramdisk/
          (concat (getenv "HOME") "/ramdisk/")))
    ;; if ~/ramdisk/ exist, use it
    (if (file-exists-p user-ramdisk)
        user-ramdisk
        ;; fallcack to system default ramdisk dir
        temporary-file-directory))
  "My ramdisk path in system.")

(eval-and-compile
  ;; Add directories to emacs's `load-path' recursively.
  ;; if path does not exist, try to create directory.
  (let* ((my-lisp-dir
          (list (concat user-emacs-directory "elpa/") ; package installed by package.el
                (concat user-emacs-directory "local-lisp/") ; local lisp I used
                (concat user-emacs-directory "styles/"))) ; themes I used
         (sys-lisp-dir (cl-ecase system-type ; add some system site-lisp to load-path
                         ((darwin)    '("/usr/local/share/emacs/site-lisp/"))
                         ((gnu/linux) '("/usr/share/emacs/site-lisp/"))
                         ((t) nil)))  ; FIXME: Add more platform support
         (lisp-dir (append my-lisp-dir sys-lisp-dir)))
    ;; my-lisp-dir should always exist, but sys-lisp-dir may not exist
    (dolist (lisp-path my-lisp-dir)
      (when (not (file-exists-p lisp-path))
        (make-directory lisp-path t)))
    (dolist (lisp-path lisp-dir)
      (when (file-exists-p lisp-path)
        (let* ((default-directory lisp-path))
          (setq load-path
                (append
                 (let ((load-path (copy-sequence load-path)))
                   (append
                    (copy-sequence (normal-top-level-add-to-load-path '(".")))
                    (normal-top-level-add-subdirs-to-load-path)))
                 load-path)))))))

(when (eq system-type 'darwin)
  (setq-default mac-option-modifier 'super)
  (setq-default mac-command-modifier 'meta))

(setq-default custom-file (concat user-cache-directory "custom.el"))
;; load custom-file only when file exist
(when (file-exists-p custom-file)
  (load-file custom-file))

;; load the `load-modules.el' file which help me load external modulept
(let ((script (concat user-modules-directory "load-modules.el")))
  (when (file-exists-p script)
    (load script)))

(unless (string-equal "root" (getenv "USER"))
  (require 'server)
  (when (fboundp 'server-running-p)
    (unless (server-running-p) (server-start))))

(let ((backup-dir (concat user-cache-directory "backup")))
  ;; Move backup file to `~/.emacs.d/.cache/backup'
  (setq backup-directory-alist `(("." . ,backup-dir)))
  ;; Makesure backup directory exist
  (when (not (file-exists-p backup-dir))
    (make-directory backup-dir t)))

(setq delete-by-moving-to-trash nil)
(setq version-control t)
(setq kept-old-versions 10)
(setq kept-new-versions 20)
(setq delete-old-versions t)
(setq backup-by-copying t)

(setq ring-bell-function #'ignore)

(setq initial-scratch-message "")

(setq visible-bell t)

(defalias 'yes-or-no-p 'y-or-n-p)

(modify-all-frames-parameters '((fullscreen . maximized)))

(setq user-full-name "Yen-Chin, Lee")
(setq user-mail-address "coldnew.tw@gmail.com")

(defun my-load-secret ()
  "Load my secret setting include password... etc."
  (let ((secret "~/.secret.el.gpg"))
    (when (file-exists-p secret) (load-file secret))))

(eval-and-compile
  (require 'package)			; built-in

  ;; melpa
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/") t)

  ;; For important compatibility libraries like cl-lib
  (when (< emacs-major-version 24)
    (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))))

;; This must come before configurations of installed packages.
;; Don't delete this line. If you don't want it, just comment it out by adding a
;; semicolon to the start of the line. You may delete these explanatory
;; comments.
(eval-and-compile
  (when (< emacs-major-version 27)
    (package-initialize)))

(eval-and-compile
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))) ; Installed by packages.el

(eval-when-compile (require 'use-package))

(setq use-package-verbose t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(use-package paradox
  :ensure t
  :commands (paradox-enable)
  :hook (after-init . paradox-enable)
  :init
  (setq paradox-execute-asynchronously t
        paradox-spinner-type 'progress-bar
        paradox-github-token t
        paradox-display-star-count nil))

(use-package f :ensure t)
(use-package s :ensure t)
(use-package dash :ensure t)
(use-package htmlize :ensure t)
(use-package async :ensure t)

(prefer-coding-system 'utf-8)

(set-language-environment "UTF-8")

(set-default-coding-systems 'utf-8)

(setq system-time-locale "C")

(eval-after-load 'bookmark
  '(progn
     (setq abbrev-file-name
           (concat user-cache-directory "abbrev_defs"))))

(eval-when-compile (defvar eshell-directory-name)) ; defined in esh-mode.el

(with-eval-after-load 'esh-mode
  (setq-default eshell-directory-name
                (concat user-cache-directory "eshell")))

(with-eval-after-load 'em-hist
  (setq-default eshell-history-file-name
                (expand-file-name "history" eshell-directory-name)))

(eval-after-load 'bookmark
  '(progn
     (setq-default bookmark-default-file
                   (concat user-cache-directory "bookmarks"))))

(eval-after-load 'idlwave
  '(progn
     (setq-default idlwave-config-directory
           (concat user-cache-directory "idlwave"))))

;; change srecode cache file path
(eval-after-load 'srecode
  '(progn
     (setq-default srecode-map-save-file
                   (concat user-cache-directory "srecode-map.el"))))

(eval-after-load 'request
  '(progn
     (setq-default request-storage-directory
                   (concat user-cache-directory "request"))))

(eval-after-load 'nsm
  '(progn
     (setq-default nsm-settings-file
                   (concat user-cache-directory "network-security.data"))))

(eval-after-load 'url
  '(progn
     (setq url-configuration-directory
           (file-name-as-directory
            (concat user-cache-directory "url")))))

;; NOTE:
;; `auto-save-list-file-prefix' defined in startup.el, but
;; startup.el doesn't have provide pacage name (provide 'startup)
;;
(setq-default auto-save-list-file-prefix
              (cond ((eq system-type 'ms-dos)
                     ;; MS-DOS cannot have initial dot, and allows only 8.3 names
                     (file-name-as-directory
                      (concat user-cache-directory "auto-save.list/_saves-")))
                    (t
                     (file-name-as-directory
                      (concat user-cache-directory "auto-save-list/.saves-")))))

(use-package discover-my-major
  :defer 2	      ; Install pkg if not exist after 2 sec idle time
  :commands (discover-my-major))

(when (require 'doxymacs nil 'noerror)
  (add-hook 'prog-mode-hook #'(lambda () (doxymacs-mode))))

(use-package esup
  :ensure t
  :commands (esup)
  :init
  ;; Work around a bug where esup tries to step into the byte-compiled
  ;; version of `cl-lib', and fails horribly.
  ;; see:
  ;; https://github.com/jschaf/esup/issues/54#issuecomment-651247749
  (setq esup-depth 0))

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package expand-region
  :ensure t
  :bind (("M-v" . er/expand-region)))

(use-package fancy-narrow :ensure t)

(use-package focus :ensure t)

(use-package fontawesome :ensure t)

(use-package google-translate :ensure t)

(use-package goto-last-change
  :ensure t)

(use-package howdoi
  :defer 2
  :commands (howdoi-query howdoi-query-line-at-point))

(use-package hungry-delete
  :ensure t
  :config
  (global-hungry-delete-mode))

(use-package iedit
  :ensure t
  :bind (("C-;" . iedit-mode)))

(use-package manage-minor-mode :ensure t)

(use-package mwim
  :ensure t
  :bind
  (("C-a" . mwim-beginning-of-code-or-line)
   ("C-e" . mwim-end-of-code-or-line)))

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

(use-package password-generator :ensure t)

(use-package rainbow-mode :ensure t)

(use-package smartparens
  :ensure t
  :commands (smartparens-mode)
  :config
  (smartparens-mode 1))

(use-package sx :ensure t)

(use-package tldr
  :defer 2
  :commands (tldr)
  :config
  (setq tldr-directory-path (concat user-cache-directory "tldr/"))
  (setq tldr-saved-zip-path (concat user-cache-directory "tldr-source.zip")))

(use-package url-shortener :ensure t)

(use-package verify-url
  :defer 2
  :commands (verify-url))

(use-package visual-regexp :ensure t)

(use-package webpaste
  :ensure t)

(use-package which-key
  :ensure t
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

(defun my/nuke-all-buffers ()
  "Kill all buffers, leaving *scratch* only."
  (interactive)
  (mapcar (lambda (x) (kill-buffer x)) (buffer-list))
  (delete-other-windows))

(defun my/save-buffer-always ()
  "Save the buffer even if it is not modified."
  (interactive)
  (set-buffer-modified-p t)
  (save-buffer))

(defun my/minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(defun my/untabify-buffer ()
  (interactive)
  (save-excursion
    (untabify (point-min) (point-max))))

(defun my/indent-whole-buffer ()
  "Indent whole buffer."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max))))

(defun my/cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (save-excursion
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max))
    (untabify (point-min) (point-max))))

(defun my/eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun my/quick-folding-source ()
  "Use emacs buildin easy to folding code."
  (interactive)
  (set-selective-display
   (if selective-display nil 1)))

(defun my/dos2unix ()
  "Convert buffer file from dos file to unix file."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix 't) )

(defun my/unix2dos ()
  "Convert buffer file from unix file to dos file."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos 't))

(defun my/insert-U200B-char ()
  "Insert <U200B> char, this character is nice use in org-mode."
  (interactive)
  (insert "\ufeff"))

(defun my/insert-empty-line ()
  "Insert an empty line after current line and position cursor on newline."
  (interactive)
  (move-end-of-line nil)
  (open-line 1)
  (forward-line 1))

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

(defun my/delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun my/set-mark-mode/rectangle-mark-mode ()
  "toggle between set-mark-command or rectangle-mark-mode"
  (interactive)
  (if (not mark-active)
     (call-interactively 'set-mark-command)
    (call-interactively 'rectangle-mark-mode)))

(defun my/copy-and-comment ()
  "Copy region and comment it."
  (interactive)
  (kill-ring-save (region-beginning) (region-end))
  (comment-dwim nil))

(defun my/file-reopen-as-root ()
  (interactive)
  (when buffer-file-name
    (find-alternate-file
     (concat "/sudo:root@localhost:"
             buffer-file-name))))

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

(defun my/set-file-executable()
  "Add executable permissions on current file."
  (interactive)
  (when (buffer-file-name)
    (set-file-modes buffer-file-name
                    (logior (file-modes buffer-file-name) #o100))
    (message (concat "Made " buffer-file-name " executable"))))

(defun my/clone-file-and-open (filename)
  "Clone the current buffer writing it into FILENAME and open it"
  (interactive "FClone to file: ")
  (save-restriction
    (widen)
    (write-region (point-min) (point-max) filename nil nil nil 'confirm))
  (find-file filename))

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

(defun my/eval-buffer-until-error ()
  "Evaluate emacs buffer until error occured."
  (interactive)
  (goto-char (point-min))
  (while t (eval (read (current-buffer)))))

(defun my/what-face (pos)
  "Display face found at the current point."
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defun my/reload-init ()
  "Reload init.el file"
  (interactive)
  (load-file user-init-file))

(defun my/other-window-or-split ()
  "Switch to other window or split it."
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1))

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

(when (featurep 'menu-bar) (menu-bar-mode -1))

(when (featurep 'tool-bar) (tool-bar-mode -1))

(blink-cursor-mode -1)

(when (featurep 'scroll-bar) (scroll-bar-mode -1))

(setq inhibit-startup-screen t)

;; Make `load-theme' fully unload previous theme before loading a new one.
(defadvice load-theme
    (before theme-dont-propagate activate)
  (mapc #'disable-theme custom-enabled-themes))

(require 'day-coldnew-theme)
(require 'night-coldnew-theme)
(load-theme 'night-coldnew t nil)  ; default use `night-coldnew-theme'

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
  "Decrease emacs's font-size acording emacs-font-size-pair-list."
  (interactive) (my/emacs-step-font-size 1))

(defun my/decrease-emacs-font-size ()
  "Increase emacs's font-size acording emacs-font-size-pair-list."
  (interactive) (my/emacs-step-font-size -1))

(bind-keys :map global-map
           ("C-=" . my/increase-emacs-font-size)
           ("C--" . my/decrease-emacs-font-size))

(when (require 'minibuffer)                  ; buildin
  ;; only use `bar' type of cursor shape
  (add-hook 'minibuffer-setup-hook #'(lambda () (setq cursor-type 'bar)))
  ;; define some helper function to insert to minibuffer quickly
  (defun my/minibuffer-insert (p)
    (kill-line 0) (insert p))
  
  (defun my/minibuffer-switch-to-ramdisk ()
    "Insert ramdisk path according to system type"
    (interactive)
    (my/minibuffer-insert user-ramdisk-directory))
  
  (defun my/minibuffer-switch-to-home ()
    "Insert $HOME path."
    (interactive)
    (my/minibuffer-insert (file-name-as-directory (getenv "HOME"))))
  
  (defun my/minibuffer-switch-to-rootdir ()
    "Insert / path."
    (interactive)
    (my/minibuffer-insert "/"))
  
  (defun my/minibuffer-switch-to-tramp ()
    "Insert /ssh:."
    (interactive)
    (my/minibuffer-insert "/ssh:"))
  (use-package savehist
    :config
    (setq savehist-file (concat user-cache-directory "savehist.dat"))
    (savehist-mode 1))
  (bind-keys :map minibuffer-local-map
             ("C-w" . backward-kill-word)
             ("M-p" . previous-history-element)
             ("M-n" . next-history-element)
             ("C-g" . minibuffer-keyboard-quit)
             ("M-t" . my/minibuffer-switch-to-ramdisk)
             ("M-h" . my/minibuffer-switch-to-home)
             ("M-/" . my/minibuffer-switch-to-rootdir)
             ("M-s" . my/minibuffer-switch-to-tramp)))

(use-package evil
  :ensure t
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
  (evil-define-key 'normal my-editor-map
      (kbd "C-x C-f") 'helm-find-files
      (kbd "C-x C-q") 'read-only-mode
      (kbd "C-x M-1") 'deft-or-close
      (kbd "C-x M-2") 'eshell
      (kbd "C-x M-3") 'mu4e
      (kbd "C-x M-4") 'erc-start-or-switch
      (kbd "C-x vl") 'magit-log
      (kbd "C-x vp") 'magit-push
      (kbd "C-x vs") 'magit-status
      (kbd "C-x b") 'helm-buffers-list
      (kbd "M-[") 'winner-undo
      (kbd "M-]") 'winner-redo
      (kbd "M-x") 'helm-M-x
      (kbd "M-s") 'helm-occur
      (kbd "C-x C-o") 'other-frame
      (kbd "M-o") 'other-window)
  (evil-define-key 'insert my-editor-map
    (kbd "<delete>") 'hungry-delete-backward
    (kbd "C-;") 'iedit-mode
    (kbd "C-d") 'hungry-delete-forward
    (kbd "C-l") 'hungry-delete-backward
    (kbd "C-n") 'evil-next-line
    (kbd "C-o") 'evil-execute-in-normal-state
    (kbd "C-p") 'evil-previous-line
    (kbd "C-v") 'set-mark-mode/rectangle-mark-mode
    (kbd "C-w") 'backward-kill-word
    (kbd "C-x C-f") 'helm-find-files
    (kbd "C-x C-n") 'company-complete
    (kbd "C-x C-o") 'other-frame
    (kbd "C-x C-q") 'read-only-mode
    (kbd "C-x M-1") 'deft-or-close
    (kbd "C-x M-2") 'eshell
    (kbd "C-x M-3") 'mu4e
    (kbd "C-x M-4") 'erc-start-or-switch
    (kbd "C-x T") 'sane-term
    (kbd "C-x b") 'helm-buffers-list
    (kbd "C-x t") 'sane-term
    (kbd "C-x vl") 'magit-log
    (kbd "C-x vp") 'magit-push
    (kbd "C-x vs") 'magit-status
    (kbd "M-<SPC>") 'insert-U200B-char
    (kbd "M-[") 'winner-undo
    (kbd "M-]") 'winner-redo
    (kbd "M-s") 'helm-occur
    (kbd "M-v") 'er/expand-region
    (kbd "M-x") 'helm-M-x
    (kbd "M-y") 'helm-show-kill-ring
    (kbd "M-y") 'helm-show-kill-ring
    (kbd "M-z")   'zzz-to-char
    (kbd "s-<RET>") 'insert-empty-line
    (kbd "s-<SPC>") 'insert-U200B-char
    (kbd "C-x C-d") 'dired
    ;; (kbd "M-o") 'other-window
    ;; (kbd "TAB") 'yas/expand
    )
  (evil-ex-define-cmd "ag" 'helm-ag)
  (evil-ex-define-cmd "agi[nteractive]" 'helm-do-ag)
  (evil-ex-define-cmd "google" 'helm-google)
  (evil-ex-define-cmd "google-suggest" 'helm-google-suggest)
  (evil-ex-define-cmd "gtag" 'ggtags-create-tags)
  (evil-ex-define-cmd "howdoi" 'howdoi-query))

(use-package evil-leader
  :ensure t
  :after evil
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

(use-package evil-surround
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-quickscope
  :ensure t
  :after evil
  :config
  (add-hook 'prog-mode-hook 'turn-on-evil-quickscope-always-mode))

(use-package evil-terminal-cursor-changer
  :ensure t
  :after evil
  :commands (evil-terminal-cursor-changer-activate)
  :config (evil-terminal-cursor-changer-activate))

(use-package vi-tilde-fringe
  :ensure t
  :if window-system
  :config
  (global-vi-tilde-fringe-mode))

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

(defvar my-editor-map (make-keymap))

(define-minor-mode my-editor-mode
  "My editor minor mode."
  :init-value t
  :keymap my-editor-map)

(define-globalized-minor-mode global-my-editor-mode
  my-editor-mode (lambda ()
                        (if (not (minibufferp (current-buffer)))
                            (my-editor-mode 1))))

;; Gloabal enable
(global-my-editor-mode t)

(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)
(setq revert-without-query '(".*")) ;; disable revert query

;; https://www.emacswiki.org/emacs/LockFiles
(when (version<= "24.3" emacs-version)
  (setq create-lockfiles nil))

(use-package editorconfig
  :ensure t
  :if (executable-find "editorconfig")
  :mode ("\\.editorconfig\\'" . conf-unix-mode)
  :commands editorconfig-mode
  :init
  (add-hook 'prog-mode-hook #'editorconfig-mode))

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
  :config
  ;; Start the Pinentry service
  (pinentry-start))

(use-package tramp
  :ensure t
  :init
  (setq tramp-persistency-file-name (concat user-cache-directory "tramp"))
  :config
  (setq tramp-default-method "rsync"))

(if (version<= "26.1" emacs-version)
    ;; emacs 26.1 has display-line-number-mode, which is written in C
    (progn
      (require 'display-line-numbers)
      ;; Only use line number in `prog-mode-hook'
      (add-hook 'prog-mode-hook #'display-line-numbers-mode))
    ;; for emacs version less than 26, use linum instead
    (use-package linum :ensure t :init (global-linum-mode 1)))

;; disble some mode with linum
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

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package recentf
  :straight (:type built-in)
  :init (setq recentf-save-file (expand-file-name "recentf" user-cache-directory))
  :config
  (recentf-mode 1))

(use-package highlight-numbers
  :ensure t
  :config
  ;; json-mode has it's own highlight numbers method
  (add-hook 'prog-mode-hook #'(lambda()
                               (if (not (derived-mode-p 'json-mode))
                                   (highlight-numbers-mode)))))

(use-package highlight-escape-sequences
  :ensure t
  :config
  ;; Make face the same as builtin face
  (put 'font-lock-regexp-grouping-backslash 'face-alias 'font-lock-builtin-face)
  ;; Enable globally
  (hes-mode 1))

(defun font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations.
This functions should be added to the hooks of major modes for programming."
  (font-lock-add-keywords
   nil
   '(("\\<\\(FIX\\(ME\\)?\\|BUG\\|HACK\\):" 1 font-lock-warning-face t)
     ("\\<\\(NOTE\\):" 1 'org-level-2 t)
     ("\\<\\(TODO\\):" 1 'org-todo t)
     ("\\<\\(DONE\\):" 1 'org-done t))
   ))

(add-hook 'prog-mode-hook 'font-lock-comment-annotations)

(use-package indent-guide
  :ensure t
  :config
  ;; Only show indent-guide in idle-time.
  (setq indent-guide-delay 0.1))

(show-paren-mode 1)
(setq show-paren-delay 0)               ; no delay

(use-package dtrt-indent
  :ensure t
  :config
  ;; enable dtrt-indent-mode globally
  (dtrt-indent-mode 1))

(use-package whitespace-cleanup-mode
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'whitespace-cleanup-mode))

(use-package edit-server
  :ensure t
  :config
  (edit-server-start))

(setq tab-always-indent 'complete)

(use-package symbol-overlay
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'symbol-overlay-mode)
  ;; (global-set-key (kbd "M-i") 'symbol-overlay-put)
  (define-key symbol-overlay-map (kbd "p") 'symbol-overlay-jump-prev) ;; 次のシンボルへ
  (define-key symbol-overlay-map (kbd "n") 'symbol-overlay-jump-next) ;; 前のシンボルへ
  (define-key symbol-overlay-map (kbd "C-g") 'symbol-overlay-remove-all) ;; ハイライトキャンセル
  )

(setq mouse-wheel-scroll-amount '(1)) ; Distance in pixel-resolution to scroll each mouse wheel event.
(setq mouse-wheel-progressive-speed nil) ; Progressive speed is too fast for me.

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

;; Create *scratch* automatically
(run-with-idle-timer 1 t
                     #'(lambda ()
                        (unless (get-buffer "*scratch*")
                          (with-current-buffer (get-buffer-create "*scratch*")
                            (lisp-interaction-mode)))))

(use-package uniquify
  :ensure nil                           ; built-in
  :config
  ;; starting separator for buffer name components
  (setq uniquify-separator " • ")
  ;; rerationalize buffer names after a buffer has been killed.
  (setq uniquify-after-kill-buffer-p t)
  ;; ignore non file buffers
  (setq uniquify-ignore-buffers-re "^\\*"))

(dolist
    (r `(
	   ;; emacs's config.org
	   (?e (file . "~/.emacs.d/init.org"))
	   ;; tasks: todo
	   (?t (file . "~/Org/tasks/todo.org"))
	   ;; tasks: personal
	   (?p (file . "~/Org/tasks/personal.org"))
	   ;; tasks: work
	   (?w (file . "~/Org/tasks/work.org"))
	   ;; Offilce docs
	   (?W (file . "~/Org/Weintek/index.org"))
	   ;; My personal note
	   (?n (file . "~/Org/Note.org"))
	   ;; blogging ideas
	   (?b (file . "~/Org/blog.org"))
	   ;; Finance
	   (?f (file . "~/Org/finance/personal.org"))
	   ))
  (set-register (car r) (cadr r)))

(bind-keys :map my-editor-map
           ;("C-x n" . bm-next)
           ;("C-x p" . bm-previous)
           ;("C-x ." . bm-toggle)
           )

(use-package helm
  :straight t
  :init
  (add-hook 'after-init-hook #'helm-mode)
  (add-hook 'after-init-hook #'helm-autoresize-mode)
  (add-hook 'after-init-hook #'helm-adaptive-mode)
  (add-hook 'after-init-hook #'helm-popup-tip-mode)
  :config
  ;; Use fuzzy match in helm
  (setq helm-M-x-fuzzy-match t)
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-recentf-fuzzy-match t)
  ;; make helm can select anything even not match
  (setq helm-move-to-line-cycle-in-source nil)
  (setq helm-ff-search-library-in-sexp t)
  (setq helm-ff-file-name-history-use-recentf t)
  (bind-keys :map helm-map
             ("TAB"   . helm-execute-persistent-action)
             ("<tab>" . helm-execute-persistent-action)
             ("C-w"   . backward-kill-word)
             ("M-t" . my/minibuffer-switch-to-ramdisk)
             ("M-h" . my/minibuffer-switch-to-home)
             ("M-/" . my/minibuffer-switch-to-rootdir)
             ("M-s" . my/minibuffer-switch-to-tramp)
             ("M-v" . my/minibuffer-switch-to-vm)
             ("M-c" . my/minibuffer-switch-to-cluster)
             ("C-z" . helm-select-action)))

(use-package helm-bm :ensure t :after (helm))

(use-package helm-dash :ensure t :after (helm))

(use-package helm-gtags
  :ensure t
  :after (helm)
  :config
  (setq helm-gtags-ignore-case t)
  (setq helm-gtags-auto-update t)
  (setq helm-gtags-use-input-at-cursor t)
  (setq helm-gtags-pulse-at-cursor t)
  ;; add to following modes
  (add-hook 'c-mode-hook #'helm-gtags-mode)
  (add-hook 'c++-mode-hook #'helm-gtags-mode))

(use-package helm-c-yasnippet
  :ensure t
  :after (helm yasnippet)
  :config
  (setq helm-yas-space-match-any-greedy t))

(use-package helm-smex :ensure t :after (helm))

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
        '(("google" . "http://www.google.com/search?q=")      ; ex: [[google:hi emacs]]
          ("google-map" . "http://maps.google.com/maps?q=%s") ; ex: [[google-map:taiwan]]
          ("wiki" . "https://en.wikipedia.org/wiki/%s")       ; ex: [[wiki:emacs]]
          ))
  ;; make agenda show on current window
  (setq org-agenda-window-setup 'current-window)
  ;; highlight current in agenda
  (add-hook 'org-agenda-mode-hook 'hl-line-mode)
  
  ;; Setup files for agenda
  (setq org-directory "~/Org/tasks")
  ;; U all .org files in `org-directory'
  (setq org-agenda-files
        (find-lisp-find-files org-directory "\.org$"))
  ;;
  (setq org-default-notes-file (f-join org-directory "tasks" "TODO.org"))
  
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
             ("C-c C-c" . org-edit-src-exit)))

(use-package org-indent
  :ensure nil				; build-in
  :after (org)
  :config
  ;; Enable `org-indent-mode' by default
  (add-hook 'org-mode-hook #'(lambda () (org-indent-mode t))))

(use-package org-bullets
  :ensure t
  :after (org)
  :config
  (add-hook 'org-mode-hook #'(lambda () (org-bullets-mode 1))))

(use-package deft
  :ensure t
  :config
  ;; default use org-mode
  (setq deft-default-extension "org")
  ;; default directory set to ~/Org
  (setq deft-directory "~/Org")
  ;; Do not make deft automatically save file
  (setq deft-auto-save-interval 0)
  ;; Recursive search
  (setq deft-recursive t)

  ;; setup an minor-mode to quickly kill all deft buffers
  (define-minor-mode deft-note-mode "Deft notes" nil " Deft-Notes" nil)
  (setq deft-text-mode 'deft-note-mode)

  ;; Quickly kill deft buffers
  (defun my/kill-all-deft-notes ()
    (interactive)
    (save-excursion
      (let ((count 0))
        (dolist (buffer (buffer-list))
          (set-buffer buffer)
          (when (not (eq nil deft-note-mode))
            (setq count (1+ count))
            (kill-buffer buffer))))))

  ;; Enable/Disable defts
  (defun deft-or-close ()
    (interactive)
    (if (or (eq major-mode 'deft-mode) (not (eq nil deft-note-mode)))
        (progn (my/kill-all-deft-notes) (kill-buffer "*Deft*"))
        (deft)))

  (defun my/deft-practice ()
    "Use deft to quickly see my blog drafts."
    (interactive)
    (let ((deft-directory "~/Workspace/practice")
          (deft-extensions '("md" "org"))))
    (deft)))

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

(use-package org-download
  :ensure t
  :config
  ;; add support to dired
  (add-hook 'dired-mode-hook 'org-download-enable))

(use-package plantuml-mode
  :ensure t
  :mode ("\\.plantuml\\'")
  :init
  ;; FIXME: setup plantuml jar path
  ;; FIXME: add org-mode support
  )

(use-package bison-mode
  :ensure t
  :mode ("\\.y\\'" "\\.l\\'" "\\.jison\\'"))

(use-package gn-mode
  :ensure t
  :mode ("BUILD.gn" "\\.gni?\\'"))

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

(use-package nasm-mode :ensure t)

(use-package toml-mode
  :ensure t
  :mode "\\.toml$")

(use-package yaml-mode
  :ensure t
  :mode "\\.yml$")

(use-package ess
  :ensure t
  :mode ("\\.[rR]\\'" . R-mode)
  :config
  )

(use-package qml-mode
  :ensure t
  :mode "\\.qml$"
  :config
  (use-package indent-guide
    :ensure t
    :config
    (add-hook 'qml-mode-hook #'indent-guide-mode)))

(use-package vala-mode
  :ensure t
  :mode ("\\.vala\\'" "\\.vapi\\'")
  :config
  )

(use-package verilog-mode
  :mode ("\\.v\\'")
  :config
  (setq verilog-linter "verilator --lint-only")
  ;; https://github.com/flycheck/flycheck/issues/1250
  (setq flycheck-verilog-verilator-executable "/usr/bin/verilator_bin"))

(use-package groovy-mode
  :mode (("\\.groovy" . groovy-mode)
         ("/Jenkinsfile" . groovy-mode))
  :ensure t)

(use-package svelte-mode
  :ensure t
  :mode ("\\.svelte\\'" . svelte-mode))

(use-package dockerfile-mode
  :ensure t
  :mode "Dockerfile\\'")

(use-package ssh-config-mode
  :ensure t
  :mode ((".ssh/config\\'"       . ssh-config-mode)
         ("sshd?_config\\'"      . ssh-config-mode)
         ("known_hosts\\'"       . ssh-known-hosts-mode)
         ("authorized_keys\\'"   . ssh-authorized-keys-mode)))

(use-package systemd
  :ensure t)

(use-package cmake-mode
  :ensure t
  :mode (("CMakeLists\\.txt\\'" . cmake-mode) ("\\.cmake\\'" . cmake-mode)))

;; cmake-font-lock: emacs font lock rules for CMake
;; https://github.com/Lindydancer/cmake-font-lock
(use-package cmake-font-lock
  :ensure t
  :config
  (autoload 'cmake-font-lock-activate "cmake-font-lock" nil t)
(add-hook 'cmake-mode-hook 'cmake-font-lock-activate))

(use-package meson-mode
  :ensure t
  :mode (("meson\\.build\\'" . meson-mode)))

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
               ;; Make TAB equivilent to 8 spaces
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
            '(lambda ()
  
               ;; Use stroustrup style
               (c-set-style "stroustrup")
  
               ;; Setting indentation lvel
               (setq c-basic-offset 4)
  
               ;; Make TAB equivilent to 4 spaces
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
             ("M-." . helm-gtags-dwim)
             ("M-," . helm-gtags-pop-stack)))

(use-package c-eldoc
  :ensure t
  :config
  (add-hook 'c-mode-common-hook
            #'(lambda ()
               (setq c-eldoc-includes "`pkg-config --cflags --libs` -I./ -I../")
               (c-turn-on-eldoc-mode))))

(use-package cwarn
  :config
  (add-hook 'c-mode-common-hook #'(lambda () (cwarn-mode 1))))

(defun my/cc-mode/highlight-if-0 ()
  "highlight c/c++ #if 0 #endif macros"
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

(use-package srefactor
  :ensure t
  :defer t
  :after (cc-mode))

(use-package cff
  :ensure t
  :after (cc-mode))

;; adds font-lock highlighting for modern C++ upto C++17
;; https://github.com/ludwigpacifici/modern-cpp-font-lock
(use-package modern-cpp-font-lock
  :ensure t
  :hook (c++-mode . modern-c++-font-lock-mode)
  :after (cc-mode))

(defun my/c-kill-defun ()
  "Move backward to the beging of top level declaration and save
this declaration to the kill-ring."
  (interactive)
  (save-excursion
    (kill-region
     (progn (c-beginning-of-defun) (point))
     (progn (c-end-of-defun)       (point)))))

;; clang-format: format C/C++ buffers using clang-format
;; https://github.com/emacsorphanage/clang-format
(use-package clang-format
  :ensure t)

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
              '(lambda ()
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
  
  (add-hook 'emacs-lis-mode
            (lambda () (my/emacs-lisp/enable-check-parens-on-save)))
  (use-package litable
    :ensure t
    :config
    ;; Save cache file to `user-cache-direcotry'
    (setq litable-list-file (concat user-cache-directory ".litable-lists.el"))
    ;; Enable litable-mode globally
    (litable-mode))
  (use-package page-break-lines
    :ensure t
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
  ;;               ("C-c '" . my/narrow-or-widen-dwim)
  ))

(use-package cider
  :ensure t :defer t
  :config
  (setq
    cider-repl-history-file ".cider-repl-history"  ;; not squiggly-related, but I like it
	nrepl-log-messages t)                          ;; not necessary, but useful for trouble-shooting
  (flycheck-clojure-setup))                        ;; run setup *after* cider load


(use-package flycheck-clojure
  :defer t
  :commands (flycheck-clojure-setup)               ;; autoload
  :config
  (eval-after-load 'flycheck
    '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package flycheck :ensure t)
(use-package flycheck-pos-tip :ensure t
  :after flycheck)

(setq vc-handled-backends nil)

(use-package magit
  :ensure t
  :config
  (setq magit-commit-arguments '("--verbose" "--signoff")))

(setq magit-diff-refine-hunk 'all)

(use-package git-modes :ensure t)

(use-package git-gutter-fringe
  :ensure t
  :if window-system                     ; git-gutter-fringe only work on GUI
  :config
  ;; enable globally
  (git-gutter-mode))

;; FIXME:
(use-package llm
  :ensure t :defer t)
;; FIXME:
(use-package llm-ollama
  :ensure t :defer t)

(use-package magit-gptcommit
  :demand t
  :after magit llm llm-ollama
  :hook (after-init . magit-gptcommit-status-buffer-setup)
  :config

  (setq magit-gptcommit-llm-provider
	(make-llm-ollama
	 :chat-model "gpt-oss:20b"
	 :host "127.0.0.1"
	 :port 11434))
  ;; add to magit's transit buffer
  (magit-gptcommit-status-buffer-setup)
  :bind (:map git-commit-mode-map
              ("C-c C-g" . magit-gptcommit-commit-accept)))

(use-package flycheck
  :ensure t
  :config
  ;; enable globally
  (global-flycheck-mode))

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
          (toml       . ("https://github.com/tree-sitter/tree-sitter-toml"))
          (tsx        . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src"))
          (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src"))
          (vue        . ("https://github.com/merico-dev/tree-sitter-vue"))
          (yaml       . ("https://github.com/ikatyang/tree-sitter-yaml"))
          (zig        . ("https://github.com/GrayJack/tree-sitter-zig")))
        )
  ;; treesit mode will create lang-ts-mode, take pyton for example, we
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

(use-package eca
  :ensure t)

(use-package yasnippet
  :ensure t
  :mode ("emacs.+/snippets/" . snippet-mode)
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
  
  Take emacs-lisp-mode as example, if you wand to use <r to expand your snippet `require'
  in yasnippet, you muse setup the emacs-lisp-mode-expand-alist variable.
  
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

(use-package flymake-shell
  :ensure t
  :config (add-hook 'sh-set-shell-hook 'flymake-shell-load))

(use-package bitbake
  :ensure t
  :mode ("\\.bb\\'" "\\.bbappend\\'"))

(use-package dts-mode :ensure t
  :mode ("\\.dts\\'" "\\.dtsi\\'"))

(use-package dart-mode
  :ensure t
  :mode ("\\.dart\\'")
  :config
  ;; enable analyzer support
  (setq dart-enable-analysis-server t)
  ;; add flycheck support
  (add-hook 'dart-mode-hook 'flycheck-mode))

(use-package gnuplot :ensure t
  :commands gnuplot-mode
  :mode "\\.gp$")

(use-package graphviz-dot-mode :ensure t
  :mode "\\.dot\\'"
  :config
  ;; alias `dot-mode' to graphviz-dot-mode
  (defalias 'dot-mode 'graphviz-dot-mode))

(use-package glsl-mode :ensure t
  :mode (("\\.vs\\'" . glsl-mode)
         ("\\.fs\\'" . glsl-mode)
         ("\\.gs\\'" . glsl-mode))
  :config
  (setq glsl-other-file-alist '(("\\.fs$" (".vs")) ("\\.vs$" (".fs")))))

(use-package js2-mode
  :ensure t
  :mode "\\.js\\'")

(use-package js2-refactor
  :ensure t)

(use-package nvm :ensure t)

(use-package import-js :ensure t)

(use-package json-mode :ensure t
  :mode "\\.json\\'")

(use-package json-reformat :ensure t :commands json-reformat-region)

(use-package flymake-json :ensure t
  :config
  (add-hook 'json-mode-hook (lambda () (flymake-json-load))))

(use-package po-mode :ensure t
  :mode "\\.po\\'\\|\\.po\\."
  :config

  ;; To use the right coding system automatically under Emacs 20 or newer,
  ;; also add:
  (when (require 'po nil 'noerror)
    (modify-coding-system-alist 'file "\\.po\\'\\|\\.po\\."
                                'po-find-file-coding-system))
  )

(use-package python
  :mode (("SCons\\(truct\\|cript\\)\\'" . python-mode)
         ("DEPS" . python-mode)))

(use-package ruby-mode
  :ensure nil				; built-in
  :mode (("Gemfile\\'"  . ruby-mode)
         ("Kirkfile\\'" . ruby-mode)
         ("Rakefile\\'" . ruby-mode)
         ("Vagrantfile\\'" . ruby-mode)
         ("\\.builder\\'"  . ruby-mode)
         ("\\.gemspec\\'"  . ruby-mode)
         ("\\.irbrc\\'" . ruby-mode)
         ("\\.pryrc\\'" . ruby-mode)
         ("\\.rake\\'"  . ruby-mode)
         ("\\.rjs\\'"   . ruby-mode)
         ("\\.ru\\'"    . ruby-mode)
         ("\\.rxml\\'"  . ruby-mode))
  :config
  ;; We never want to edit Rubinius bytecode
  (add-to-list 'completion-ignored-extensions ".rbc"))

(use-package go-mode
  :ensure t
  :config
  (use-package company-go
    :ensure t
    :config
    (defun my/setup-go-mode-company-go ()
      "Hook for running on company-go"
      ;; we only want to use company-go - it's so accurate we won't need
      ;; any other completion engines
      (set (make-local-variable 'company-backends) '(company-go)))
    (add-hook 'go-mode-hook 'my/setup-go-mode-company-go))
  (defun my/setup-go-mode-gofmt-hook ()
    ;; Use goimports instead of go-fmt
    (setq gofmt-command "goimports")
    ;; Call Gofmt before saving
    (add-hook 'before-save-hook 'gofmt-before-save))
  (add-hook 'go-mode-hook 'my/setup-go-mode-gofmt-hook)
  (bind-keys :map go-mode-map
             ("M-." . godef-jump)))

(use-package company-go
  :ensure t
  :config
  (defun my/setup-go-mode-company-go ()
    "Hook for running on company-go"
    ;; we only want to use company-go - it's so accurate we won't need
    ;; any other completion engines
    (set (make-local-variable 'company-backends) '(company-go)))
  (add-hook 'go-mode-hook 'my/setup-go-mode-company-go))

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

(use-package lispy
  :ensure t
  :config

  (defun my/up-list (&optional arg)
    "My special lisp moving stragedy."
    (interactive)
    (or arg (setq arg -1))
    (condition-case ex
        (up-list arg)
      ('error (progn
                (lispy-backward arg)
                (beginning-of-line)
                (up-list arg)))))

  (defun my/down-list (&optional arg)
    "My special lisp moving stragedy."
    (interactive)
    (or arg (setq arg 1))
    (condition-case ex
        (down-list arg)
      ('error (progn
                (lispy-forward arg)
                (end-of-line)
                (down-list arg)))))

  ;; My special hack for lispy-mode
  (defun my/lispy-mode ()
    (lispy-mode 1)
    ;; `M-m' is preserved for mode setting
    (define-key lispy-mode-map (kbd "M-m") nil)
    ;; `M-s' is for my search command, rebind to `C-c s'
    (define-key lispy-mode-map (kbd "M-s") nil)
    (define-key lispy-mode-map (kbd "C-c s") 'lispy-splice)
    ;; `[' and `]' just insert them
    (define-key lispy-mode-map (kbd "[") 'lispy-open-square)
    (define-key lispy-mode-map (kbd "]") 'lispy-close-square)
    ;; My special lisp moving cmd
    (define-key lispy-mode-map (kbd "C-M-f") 'my/down-list)
    (define-key lispy-mode-map (kbd "C-M-b") 'my/up-list))

  (add-hook 'emacs-lisp-mode-hook #'my/lispy-mode)
  (add-hook 'lisp-interaction-mode-hook #'my/lispy-mode)
  (add-hook 'clojure-mode-hook #'my/lispy-mode)
  (add-hook 'scheme-mode-hook #'my/lispy-mode)
  (add-hook 'lisp-mode-hook #'my/lispy-mode))

(use-package indent-guide
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook #'indent-guide-mode)
  (add-hook 'lisp-interaction-mode-hook #'indent-guide-mode)
  (add-hook 'clojure-mode-hook #'indent-guide-mode)
  (add-hook 'scheme-mode-hook #'indent-guide-mode)
  (add-hook 'lisp-mode-hook #'indent-guide-mode))

(global-prettify-symbols-mode 1)

(use-package web-mode
  :ensure t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.ejs?\\'" . web-mode)))

(use-package css-mode
  :ensure nil				; built-in
  :mode "\\.css\\'")

(use-package css-eldoc
  :ensure t
  :config
  (add-hook 'css-mode-hook 'turn-on-css-eldoc)
  (add-hook 'scss-mode-hook 'turn-on-css-eldoc)
  (add-hook 'less-css-mode-hook 'turn-on-css-eldoc))

(use-package less-css-mode
  :ensure t
  :mode ("\\.less$" . less-css-mode))

(use-package scss-mode
  :ensure t
  :mode "\\.scss\\'"
  :config
  ;; dont' build scss to css after save file
  (setq scss-compile-at-save nil))

(use-package mustache-mode :mode "\\.mustache$" :ensure t)

(defadvice term-handle-exit (after kill-buffer-after-exit activate)
  "Kill the term buffer if the process finished."
  (kill-buffer (current-buffer)))

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

(defun eshell/.. (&optional level)
  "Go up LEVEL directories"
  (interactive)
  (let ((level (or level 1)))
    (eshell/cd (make-string (1+ level) ?.))
    (eshell/ls)))

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
    ;; We have to expand the file names or else naming a directory in an
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

(use-package winner                     ; builtin
  :commands (winner-undo winner-redo)
  :config
  ;; I use my own keymap for winner-mode
  (setq winner-dont-bind-my-keys t)
  ;; Start winner-mode globally
  (winner-mode t))

(use-package eyebrowse
  :ensure t
  :config
  ;; enable eyebrowse globally
  (eyebrowse-mode t))

(use-package window-numbering
  :ensure t)

(bind-keys :map global-map
           ("C-x C-s" . my/save-buffer-always))

(let ((secret "~/.personal.el"))
  (when (file-exists-p secret) (load-file secret)))
