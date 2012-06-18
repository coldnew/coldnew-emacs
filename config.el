
(eval-when-compile (require 'cl))

(setq inhibit-startup-message t)

(setq initial-scratch-message "")

(setq visible-bell t)

(if (featurep 'tool-bar) (tool-bar-mode -1))

(if (featurep 'scroll-bar) (scroll-bar-mode -1))

(blink-cursor-mode -1)

(if (featurep 'menu-bar) (menu-bar-mode -1))

(setq time-stamp-active      t ) ; do enable time-stamps
(setq time-stamp-line-limit 10 ) ; check first 10 buffer lines for Time-stamp:
(setq time-stamp-format "%04y-%02m-%02d %02H:%02M:%02S (%u)") ; date format

(fset 'yes-or-no-p 'y-or-n-p)

;; TODO: add comment
(setq stack-trace-on-error t)
(setq imenu-auto-scan t)
;;(setq redisplay-dont-pause t)

;; xrelated srtting
(setq x-select-enable-clipboard t)
(setq select-active-regions t)
(setq x-gtk-use-system-tooltips nil)    ; disable gtk-tooltip

(setq max-lisp-eval-depth '40000)
(setq max-specpdl-size 5)  ; default is 1000, reduce the backtrace level
(setq debug-on-error t)    ; now you should get a backtrace

;; nice scrolling
(setq scroll-margin                   0 )
(setq scroll-conservatively      100000 )
(setq scroll-preserve-screen-position 1 )
(setq scroll-up-aggressively       0.01 )
(setq scroll-down-aggressively     0.01 )

(defvar emacs-dir "~/.emacs.d/"
  "The top-level emacs-configure directory.")

(defvar emacs-config-file (concat emacs-dir "config.org")
  "File to place emacs configs.")

(defvar emacs-themes-dir (concat emacs-dir "themes/")
  "directory to place emacs theme.")
(defvar emacs-lisp-dir   (concat emacs-dir "lisp/")
  "directory to place lisp packages from internet.")
(defvar emacs-elpa-dir   (concat emacs-lisp-dir "elpa/")
  "directory to place ELPA lisp packages from internet.")
(defvar emacs-snippets-dir (concat emacs-dir "snippets/")
  "directory to place yasnippet files.")
(defvar emacs-recipes-dir (concat emacs-dir "recipes/")
  "directory to place local el-get recepies.")

(defvar emacs-custom-file (concat emacs-dir "custom.el")
  "store customize UI config.")

(defvar emacs-bin-dir    (concat emacs-dir "bin/")
  "directory to place binary staff.")
(defvar emacs-cache-dir  (concat emacs-dir "cache/")
  "cache file directory.")
(defvar emacs-backup-dir (concat emacs-dir "backup/")
  "directory to backup files.")
(defvar emacs-authinfo-file (concat emacs-dir ".authinfo.gpg")
  "file that save secret")
(defvar emacs-bookmark-file (concat emacs-cache-dir "bookmarks")
  "File to save bookmarks")
                                    ;; (defvar emacs-log-dir (concat emacs-var-dir "log/")
;;   "log file directory."

(defvar emacs-default-shell "/bin/bash"
  "Default shell for cemacs.")
(defvar emacs-popup-shell-window-height 30
  "Window hight of popup shell.")
(defvar emacs-popup-shell-window-position "bottom"
  "Make popup shell window at buttom by default.")

;; TODO: make it work on every platform, now only has Linux support
(setenv "PATH"
        (concat
         emacs-bin-dir ":"
         "~/.lein/bin" ":"
         (getenv "PATH")
         ))

(setq exec-path (cons emacs-bin-dir exec-path))

(defun change-mouse-to-left ()
  (interactive)
  (shell-command "xmodmap -e \"pointer = 3 2 1\""))

(defun change-mouse-to-right ()
  (interactive)
  (shell-command "xmodmap -e \"pointer = 1 2 3\""))

(defun swap-ctrl-caps ()
  "swap control and capslock"
  (shell-command "setxkbmap -option ctrl:swapcaps"))

(defun make-caps-as-ctrl ()
  "make capslock as control-key"
  (shell-command "setxkbmap -option ctrl:nocaps"))

;; only disable capslock and make it as control
(cond ((eq window-system 'x)
       ;; make caps lock a control key
       (make-caps-as-ctrl)
       (change-mouse-to-left)))

(defvar mac-p     (eq system-type 'darwin)
  "Return nil if OS is not Mac.")

(defvar linux-p   (and (eq system-type 'gnu/linux) (not (eq system-type 'drawin)))
  "Return nil if OS is not Linux.")

(defvar linux-32bit-p (and (string-match (rx bos "x86-") system-configuration) linux-p)
"Return nil if OS is not 32-bit linux.")

(defvar linux-64bit-p (and (string-match (rx bos "x86_64") system-configuration) linux-p)
  "Return nil if OS is not 64-bit linux.")

(defvar cygwin-p  (eq system-type 'cygwin)
  "Return nil if OS is not CygWin.")

(defvar windows-p (eq system-type 'windows-nt)
  "Return nil if OS is not Windows.")

(defvar root-p (zerop (user-real-uid))
  "Return nil if user is not root user.")

(defvar display-1280x800-p   (and (= (display-pixel-width) 1280)
                                  (= (display-pixel-height) 800))
  "Return nil if current display's resolution is not 1280x800")

(defvar display-1280x1024-p  (and (= (display-pixel-width) 1280)
                                  (= (display-pixel-height) 1024))
  "Return nil if current display's resolution is not 1280x1024")

(defvar display-1920x1080-p  (and (= (display-pixel-width) 1920)
                                  (= (display-pixel-height) 1080))
  "Return nil if current display's resolution is not 1920x1080")

(defface mode-line-read-only-face
  '((t (:foreground "#C82829" :bold t)))
  "face for mode-name-string in modeline."
  :group 'mode-line)

(defface mode-line-modified-face
  '((t (:inherit 'font-lock-function-name-face :bolt t)))
  "face for mode-name-string in modeline."
  :group'mode-line)

(defface mode-line-mode-name-face
  '((t (:inherit font-lock-keyword-face)))
  "face for mode-name-string in modeline."
  :group 'mode-line)

(defface mode-line-normal-state-face
  '((t (:inherit font-lock-function-name-face)))
  "face for emacs normal state"
  :group 'mode-line)

(defface font-lock-escape-char-face
  '((((class color)) (:foreground "seagreen2")))
  "highlight c escapes char like vim"
  :group 'font-lock-faces)

(defface mode-line-evil-state-string-N
  '((t (:inherit font-lock-function-name-face)))
  "face for vim-string in normal-map on mode-line."
  :group 'mode-line)

(defface mode-line-evil-state-string-I
  '((t (:inherit font-lock-constant-face)))
  "face for vim-string in insert-map on mode-line."
  :group 'mode-line)

(defface mode-line-evil-state-string-V
  '((t (:inherit font-lock-variable-name-face)))
  "face for vim-string in visual-map on mode-line."
  :group 'mode-line)

(defface mode-line-evil-state-string-E
  '((t (:inherit font-lock-string-face)))
  "face for vim-string in emacs-map on mode-line."
  :group 'mode-line)

(defmacro require* (feature &optional file)
    "Try to require FEATURE, but don't signal an error if `reauire' fails.
  If this package does not exist, use el-get reinstall it."
    `(let* ((require-result (require ,feature ,file 'noerror)))
       ;; if package does not exist, reinstall it
       (if-not require-result (el-get-reinstall ,feature)
               ;; TODO: add require-result buffer
               )))

(font-lock-add-keywords 'emacs-lisp-mode
                        '(("(\\(require\*\\*\\)\\s [ \t']*\\(\\sw+\\)?"
                           (1 font-lock-keyword-face)
                           (2 font-lock-constant-face nil t))))
(font-lock-add-keywords 'lisp-interaction-mode
                        '(("(\\(require\*\\*\\)\\s [ \t']*\\(\\sw+\\)?"
                           (1 font-lock-keyword-face)
                           (2 font-lock-constant-face nil t))))

(setq custom-file emacs-custom-file)

;; Only start server mode if I'm not root
(unless (string-equal "root" (getenv "USER"))
  (require 'server)
  (unless (server-running-p) (server-start)))

(cond

(display-1920x1080-p
 (setq default-frame-alist
       (append (list
                '(width  . 134)
                '(height . 45)
                '(top    . 90)
                '(left   . 500))
               default-frame-alist)))

(display-1280x1024-p
 (setq default-frame-alist
       (append (list
                '(width  . 114)
                '(height . 40)
                '(top    . 90)
                '(left   . 300))
               default-frame-alist)))

(display-1280x800-p
 (setq default-frame-alist
       (append (list
                '(width  . 114)
                '(height . 40)
                '(top    . 90)
                '(left   . 300))
               default-frame-alist)))

(t
 (setq default-frame-alist
       (append (list
                '(width  . 100)
                '(height . 40)
                '(top    . 90)
                '(left   . 100))
               default-frame-alist)))

)

(prefer-coding-system 'utf-8 )

(set-language-environment 'utf-8 )

(set-buffer-file-coding-system 'utf-8 )

(set-keyboard-coding-system    'utf-8 )

(set-terminal-coding-system    'utf-8 )

(set-selection-coding-system   'utf-8 )

(set-clipboard-coding-system   'utf-8 )

(set-file-name-coding-system   'utf-8 )

(setq system-time-locale "en_US" )

(setq-default el-get-dir emacs-lisp-dir)

(unless (require 'el-get nil t)
  (setq el-get-install-branch "master")
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (end-of-buffer)
    (eval-print-last-sexp)))

(unless (file-exists-p el-get-recipe-path-elpa)
  (el-get-elpa-build-local-recipies))

(require 'el-get)

(add-to-list 'el-get-recipe-path emacs-recipes-dir)

(add-to-list 'auto-mode-alist '("\\.rcp$" . emacs-lisp-mode))

(setq el-get-sources '(

(:name powerline
       :type github
       :pkgname "jonathanchu/emacs-powerline")

(:name lusty-explorer
       :type github
       :pkgname sjbach/lusty-emacs
       :description "LustyExplorer is a fast and responsive way to manage files and buffers")

))

(setq-default package-user-dir emacs-elpa-dir)

(require 'package)

(setq package-archives
      '(("ELPA" . "http://tromey.com/elpa/")
        ("gnu" . "http://elpa.gnu.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")))

(defmacro comment (&rest body)
  "Comment out one or more s-expressions."
  nil)

(defmacro -> (x &optional form &rest more)
  (cond ((not (null more))
         `(-> (-> ,x ,form) ,@more))
        ((not (null form))
         (if (sequencep form)
             `(,(first form) ,x ,@(rest form))
           (list form x)))
        (t x)))

(defmacro ->> (x form &rest more)
  (cond ((not (null more)) `(->> (->> ,x ,form) ,@more))
        (t (if (sequencep form)
               `(,(first form) ,@(rest form) ,x)
             (list form x)))))

(defmacro -?> (x form &rest more)
  (cond ((not (null more)) `(-?> (-?> ,x ,form) ,@more))
        (t (if (sequencep form)
               `(if (null ,x) nil
                  (,(first form) ,x ,@(rest form)))
             `(if (null ,x) nil
                ,(list form x))))))

(defmacro -?>> (x form &rest more)
  (cond ((not (null more)) `(-?>> (-?>> ,x ,form) ,@more))
        (t (if (sequencep form)
               `(if (null ,x) nil
                  (,(first form) ,@(rest form) ,x))
             `(if (null ,x) nil
                ,(list form x))))))

(defmacro if-not (test then &optional else)
  "Evaluates test. If logical false, evaluates and returns then expr,
  otherwise else expr, if supplied, else nil."
  `(if (not ,test) ,then ,else))

(require* 'ascii)
(require* 'ace-jump-mode)
(require* 'expand-region)
(require* 'unicad)
(require* 'iedit)
(require* 'rainbow-mode)
(require* 'smarter-compile)
(require* 'sr-speedbar)
(require* 'tabbar)
(require* 'sauron)
(require* 'traverselisp)
(require* 'helm)
(require* 'space-chord)
(require* 'smallurl)
(require* 'switch-window)
(require* 'pretty-lambdada)
(require 'projectile)
(projectile-mode)

(require 'parenface)
(set-face-foreground 'paren-face "green")

(require 'misc)
(require 'cc-mode)

(defadvice kill-emacs (around recompile-emacs-config activate)
  "Before exit emacs, kill config.el which is generate by config.org."
  (let ((file-name (expand-file-name (concat emacs-dir "config.el"))))
    (if (file-exists-p file-name)
        (delete-file file-name nil))
    ad-do-it))

(defun flatten (structure)
  "Flatten the nesting in an arbitrary list of values."
  (cond ((null structure) nil)
        ((atom structure) `(,structure))
        (t (mapcan #'flatten structure))))

(defun list-to-string (char-list)
  "RETURN: A new string containing the characters in char-list."
  (let ((result (make-string (length char-list) 0))
        (i 0))
    (dolist (char char-list)
      (aset result i char)
      (setq i (1+ i)))
    result))

(defun search-backward-to-char (chr)
  "Search backwards to a character"
  (while (not (= (char-after) chr))
    (backward-char 1)))

(defun search-forward-to-char (chr)
  "Search forwards to a character"
  (while (not (= (char-before) chr))
    (forward-char 1)))

(defun coldnew/recursive-find-file (file &optional directory)
  "Find the first FILE in DIRECTORY or it's parents.
If file does not exist return nil."
  (let ((directory (or directory
                       (file-name-directory (buffer-file-name))
                       (pwd))))
    (if (file-exists-p (expand-file-name file directory))
        (expand-file-name file directory)
      (unless (string= "/" directory)
        (coldnew/recursive-find-file file (expand-file-name ".." directory))))))

(defun font-exist-p (fontname)
  "test if this font is exist or not."
  (if (not (x-list-fonts fontname))
      nil t))

(defun dos->unix (buf)
  "Convert buffer file from dos file to unix file."
  (let* (current-buf (current-buffer))
    (if (not (eq current-buf buf))
        (switch-to-buffer buf))
    (goto-char(point-min))
    (while (search-forward "\r" nil t) (replace-match ""))))

(defun unix->dos (buf)
  "Convert buffer file from unix file to dos file."
  (let* (current-buf (current-buffer))
    (if (not (eq current-buf buf))
        (switch-to-buffer buf))
    (goto-char(point-min))
    (while (search-forward "\n" nil t) (replace-match "\r\n"))))

(defun file->string (file)
  "Convert file to string in buffer with quote."
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (buffer-string))))

(defun get-buffers-matching-mode (mode)
  "Returns a list of buffers where their major-mode is equal to MODE"
  (let ((buffer-mode-matches '()))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (if (eq mode major-mode)
            (add-to-list 'buffer-mode-matches buf))))
    buffer-mode-matches))

(defun show-buffer-major-mode (buffer-or-string)
  "Returns the major mode associated with a buffer."
  (with-current-buffer buffer-or-string major-mode))

(defun get-ip-address (&optional dev)
  "get the IP-address for device DEV (default: eth0)"
  (let ((dev (if dev dev "eth0")))
    (format-network-address (car (network-interface-info dev)) t)))

(defun current-date-time ()
  "return current date in `%Y-%m-%d' format, ex:`2012-04-25'."
  (let ((system-time-locale "en_US")
        (format "%Y-%m-%d"))
    (format-time-string "%Y-%m-%d")))

(defun day-of-week (year month day)
  "Returns the day of the week as an integer.
   Monday is 1."
  (nth 6 (decode-time (encode-time 0 0 0 day month year))))

(defun day-of-week-in-string (year month day)
  "Return the day of the week as day name."
  (let* ((day-names '("Sunday" "Monday" "Tuesday" "Wednesday"
                      "Thursday" "Friday" "Saturday"))
         (day-index (nth 6 (decode-time (encode-time 0 0 0 day month year)))))
    (nth day-index day-names)))

(defun map-define-key (mode-map keylist fname)
  "Like define-key but the key arg is a list that should be mapped over.
   For example: (map-define-key '(a b c d) 'function-name)."
  (mapc (lambda (k) (define-key mode-map k fname))
        keylist))

(defun emacs-process-p (pid)
  "If pid is the process ID of an emacs process, return t, else nil.
Also returns nil if pid is nil."
  (when pid
    (let ((attributes (process-attributes pid)) (cmd))
      (dolist (attr attributes)
        (if (string= "comm" (car attr))
            (setq cmd (cdr attr))))
      (if (and cmd (or (string= "emacs" cmd) (string= "emacs.exe" cmd))) t))))

;;;; ---------------------------------------------------------------------------
;;;; Region
;;;; ---------------------------------------------------------------------------
(defun select-region-to-before-match (match &optional dir)
  "Selects from point to the just before the first match of
'match'.  The 'dir' controls direction, if nil or 'forwards then
go forwards, if 'backwards go backwards."
  (let ((start (point))
        (end nil))

    (transient-mark-mode 1)    ;; Transient mark
    (push-mark)                ;; Mark the start, where point is now

    (if (or (null dir)
            (equalp 'forwards dir))

        ;; Move forwards to the next match then back off
        (progn
          (search-forward match)
          (backward-char))

      ;; Or search backwards and move forwards
      (progn
        (search-backward match)
        (forward-char)))

    ;; Store, then hilight
    (setq end (point))
    (exchange-point-and-mark)

    ;; And return, swap the start/end depending on direction we're going
    (if (or (null dir)
            (equalp 'forwards dir))
        (list start end)
      (list end start))))

;; Font type setting
;; (defvar emacs-english-font "Inconsolata"
(defvar emacs-english-font "Monaco"
  "The font name of English.")
;; (defvar emacs-cjk-font "LiHei Pro"
(defvar emacs-cjk-font "Hiragino Sans GB W3"
  "The font name for CJK.")
(defvar emacs-symbol-font "Monaco"
  "The font name for Synbol.")

;; font size setting
;; (defvar emacs-english-font-size 11.5
(defvar emacs-english-font-size 11.5
  "Default English font size.")
;;(defvar emacs-cjk-font-size 15

(defvar emacs-cjk-font-size 13.5
  ;; (defvar emacs-cjk-font-size 14
  "Default CJK font size.")
(defvar emacs-symbol-font-size 16
  "Default Symbol font size.")

;; Use my defined font under X
(cond ((eq window-system 'x)
       ;; Setting English Fonts
       (if (font-exist-p emacs-english-font)
           (set-frame-font (format "%s-%s" (eval emacs-english-font) (eval emacs-english-font-size))))

       ;; Setting Chinese Fonts
       (if (font-exist-p emacs-cjk-font)
           (set-fontset-font (frame-parameter nil 'font)
                             'han (format "%s-%s" (eval emacs-cjk-font) (eval emacs-cjk-font-size))))

       ;; Setting Symbol Fonts
       (if (font-exist-p emacs-symbol-font)
           (set-fontset-font (frame-parameter nil 'font)
                             'symbol (format "%s-%s" (eval emacs-symbol-font) (eval emacs-symbol-font-size))))
       ))


;; list text sample
(setq-default list-faces-sample-text
              (concat
               "ABCDEFTHIJKLMNOPQRSTUVWXYZ abcdefghijklmnopqrstuvwxyz 11223344556677889900"
               "ABCDEFTHIJKLMNOPQRSTUVWXYZ abcdefghijklmnopqrstuvwxyz 壹貳參肆伍陸柒捌玖零"
               ))

(setq custom-theme-directory emacs-themes-dir)

(load-theme 'coldnew-night t)

(require 'auto-complete)
(require 'auto-complete-config)
(require* 'auto-complete-clang)

(ac-config-default)

(setq ac-use-fuzzy nil)

(setq ac-auto-start 4)

;; Ignore case if completion target string doesn't include upper characters
(setq ac-ignore-case 'smart)

;; Enable auto-complete quick help
(setq ac-use-quick-help t)

;; After 0.01 sec, show help window
(setq ac-quick-help-delay 0.5)

;; Enable ac-comphist
(setq ac-use-comphist t)

;; Setting ac-comphist data
(setq ac-comphist-file (concat emacs-cache-dir "auto-complete.dat"))

;; Show menu
(setq ac-auto-show-menu t)

;; Enable ac-menu-map
(setq ac-use-menu-map t)

(define-key ac-menu-map (kbd "C-n") 'ac-next)
(define-key ac-menu-map (kbd "C-p") 'ac-previous)
(define-key ac-completing-map "\t" 'ac-complete)
(define-key ac-complete-mode-map [tab] 'ac-expand)

(require* 'auto-indent-mode)

(require* 'android-mode)
;; Set my android-emulator-path
(setq android-mode-sdk-dir "/opt/android-sdk-update-manager/")

(require* 'elscreen)

(elscreen-start)

(setq elscreen-tab-display-control t)

(setq elscreen-tab-display-kill-screen nil)

(defmacro elscreen-create-automatically (ad-do-it)
  (` (if (not (elscreen-one-screen-p))
         (, ad-do-it)
       (elscreen-create)
       (elscreen-notify-screen-modification 'force-immediately)
       (elscreen-message "New screen is automatically created"))))

(defadvice elscreen-next (around elscreen-create-automatically activate)
  (elscreen-create-automatically ad-do-it))

(defadvice elscreen-previous (around elscreen-create-automatically activate)
  (elscreen-create-automatically ad-do-it))

(defadvice elscreen-toggle (around elscreen-create-automatically activate)
  (elscreen-create-automatically ad-do-it))

(require 'eshell)
(require 'em-dirs)
(require 'em-hist)
(require 'em-prompt)
(require 'em-term)
(require 'em-cmpl)

(setq eshell-prompt-function
      '(lambda ()
         (concat
          user-login-name "@" system-name " "
          (if (search (directory-file-name (expand-file-name (getenv "HOME"))) (eshell/pwd))
              (replace-regexp-in-string (expand-file-name (getenv "HOME")) "~" (eshell/pwd))
            (eshell/pwd))
          (if (= (user-uid) 0) " # " " $ ")
          )))

;;; change history file path
(setq eshell-last-dir-ring-file-name (concat emacs-cache-dir "eshell-lastdir"))
(setq eshell-history-file-name (concat emacs-cache-dir "eshell-history"))

;; other setting
(setq eshell-save-history-on-exit t)
(setq eshell-ask-to-save-last-dir nil)
(setq eshell-history-size 512)
(setq eshell-hist-ignoredups t)
(setq eshell-cmpl-cycle-completions nil)
(setq eshell-scroll-to-bottom-on-output t)
(setq eshell-show-maximum-output t)

;;;; ---------------------------------------------------------------------------
;;;; Hooks
;;;; ---------------------------------------------------------------------------

;;; Make eshell prompt more colorful
(add-to-list 'eshell-output-filter-functions 'coldnew/colorfy-eshell-prompt)

;; my auto-complete for elisp
(add-hook 'eshell-mode-hook 'auto-complete-mode)
(add-hook 'eshell-mode-hook 'ac-eshell-mode-setup)

;; use helm to complete esehll
(when (featurep 'helm)
  (add-hook 'eshell-mode-hook
            #'(lambda ()
                (define-key eshell-mode-map
                  [remap pcomplete]
                  'helm-esh-pcomplete))))


;; define ac-source for eshell-pcomplete
(ac-define-source eshell-pcomplete
  '((candidates . pcomplete-completions)
    (cache)
    (symbol . "f")))

(defun ac-eshell-mode-setup ()
  "auto-complete settings for eshell-mode"
  (setq ac-sources
        '(
          ac-source-eshell-pcomplete
          ;; ac-source-symbols
          ;; ac-source-variables
          ;; ac-source-functions
          ;; ac-source-features
          ;; ac-source-filename
          ;; ac-source-files-in-current-dir
          ;; ac-source-words-in-same-mode-buffers
          )))

;; find-file
;; (defun eshell/ef (file) (find-file file))
(defun eshell/ef (&rest args) (eshell/emacs args))

;; ediff
(defun eshell/ed (file1 file2) (ediff file1 file2))

;; clear
(defun eshell/clear ()
  "Clears the shell buffer ala Unix's clear or DOS' cls"
  (interactive)
  ;; the shell prompts are read-only, so clear that for the duration
  (let ((inhibit-read-only t))
    ;; simply delete the region
    (delete-region (point-min) (point-max))))


(defun eshell/info (subject)
  "Read the Info manual on SUBJECT."
  (let ((buf (current-buffer)))
    (Info-directory)
    (let ((node-exists (ignore-errors (Info-menu subject))))
      (if node-exists
          0
        ;; We want to switch back to *eshell* if the requested
        ;; Info manual doesn't exist.
        (switch-to-buffer buf)
        (eshell-print (format "There is no Info manual on %s.\n"
                              subject))
        1))))

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
    (mapc #'find-file (mapcar #'expand-file-name (eshell-flatten-list (reverse args))))))

(defun coldnew/colorfy-eshell-prompt ()
  (interactive)
  (let* ((mpoint)
         (user-string-regexp (concat "^" user-login-name "@" system-name)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward (concat user-string-regexp ".*[$#]") (point-max) t)
        (setq mpoint (point))
        (overlay-put (make-overlay (point-at-bol) mpoint) 'face '(:foreground "dodger blue")))
      (goto-char (point-min))
      (while (re-search-forward user-string-regexp (point-max) t)
        (setq mpoint (point))
        (overlay-put (make-overlay (point-at-bol) mpoint) 'face '(:foreground "green3"))
        ))))

(require* 'helm)
(require 'helm-config)

;; Use predefined configurations for `helm.el'
(setq helm-config t)

;; Enable helm globally
(helm-mode 1)

;; Enable dired binding
(helm-dired-bindings 1)

(require 'helm-projectile)

(require 'helm-etags+)
(require 'ctags-update)
(ctags-update-minor-mode 1)
(defun coldnew/helm-filelist ()
  "Preconfigured `anything' to open files/buffers/bookmarks instantly.
 This is a replacement for `anything-for-files'.
 See `anything-c-filelist-file-name' docstring for usage."
  (interactive)
  (helm-other-buffer
   '(
     helm-c-source-buffers-list
     helm-c-source-recentf
     helm-c-source-ffap-line
     helm-c-source-ffap-guesser
     helm-c-source-bookmarks
     helm-c-source-file-cache
      helm-c-source-projectile-files-list
     helm-c-source-files-in-current-dir
     helm-c-source-locate)
   "*coldnew/filelist*"))


(defun coldnew/helm-occur ()
  "I don't like highlight when goto lines."
  (interactive)
  ;; FIXME: is there more elegent way to make temp face?
  (set (make-local-variable 'face-remapping-alist) '((helm-selection-line nil)))
  (helm-occur))

(defun helm-c-occur-get-line (s e)
  "rewrite `helm-c-occur-get-line' to make it color on line-number."
  (concat (propertize (format "%7d" (line-number-at-pos (1- s))) 'face '((:foreground "red")))
          (format ": %s" (buffer-substring s e))))

(require* 'lusty-explorer)

(add-hook 'lusty-setup-hook
          '(lambda ()
             (define-key lusty-mode-map (kbd "RET") 'lusty-select-current-name)
             ))

(when (featurep 'helm)
  (add-to-list 'helm-completing-read-handlers-alist '(lusty-file-explorer . nil))
  (add-to-list 'helm-completing-read-handlers-alist '(lusty-buffer-explorer . nil)))

(require 'org-install)
(require 'org-table)

(require* 'google-weather)
(require 'org-google-weather)

;;;;;;;; org-mode extensions
(add-to-list 'auto-mode-alist '("\\.txt$" . org-mode))
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-to-list 'auto-mode-alist '("\\.org_archive$" . org-mode))

(setq org-directory "~/Dropbox/Org/")
;; do not show leading stars
(setq org-hide-leading-stars t)
;; do not fold every content
(setq org-startup-folded nil)
;; indent all at startup
(setq org-startup-indented t)
;; Make org-mode compatible with cua-mode
(setq org-CUA-compatible t)
(setq org-src-window-setup 'current-window)
(setq org-src-fontify-natively t)
(setq org-agenda-files (list "~/Dropbox/Org/"))
(setq org-log-done t)
(setq org-pretty-entities t)
(setq org-use-speed-commands t)

(setq org-tag-alist '(
                      (:startgroup . nil) ("Business" . ?b) ("School" . ?s) ("Weintek" . ?w) ("Personal" . ?p) (:endgroup . nil)
                      ))

(add-to-list 'org-structure-template-alist
             '("E" "#+begin_src emacs-lisp\n?\n#+end_src"))

(setq org-default-notes-file (concat org-directory "TODO.org"))
(setq org-capture-templates '(("t" "TODO" entry (file+headline "" "Tasks")
                               "* TODO %?\n %i\n %a")
                              ("f" "FIXME" entry (file+headline "" "Tasks")
                               "* FIXME %?\n %i\n %a")
                              ("w" "Weintek" entry (file+headline "" "Weintek")
                               "* TODO %?\n %i\n %a")
                              ))

(add-hook 'org-capture-mode-hook
          '(lambda ()
             (define-key coldnew/command-mode-map "c" 'org-capture-finalize)
             ))

(setq org-agenda-window-setup 'current-window)

(add-hook 'org-agenda-mode-hook 'hl-line-mode)

(add-hook 'org-agenda-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-g") 'org-agenda-exit)))

(require 'org-crypt)

(setq org-crypt-tag-matcher "encrypt")

(org-crypt-use-before-save-magic)

(setq org-tags-exclude-from-inheritance (quote ("encrypt")))

(setq org-crypt-disable-auto-save 'encrypt)

(add-hook 'org-mode-hook
            '(lambda ()
               (define-key org-mode-map (kbd "C-c C-p") 'org-backward-same-level)
               (define-key org-mode-map (kbd "C-c C-n") 'org-forward-same-level)
               (define-key org-mode-map (kbd "C-c C-b") 'coldnew/org-up-parent)
               (define-key org-mode-map (kbd "C-c C-f") 'coldnew/org-down-children)
               (define-key coldnew/command-mode-map "c" 'org-edit-special)
               ))

(add-hook 'org-mode-hook
          '(lambda ()
             (define-key org-mode-map (kbd "C-\'") nil)
             (define-key org-mode-map (kbd "C-,") nil)
             ))

(add-hook 'org-mode-hook
          '(lambda ()
             (define-key org-mode-map (kbd "C-c b") 'org-metaleft)
             (define-key org-mode-map (kbd "C-c f") 'org-metaright)
             (define-key org-mode-map (kbd "C-c p") 'org-metaup)
             (define-key org-mode-map (kbd "C-c n") 'org-metadown)))

(add-hook 'org-src-mode-hook
          '(lambda ()
             ;;(local-set-key (kbd "C-c C-c") 'org-edit-src-exit)
             (define-key coldnew/command-mode-map "c" 'org-edit-src-exit)
             ))

;;  (add-to-list 'org-mode-hook '(lambda () (linum-mode -1)))

(org-babel-do-load-languages
 'org-babel-load-languages
 '( (perl . t)
    (ruby . t)
    (sh . t)
    (python . t)
    (emacs-lisp . t)
    ))

(when (featurep 'yasnippet)
  (add-hook 'org-mode-hook
            (lambda ()
              (org-set-local 'yas/trigger-key [tab])
              (define-key yas/keymap [tab] 'yas/next-field-group))))

;; Make windmove work in org-mode:
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

(defun org-archive-done-tasks ()
  "Make all DONE subtree to archive."
  (interactive)
  (org-map-entries 'org-archive-subtree "/DONE" 'file))

(defun coldnew/org-up-parent ()
  "Move to the parent of current point. If current is the parent heading,
          move to the previous parent heading."
  (interactive)
  (if (not (org-on-heading-p))
      (outline-back-to-heading)
    (let* ((level (funcall outline-level))
           (point-to-move-to
            (save-excursion
              (outline-previous-visible-heading 1)
              (while (>= (funcall outline-level) level)
                (outline-previous-visible-heading 1))
              (point))))
      (if point-to-move-to
          (goto-char point-to-move-to))))
  (org-cycle))

(defun coldnew/org-down-children ()
  "Move to children of current heading. If current heading only has subtree,
        expand the subtree."
  (interactive)
  (outline-back-to-heading)
  (show-children)
  (let* ((level (funcall outline-level))
         (point-to-move-to
          (save-excursion
            (outline-next-visible-heading 1)
            (if (<= (funcall outline-level) level)
                nil
                      (point)))))
            (if point-to-move-to
                (goto-char point-to-move-to)
              (show-subtree))))

(require* 'smex)

(smex-initialize)

(setq smex-save-file (concat emacs-cache-dir "smex.dat"))

(require 'woman)

(setq woman-cache-filename (concat emacs-cache-dir "woman.cache"))
(setq woman-use-topic-at-point nil)
;; Colorful fonts
(setq woman-fontify t)
(setq woman-fill-column 100)

(require 'info)

(define-key minibuffer-local-map (kbd "M-l") 'backward-kill-word)
(define-key minibuffer-local-map (kbd "C-w") 'kill-word)
(define-key minibuffer-local-map (kbd "C-u") '(lambda() (interactive) (kill-line 0)))
(define-key minibuffer-local-map (kbd "M-p") 'previous-history-element)
(define-key minibuffer-local-map (kbd "M-n") 'next-history-element)
(define-key minibuffer-local-map (kbd "C-g") 'minibuffer-keyboard-quit)
(define-key minibuffer-local-map (kbd "C-r") 'resolve-sym-link)

;;;; ---------------------------------------------------------------------------
;;;; initial setting
;;;; ---------------------------------------------------------------------------
(setq enable-recursive-minibuffers     t )
(setq max-mini-window-height         .25 ) ; 2 lines high
(setq minibuffer-electric-default-mode t )


;;;; ---------------------------------------------------------------------------
;;;; Hooks
;;;; ---------------------------------------------------------------------------

;; Abort the minibuffer when using the mouse
(add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)

;;;; ---------------------------------------------------------------------------
;;;; Functions
;;;; ---------------------------------------------------------------------------

(defun stop-using-minibuffer ()
  "kill the minibuffer"
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))

;;;; ---------------------------------------------------------------------------
;;;; Commands
;;;; ---------------------------------------------------------------------------
(defun resolve-sym-link ()
  "Replace the string at the point with the true path."
  (interactive)
  (beginning-of-line)
  (let* ((file (buffer-substring (point)
                                 (save-excursion (end-of-line) (point))))
         (file-dir (file-name-directory file))
         (file-true-dir (file-truename file-dir))
         (file-name (file-name-nondirectory file)))
    (delete-region (point) (save-excursion (end-of-line) (point)))
    (insert (concat file-true-dir file-name))))

(require 'ido)

(add-hook 'ido-setup-hook
          '(lambda ()
             (define-key ido-completion-map (kbd "C-f") 'ido-next-match)
             (define-key ido-completion-map (kbd "C-b") 'ido-prev-match)
             ))

(setq-default yas/snippet-dirs emacs-snippets-dir)

(require 'yasnippet)
(require 'dropdown-list)

(yas/initialize)

(yas/load-directory emacs-snippets-dir)

(setq yas/prompt-functions '(yas/dropdown-prompt yas/ido-prompt yas/completing-prompt))

(add-hook 'after-save-hook 'coldnew/update-yasnippets-on-save)

(defun yas/dir ()
  (file-name-directory (buffer-file-name)))
(defun yas/file ()
  (file-name-nondirectory (buffer-file-name)))
(defun yas/file-sans ()
  (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))
(defun yas/file-ext ()
  (file-name-extension (file-name-nondirectory (buffer-file-name))))
(defun yas/file-sans-upcase ()
  (upcase (yas/file-sans)))
(defun yas/year ()
  (format-time-string "%Y"))
(defun yas/user-name ()
  (insert user-full-name))
(defun yas/login-name ()
  (insert user-login-name))
(defun yas/user-email ()
  (insert user-mail-address))
(defun yas/user-nickname ()
  (insert user-nickname))
(defun coldnew/update-yasnippets-on-save ()
  "automatic reloadinf of ghanged snippets"
  (when (string-match "/snippets/" buffer-file-name)
    (yas/load-snippet-dirs)))

(require 'ibuffer)
(require 'ibuf-ext)

;;;; Settings
(setq ibuffer-always-compile-formats         t )
(setq ibuffer-default-shrink-to-minimum-size t )
(setq ibuffer-expert                         t )
(setq ibuffer-show-empty-filter-groups     nil )
(setq ibuffer-use-other-window             nil )
(setq ibuffer-always-show-last-buffer      nil )

(require 'ibuffer-git)
(setq ibuffer-formats
      '((mark modified read-only git-status-mini " "
              (name 23 23 :left :elide)
              " "
              (size-h 9 -1 :right)
              "  "
              (mode 16 16 :left :elide)
              " "
              (git-status 8 8 :left)
              "    "
              ;;              (eproject 16 16 :left :elide)
              ;;              "      "
              filename-and-process)))

;;;; buffer-list
(setq ibuffer-saved-filter-groups
      '(("default"
         ("*Buffer*" (or
                      (name . "^TAGS\\(<[0-9]+>\\)?$")
                      (name . "^\\**Loading Log\\*$")
                      (name . "^\\*coldnew/filelist\\*$")
                      (name . "^\\*Backtrace\\*$")
                      (name . "^\\*Buffer List\\*$")
                      (name . "^\\*CEDET Global\\*$")
                      (name . "^\\*Compile-Log\\*$")
                      (name . "^\\*Completions\\*$")
                      (name . "^\\*EGG:*")
                      (name . "^\\*Kill Ring\\*$")
                      (name . "^\\*Occur\\*$")
                      (name . "^\\*Customize*")
                      (name . "^\\*Process List\\*$")
                      (name . "^\\*Shell Command Output\\*")
                      (name . "^\\*Warnings\\*$")
                      (name . "^\\*compilation\\*$")
                      (name . "^\\*el-get*")
                      (name . "^\\*grep\\*$")
                      (name . "^\\*gud\\*$")
                      (name . "^\\*ielm\\*")
                      (name . "^\\*im.bitlbee.org\\*$")
                      (name . "^\\*scratch\\*$")
                      (name . "^\\*tramp")
                      (name . "^\\*wclock\\*$")
                      (name . "^ipa*")
                      (name . "^loaddefs.el$")
                      (name . "^\\*Messages\\*$")
                      (name . "^\\*WoMan-Log\\*$")
                      ))
         ("Version Control" (or (mode . svn-status-mode)
                                (mode . svn-log-edit-mode)
                                (name . "^\\*svn*\\*")
                                (name . "^\\*vc*\\*$")
                                (name . "^\\*Annotate")
                                (name . "^\\*git-*")
                                (name . "^\\*cvs*")
                                (name . "^\\*vc-*")
                                (mode . egg-status-buffer-mode)
                                (mode . egg-log-buffer-mode)
                                (mode . egg-commit-buffer-mode)))
         ("Help" (or (mode . woman-mode)
                     (mode . man-mode)
                     (mode . info-mode)
                     (mode . help-mode)
                     (name . "\\*Help\\*$")
                     (name . "\\*info\\*$")))
         ("Dired" (or (mode . dired-mode)
                      (mode . nav-mode)))
         ("IRC"   (or (mode . erc-mode)
                      (mode . rcirc-mode)))
         ("Jabber" (or (mode . jabber-roster-mode)
                       (mode . jabber-chat-mode)))
         ("Terminal" (or (mode . eshell-mode)
                         (mode . term-mode)
                         (mode . inferior-python-mode)
                         (mode . eshell-mode)
                         (mode . comint-mode)
                         (name . "\\*scheme\\*$")))
         ("Config" (name . "*.conf$"))
         ("Text" (or (mode . text-mode)
                     (name . "*.txt$")))
         ("w3m"   (or (mode . w3m-mode)
                      (name . "^\\*w3m*")))
         ("Org"   (mode . org-mode))
         ("LaTEX" (or (mode . latex-mode)
                      (name . "*.tex$")))
         ("Verilog" (mode . verilog-mode))
         ("Web Develop" (or (mode . html-mode)
                            (mode . css-mode)))
         ("Shell Script" (or (mode . shell-script-mode)
                             (mode . shell-mode)
                             (mode . sh-mode)
                             (mode . ruby-mode)))
         ("Perl"  (or (mode . cperl-mode)
                      (mode . perl-mode)))
         ("Python" (or (mode . python-mode)
                       (mode . ipython-mode)))
         ("Octave" (or (mode . octave-mode)
                       (mode . inferior-octave-mode)))
         ("Scala" (or (mode . scala-mode)
                      (name . "\\*inferior-scala\\*$")))
         ("Diff" (mode . diff-mode))
         ;;      ("Project" (mode . qmake-mode))
         ("C++ . C#" (or (mode . c++-mode)
                         (mode . csharpmode)))
         ("C"          (mode . c-mode))
         ("Object-C"   (mode . objc-mode))
         ("Snippet" (or (mode . snippet-mode)
                        (name . "*.yas$")))
         ("newLisp"  (mode . newlisp-mode))
         ("Common Lisp"   (mode . slime-mode))
         ("Scheme"  (or (mode . scheme-mode)
                        (mode . gambit-mode)))
         ("Clojure" (or (mode . clojure-mode)
                        (name . "\\*slime-repl clojure\\*")))
         ("Emacs recipes" (name . "*.rcp$"))
         ("Emacs" (or (mode . emacs-lisp-mode)
                      (mode . lisp-interaction-mode)
                      ))
         )))

(setq ibuffer-never-show-predicates
      (list
       "^\\*Buffer List\\*$"
       "^\\*CEDET Global\\*$"
       "^\\*MiniBuf-*"
       "^\\*Egg:Select Action\\*$"
       "^\\*Ido Completions\\*$"
       "^\\*SPEEDBAR\\*$"
       "^\\*nav\\*$"
       "^\\*swank\\*$"
       "^\\*slime-events\\*$"
       "^\\*RE-Builder\\*$"
       "^\\*pomodoro\\*$"
       "^\\*Project Buffers\\*$"
       "^eproject$"
       "\\*fsm-debug\\*$"
       ;; "^"
       "^\\*.*\\(-preprocessed\\)\\>\\*"
       "^\\*ORG.*\\*"
       "^\\*ac-mode-*"
       ".loaddefs.el$"
       "^loaddefs.el$"
       "^\\*magit*"
       "\\*GTAGS SELECT\\**"
       "\\*Symref*"
       "\\*cscope\\*"
       "\\*helm*"
       ))

;;;; Advice
;; Reverse group list
(defadvice ibuffer-generate-filter-groups (after reverse-ibuffer-groups () activate)
  (setq ad-return-value (nreverse ad-return-value)))

;; Switching to ibuffer puts the cursor on the most recent buffer
(defadvice ibuffer (around ibuffer-point-to-most-recent activate)
  "Open ibuffer with cursor pointed to most recent buffer name"
  (let ((recent-buffer-name (buffer-name)))
    ad-do-it
    (ibuffer-jump-to-buffer recent-buffer-name)))

;; Kill ibuffer after quit
(defadvice ibuffer-quit (after kill-ibuffer activate)
  "Kill the ibuffer buffer on exit."
  (kill-buffer "*Ibuffer*"))

(define-key ibuffer-mode-map (kbd "C-x C-f") 'lusty-file-explorer)

;; Use human readable Size column instead of original one
(define-ibuffer-column size-h
  (:name "Size" :inline t)
  (cond
   ((> (buffer-size) 1000) (format "%7.3fK" (/ (buffer-size) 1000.0)))
   ((> (buffer-size) 1000000) (format "%7.3fM" (/ (buffer-size) 1000000.0)))
   (t (format "%8dB" (buffer-size)))))

(add-hook 'ibuffer-mode-hook 'hl-line-mode)

(add-hook 'ibuffer-mode-hook '(lambda () (ibuffer-switch-to-saved-filter-groups "default")))

(add-hook 'ibuffer-mode-hook 'ibuffer-do-sort-by-filename/process)

(require 'winner)

;; do not use default keybindings
(setq winner-dont-bind-my-keys t)
;; Enable winner-mode
(winner-mode t)

(require 'speedbar)
(setq speedbar-use-images nil)
(require 'sr-speedbar)
(setq sr-speedbar-right-side nil)
(setq sr-speedbar-refresh-turn-on t)

(require 'shell-pop)

(shell-pop-set-internal-mode "eshell")
(shell-pop-set-internal-mode-shell emacs-default-shell)
(shell-pop-set-window-height emacs-popup-shell-window-height)
(shell-pop-set-window-position emacs-popup-shell-window-position)

(defadvice shell-pop (before kill-dead-term activate)
  "If there is a stopped ansi-term, kill it and create a new one."
  (let ((running-p (term-check-proc (buffer-name)))
        (term-p (string= "term-mode" major-mode)))
    (if term-p
        (when (not running-p)
          (kill-buffer (buffer-name))
          (shell-pop-out)))))

(require 'multi-term)
(setq multi-term-program emacs-default-shell)

(require 'term)
(require 'ansi-color)

(setq term-default-bg-color nil)
(setq term-default-fg-color nil)

;; reset ansi-color
(setq-default ansi-color-names-vector
              (vector (frame-parameter nil 'background-color)
                      "#0B0B0E" "#CA3839" "#8ae234" "#edd400"
                      "#729fcf" "#ad7fa8" "cyan3"   "#DCDCDC"))
(setq ansi-term-color-vector ansi-color-names-vector)
;; (setq ansi-color-map (ansi-color-make-color-map))

;;;; keybindings
;; (define-key term-raw-map (kbd "<f4>") 'shell-pop)
(define-key term-raw-map (kbd "M-x") 'execute-extended-command)
(define-key term-raw-map (kbd "C-g") 'term-interrupt-subjob)
(define-key term-raw-map (kbd "C-n") 'term-send-down)
(define-key term-raw-map (kbd "C-p") 'term-send-up)
(define-key term-raw-map (kbd "<enter>") 'term-send-input)
(define-key term-raw-map (kbd "C-o") 'coldnew/execute-in-command-mode)

(require 'comint)
;; Do not show password in comint-mode
(setq comint-output-filter-functions  '(comint-watch-for-password-prompt))
(setq comint-password-prompt-regexp
      "\\(\\([Oo]ld \\|[Nn]ew \\|^\\)[Pp]assword\\|Enter password\\):\\s *\\'")

;;;; Keybindings
(define-key comint-mode-map (kbd "C-g") 'comint-interrupt-subjob)

(require 'undo-tree)

(global-undo-tree-mode)

(define-key undo-tree-visualizer-map (kbd "C-g") 'undo-tree-visualizer-quit)

(require 'hungry-delete)
(add-hook 'coldnew-editor-hook 'turn-on-hungry-delete-mode)

(require 'cua-base)
(require 'cua-rect)
;; don't add C-x, C-c, C-v
(setq cua-enable-cua-keys nil)
(setq cua-rectangle-mark-key (kbd "C-c RET"))
;; Enable cua-mode
(cua-mode t)

(require 'paredit)

(defun paredit-blink-paren-match (another-line-p)
  "redefine this function, i don't like paredit to blikn math paren")

(defadvice paredit-backward-delete (around paredit-backward-delete activate)
  "Intergrated paredit-backward-delete with hungry-delete."
  ad-do-it
  (when (featurep 'hungry-delete)
    (if (eq (char-before) ?\s)
        (hungry-delete-backward))))

(defadvice paredit-forward-delete (around paredit-forward-delete activate)
  "Intergrated paredit-forward-delete with hungry-delete."
  ad-do-it
  (when (featurep 'hungry-delete)
    (if (eolp)
        (hungry-delete-forward))))

(require 'projectile)
(projectile-global-mode)
(setq projectile-enable-caching t)

(require 'hideshow)
(require* 'hideshowvis)

;;; enable following mode to use hideshow
(dolist (hook (list 'emacs-lisp-mode-hook
                    'c++-mode-hook
                    'c-mode-hook))
  (add-hook hook 'hideshowvis-enable))

(require 'slime)
(require 'ac-slime)
;; Save REPL history to emacs-cache-dir
(setq slime-repl-history-file (concat emacs-cache-dir "slime-hist.dat"))

;; REPL history size set to 300
(setq slime-repl-history-size 300)

;; Use global programming mode
(add-hook 'slime-repl-mode-hook 'programming-mode)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)

(defun ielm-auto-complete ()
  "Enables `auto-complete' support in \\[ielm]."
  (setq ac-sources '(ac-source-functions
                     ac-source-variables
                     ac-source-features
                     ac-source-symbols
                     ac-source-words-in-same-mode-buffers))
  (add-to-list 'ac-modes 'inferior-emacs-lisp-mode)
  (auto-complete-mode 1))
(add-hook 'ielm-mode-hook 'ielm-auto-complete)

;; FIXME: move to other place
(defun notify-compilation-result(buffer msg)
  "Notify that the compilation is finished,
   close the *compilation* buffer if the compilation is successful,
   and set the focus back to Emacs frame"
  (if (string-match "^finished" msg)
      (progn
        (delete-windows-on buffer)
        (message (propertize "COMPILATION SUCCESSFUL :-) " 'face 'font-lock-warning-face))
        ;;       (tooltip-show "\n Compilation Successful :-) \n ")
        )
    (tooltip-show "\n Compilation Failed :-( \n "))
  ;; FIXME: When I use dualscreen, following functiokn will make error,
  ;;        after compilation, current frame will jump to another DISPLAY
  ;;  (setq current-frame (car (car (cdr (current-frame-configuration)))))
  ;; (select-frame-set-input-focus current-frame)
  )

(add-to-list 'compilation-finish-functions 'notify-compilation-result)
;; Make compilaction buffer always scrolls to follow output as it comes in.
(setq compilation-scroll-output t)

;; Auto jump to the first error.
(setq compilation-auto-jump-to-first-error t)

(require* 'cmake-mode)

(add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode))
(add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))

(require 'epa-file)

;; use local gpg program instaed of system one
;; only work under linux
(cond
 ;; (linux-32bit-p (setq epg-gpg-program (concat emacs-bin-dir "gpg-x86")))
 (linux-64bit-p (setq epg-gpg-program (concat emacs-bin-dir "gpg")))
 )

(setenv "GPG_AGENT_INFO" nil)

(epa-file-enable)

;; Control whether or not to pop up the key selection dialog.
(setq epa-file-select-keys 0)

;; Cache passphrase for symmetric encryption.
(setq epa-file-cache-passphrase-for-symmetric-encryption t)

(defvar coldnew-global-keymaps-alist nil
  "Association list of keymap variables.
Entries have the form (MODE . KEYMAP), where KEYMAP
is the variable containing the keymap for MODE.")

(defvar coldnew-local-keymaps-alist nil
  "Association list of keymap variables that must be
reinitialized in each buffer. Entries have the form
\(MODE . KEYMAP), where KEYMAP is the variable containing
the keymap for MODE.")

(defmacro coldnew/define-state (state doc &rest body)
  "Define an coldnew-editor mode MODE.
DOC is a general description and shows up in all docstrings;
the first line of the string should be the full name of the state.
Then follows one or more optional keywords:

:tag STRING             Mode line indicator.
:message STRING         Echo area message when changing to STATE.
:cursor SPEC            Cursor to use in STATE.
:entry-hook LIST        Hooks run when changing to STATE.
:exit-hook LIST         Hooks run when changing from STATE.
:suppress-keymap FLAG   If FLAG is non-nil, disabling bindings to
                        `self-insert-command'."
  (declare (indent defun)
           (debug (&define name
                           [&optional stringp]
                           [&rest [keywordp sexp]]
                           def-body)))
  (let* (
         (name (intern (format "%s" state)))
         (doc (if (or (null doc) (string= doc "")) ""
                (format "\n%s" doc)))
         (toggle (intern (format "coldnew-%s" state)))
         (major-mode (intern (format "%s-modes" toggle)))
         (minor-mode (intern (format "%s-minor-mode" toggle)))
         (keymap (intern (format "%s-map" toggle)))
         (local (intern (format "%s-local-minor-mode" toggle)))
         (local-keymap (intern (format "%s-local-map" toggle)))
         (tag (intern (format "%s-tag" toggle)))
         (message (intern (format "%s-message" toggle)))
         (cursor (intern (format "%s-cursor" toggle)))
         (entry-hook (intern (format "%s-entry-hook" toggle)))
         (exit-hook (intern (format "%s-exit-hook" toggle)))
         (predicate (intern (format "%s-p" toggle)))
         arg cursor-value enable entry-hook-value exit-hook-value
         input-method key message-value suppress-keymap tag-value)
    ;; collect keywords
    (while (keywordp (car-safe body))
      (setq key (pop body)
            arg (pop body))
      (cond
       ((eq key :tag)
        (setq tag-value arg))
       ((eq key :message)
        (setq message-value arg))
       ((eq key :cursor)
        (setq cursor-value arg))
       ((eq key :entry-hook)
        (setq entry-hook-value arg)
        (unless (listp entry-hook-value)
          (setq entry-hook-value (list entry-hook-value))))
       ((eq key :exit-hook)
        (setq exit-hook-value arg)
        (unless (listp exit-hook-value)
          (setq exit-hook-value (list entry-hook-value))))
       ;; ((eq key :enable)
       ;;  (setq enable arg))
       ((eq key :input-method)
        (setq input-method arg))
       ((eq key :suppress-keymap)
        (setq suppress-keymap arg))))
    ;; macro expansion
    `(progn
       (defvar ,minor-mode nil
         ,(format "Non-nil if %s is enabled.
                            Use the command `%s' to change this variable." name toggle))
       (defvar ,major-mode nil
         ,(format "Modes that should come up in %s." name))
       ;; (defun ,predicate (&optional state)
       ;;        ,(format "Whether the current `coldnew-editor' state is `%s'.)" name)
       ;;        )
       )
    ))

;;;; lisp common setting
  (defun coldnew-lisp-common-setting ()
    "coldnew's common setting for lisp-like mode"
    ;; Use coldnew's editor mode
    (coldnew-editor-mode)
    ;; Use Greek character lambda insteda of string
    (turn-on-pretty-lambda-mode)
    (require 'rainbow-delimiters)
;;    (rainbow-delimiters-mode)
    )


  ;;;; cc-mode common setting
  (defun coldnew-cc-mode-common-setting ()
    "coldnew's common setting for cc-mode"
    ;; Use coldnew's editor mode
    (coldnew-editor-mode)
    ;; enable doxygen
    ;;  (doxymacs-mode t)
    ;;  (doxymacs-font-lock)

    ;; hide-if-def mode
    (hide-ifdef-mode)
    ;; use shadow
    (setq hide-ifdef-shadow t)

    ;; gtags
    ;;  (gtags-mode t)
    ;;  (if-not (string-match "/usr/src/linux/" (expand-file-name default-directory))
    ;;        (gtags-create-or-update))

    ;; keybindings
    (local-set-key (kbd "C-x C-o") 'ff-find-other-file)
    (local-set-key (kbd "C-x C-o") 'ff-find-other-file)
    (local-set-key (kbd "<f9>") 'smarter-compile)
    )

(defvar coldnew-editor-hook nil
  "Hooks for coldnew-editor-mode.")

(defvar coldnew-editor-state "Emacs"
  "default editor mode is Emacs-mode")

(defvar coldnew-editor-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for coldnew-editor-mode.")

(define-minor-mode coldnew-editor-mode
  "Minor mode for coldnew's editor."
  :init-value t
  :lighter " coldnew-editor"
  :keymap coldnew-editor-map
  (run-hooks 'coldnew-editor-hook))

(defun coldnew/disable-mode-according-state ()
  (cond
   ((string= "View"  coldnew-editor-state) (view-mode -1))
   ((string= "Command"  coldnew-editor-state) (coldnew/command-mode -1))
   ))

(defun coldnew/switch-to-emacs-mode ()
  (interactive)
  ;; disable other state according mode
  (coldnew/disable-mode-according-state)
  (setq coldnew-editor-state "Emacs"))

(defun coldnew/switch-to-command-mode ()
  (interactive)
  ;; disable other state according mode
  (coldnew/disable-mode-according-state)
  (setq coldnew-editor-state "Command")
  (coldnew/command-mode 1))

(defvar coldnew/command-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    ;; simulate vim keys
    (define-key map "i" 'coldnew/switch-to-emacs-mode)
    (define-key map "%" 'match-paren)
    (define-key map "." 'repeat)
    (define-key map "*" 'vjo-forward-current-word-keep-offset)
    (define-key map "#" 'vjo-backward-current-word-keep-offset)
    ;; my keymap
    (define-key map "C" 'capitalize-word-backward)
    (define-key map "c" 'capitalize-word-backward)
    (define-key map "L" 'downcase-word-backward)
    (define-key map "U" 'upcase-word-backward)
    (define-key map "Z" 'zap-up-to-char-backward)
    (define-key map "e" 'ecb-toggle-ecb-windows)
    ;; (define-key map "a" 'backward-sentence)
    ;; (define-key map "e" 'forward-sentence)
    (define-key map "g" 'linum-ace-jump)
    (define-key map "o" 'org-ido-switchb)
    (define-key map "q" 'quoted-insert)
    (define-key map "r" 'org-capture)
    (define-key map "w" 'kill-region)
    (define-key map "n" 'windmove-down)
    (define-key map "p" 'windmove-up)
    (define-key map "f" 'windmove-right)
    (define-key map "b" 'windmove-left)
    (define-key map "\C-m" 'bm-toggle)
    (define-key map "\C-n" 'bm-next)
    (define-key map "\C-p" 'bm-previous)
    (define-key map "x" 'helm-M-x)
    (define-key map "y" 'yank)
    (define-key map "z" 'zap-up-to-char)
    ;; FIXME: I think this must change to vc-next-action
    (define-key map "v" 'egg-next-action)
    (define-key map "h" 'coldnew/helm-filelist)
    (define-key map (kbd "<SPC>") 'ace-jump-mode)
    (define-key map (kbd "(")  '(lambda () (interactive) (delete-between-pair ?\()))
    (define-key map (kbd "\"") '(lambda () (interactive) (delete-between-pair ?\")))
    (define-key map (kbd "[")  '(lambda () (interactive) (delete-between-pair ?\[)))
    (define-key map (kbd "{")  '(lambda () (interactive) (delete-between-pair ?\{)))
    (define-key map (kbd "\'") '(lambda () (interactive) (delete-between-pair ?\')))
    ;; elscreen
    ;; TODO: move to coldnew-elscreen
    (define-key map "t0" 'elscreen-jump-0)
    (define-key map "t9" 'elscreen-jump-9)
    (define-key map "tc" 'elscreen-create)
    (define-key map "td" 'elscreen-kill)
    (define-key map "tn" 'elscreen-next)
    (define-key map "tp" 'elscreen-previous)
    map)
  "Keymap for coldnew-editor-Mode.")

(define-minor-mode coldnew/command-mode
  "Minor mode like vi's normal mode"
  :init-value nil
  :global t
  :lighter " "
  :keymap coldnew/command-mode-map
  (if coldnew/command-mode
      (progn
        ;; use key-chord
        (setq input-method-function 'key-chord-input-method))
    (progn
      ;; disable keychord
      (setq input-method-function nil))))

(add-hook 'post-command-hook 'coldnew/set-mode-according-state)

(defun coldnew/toggle-state ()
  (interactive)
  (cond
   ((string= "Command" coldnew-editor-state) (coldnew/switch-to-emacs-mode))
   ((string= "Emacs"   coldnew-editor-state) (coldnew/switch-to-command-mode)) )
  )

(defvar coldnew/buffer-state-alist
  '((eshell-mode . "Emacs")
    (term-mode   . "Emacs")
    (ibuffer-mode . "Emacs")
    ))

(defun coldnew/set-mode-according-state ()
  (let* ((mode major-mode)
         (state (cdr-safe (assoc mode coldnew/buffer-state-alist))))
    (if (minibufferp) (setq state "Emacs"))
    (cond
     ((string= "Command" state) (coldnew/switch-to-command-mode))
     ((string= "Emacs"   state) (coldnew/switch-to-emacs-mode)) )
    ))

;; FIXME: change to my own code one day
(defun coldnew/evil-delay (condition form hook &optional append local name)
  "Execute FORM when CONDITION becomes true, checking with HOOK.
NAME specifies the name of the entry added to HOOK. If APPEND is
non-nil, the entry is appended to the hook. If LOCAL is non-nil,
the buffer-local value of HOOK is modified."
  (if (and (not (booleanp condition)) (eval condition))
      (eval form)
    (let* ((name (or name (format "evil-delay-form-in-%s" hook)))
           (fun (make-symbol name))
           (condition (or condition t)))
      (fset fun `(lambda (&rest args)
                   (when ,condition
                     (remove-hook ',hook #',fun ',local)
                     ,form)))
      (put fun 'permanent-local-hook t)
      (add-hook hook fun append local))))

(defun coldnew/execute-in-command-mode ()
  "Execute the next command in Command mode."
  (interactive)
  (coldnew/evil-delay '(not (eq this-command #'coldnew/execute-in-command-mode))
                      `(progn (coldnew/switch-to-emacs-mode))
                      'post-command-hook)
  (coldnew/switch-to-command-mode)
  )

(global-set-key (kbd "C-o") 'coldnew/execute-in-command-mode)
(global-set-key (kbd "C-j") 'linum-ace-jump)

;;;; ---------------------------------------------------------------------------
;;;; Initial Editor Setting
;;;; ---------------------------------------------------------------------------
(setq indent-tabs-mode nil )          ; don't use tabs to indent
(setq tab-width          8 )          ; default tab-width is 8
(setq line-spacing       4 )          ; Additional space between lines
(setq fill-column      100 ) ; column beyond which automatic line-wrapping shold happen
(setq kill-ring-max    300 ) ; Maximum lenght of kill-ring
(setq require-final-newline  t ) ; Auto add a newline at the end of line
(setq next-line-add-newlines t ) ;
(setq shift-select-mode      t ) ; Enable shift-select mode

;; Enable global font-lock
(global-font-lock-mode t)
;; Revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)
;; Show matching parentheses all the time
(show-paren-mode t)
;; Enable auto-complete-mode
(global-auto-complete-mode t)
;; Enable delete-selection-mode
(delete-selection-mode t)

;; After save buffer, indent whole file.
     (add-hook 'coldnew-editor-hook 'indent-file-after-save)
     ;; Before save buffer, cleanup whitespace
     (add-hook 'coldnew-editor-hook 'cleanup-whitespace-before-save)
     ;; Enable line-number
     (require 'linum)
    ;; (add-hook 'coldnew-editor-hook 'linum-mode)
     ;; use electric-indent-mode
     (add-hook 'coldnew-editor-hook 'electric-indent-mode)
     ;; highlight special keywords like TODO, BUF
     (add-hook 'coldnew-editor-hook 'highlight-additional-keywords)
   ;; highlight fontify numbers and constant
;;    (add-hook 'coldnew-editor-hook 'highlight-fontify-numbers)
    ;; highlight escape char in string
    (add-hook 'coldnew-editor-hook 'highlight-escape-char)
  ;;   ;; Add spaces between Chinese and English character.
    (add-hook 'before-save-hook 'insert-space-between-english-chinese)

;;   ;; Color nested parentheses, brackets, and braces according to their dept
;;   (require 'rainbow-delimiters)
;;   (add-hook 'coldnew-editor-hook 'rainbow-delimiters-mode)

(defun indent-file-after-save ()
  "Indent whole file after saved."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            '(lambda ()
               (indent-region (point-min) (point-max) nil)
               (save-buffer))))

(defun cleanup-whitespace-before-save ()
  "Cleanup whitespaces before save to a file."
  (make-local-variable 'before-save-hook)
  (add-hook 'before-save-hook
            '(lambda ()
               (whitespace-cleanup)
               (delete-trailing-whitespace))))

(defun highlight-additional-keywords ()
  "Highlight additional keywords."
  (font-lock-add-keywords nil '(("\\<\\(FIXME\\|BUG\\|HACK\\):" 1 font-lock-warning-face t)))
  (font-lock-add-keywords nil '(("\\<\\(NOTE\\):" 1 'org-level-2 t)))
  (font-lock-add-keywords nil '(("\\<\\(TODO\\):" 1 'org-todo t)))
  (font-lock-add-keywords nil '(("\\<\\(DONE\\):" 1 'org-done t)))
  )

(defun highlight-fontify-numbers ()
  "Use this function as a hook to fontify numbers as constant"
  (font-lock-add-keywords nil
                          '(
                            ;; hexadecimal
                            ("\\b\\(0x[0-9a-fA-F]+\\)" 1 font-lock-constant-face)
                            ;; float
                            ("\\b\\([+-]?[0-9]+\\.[0-9]+\\)" 1 font-lock-constant-face)
                            ;; int
                            ("[\`^(\{\[,\+\-\*/\%=\s-]\\(-?[0-9]+U?L?L?\\)" 1 font-lock-constant-face)
                            )))

(defun highlight-escape-char ()
  "Use this function as a hook to fontify escape char."
  (font-lock-add-keywords nil
                          '(
                            ("\\\\\\(?:[abfnrtv'\"?\\0]\\|x[a-fA-F]\\{2\\}\\|[0-7]\\{3\\}\\)"
                             0 'font-lock-escape-char-face prepend)
                            )))

(defun insert-space-between-english-chinese ()
  "Insert a space between English words and Chinese charactors"
  (save-excursion
    (goto-char (point-min))
    (while (or (re-search-forward "\\(\\cc\\)\\([a-zA-Z0-9]\\)" nil t)
               (re-search-forward "\\([a-zA-Z0-9]\\)\\(\\cc\\)" nil t))
      (replace-match "\\1 \\2" nil nil))
    (goto-char (point-min))
    (while (or (re-search-forward "\\([。，！？；：「」（）、]\\) \\([a-zA-Z0-9]\\)" nil t)
               (re-search-forward "\\([a-zA-Z0-9]\\) \\([。，！？；：「」（）、]\\)" nil t))
      (replace-match "\\1\\2" nil nil))))

(require 'semantic)

;; Enable Semantic features
(semantic-mode 1)

;; Maintain tag database
(global-semanticdb-minor-mode 1)
;; Reparse buffer when idle
(global-semantic-idle-scheduler-mode 1)
;; Show completions when idle
(global-semantic-idle-completions-mode 1)
;; Provide `switch-to-buffer'-like keybinding for tag names.
(global-semantic-mru-bookmark-mode 1)
;; Show summary of tag at point
(global-semantic-idle-summary-mode 1)
;;;; Disable
;; Highlight the current tag.
(global-semantic-highlight-func-mode -1)
;; Show current fun in header line
(global-semantic-stickyfunc-mode -1)
;; Additional tag decorations
(global-semantic-decoration-mode -1)

;;;; Enable support for GNU Global
(semanticdb-enable-gnu-global-databases 'c-mode)
(semanticdb-enable-gnu-global-databases 'c++-mode)

(setq semanticdb-default-save-directory (concat emacs-cache-dir "semanticdb"))
(set-default 'semantic-case-fold t)

(require* 'ecb)

(setq ecb-tip-of-the-day nil)

(setq ecb-auto-compatibility-check nil)

(defun ccc-toggle-ecb-mode ()
  "Toggle ecb-minor-mode and resize window accordingly"
  (interactive)
  (if ecb-minor-mode
      (progn
        (let ((w (frame-width ecb-frame)))
          (message "%i" w)
          )
        (ecb-minor-mode)
        )
    (progn
      (message "%s" "turning on, make frame wider")
      (ecb-minor-mode)
      )
    ))

(setq bookmark-default-file emacs-bookmark-file)

(require* 'bm)

(require* 'xcscope)
(require* 'xcscope+)
(setq cscope-do-not-update-database t)

(auto-compression-mode 1)

(add-to-list 'auto-mode-alist '("\\.el$" . emacs-lisp-mode))

(require 'eldoc)
(require 'eldoc-extension)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

(when (featurep 'paredit)
  (eldoc-add-command 'paredit-backward-delete 'paredit-close-round))

(require 'highlight-cl)
(add-hook 'emacs-lisp-mode-hook 'highlight-cl-add-font-lock-keywords)

(add-hook 'emacs-lisp-mode-hook 'auto-indent-minor-mode)

(add-hook 'emacs-lisp-mode-hook 'coldnew-lisp-common-setting)

(add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
(defun ac-emacs-lisp-mode-setup ()
    "auto-complete settings for emacs-lisp-mode"
    (setq ac-sources
          '(ac-source-symbols
            ac-source-variables
            ac-source-functions
            ac-source-features
            ac-source-filename
            ac-source-files-in-current-dir
            ac-source-words-in-same-mode-buffers
            )))

(defun remove-elc-when-visit ()
  "After visit elisp file, remove .elc extension file."
  (make-local-variable 'find-file-hook)
  (add-hook 'find-file-hook
            (lambda ()
              (if (and (file-exists-p (concat buffer-file-name "c"))
                       (file-writable-p (concat buffer-file-name "c")))
                  (delete-file (concat buffer-file-name "c"))))))

(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (define-key emacs-lisp-mode-map (kbd "M-i r") '(lambda () (interactive) (insert "require") (yas/expand)))
             ))

(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (define-key emacs-lisp-mode-map (kbd "C-c C-b") 'backward-sexp)
             ))

(add-to-list 'auto-mode-alist '("\\.c$" . c-mode))
(add-to-list 'auto-mode-alist '("\\.h$" . c-mode))

(add-hook 'c-mode-hook
          '(lambda ()
             (c-set-style "linux")))

(add-hook 'c-mode-hook
        '(lambda ()
           (require 'c-eldoc)
           (setq c-eldoc-includes "`pkg-config gtk+-3.0 opencv --cflags --libs` -I./ -I../")
           (c-turn-on-eldoc-mode)))

;; use my cc-mode-common-setting
;;  (add-hook 'c-mode-hook 'coldnew-cc-mode-common-setting)

  ;; use ctypes
  (require 'ctypes)
  (setq-default ctypes-file-name (concat emacs-cache-dir "ctypes_std_c.dat"))
  (add-hook 'ctypes-load-hook 'my-ctypes-load-hook)

  ;; use hide-if-def-mode
  ;;(add-hook 'c-mode-hook 'hide-ifdef-mode)

  (require 'gccsense)

  ;;;; ---------------------------------------------------------------------------
  ;;;; Functions
  ;;;; ---------------------------------------------------------------------------
  (defun my-ctypes-load-hook ()
    (ctypes-read-file ctypes-file-name nil t t))


  ;;;; ---------------------------------------------------------------------------
  ;;;; Commands
  ;;;; ---------------------------------------------------------------------------

  (defun c-mode:insert-inc-or-if ()
    "If at the start of line. add `inc' and expand it,
  else add `if' and expand it."
    (interactive)
    (let* ((current (point))
           (begin (line-beginning-position)))
      (if (eq current begin)
          (progn
            (c-mode:insert-include)
            (newline-and-indent))
        (progn
          (insert "if")
          (yas/expand)))))

(require* 'guess-offset)

(define-key c-mode-map (kbd "C-x C-o") 'ff-find-other-file)

(defun c-mode:insert-main-function ()
  "insert main()."
  (interactive)
  (let* ((current (point))
         (begin (line-beginning-position)))
    (if (equal current begin)
        (insert "main"))
    (yas/expand)))

(add-to-list 'auto-mode-alist '("\\.hpp$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cpp$" . c++-mode))
(add-to-list 'magic-mode-alist
             `(,(lambda ()
                  (and (string= (file-name-extension buffer-file-name) "h")
                       (re-search-forward "\\W\\(class\\|template\\namespace\\)\\W"
                                          magic-mode-regexp-match-limit t)))
               . c++-mode))

;;;; CodingStyle
(add-hook 'c++-mode-hook
          '(lambda ()

             ;; TODO: add comment here
             (setq c-macro-shrink-window-flag t)
             (setq c-macro-preprocessor "cpp")
             (setq c-macro-cppflags " ")
             (setq c-macro-prompt-flag t)

             ;; Use linux-kernel style
             (c-set-style "linux")

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
             (c-set-offset 'substatement-open '0)
             ;; make open-braces after a case
             (c-set-offset 'case-label '+)

             ))

;; use my cc-mode-common-setting
;;(add-hook 'c++-mode-hook 'coldnew-cc-mode-common-setting)

;;;; Auto Complete
;;;; ---------------------------------------------------------------------------

  ;; Default clang completion flags
  ;;    (setq clang-completion-flags
  (setq ac-clang-flags
        (split-string
         (concat
          "-pthread -I./ -I../ "
          (shell-command-to-string "pkg-config --cflags-only-I opencv gtk+-3.0"))))

(add-hook 'c++-mode-hook
          '(lambda ()
             ;; Enable c-eldoc
             (require 'c-eldoc)
             (setq c-eldoc-includes "`pkg-config gtk+-3.0 opencv --cflags --libs` -I./ -I../")
             (c-turn-on-eldoc-mode)))

;; my auto-complete for cpp
(add-hook 'c++-mode-hook 'ac-cpp-mode-setup)
(require 'auto-complete-clang)
(defun ac-cpp-mode-setup ()
  "auto-complete settings for c-mode."
  (setq ac-sources '(
                     ac-source-clang
                     ac-source-dictionary
                     ac-source-abbrev
                     ac-source-semantic
                     ac-source-filename
                     ac-source-files-in-current-dir
                     ac-source-words-in-same-mode-buffers
                     )))

(define-key c++-mode-map (kbd "C-x C-o") 'ff-find-other-file)

(define-key c++-mode-map (kbd "C-c s") 'cscope-find-this-symbol)
 (define-key c++-mode-map (kbd "C-c g") 'cscope-find-global-definition)
 (define-key c++-mode-map (kbd "C-c I") 'cscope-create-list-of-files-to-index)
;; remember to remove
 (define-key c++-mode-map (kbd "C-c f") 'cscope-find-this-file)

(add-to-list 'auto-mode-alist '("\\.sh" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.bash" . sh-mode))

;;;; ---------------------------------------------------------------------------
;;;; Hooks
;;;; ---------------------------------------------------------------------------

;; use my editor-mode
(add-hook 'shell-mode-hook '(lambda() (coldnew-editor-mode 1)))

;; auto-complete
(add-hook 'shell-mode-hook 'ac-shell-script-mode-setup)

;; bash-completion
(require 'bash-completion)
(bash-completion-setup)

;; use flymake-shell
(require* 'flymake-shell)
(add-hook 'shell-mode-hook 'flymake-shell-load)


;;;; ---------------------------------------------------------------------------
;;;; Keybindings
;;;; ---------------------------------------------------------------------------


;;;; ---------------------------------------------------------------------------
;;;; Auto Complete
;;;; ---------------------------------------------------------------------------

;; define ac-source for pcomplete
(ac-define-source pcomplete
  '((candidates . pcomplete-completions)
    (cache)
    (symbol . "f")))

(defun ac-shell-script-mode-setup ()
  "auto-complete settings for shell-script-mode"
  (setq ac-sources
        '(ac-source-pcomplete
          ac-source-dictionary
          ac-source-filename
          ac-source-files-in-current-dir
          ac-source-words-in-same-mode-buffers
          )))

;;;; ---------------------------------------------------------------------------
;;;; Flymake
;;;; ---------------------------------------------------------------------------

;;;; ---------------------------------------------------------------------------
;;;; Functions
;;;; ---------------------------------------------------------------------------


;;;; ---------------------------------------------------------------------------
;;;; Commands
;;;; ---------------------------------------------------------------------------

;;;; ---------------------------------------------------------------------------
;;;; Initial modeline setting
;;;; ---------------------------------------------------------------------------
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;;;; ---------------------------------------------------------------------------
;;;; Functions
;;;; ---------------------------------------------------------------------------
(defun mode-line-major-mode ()
  "Get major-mode name with << >>."
  (concat "<< " (propertize mode-name 'face 'mode-line-mode-name-face) " >>"))
(defun coldnew-editor-mode-string ()
  (let* ((mode-string-face
          (cond
           ((string= "Command" coldnew-editor-state) 'mode-line-evil-state-string-V)
           ((string= "View" coldnew-editor-state) 'mode-line-evil-state-string-N)
           (t 'mode-line-evil-state-string-E)
           )))
    (concat "<" (propertize coldnew-editor-state 'face mode-string-face) ">")))
;;;; ---------------------------------------------------------------------------
;;;; modeline User-Interfaced setting
;;;; ---------------------------------------------------------------------------
(setq-default mode-line-format
              '((" "
                 mode-line-mule-info
                 ;; read-only or modified status
                 (:eval
                  (cond (buffer-read-only
                         (propertize "RO" 'face 'mode-line-read-only-face))
                        ((buffer-modified-p)
                         (propertize "**" 'face 'mode-line-modified-face))
                        (t "--")))
                 "   "
                 (:eval (coldnew-editor-mode-string))
                 "   "
                 ;;              (:eval (mode-line-state))
                 mode-line-buffer-identification
                 "   "
                 ;; major-mode name
                 (:eval (mode-line-major-mode))
                 "   "
                 ;; line and column
                 "("
                 (:eval (propertize "%02l" 'face 'font-lock-type-face))
                 ","
                 (:eval (propertize "%02c" 'face 'font-lock-type-face))
                 ")"

                 "   "
                 (vc-mode vc-mode)
                 "   "
                 ;; relative position, size of file
                 "["
                 (:eval (propertize "%p" 'face 'font-lock-constant-face)) ;; % above top
                 "/"
                 (:eval (propertize "%I" 'face 'font-lock-constant-face)) ;; size
                 "] "
                 )))

(defun scratch-toggle ()
  "Toggle between *scratch* buffer and the current buffer.
   If the *scratch* buffer does not exist, create it."
  (interactive)
  (let ((scratch-buffer-name (get-buffer-create "*scratch*")))
    (if (equal (current-buffer) scratch-buffer-name)
        (switch-to-buffer (other-buffer))
      ;;        (previous-user-buffer)
      (progn
        (switch-to-buffer scratch-buffer-name)
        (unless (equal major-mode 'lisp-interaction-mode)
          (lisp-interaction-mode))))))

(defun save-buffer-always ()
  "Save the buffer even if it is not modified."
  (interactive)
  (set-buffer-modified-p t)
  (save-buffer))

(defun window-horizontal-to-vertical ()
  "Switches from a horizontal split to a vertical split."
  (interactive)
  (let ((one-buf (window-buffer (selected-window)))
        (buf-point (point)))
    (other-window 1)
    (delete-other-windows)
    (split-window-horizontally)
    (switch-to-buffer one-buf)
    (goto-char buf-point)))

(defun window-vertical-to-horizontal ()
  "Switches from a vertical split to a horizontal split."
  (interactive)
  (let ((one-buf (window-buffer (selected-window)))
        (buf-point (point)))
    (other-window 1)
    (delete-other-windows)
    (split-window-vertically)
    (switch-to-buffer one-buf)
    (goto-char buf-point)))

(defun my-column-ruler (width)
  "Display temp ruler at point."
  (interactive `(,(+ (window-hscroll)(window-width))))
  (momentary-string-display
   (let* ((iterations (/ (1- width) 10))
          (result1 "|...|....|")
          (result2 "1   5   10")
          (inc1 "....|....|")
          (inc2 "        %d0")
          (i 1))
     (while  (<= i iterations)
       (setq i (1+ i))
       (setq result1 (concat result1 inc1))
       (setq result2 (concat result2 (substring (format inc2 i) -10))))
     (concat (substring result2 0 width) "\n"
             (substring result1 0 width) "\n"))
   (line-beginning-position)
   nil "[space] Clears ruler"))

(defun swap-with (dir)
  (interactive)
  (let ((other-window (windmove-find-other-window dir)))
    (when other-window
      (let* ((this-window (selected-window))
             (this-buffer (window-buffer this-window))
             (other-buffer (window-buffer other-window))
             (this-start (window-start this-window))
             (other-start (window-start other-window)))
        (set-window-buffer this-window other-buffer)
        (set-window-buffer other-window this-buffer)
        (set-window-start this-window other-start)
        (set-window-start other-window this-start)))))

;; (global-set-key (kbd "C-M-J") (lambda () (interactive) (swap-with 'down)))
;; (global-set-key (kbd "C-M-K") (lambda () (interactive) (swap-with 'up)))
;; (global-set-key (kbd "C-M-H") (lambda () (interactive) (swap-with 'left)))
;; (global-set-key (kbd "C-M-L") (lambda () (interactive) (swap-with 'right)))

;; (global-set-key (kbd "M-J") (lambda () (interactive) (enlarge-window 1)))
;; (global-set-key (kbd "M-K") (lambda () (interactive) (enlarge-window -1)))
;; (global-set-key (kbd "M-H") (lambda () (interactive) (enlarge-window -1 t)))
;; (global-set-key (kbd "M-L") (lambda () (interactive) (enlarge-window 1 t)))


(defun select-forwards-to-before-match (match)
  "Selects forwards to just before next match, uses
select-region-to-before-match"
  (interactive "MSelect forwards to just before: ")
  (select-region-to-before-match match 'forwards))

(defun select-backwards-to-before-match (match)
  "Selects backwards to just before next match, uses
select-region-to-before-match"
  (interactive "MSelect backwards to just before: ")
  (select-region-to-before-match match 'backwards))

(defun kill-forwards-to-before-match (match)
  "Selects forwards to just before next match, uses
select-region-to-before-match, then kills that region."
  (interactive "MKill forwards to just before: ")
  (let* ((positions (select-region-to-before-match match 'forwards))
         (start (car positions))
         (end (cadr positions)))
    (kill-region start end)))

(defun kill-backwards-to-before-match (match)
  "Selects backwards to just before next match, uses
select-region-to-before-match, then kills that region."
  (interactive "MKill backwards to just before: ")
  (let* ((positions (select-region-to-before-match match 'backwards))
         (start (car positions))
         (end (cadr positions)))
    (kill-region start end)))

(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

(defun delete-between-pair (char)
  "Delete in between the given pair"
  (interactive "cDelete between char: ")
  (let ((pair-char))
    (search-backward-to-char char)
    (forward-char 1)
    (cond
     ((char-equal char ?\() (setq pair-char ?\)))
     ((char-equal char ?\") (setq pair-char ?\"))
     ((char-equal char ?\') (setq pair-char ?\'))
     ((char-equal char ?\[) (setq pair-char ?\]))
     ((char-equal char ?\{) (setq pair-char ?\}))
     ((char-equal char ?\<) (setq pair-char ?\>)))
    (zap-up-to-char 1 pair-char)))

(defun cua-set-mark-or-rectangle-mark (&optional arg)
  "toggle between cua-set-mark or cua-rectangle-mark"
  (interactive "P")
  (if (or (not mark-active) arg)
      (cua-set-mark arg)
    (cua-toggle-rectangle-mark)))


(defadvice kill-ring-save (before slickcopy activate compile)
  "When called interactively with no active region, copy a single line
instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(defadvice kill-region (before slickcut activate compile)
  "When called interactively with no active region, kill a single line
instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(defun zap-up-to-char-backward (arg char)
  (interactive "p\ncZap up to char backward: ")
  (zap-up-to-char (- arg) char))

(defun go-to-char (arg char)
  (interactive "p\ncGo to char: ")
  (forward-char 1)
  (if (if arg
          (search-forward (char-to-string char) nil nil arg)
        (search-forward (char-to-string char)))
      (backward-char 1)))

(defun go-back-to-char (arg char)
  (interactive "p\ncGo back to char: ")
  (forward-char -1)
  (if arg
      (search-backward (char-to-string char) nil nil arg)
    (search-backward (char-to-string char))))

(defun vjo-forward-current-word-keep-offset ()
  " (Vagn Johansen 1999)"
  (interactive)
  (let ((re-curword) (curword) (offset (point))
        (old-case-fold-search case-fold-search) )
    (setq curword (thing-at-point 'symbol))
    (setq re-curword (concat "\\<" (thing-at-point 'symbol) "\\>") )
    (beginning-of-thing 'symbol)
    (setq offset (- offset (point)))    ; offset from start of symbol/word
    (setq offset (- (length curword) offset)) ; offset from end
    (forward-char)
    (setq case-fold-search nil)
    (if (re-search-forward re-curword nil t)
        (backward-char offset)
      ;; else
      (progn (goto-char (point-min))
             (if (re-search-forward re-curword nil t)
                 (progn (message "Searching from top. %s" (what-line))
                        (backward-char offset))
               ;; else
               (message "Searching from top: Not found"))
             ))
    (setq case-fold-search old-case-fold-search)
    ))

(defun vjo-backward-current-word-keep-offset ()
  " (Vagn Johansen 2002)"
  (interactive)
  (let ((re-curword) (curword) (offset (point))
        (old-case-fold-search case-fold-search) )
    (setq curword (thing-at-point 'symbol))
    (setq re-curword (concat "\\<" curword "\\>") )
    (beginning-of-thing 'symbol)
    (setq offset (- offset (point)))    ; offset from start of symbol/word
    (forward-char)
    (setq case-fold-search nil)
    (if (re-search-backward re-curword nil t)
        (forward-char offset)
      ;; else
      (progn (goto-char (point-max))
             (if (re-search-backward re-curword nil t)
                 (progn (message "Searching from bottom. %s" (what-line))
                        (forward-char offset))
               ;; else
               (message "Searching from bottom: Not found"))
             ))
    (setq case-fold-search old-case-fold-search)
    ))

;; ;; FIXME: need to test
;; (defun set-frame-size-according-to-resolution ()
;;   (interactive)
;;   (if window-system
;;       (progn
;;      (if (> (x-display-pixel-width) 1500) ;; 1500 is the delimiter marging in px to consider the screen big
;;          (set-frame-width (selected-frame) 237) ;; on the big screen make the fram 237 columns big
;;        (set-frame-width (selected-frame) 177)) ;; on the small screen we use 177 columns
;;      (setq my-height (/ (- (x-display-pixel-height) 150) ;; cut 150 px of the screen height and use the rest as height for the frame
;;                         (frame-char-height)))
;;      (set-frame-height (selected-frame) my-height)
;;      (set-frame-position (selected-frame) 3 90) ;; position the frame 3 pixels left and 90 px down
;;      )))

;; (set-frame-size-according-to-resolution)
;;  (global-set-key (kbd "C-x 9") 'set-frame-size-according-to-resolution)

(defun open-in-largest-window()
  "Open current buffer in largest window"
  (interactive)
  (let ((oldbuf (current-buffer)))
    (select-window (get-largest-window))
    (switch-to-buffer oldbuf))
  )
;;(global-set-key (kbd "C-x 5") 'open-in-largest-window)

(defun gitg ()
  "Launch gitg in the current directory."
  (interactive)
  (start-process "gitg" nil "gitg"))

(defun fullscreen-window ()
  "Make the window full-screen."
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen))
        (old-value nil))
    (set-frame-parameter nil 'fullscreen
                         (if (equal 'fullboth current-value)
                             'old-value
                           (progn
                             (setq old-value current-value)
                             'fullboth)
                           ))))

(defun windmove-down-fullscreen ()
  "Select window below current one and make it fullscreen."
  (interactive)
  (if (windmove-down)
      (delete-other-windows)))

(defun windmove-up-fullscreen ()
  "Select window above the current one and make it fullscreen."
  (interactive)
  (if (windmove-up)
      (delete-other-windows)))

(defun windmove-left-fullscreen ()
  "Select window left to current one and make it fullscreen."
  (interactive)
  (if (windmove-left)
      (delete-other-windows)))

(defun windmove-right-fullscreen ()
  "Select window right to current one and make it fullscreen."
  (interactive)
  (if (windmove-right)
      (delete-other-windows)))

(defun sudo-edit (&optional arg)
  "Edit file with sudo in emacs"
  (interactive "p")
  (if (or arg (not buffer-file-name))
      ;; (find-file (concat "/sudo:root@localhost:" (anything-read-file-name "File: ")))
      (find-file (concat "/sudo:root@localhost:" (read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;;;; ---------------------------------------------------------------------------
;;;; Buffer
;;;; ---------------------------------------------------------------------------
(defun save-buffer-always ()
  "Save the buffer even if it is not modified."
  (interactive)
  (set-buffer-modified-p t)
  (save-buffer))

(defun toggle-selective-display (column)
  (interactive "P")
  (set-selective-display
   (if selective-display nil (or column 1))))

;;;; ---------------------------------------------------------------------------
;;;; Insert
;;; ---------------------------------------------------------------------------
(require 'mm-url)

(defun insert-tinyurl (url)
  "Insert a shortend URL at point by passed in URL"
  (interactive "sEnter url: " )
  (let* ((url (replace-regexp-in-string "^http://" "" url))
         (tinyurl
          (save-excursion
            (with-temp-buffer
              (mm-url-insert
               (concat "http://tinyurl.com/api-create.php?url=http://" url))
              (kill-ring-save (point-min) (point-max))
              (buffer-string)))))
    (insert tinyurl)))

;;;; ---------------------------------------------------------------------------
;;;; Date
;;;; ---------------------------------------------------------------------------


;;;; ---------------------------------------------------------------------------
;;;; Search
;;;; ---------------------------------------------------------------------------
(defun multi-occur-in-this-mode ()
  "Show all lines matching REGEXP in buffers with this major mode."
  (interactive)
  (multi-occur
   (get-buffers-matching-mode major-mode)
   (car (occur-read-primary-args)))
  (select-window (get-buffer-window "*Occur*")))

;;;; ---------------------------------------------------------------------------
;;;; Delete
;;;; ---------------------------------------------------------------------------
(defun delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun backward-delete-word (arg)
  "Delete characters backward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-word (- arg)))

;;;; ---------------------------------------------------------------------------
;;;; key-macro
;;;; ---------------------------------------------------------------------------
(defun toggle-kbd-macro-recording-on ()
  "One-key keyboard macros: turn recording on."
  (interactive)
  (define-key
    global-map
    (this-command-keys)
    'toggle-kbd-macro-recording-off)
  (start-kbd-macro nil))

(defun toggle-kbd-macro-recording-off ()
  "One-key keyboard macros: turn recording off."
  (interactive)
  (define-key
    global-map
    (this-command-keys)
    'toggle-kbd-macro-recording-on)
  (end-kbd-macro))

;;;; ---------------------------------------------------------------------------
;;;; Commands that define for key-chord
;;;; ---------------------------------------------------------------------------
(defun upcase-word-backward ()
  "upcase word backward."
  (interactive)
  (upcase-word -1))

(defun downcase-word-backward ()
  "downcase word backward."
  (interactive)
  (downcase-word -1))

(defun capitalize-word-backward ()
  "captialize word backward."
  (interactive)
  (capitalize-word -1))

(defun backward-kill-line (arg)
  "Kill chars backward until encountering the end of a line."
  (interactive "p")
  (kill-line 0))

(defun insert-empty-line ()
  "Insert an empty line after current line and position cursor on newline."
  (interactive)
  (move-end-of-line nil)
  (open-line 1)
  (next-line 1))

(defun insert-date-time ()
  "Insert current-date."
  (interactive)
  (insert (current-date-time)))

(defun unix2dos ()
  "Convert buffer file from unix file to dos file."
  (interactive)
  (unix->dos (current-buffer)))

(defun dos2unix ()
  "Convert buffer file from dos file to unix file."
  (interactive)
  (dos->unix (current-buffer)))

(require 'erc)
(require 'erc-log)
(require 'erc-notify)
(require 'erc-spelling)
(require 'erc-autoaway)

(setq erc-prompt ">>")

(setq erc-server-coding-system '(utf-8 . utf-8))

;; make sure to use wildcards for e.g. freenode as the actual server
;; name can be be a bit different, which would screw up autoconnect
(erc-autojoin-mode t)
(setq erc-autojoin-channels-alist
      '((".*\\.freenode.net" "#emacs"  "#clojure")
        ))

;; check channels
;; exclude boring stuff from tracking
(erc-track-mode t)
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"

                                "324" "329" "332" "333" "353" "477"))
;; don't show any of this
(setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))
;; Interpret mIRC-style color commands in IRC chats
(setq erc-interpret-mirc-color t)
;; The following are commented out by default, but users of other
;; non-Emacs IRC clients might find them useful.
;; Kill buffers for channels after /part
(setq erc-kill-buffer-on-part t)
;; Kill buffers for private queries after quitting the server
(setq erc-kill-queries-on-quit t)
;; Kill buffers for server messages after quitting the server
(setq erc-kill-server-buffer-on-quit t)
;; open query buffers in the current window
(setq erc-query-display 'buffer)
;; logging
(setq erc-log-channels-directory "~/.erc/logs/")
(if (not (file-exists-p erc-log-channels-directory))
    (mkdir erc-log-channels-directory t))
(setq erc-save-buffer-on-part t)
(defadvice save-buffers-kill-emacs (before save-logs (arg) activate)
  (save-some-buffers t (lambda () (when (eq major-mode 'erc-mode) t))))
;; truncate long irc buffers
(erc-truncate-mode +1)
;; enable spell checking
(erc-spelling-mode 1)
;; autoaway setup
(setq erc-auto-discard-away t)
(setq erc-autoaway-idle-seconds 600)
(setq erc-autoaway-use-emacs-idle t)

(defun i-wanna-be-social ()
  "Connect to IM networks using bitlbee."
  (interactive)
  (erc :server "localhost" :port 6667 :nick "user"))

(defun erc-cmd-WII (nick &rest ignore)
  "`/WHOIS' command with extra user information."
  (erc-send-command (mapconcat #'identity
                               (list "WHOIS" nick nick) " ")))
(defun erc-cmd-IDENTIFY (password &rest ignore)
  "Short-hand alias for `/msg NickServ identify PASS'."
  (erc-send-command (mapconcat #'identity
                               (list "identify" password) " ")))
(defun erc-cmd-CS (&rest args)
  "Short alias for `/chanserv ARGS'."
  (let ((command-args (append (list "CHANSERV") args)))
    (let ((chanserv-command (mapconcat #'identity command-args " ")))
      (erc-send-command chanserv-command))))

(defun erc-cmd-MS (&rest args)
  "Short alias for `/memoserv ARGS'."
  (let ((command-args (append (list "MEMOSERV") args)))
    (let ((memoserv-command (mapconcat #'identity command-args " ")))
      (erc-send-command memoserv-command))))

(defun erc-cmd-NS (&rest args)
  "Short alias for `/nickserv ARGS'."
  (let ((command-args (append (list "NICKSERV") args)))
    (let ((nickserv-command (mapconcat #'identity command-args " ")))
      (erc-send-command nickserv-command))))

(require 'jabber)

(defalias 'git-log    'egg-log)
(defalias 'git-status 'egg-status)
(defalias 'git-rebase 'magit-rebase-step)
(defalias 'git-push   'magit-push)
(defalias 'git-commit 'egg-commit-log-edit)

(defalias 'coldnew/set-mark-command 'cua-set-mark-or-rectangle-mark)
(defalias 'coldnew/folding-toggle   'toggle-selective-display)

(require 'egg)

;; do not auto-update egg-status on file save
(setq egg-auto-update nil)

;; do not switch to the status buffer in the same window
(setq egg-switch-to-buffer t)

;; make egg auto guess next action
(setq egg-confirm-next-action nil)

;; remodify next-action priority
(defsubst egg-guess-next-action (desc)
  (cond
   ((memq :file-has-merged-conflict desc) :merge-file)
   ((memq :file-is-modified desc)         :stage-file)
   ((memq :has-staged-changes desc)       :commit)
   ((memq :file-is-unmerged desc)         :stage-file)
   ((memq :wdir-has-merged-conflict desc) :status)
   ((memq :wdir-has-unmerged-files  desc) :stage-all)
   ((memq :wdir-is-modified desc)         :stage-all)
   ((memq :rebase-in-progress desc)       :rebase-continue)
   (t                                     :quit)))

(add-hook 'egg-commit-buffer-mode-hook
          '(lambda ()
             (define-key coldnew/command-mode-map "c" 'egg-log-msg-done)
             ))

(defadvice egg-status (around goto-egg-status-buffer activate)
  "Delete other windows after visiting egg-status."
  ad-do-it
  (delete-other-windows))

(defadvice egg-commit-log-edit (around goto-egg-commit-buffer activate)
  "Delete other windows after visiting egg-commit-buffer."
  ad-do-it
  (delete-other-windows))

;;;;;;;; Magit
(require 'magit)
;; if use magit, do not use vc-git to handle Git interface.
;; (when (featurep 'magit)
;;   (setq vc-handled-backends (remq 'Git vc-handled-backends)))

(require 'git-emacs)
;;   ;; disable git-emacs's advice
;;   (ad-disable-advice 'vc-next-action 'around 'git--vc-git-next-action)
;;   (ad-activate 'vc-next-action)
(define-key git--branch-mode-map (kbd "C-g") 'git--quit-buffer)

;; Auto add HEADER in new file
(add-hook 'find-file-hook
          '(lambda ()
             (when (and (buffer-file-name)
                        (not (file-exists-p (buffer-file-name)))
                        (= (point-max) 1))
               (let ((header-snippet "HEADER")
                     (yas/fallback-behavior 'return-nil))
                 (insert header-snippet)
                 ;; if can't expand snippet, clear whole buffer
                 (if (not (yas/expand))
                     (delete-region (point-min) (point-max)))))))

;; If save a newfile to nonexist directory, create the directory before save.
(add-hook 'before-save-hook
          '(lambda ()
             (or (file-exists-p  (file-name-directory buffer-file-name))
                 (make-directory (file-name-directory buffer-file-name) t))))

(setq auto-save-interval  50)           ; Number of input events between auto-saves
(setq auto-save-timeout   30)           ; Number of seconds idle time before auto-save
(setq auto-save-visited-file-name t)    ; auto-save buffer in the file it is visiting
(setq delete-by-moving-to-trash nil)    ; delete file don't use system's trash can
(setq delete-auto-save-files      t)    ; delete auto-save file when bffer is saved or killed
(setq auto-save-default    t)           ; auto-save of every file-visiting buffer

;; if emacs-backup-dir does not exist, create it
(if (not (file-exists-p emacs-backup-dir))
    (make-directory emacs-backup-dir t))

(setq backup-directory-alist `(("." . ,emacs-backup-dir)))
(setq version-control      t )          ; enable version-control
(setq backup-by-copying    t )          ; backup by copy
(setq kept-old-versions   10 )          ; keep 10 old-version
(setq kept-new-versions   20 )          ; keep 20 new-version
(setq delete-old-versions  t )          ; delete non-of-above version

;; change auto-save-list setting
(setq auto-save-list-file-prefix (concat emacs-backup-dir "auto-saves-"))
(setq auto-save-file-name-transforms `((".*"  ,emacs-backup-dir)))

(require 'filecache)
;; setup file-cache-filter-regexps
(setq file-cache-filter-regexps
      '("~$" "\\.o$" "\\.exe$" "\\.a$" "\\.out$"  "\\.elc$"
        "\\.output$" "\\.$" "#$" "\\.class$" "\\.pyc$" "\\.png$" "\\.jpg$" "\\.gif$"
        "\\.svn$" "\\.svn-base$" "\\.git$" "\\.hg$"
        ))

(file-cache-add-directory-list '("~/.emacs.d/" ))

(require 'savehist)
;; keep minibuffer history between session
(setq savehist-file (concat emacs-cache-dir "savehist.dat"))
(savehist-mode 1)

(require 'saveplace)
(setq save-place-file (concat emacs-cache-dir "saveplace.dat"))
(setq-default save-place t)

(require 'recentf)
(require 'recentf-ext)
;; Setting cache file for recentf
(setq recentf-save-file (concat emacs-cache-dir "recentf"))
;; Following file won;t contain in recentf
(setq recentf-exclude '("\\.elc$" "\\.pyc$" "\\.recentd$" "^/tmp/"))

(require 'desktop)
(setq desktop-path (list emacs-cache-dir))
(setq desktop-dirname emacs-cache-dir)
(setq desktop-base-file-name "desktop.dat")
(setq desktop-missing-file-warning nil)

;; Enable desktop
(desktop-save-mode t)

(defadvice desktop-owner (after pry-from-cold-dead-hands activate)
  "Don't allow dead emacsen to own the desktop file."
  (when (not (emacs-process-p ad-return-value))
    (setq ad-return-value nil)))

(defun desktop-in-use? ()
  (and (file-exists-p desktop-base-file-name) (file-exists-p desktop-base-lock-name)))

(defun autosave-desktop ()
  (if (desktop-in-use?) (desktop-save-in-desktop-dir)))

;; auto save desktop
(add-hook 'after-init-hook
          (lambda ()
            (setq *desktop-saver-timer*
                  (run-with-timer 5 300 'autosave-desktop))))

;; Following modes are ignore and won't save to desktop
(setq desktop-buffers-not-to-save
      (concat "\\("
              "^\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
              "\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
              "\\)$"))

(add-to-list 'desktop-modes-not-to-save 'fundamental-mode)
(add-to-list 'desktop-modes-not-to-save 'dired-mode)
(add-to-list 'desktop-modes-not-to-save 'Info-mode)
(add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
(add-to-list 'desktop-modes-not-to-save 'w3m-mode)
(add-to-list 'desktop-modes-not-to-save 'view-mode)

(defvar emacs-normal-cursor-color "white")
(defvar emacs-normal-cursor-type 'bar)

(defvar emacs-read-only-cursor-color "gray")
(defvar emacs-read-only-cursor-type 'box)

(defvar emacs-overwrite-cursor-color "yellow")
(defvar emacs-overwrite-cursor-type 'hbar)

;;;; ---------------------------------------------------------------------------
;;;; Hooks
;;;; ---------------------------------------------------------------------------
(add-hook 'post-command-hook 'coldnew/set-cursor-according-mode)


;;;; ---------------------------------------------------------------------------
;;;; Functions
;;;; ---------------------------------------------------------------------------
(defun coldnew/set-cursor-according-mode ()
  "change cursor shap and color according mode"
  (cond
   ((string= "Command" coldnew-editor-state)
    (setq cursor-type emacs-read-only-cursor-type)
    (setq cursor-color emacs-read-only-cursor-color))
   (buffer-read-only
    (setq cursor-type emacs-read-only-cursor-type)
    (setq cursor-color emacs-read-only-cursor-color))
   ;; (overwrite-mode
   ;;   (set-cursor-color djcb-overwrite-color)
   ;;   (setq cursor-type djcb-overwrite-cursor-type))
   (t
    (setq cursor-type emacs-normal-cursor-type)
    (setq cursor-color emacs-normal-cursor-color))))

(defmacro coldnew/global-unset-key (key &rest bindings)
  `(progn
     (let* ((keyf ,key)
            (bindings (list ,@bindings)))
       (with-temp-buffer
         ;;(with-current-buffer (get-buffer-create "tttmp")
         (insert "| unset key ")
         (newline)
         (insert "|-")
         (newline)
         (while keyf
           (global-unset-key keyf)
           (insert (format "| %s \n" (coldnew/parse-keymap keyf)))
           (setq keyf (pop bindings))
           )
         (princ (buffer-string))
         ))))

(defun coldnew/parse-keymap (s)
  "Parse \C-x to Ctrl-x, and return string."
  (let ((char-list (string-to-list (replace-regexp-in-string " " "" s)))
        (result-list))
    (dolist (char char-list)
      ;;\A to \Z
      ;; set char to cdr of coldnew/keymap-string-alist if assoc succes
      ;; else set char to char
      (setq char (or (reverse (string-to-list (cdr-safe (assoc char coldnew/keymap-string-alist))))
                     char))
      (setq result-list (cons char result-list))
      (setq result-list  (cons  ?\s result-list))
      )
    ;;result-list
    (list-to-string (nreverse
                     (cdr-safe
                      (flatten result-list))))
    ))

(defvar coldnew/keymap-string-alist
  (let ((result))
    (dolist (char (number-sequence ?\^a ?\^z))
      (add-to-list 'result (cons char (concat "Ctrl-" (char-to-string (1- (+ char ?a)))))))
    result))

(coldnew/parse-keymap (kbd "C-a e C-x y"))
(coldnew/parse-keymap "\C-x e ")

(setq show-paren-delay 0)

(coldnew/global-unset-key
   (kbd "C-x e")
   (kbd "C-x d")
   "\C-x e"
;;   "\C-c e"
   )

(global-set-key (kbd "<f1>")     'woman)
(global-set-key (kbd "<f2>")     'shell-pop)
(global-set-key (kbd "<f3>")     'call-last-kbd-macro)
(global-set-key (kbd "<f4>")     'sr-speedbar-toggle)

(global-set-key (kbd "C-n") 'next-line)
(global-set-key (kbd "C-p") 'previous-line)
(global-set-key (kbd "C-b") 'backward-char)
(global-set-key (kbd "C-f") 'forward-char)
(global-set-key (kbd "C-e") 'move-end-of-line)
(global-set-key (kbd "C-a") 'move-beginning-of-line)

(global-set-key (kbd "C-l") 'hungry-delete-backward)
(global-set-key (kbd "C-d") 'hungry-delete-forward)
;;(global-set-key (kbd "C-u") 'backward-kill-line)

(global-set-key (kbd "C-;") 'iedit-mode)
(global-set-key (kbd "C-v") 'coldnew/set-mark-command)
(global-set-key (kbd "C-/") 'undo-tree-undo)
(global-set-key (kbd "C-]") 'coldnew/toggle-state)

(global-set-key (kbd "C-x C-f")  'lusty-file-explorer)
(global-set-key (kbd "C-x C-b")  'ibuffer)
(global-set-key (kbd "C-x b")    'coldnew/helm-filelist)
(global-set-key (kbd "C-x C-d")  'dired)
(global-set-key (kbd "C-x C-r")  'sudo-edit)
(global-set-key (kbd "C-x vv") 'egg-next-action)
(global-set-key (kbd "C-x M-x") 'helm-M-x)
(global-set-key (kbd "C-x o")   'switch-window)
(global-set-key (kbd "C-x C-l") 'recenter-top-bottom)
(global-set-key (kbd "C-x C-n") 'auto-complete)
(global-set-key (kbd "C-x C-s") 'save-buffer-always)
(global-set-key (kbd "C-x f") 'fullscreen-window)
(global-set-key (kbd "C-x s") 'shell-command)

(global-set-key (kbd "C-c C-h") 'coldnew/folding-toggle)

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c C-l") 'org-insert-link)
(global-set-key (kbd "C-c o") 'org-iswitchb)

(global-set-key (kbd "C-M-,") 'beginning-of-buffer)
(global-set-key (kbd "C-M-.") 'end-of-buffer)
(global-set-key (kbd "C-M-n") 'scroll-other-window)
(global-set-key (kbd "C-M-p") 'scroll-other-window-down)

(global-set-key (kbd "M-f") 'forward-word)
(global-set-key (kbd "M-b") 'backward-word)
(global-set-key (kbd "M-e") 'forward-sentence)
(global-set-key (kbd "M-a") 'backward-sentence)

(global-set-key (kbd "M-s") 'coldnew/helm-occur)
(global-set-key (kbd "M-l") 'backward-delete-word)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "M-n") 'scroll-up)
(global-set-key (kbd "M-p") 'scroll-down)
(global-set-key (kbd "M-g") 'linum-ace-jump)
(global-set-key (kbd "M-v") 'er/expand-region)
(global-set-key (kbd "M-q") 'coldnew/switch-to-command-mode)
(global-set-key (kbd "M-/") 'undo-tree-redo)
(global-set-key (kbd "M-.") 'helm-etags+-select-one-key)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; (global-set-key (kbd "M-1") 'delete-other-windows)
;; (global-set-key (kbd "M-2") 'split-window-below)
;; (global-set-key (kbd "M-3") 'split-window-right)
;; (global-set-key (kbd "M-4") 'delete-window)
;; (global-set-key (kbd "M-0") 'other-window)
;;  (global-set-key (kbd "M-j") 'switch-window)
;;  (global-set-key (kbd "M-i") 'yas/expand)

;;  (global-set-key (kbd "C-0 e") 'ecb-toggle-ecb-windows)
;;  (global-set-key (kbd "C-0 1") 'ecb-goto-window-edit1)
;;  (global-set-key (kbd "C-0 2") 'ecb-goto-window-edit2)
;;  (global-set-key (kbd "C-0 m") 'ecb-goto-window-methods)
; ; (global-set-key (kbd "C-0 l") 'ecb-toggle-layout)

;;  (global-set-key (kbd "M-j n") 'windmove-down)
;;  (global-set-key (kbd "M-j p") 'windmove-up)
;;  (global-set-key (kbd "M-j b") 'windmove-left)
;;  (global-set-key (kbd "M-j f") 'windmove-right)

(global-set-key [(shift return)] 'insert-empty-line)

(global-set-key (kbd "s-n") 'windmove-down)
(global-set-key (kbd "s-p") 'windmove-up)
(global-set-key (kbd "s-b") 'windmove-left)
(global-set-key (kbd "s-f") 'windmove-right)

(global-set-key (kbd "<delete>") 'hungry-delete-forward)
(global-set-key (kbd "C-.") 'repeat)
(global-set-key (kbd "%") 'match-paren)

(global-set-key (kbd "(") 'paredit-open-parenthesis)
(global-set-key (kbd ")") 'paredit-open-parenthesis)
(global-set-key (kbd "[") 'paredit-open-square)
(global-set-key (kbd "{") 'paredit-open-curly)
(global-set-key (kbd "\"") 'paredit-doublequote)

(dolist
    (r `(
         ;; emacs-config
         (?e (file . ,(expand-file-name emacs-config-file)))
         ;; TODO.org
         (?t (file . "~/Dropbox/Org/TODO.org"))
         ;; (?b (file . "~/personal/business.org"))
         ))
  (set-register (car r) (cadr r)))

(defun google-weather-build-url (location &optional language)
  "Build URL to retrieve weather for LOCATION in LANGUAGE."
  (concat "http" (when google-weather-use-https "s") "://" google-weather-url "?weather=" (url-hexify-string location)
          (when language
            (concat "&hl=" language))
          ;; add for encoding
          (concat "&oe=" "utf8")))
