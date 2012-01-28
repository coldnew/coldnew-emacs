;;
(eval-when-compile (require 'cl))

;;;;;;;; Packages Import
(require 'coldnew-macro)
(require 'coldnew-functions)
(require 'coldnew-commands)
(require 'coldnew-variables)
(require 'coldnew-vim)



;;;;;;;; Ansi-Color
;; reset ansi-color to match with my theme
;; default is [ "black" "red" "green" yellow" "blue" "magenta" "cyan" "white"]
;;
(when (require* 'ansi-color)
  (setq ansi-color-names-vector
	(vector (frame-parameter nil 'background-color)
		"#ffffff" "#CA3839" "#8ae234" "#edd400"
		"#729fcf" "#ad7fa8" "cyan3"   "#eeeeec")
	ansi-term-color-vector ansi-color-names-vector
	ansi-color-map (ansi-color-make-color-map))
  )

;;;;;;;; Shell-pop
;;
(when (require* 'shell-pop)
  (shell-pop-set-internal-mode "ansi-term")
  ;;  (shell-pop-set-internal-mode "multi-term")
  (shell-pop-set-internal-mode-shell emacs-default-shell)
  (shell-pop-set-window-height emacs-popup-shell-window-height)
  (shell-pop-set-window-position emacs-popup-shell-window-position)
  )

;;;;;;;; Multi-term
;;
(when (require* 'multi-term)
  (setq multi-term-program emacs-default-shell)
  )

;;;;;;;; Term
;;
(when (require* 'term)
  (setq-default term-default-bg-color nil)
  (setq-default term-default-fg-color nil)

  ;; (setq term-unbind-key-list '("C-c" "C-h"))
  ;; (add-to-list 'term-bind-key-alist '("C-c" . term-interrupt-subjob))
  ;; (setq term-bind-key-alist (delete '("C-c C-c" . term-interrupt-subjob)
  ;;				    term-bind-key-alist))

  ;;;; Keybindings
  (add-hook 'term-mode-hook
	    '(lambda ()
	       ;; Add new key-map
	       (define-key term-raw-map (kbd "<f4>") 'shell-pop)
	       (define-key term-raw-map (kbd "M-x") 'anything-M-x)
	       (define-key term-raw-map (kbd "C-g") 'term-interrupt-subjob)
	       (define-key term-raw-map (kbd "C-n") 'term-send-down)
	       (define-key term-raw-map (kbd "C-p") 'term-send-up)
	       ;;	       (define-key term-raw-map (kbd "<enter>") 'term-send-input)
	       ;;	       (define-key term-raw-map (kbd "<return>") 'term-send-raw)
	       ))
  )
;;;;;;;; Comint mode
;;
(when (require* 'comint)

  ;; Do not show password in comint-mode
  (setq comint-output-filter-functions  '(comint-watch-for-password-prompt))
  (setq comint-password-prompt-regexp
	"\\(\\([Oo]ld \\|[Nn]ew \\|^\\)[Pp]assword\\|Enter password\\):\\s *\\'")

  ;;;; Hooks
  (add-hook 'comint-mode-hook
	    '(lambda ()

	       ;; Use global programming mode
	       (programming-mode)
	       ))

  ;;;; Keybindings
  (add-hook 'comint-mode-hook
	    '(lambda ()
	       ;; Normal map
	       (define-key evil-normal-state-local-map (kbd "M-k") 'comint-previous-input)
	       (define-key evil-normal-state-local-map (kbd "M-j") 'comint-previous-input)
	       (define-key evil-normal-state-local-map (kbd "C-c C-r") 'comint-clear-region)
	       ;; Insert map
	       (define-key evil-insert-state-local-map (kbd "RET") 'newline-and-indent)
	       ))
  ;;;; Functions
  (defun comint-clear-region ()
    (interactive)
    (delete-region (point-min) (point-max))
    (comint-send-input))
  )


;;;;;;;; Functions

;;;; Rewrite shell-pop function
;; I don't like use vim like key in shell,after shell popup
;; use emacs-key instead of vim-mode
;; TODO: use advice to rewrite this function.
;;
;; (defun shell-pop ()
;;   "Toggle vim-mode between shell-pop-up and shell-pop-down."
;;   (interactive)
;;   (if (equal (buffer-name) shell-pop-internal-mode-buffer)
;;       (progn
;;        (shell-pop-out)
;;        (vim:normal-mode)
;;        )
;;       (progn
;;        (shell-pop-up)
;;        (vim:emacs-mode)
;;        )))

;; ;; Use emacs-key instead of Vim-key in term-mode
;; ;; TODO: is there a better way to do this?
;; (defadvice switch-to-buffer (after switch-to-buffer activate)
;;   "After switch-to-buffer, if tht buffer is term-mode, disable vim-mode."
;;   (if (equal major-mode 'term-mode)
;;       (vim:emacs-mode)
;;       (vim:normal-mode)
;;       ))


;; ;;;;;;;; test
;; (defvar emacs-unbind-global-keys-list '()
;;   "A list to store unbind keyboardshortcuts in `current-global-moa' and other maps.
;;    Each item looks like '(MAP KEY OLD-COMMAND).")

;; (defun emacs-unset-global-key (map key-s)
;;   "Sets to nil the associated command for the specified key in specified map.
;;    It is like:

;;        \(define-key map (kbd key-s) nil))
;;    But it saves the old command associated with the specified key, so we can restore it
;;    when function `emacs-resotre-global-keys' called."
;;   (let (key oldcmd)
;;     (setq key (edmacro-parse-keys key-s))
;;     ;; get the old command associated with this key
;;     (setq oldcmd (lookup-key map key))
;;     ;; saves that shortcut in emacs-overridden-global-keys
;;     (if oldcmd
;;	(add-to-list 'emacs-unbind-global-keys-list (cons map (cons key-s (cons oldcmd nil)))))
;;     ;; redefine the key in the emacs-keymap
;;     (define-key map key nil)
;;     )
;;   )

;; (defun emacs-resotre-global-keys ()
;;   "Restore all keyboard shortcuts that were overitten by `emacs-unbind-global-key-list'."
;;   (mapc (lambda (x)
;;	  (define-key
;;	    (car x)
;;	    (edmacro-parse-keys (car (cdr x)))
;;	    (car (cdr (cdr x))))
;;	  )
;;	emacs-unbind-global-keys-list)
;;   ;; clear the list
;;   (setq emacs-unbind-global-keys-list '())
;;   )

;; (emacs-unset-global-key (current-global-map) "C-c")



(provide 'coldnew-terminal)
;; coldnew-terminal.el ends here.
