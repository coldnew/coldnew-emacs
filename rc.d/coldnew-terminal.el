;;
(eval-when-compile (require 'cl))

;;;;;;;; Packages Import
(require 'coldnew-macro)
(require 'coldnew-functions)
(require 'coldnew-commands)
(require 'coldnew-variables)
(require 'coldnew-evil)

;;;;;;;; Ansi-Color
;; reset ansi-color to match with my theme
;; default is [ "black" "red" "green" yellow" "blue" "magenta" "cyan" "white"]
;;
(when (require* 'ansi-color)
  (setq ansi-color-names-vector
	(vector (frame-parameter nil 'foreground-color)
		"#0B0B0E" "#CA3839" "#8ae234" "#edd400"
		"#729fcf" "#ad7fa8" "cyan3"   "#DCDCDC")
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
  (shell-pop-set-window-position emacs-popup-shell-window-position))

;;;;;;;; Multi-term
;;
(when (require* 'multi-term)
  (setq multi-term-program emacs-default-shell))

;;;;;;;; Term
;;
(when (require* 'term)
  ;; Remove term-mode default color
  (setq-default term-default-bg-color nil)
  (setq-default term-default-fg-color nil)

  ;; (setq term-unbind-key-list '("C-c" "C-h"))
  ;; (add-to-list 'term-bind-key-alist '("C-c" . term-interrupt-subjob))
  ;; (setq term-bind-key-alist (delete '("C-c C-c" . term-interrupt-subjob)
  ;;                    term-bind-key-alist))

  ;;;; Keybindings
  ;; (add-hook 'term-mode-hook
  ;;        '(lambda ()
  ;; Add new key-map
  (define-key term-raw-map (kbd "<f4>") 'shell-pop)
  (define-key term-raw-map (kbd "M-x") 'anything-M-x)
  (define-key term-raw-map (kbd "C-g") 'term-interrupt-subjob)
  (define-key term-raw-map (kbd "C-n") 'term-send-down)
  (define-key term-raw-map (kbd "C-p") 'term-send-up)
  (define-key term-raw-map (kbd "<enter>") 'term-send-input)
  ;;           (define-key term-raw-map (kbd "<return>") 'term-send-raw)
  ;; ))
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
  ;; (when (featurep 'evil)
  ;;   ;; Normal map
  ;;   (evil-define-key 'normal comint-mode-map (kbd "M-k") 'comint-previous-input)
  ;;   (evil-define-key 'normal comint-mode-map (kbd "M-j") 'comint-previous-input)
  ;;   (evil-define-key 'normal comint-mode-map (kbd "C-c C-r") 'comint-clear-region)
  ;;   ;; Insert map
  ;;   (evil-define-key 'insert comint-mode-map (kbd "RET") 'newline-and-indent)
  ;;   )

  ;;;; Functions
  (defun comint-clear-region ()
    (interactive)
    (delete-region (point-min) (point-max))
    (comint-send-input))
  )


;;;;;;;; Functions





(provide 'coldnew-terminal)
;; coldnew-terminal.el ends here.
