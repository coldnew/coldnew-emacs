;;; coldnew-terminal.el --- default setting for terminal or shell
(eval-when-compile (require 'cl))

;;;; ---------------------------------------------------------------------------
;;;; ansi-color
;;;; ---------------------------------------------------------------------------
(require 'ansi-color)
;; reset ansi-color
(setq ansi-color-names-vector
      (vector (frame-parameter nil 'foreground-color)
          "#0B0B0E" "#CA3839" "#8ae234" "#edd400"
          "#729fcf" "#ad7fa8" "cyan3"   "#DCDCDC")
      ansi-term-color-vector ansi-color-names-vector
      ansi-color-map (ansi-color-make-color-map))

;;;; ---------------------------------------------------------------------------
;;;; shell-pop
;;;; ---------------------------------------------------------------------------
(require 'shell-pop)
;;(shell-pop-set-internal-mode "ansi-term")
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

;;;; ---------------------------------------------------------------------------
;;;; multi-term
;;;; ---------------------------------------------------------------------------
(require 'multi-term)
(setq multi-term-program emacs-default-shell)


;;;; ---------------------------------------------------------------------------
;;;; term
;;;; ---------------------------------------------------------------------------
(require 'term)
;; ;; Remove term-mode default color
(setq-default term-default-bg-color nil)
(setq-default term-default-fg-color nil)

;;;; keybindings
;; (define-key term-raw-map (kbd "<f4>") 'shell-pop)
(define-key term-raw-map (kbd "M-x") 'execute-extended-command)
(define-key term-raw-map (kbd "C-g") 'term-interrupt-subjob)
(define-key term-raw-map (kbd "C-n") 'term-send-down)
(define-key term-raw-map (kbd "C-p") 'term-send-up)
(define-key term-raw-map (kbd "<enter>") 'term-send-input)

;;;; ---------------------------------------------------------------------------
;;;; comint
;;;; ---------------------------------------------------------------------------
(require 'comint)
;; Do not show password in comint-mode
(setq comint-output-filter-functions  '(comint-watch-for-password-prompt))
(setq comint-password-prompt-regexp
      "\\(\\([Oo]ld \\|[Nn]ew \\|^\\)[Pp]assword\\|Enter password\\):\\s *\\'")

;;;; Keybindings
(define-key comint-mode-map (kbd "C-g") 'comint-interrupt-subjob)



(provide 'coldnew-terminal)
;; coldnew-terminal.el ends here.
