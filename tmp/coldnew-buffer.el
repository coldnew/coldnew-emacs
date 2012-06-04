;;; coldnew-buffer.el ---
(eval-when-compile (require 'cl))

;;;; ---------------------------------------------------------------------------
;;;; *scratch*
;;;; ---------------------------------------------------------------------------

;; if *scratch* buffer does not exist, creat it automatically
(run-with-idle-timer 1 t
		     '(lambda () (get-buffer-create "*scratch*")))

;;;; ---------------------------------------------------------------------------
;;;; Uniquify
;;;; ---------------------------------------------------------------------------
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "/")
;; rename after killing uniquified
(setq uniquify-after-kill-buffer-p t)
;; don't muck with special buffers
(setq uniquify-ignore-buffers-re "^\\*")

;;;; ---------------------------------------------------------------------------
;;;; tempbuf
;;;; ---------------------------------------------------------------------------
(require 'tempbuf)
;; Take following mode as temp buffer
(let ((tempbuf-mode-list
       '(custom-mode
	 w3-mode
	 w3m-mode
	 Man-mode
	 woman-mode
	 help-mode
	 view-mode
	 dired-mode
	 )))
  (dolist (tempbuf-hook tempbuf-mode-list)
    (add-hook (intern (concat (symbol-name tempbuf-hook) "-hook")) 'turn-on-tempbuf-mode)))

;;;; ---------------------------------------------------------------------------
;;;; midnight
;;;; ---------------------------------------------------------------------------
(require 'midnight)

(defvar clean-buffer-delay-time (* 5 8 3600)
  "Every delay time will clean buffer.")

;; Clean the buffer-list every 8hr
(setq clean-buffer-list-delay-special clean-buffer-delay-time)

;; Kill anything, clean-buffer-list is very intelligent
;; at not killing unsaved buffer.
(setq clean-buffer-list-kill-regexps '("^.*$"))

;; After clean-buffer-delay-time, clean the buffer.
(run-at-time t clean-buffer-delay-time 'clean-buffer-list)

;; keep these buffer untouched prevent append multiple times
(defvar clean-buffer-list-kill-never-buffer-names-init
  clean-buffer-list-kill-never-buffer-names
  "Init value for clean-buffer-list-kill-never-buffer-names")

;; append to *-init instead of itself
(setq clean-buffer-list-kill-never-buffer-names
      (append
       '("*Messages*" "*cmd*" "*scratch*" "*w3m*" "*w3m-cache*" "*Inferior Octave*")
       clean-buffer-list-kill-never-buffer-names-init))

;; Prevent to kill buffers if match rules.
(defvar clean-buffer-list-kill-never-regexps-init
  clean-buffer-list-kill-never-regexps
  "Init value for clean-buffer-list-kill-never-regexps")

;; append to *-init instead of itself
(setq clean-buffer-list-kill-never-regexps
      (append
       '("^\\*EMMS Playlist\\*.*$"
	 ".*irc\\.freenode\\.net.*"
	 ".*irc\\.debian\\.org.*"
	 ".*im\\.bitlbee\\.org.*"
	 "^\\*ansi-term*"
	 "^\\*terminal*"
	 "^\\*Inferior*"
	 )))

;;;; ---------------------------------------------------------------------------
;;;; iBuffer
;;;; ---------------------------------------------------------------------------






(provide 'coldnew-buffer)
;; coldnew-buffer.el ends here.
