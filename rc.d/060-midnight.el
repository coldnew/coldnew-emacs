
;;;; Auto kill buffers not active for more than 15 minutes every 2 hour
(setq clean-buffer-list-delay-special 900)

(defvar clean-buffer-list-timer nil
  "Stores clean-buffer-list timer if there is one.
   You can disable clean-buffer-list by (cancel-timer clean-buffer-list-timer).")

(setq clean-buffer-list-timer (run-at-time t 7200 'clean-buffer-list))

;; kill everything, clean-buffer-list is very intelligent at not killing unsaved buffer.
(setq clean-buffer-list-kill-regexps
      '(".*"))

;; keep these buffer untouched prevent append multiple times
(defvar clean-buffer-list-kill-never-buffer-names-init
  clean-buffer-list-kill-never-buffer-names
  "Init value for clean-buffer-list-kill-never-buffer-names")
(setq clean-buffer-list-kill-never-buffer-names
      (append
       '("*Messages*" "*cmd*" "*scratch*" "*w3m*" "*w3m-cache*" "*Inferior Octave*")
       clean-buffer-list-kill-never-buffer-names-init))

;; prevent append multiple times
(defvar clean-buffer-list-kill-never-regexps-init
  clean-buffer-list-kill-never-regexps
  "Init value for clean-buffer-list-kill-never-regexps")
;; append to *-init instead of itself
(setq clean-buffer-list-kill-never-regexps
      (append '("^\\*EMMS Playlist\\*.*$"
		".*irc\\.freenode\\.net.*"
		".*irc\\.debian\\.org.*"
		".*im\\.bitlbee\\.org.*"
		"^\\*ansi-term*"
		"^\\*terminal*"))
      clean-buffer-list-kill-never-regexps-init)


(provide '060-midnight)
;; 060-midnight.el ends here.
