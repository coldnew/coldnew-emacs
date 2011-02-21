;;
(eval-when-compile (require 'cl))

;;;;;; Settings
;; for debugging.
(setq rcirc-debug-flag t)
;; Include date in time stamp.
(setq rcirc-time-format "%Y-%m-%d %H:%M ")
;; Enable Logging rcirc
(setq rcirc-log-flag t)
;; Put logs in this directory
(setq rcirc-log-directory "~/.emacs.d/var/log/rcirc/")
;;
(setq rcirc-notify-open t)
;;
(setq rcirc-notify-timeout 1)
;;
(setq rcirc-omit-responses '("JOIN" "PART" "QUIT" "NICK" "AWAY" "MODE"))
;; Prompt Settings
(setq rcirc-prompt " %n  >> ")
;; Do not show on modline
(setq rcirc-track-minor-mode nil)

;; Join these channels at startup.
(setq rcirc-server-alist
      '(
	("irc.freenode.net" :channels  ("#emacs" "#lisp" "##linux" "#gentoo"	; english channel
					"#tossug" "#gentoo-tw" "#python.tw" "#ubuntu-tw"))
	("irc.debian.org" :channels ("#dot"))
	("im.bitlbee.org")
	))

;;;;;; Hooks
(add-hook 'rcirc-mode-hook
	  '(lambda ()
	     ;; Keep input line at bottom.
	     (set (make-local-variable 'scroll-conservatively) 8192)
	     ;; Use Omit-mode
	     (rcirc-omit-mode)
	     ))

;;;;;; Keybindings
(add-hook 'rcirc-mode-hook
	  '(lambda ()
	     ;; Insert map
	     (vim:local-imap (kbd "RET") 'rcirc-send-input)
	     ))



;;;;;; RCIRC Commands
(defun-rcirc-command all (input)
  "Run the arguments as a command for all connections.
   Example use: /all away food or /all quit zzzz."
  (interactive "s")
  (let ((buffers (mapcar 'process-buffer (rcirc-process-list))))
    (dolist (buf buffers)
      (with-current-buffer buf
	(goto-char (point-max))
	(insert "/" input)
	(rcirc-send-input)))))

(defun-rcirc-command calc (input)
  "calculate value of some expression using bc"
  (let ((expr (mapconcat 'identity args " ")))
    (when (length expr)
      (let ((result (shell-command-to-string (concat "echo '" expr "' | bc ")))
	    (when result (rcirc-send-message (concat expr " = " result))))))))


;;;;;; Other Settings

;;;; Don't print /away messages.
(defun rcirc-handler-301 (process cmd sender args)
  "/away message handler.")


;;;;;; Functions


(provide '035-rcirc)
;; 035-rcirc.el ends here.
