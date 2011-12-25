;;
(eval-when-compile (require 'cl))

;;;;;;;; Packages Import
(require 'coldnew-macro)
(require 'coldnew-functions)
(require 'coldnew-variables)
(require 'erc)


;;;;;;;; User Config
(setq erc-prompt ">>")
(setq erc-autojoin-mode)
(setq erc-timestamp-format "%H:%M ")
(setq erc-insert-timestamp-function 'erc-insert-timestamp-right)

(setq erc-log-channels t)
(setq erc-log-mode t)
(setq erc-page-mode t)
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
				"324" "329" "332" "333" "353" "477"))
(setq erc-log-channels-directory (concat emacs-log-dir "erc"))


;; Join following channels whenever connecting to Freenode.
(setq erc-autojoin-channels-alist '(("irc.freenode.net" "#emacs" "#gentoo" "##linux")
				    ("irc.debian.org" "#dot")
				    ))

;; Interpret mIRC-style color commands in IRC chats
(setq erc-interpret-mirc-color t)


(require 'erc-services)
(erc-services-mode 1)

(setq erc-prompt-for-nickserv-password nil)


(provide 'coldnew-irc)
;; coldnew-irc.el ends here.
