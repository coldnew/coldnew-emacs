;;
(eval-when-compile (require 'cl))

;; do not start flyspell at startup
(flyspell-mode nil)

;; use hunspell instead ispell
(setq-default ispell-program-name "hunspell")

;;(setq ispell-personal-dictionary "~/.ispell-dico-perso")
(setq ispell-dictionary-alist
      '(
	;; default is en_US
	(nil
	 "[A-Za-z]" "[^A-Za-z]" "[']" t ("-d" "/usr/share/myspell/en_US" ) nil utf-8)
	))

(setq flyspell-issue-message-flag nil)

;; don't consider that a word repeated twice is an error
(setq flyspell-mark-duplications-flag nil)

;; enable the likeness criteria
(setq flyspell-sort-corrections nil)

;; don't use `M-TAB' to correct word (only use `C-.')
(setq flyspell-use-meta-tab nil)

;; dash character (`-') is considered as a word delimiter
(setq flyspell-consider-dash-as-word-delimiter-flag t)

;; Hook
(add-hook 'flyspell-mode-hook '(lambda () (auto-dictionary-mode 1)))


(provide '038-flyspell)
;; 038-flyspell.el ends here.
