
(eval-when-compile (require 'cl))

;;;;;;;; Packages Import
(require 'coldnew-macro)
(require 'coldnew-functions)
(require 'coldnew-commands)
(require 'coldnew-variables)

;;;;;;;; Loding Libraries
(require 'anything-config)
(require 'anything-match-plugin)
(require* 'linkd)


;; (setq anything-mp-matching-method 'multi2)
;;  (setq anything-mp-highlight-threshold 10)
;; Predefined configurations for `anything.el'
(setq anything-config t)

(ac-mode 1)
;; Path of file where history information is stored.
(setq anything-c-adaptive-history-file (concat emacs-cache-dir "anything.cache"))

;; Maximum number of candidates stored for a source.
(setq anything-c-adaptive-history-length 100)

;; The regexp that match boring buffers.
(setq anything-c-boring-buffer-regexp
      (concat anything-c-boring-buffer-regexp "\\|*tramp/sudo "))

;; The regexp that match boring files.
(setq anything-c-boring-file-regexp
      (concat anything-c-boring-file-regexp "\\|.out"))

;; *Minimum length to be listed by `anything-c-source-kill-ring'
(setq anything-kill-ring-threshold 20)

;; What command to use for root access.
(setq anything-su-or-sudo "sudo")

;; Default external file browser for your system.
(setq anything-c-default-external-file-browser "dolphin")

;; Wheter to use or not adaptative sorting.
(setq anything-c-use-adaptative-sorting t)

;; Whether Prompt or not when creating new file.
(setq anything-ff-newfile-prompt-p t)

;; Minimal list of compressed files extension.
(setq anything-ff-file-compressed-list '("gz" "bz2" "zip" "7z" "rar"))

;; Eldoc will show info in mode-line during this delay if user is idle.
(setq anything-c-show-info-in-mode-line-delay 5)

;; Value of requires-pattern for `anything-M-x'
(setq anything-M-x-requires-pattern 0)




(provide 'coldnew-anything)
;; coldnew-anything.el ends here.
