
(eval-when-compile (require 'cl))

;;;;;;;; Packages Import
(require 'coldnew-macro)
(require 'coldnew-functions)
(require 'coldnew-commands)
(require 'coldnew-variables)
(require* 'linkd)


(when (require* 'anything-config)

  ;; Predefined configurations for `anything.el'
  (setq anything-config t)

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

  )


;; Make lusty-explorer use it's own completion, not anything-completion
(when (require* 'lusty-explorer)
  (add-to-list 'anything-completing-read-handlers-alist '(lusty-file-explorer . nil))
  (add-to-list 'anything-completing-read-handlers-alist '(lusty-buffer-explorer . nil)))




;; (setq anything-candidate-number-limit 1000)

;; (setq anything-quick-update t)


;; Path of anything's history

;; Maximum number of candidates stored for a source.

;; (anything-completion-mode)


;; (setq anything-sources
;;       (list
;;        anything-c-source-emacs-commands
;;        anything-c-source-buffers
;;        anything-c-source-files-in-current-dir
;;        anything-c-source-recentf
;;        anything-c-source-info-pages
;;        ;; anything-c-source-include
;;        ))



  ;;;; anything-match-plugin
;; Humane match plug-in for anything
;;
;;(when (require* 'anything-match-plugin)
					;)

  ;;;; anything-show-completion
;; Show selection in buffer for anything completion
;;
;;(when (require* 'anything-show-completion)
					;)

  ;;;; anything-include
;; For C C++, Anything-source made maintenance of history of #include and reusable.
;;
;;(when (require* 'anything-include)
;;(setq anything-include-save-file (concat emacs-cache-dir "anything-include.dat"))
;;(setq anything-include-max-saved-items 300)
;;)
  ;;;; anything-complete
;; Complete with anything
;;
;;(when (require* 'anything-complete)
;; Automatically collect symbols by 100 secs
;;(anything-lisp-complete-symbol-set-timer 100)
;; Replace completion commands with anything
;;(anything-read-string-mode 1)
;;)


  ;;;; Settings



;;(setq anything-c-yas-display-key-on-candidate t)






(provide 'coldnew-anything)
;; coldnew-anything.el ends here.
