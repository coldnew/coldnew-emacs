
(eval-when-compile (require 'cl))

;;;;;;;; Packages Import
(require 'coldnew-macro)
(require 'coldnew-functions)
(require 'coldnew-commands)
(require 'coldnew-variables)

(when (require* 'anything-config)


  (setq anything-quick-update t)

  (setq anything-candidate-number-limit 1000)

  ;; Path of anything's history
  (setq anything-c-adaptive-history-file (concat emacs-cache-dir "anything.cache"))
  ;; Maximum number of candidates stored for a source.
  (setq anything-c-adaptive-history-length 300)
  ;; (setq anything-c-boring-buffer-regexp
  ;;	(concat anything-c-boring-buffer-regexp "\\|\\*tramp/sudo "))

  (setq anything-c-boring-file-regexp
	(rx (or
	     ;; Boring directories
	     (and "/" (or ".svn" "CVS" "_darcs" ".git" ".hg") (or "/" eol))
	     ;; Boring files
	     (and line-start  ".#")
	     (and (or ".class" ".la" ".o" "~" ".elc" ".out" ".swp") eol))))


  ;; FIXME: Why follwoing can't use in lusty-file-explorer only?
  ;;  it returns error messages as following:
  ;;  return-from: No catch for tag: --cl-block-anything-completing-read-default--, #<buffer coldnew-dictionary.el>
  (add-to-list 'anything-completing-read-handlers-alist '(lusty-file-explorer . nil))
  ;; NOTE: temporarily use follwoing to fix problem = =
  ;; (defadvice lusty-file-explorer (around lusty-buffer-explorer activate)
  ;;   "temporary fix ecb bug"
  ;;   (ac-mode -1)
  ;;   ad-do-it
  ;;   (ac-mode 1))

  (anything-completion-mode)

  (setq anything-sources
	(list
	 anything-c-source-emacs-commands
	 anything-c-source-buffers
	 anything-c-source-files-in-current-dir
	 anything-c-source-recentf
	 anything-c-source-info-pages
	 ;; anything-c-source-include
	 ))


  (setq anything-for-files-prefered-list
	'(anything-c-source-ffap-line
	  anything-c-source-ffap-guesser
	  anything-c-source-recentf
	  anything-c-source-bookmarks
	  anything-c-source-file-cache
	  anything-c-source-files-in-current-dir+
	  anything-c-source-locate))


  )

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
