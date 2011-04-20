(eval-when-compile (require 'cl))



(when (require* 'anything-startup)

  ;;
  (require* 'aything-config)

  ;;;; anything-match-plugin
  ;; Humane match plug-in for anything
  ;;
  (when (require* 'anything-match-plugin)
    )

  ;;;; anything-show-completion
  ;; Show selection in buffer for anything completion
  ;;
  (when (require* 'anything-show-completion)
    )

  ;;;; anything-include
  ;; For C C++, Anything-source made maintenance of history of #include and reusable.
  ;;
  (when (require* 'anything-include)
    (setq anything-include-save-file (concat emacs-cache-dir "anything-include.dat"))
    (setq anything-include-max-saved-items 300)
    )
  ;;;; anything-complete
  ;; Complete with anything
  ;;
  (when (require* 'anything-complete)
    ;; Automatically collect symbols by 100 secs
    (anything-lisp-complete-symbol-set-timer 100)
    ;; Replace completion commands with anything
    (anything-read-string-mode 1)
    )


  ;;;; Settings

  (setq anything-quick-update t)

  (setq anything-candidate-number-limit 1000)

  (setq anything-c-yas-display-key-on-candidate t)


  ;; (setq anything-sources
  ;;       (list anything-c-source-buffers
  ;;	    anything-c-source-files-in-current-dir
  ;;	    anything-c-source-recentf
  ;;	    anything-c-source-emacs-commands
  ;;	    anything-c-source-info-pages
  ;;	    anything-c-source-include
  ;;	    ))


  ;;;; Functions



  )





(provide 'coldnew-anything)
;; coldnew-anything.el ends here.
