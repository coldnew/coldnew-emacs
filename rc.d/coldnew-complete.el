
(eval-when-compile (require 'cl))

;;;;;;;; Packages Import
(require 'coldnew-macro)
(require 'coldnew-functions)
(require 'coldnew-commands)
(require 'coldnew-variables)
(require 'coldnew-evil)



;;;;;;;; Auto Complete

(when (require* 'auto-complete)

  ;; Enabls auto-complete globally
  (global-auto-complete-mode t)

  ;; Disable fuzzy match
  (setq ac-use-fuzzy nil)

  ;; Start auto-complete after insert 2 or more key.
  (setq ac-auto-start 2)

  ;; Ignore case if completion target string doesn't include upper characters
  (setq ac-ignore-case 'smart)

  ;; Enable auto-complete quick help
  (setq ac-use-quick-help t)

  ;; After 0.01 sec, show help window
  (setq ac-quick-help-delay 0.01)

  ;; Add aditional dictionary
  (add-to-list 'ac-dictionary-directories (concat emacs-etc-dir "ac-dict"))

  ;; Enable ac-comphist
  (setq ac-use-comphist t)

  ;; Setting ac-comphist data
  (setq ac-comphist-file (concat emacs-cache-dir "auto-complete.dat"))

  ;; Show menu
  (setq ac-auto-show-menu t)

  ;; Enable ac-menu-map
  (setq ac-use-menu-map t)

  ;; Setting keybinding in ac-menu-map
  (define-key ac-menu-map (kbd "C-n") 'ac-next)
  (define-key ac-menu-map (kbd "C-p") 'ac-previous)
  (define-key ac-completing-map "\t" 'ac-complete)

  ;; BUG: nouse?
  ;;
  (when (require* 'ac-anything)
    (define-key ac-completing-map (kbd "C-o") 'ac-complete-with-anything)
    )

  ;; use default config
  (when (require* 'auto-complete-config)
    (ac-config-default))

  ;; Fix popup-tip's bug
  (when (require* 'popup-pos-tip)
    (defadvice popup-tip
      (around popup-pos-tip-wrapper (string &rest args) activate)
      (if (eq window-system 'x)
	  (apply 'popup-pos-tip string args)
	  ad-do-it)))

  ;; Default sources
  (setq-default ac-sources
		'(ac-source-dictionary
		  ac-source-abbrev
		  ac-source-semantic
		  ac-source-filename
		  ac-source-files-in-current-dir
		  ac-source-words-in-same-mode-buffers
		  ))

  ;;;;;; ac-company
  ;; Use Company Backends for Auto-Complete.
  ;;
  (when (require* 'ac-company)
    (ac-company-define-source ac-source-company-abbrev    company-abbrev   (symbol . "s"))
    (ac-company-define-source ac-source-company-clang     company-clang    (symbol . "s"))
    (ac-company-define-source ac-source-company-css       company-css      (symbol . "s"))
    (ac-company-define-source ac-source-company-dabbrev   comapny-dabbrev  (symbol . "s"))
    (ac-company-define-source ac-source-company-eclim     company-eclim    (symbol . "s"))
    (ac-company-define-source ac-source-company-elisp     company-elisp    (symbol . "s"))
    (ac-company-define-source ac-source-company-etags     company-etags    (symbol . "s"))
    (ac-company-define-source ac-source-company-files     company-files    (symbol . "s"))
    (ac-company-define-source ac-source-company-gtags     company-gtags    (symbol . "s"))
    (ac-company-define-source ac-source-company-ispell    comapny-ispell   (symbol . "s"))
    (ac-company-define-source ac-source-company-keywords  company-keywords (symbol . "s"))
    (ac-company-define-source ac-source-company-nxml      company-nxml     (symbol . "s"))
    (ac-company-define-source ac-source-company-semantic  comapny-semactic (symbol . "s"))
    )


  )




(provide 'coldnew-complete)
;; coldnew-complete.el ends here.
