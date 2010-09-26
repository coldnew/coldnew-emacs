;;; Code:

(provide 'init-auto-complete)
(eval-when-compile
  (require 'cl))

(when (require 'auto-complete nil 'noerror)

  (when (require 'auto-complete-config nil 'noerror)
    ;; (require 'auto-complete-config)
    (ac-config-default))

  (setq ac-use-fuzzy      nil )
  (setq ac-auto-start     nil )		; 關閉自動補全
  (setq ac-ignore-case 'smart )		; 使用智能補全
  (setq ac-use-comphist     t )
  (setq ac-comphist-file "~/.emacs.d/var/cache/auto-complete.cache")

  (setq ac-use-menu-map     t )		; 啟用補全選單

  (define-key ac-menu-map (kbd "C-n") 'ac-next)
  (define-key ac-menu-map (kbd "C-p") 'ac-previous)
  (define-key ac-completing-map "\t" 'ac-complete)

  (when (require 'init-vim nil 'noerror)
    (vim:imap (kbd "C-p") 'auto-complete)
    (vim:imap (kbd "C-n") 'auto-complete))

  (when (require 'ac-company nil 'noerror)
    (ac-company-define-source ac-source-company-abbrev     company-abbrev)
    (ac-company-define-source ac-source-company-clang      company-clang)
    (ac-company-define-source ac-source-company-css        company-css)
    (ac-company-define-source ac-source-company-dabbrev    comapny-dabbrev)
    (ac-company-define-source ac-source-company-eclim      company-eclim)
    (ac-company-define-source ac-source-company-elisp      company-elisp)
    (ac-company-define-source ac-source-company-etags      company-etags)
    (ac-company-define-source ac-source-company-files      company-files)
    (ac-company-define-source ac-source-company-gtags      company-gtags)
    (ac-company-define-source ac-source-company-ispell     comapny-ispell)
    (ac-company-define-source ac-source-company-keywords   company-keywords)
    (ac-company-define-source ac-source-company-nxml       company-nxml)
    (ac-company-define-source ac-source-company-semantic   comapny-semactic))


  )

;;; init-auto-complete.el ends here
