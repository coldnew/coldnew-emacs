
(eval-when-compile (require 'cl))

;;;; auto-complete.el
(global-auto-complete-mode t)		; 全域啟動自動補全
(ac-config-default)

;; Fix popup-tip's bug
(defadvice popup-tip
  (around popup-pos-tip-wrapper (string &rest args) activate)
  (if (eq window-system 'x)
      (apply 'popup-pos-tip string args)
    ad-do-it))

(setq ac-use-fuzzy      nil )		; 關閉模糊補全
(setq ac-auto-start     nil )		; 關閉自動補全
(setq ac-ignore-case 'smart )		; 使用智能補全
(setq ac-use-menu-map     t )		; 啟用補全選單
(setq ac-use-comphist     t )
(setq ac-use-quick-help   t )
(add-to-list 'ac-dictionary-directories "~/.emacs.d/etc/dict")

(setq ac-comphist-file "~/.emacs.d/var/cache/auto-complete.cache")

(setq-default ac-sources
	      '(ac-source-abbrev     ac-source-semantic   ac-source-symbols
				     ac-source-filename   ac-source-functions
				     ac-source-variables  ac-source-dictionary
				     ac-source-files-in-current-dir
				     ac-source-words-in-same-mode-buffers))

(define-key ac-menu-map (kbd "C-n") 'ac-next)
(define-key ac-menu-map (kbd "C-p") 'ac-previous)
(define-key ac-completing-map "\t" 'ac-complete)

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




(provide '026-auto-complete)
;;; 026-auto-complete.el ends here
