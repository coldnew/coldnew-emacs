;;; Code:

(provide 'init-auto-complete)
(eval-when-compile
  (require 'cl))

(when (require 'auto-complete)
  (require 'auto-complete-config)

  (ac-config-default)
  (setq ac-use-fuzzy      nil )
  (setq ac-auto-start     t)
  (setq ac-ignore-case 'smart )
  (setq ac-comphist-file "~/.emacs.d/system/auto-complete.data")

  (setq ac-use-menu-map     t )
  (define-key ac-menu-map (kbd "C-n") 'ac-next)
  (define-key ac-menu-map "\C-p" 'ac-previous)
  (define-key ac-completing-map "\t" 'ac-complete)

  (define-key ac-mode-map (kbd "C-n") 'auto-complete)
  (define-key ac-mode-map (kbd "C-p") 'auto-complete)

  )

;;; init-auto-complete.el ends here
