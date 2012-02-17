;;
(eval-when-compile (require 'cl))

;;;;;;;; Packages Import
(require 'coldnew-macro)
(require 'coldnew-functions)
(require 'coldnew-commands)
(require 'coldnew-variables)
(require 'coldnew-evil)

(require 'woman)
(require 'info)


;;;;;;;; Woman
(setq woman-cache-filename (concat emacs-cache-dir "woman.cache"))
(setq woman-use-topic-at-point nil)
;; Colorful fonts
(setq woman-fontify t)
(setq woman-fill-column 100)

;;;;;;;; Info
;; Keybindings
(add-hook 'Info-mode-hook
          '(lambda ()
             (define-key evil-normal-state-local-map (kbd "j")  'Info-next)
             (define-key evil-normal-state-local-map (kbd "k") 'Info-prev)
             (define-key evil-normal-state-local-map (kbd "C-f") 'Info-scroll-up)
             (define-key evil-normal-state-local-map (kbd "C-b") 'Info-scroll-down)
             ))

(provide 'coldnew-help)
;; coldnew-help.el ends here.
