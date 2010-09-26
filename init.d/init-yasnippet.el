;; init yasnippet
(provide 'init-yasnippet)

(setq yasnippet-dir "~/.emacs.d/etc/yasnippet/snippets")

(when (require 'yasnippet nit 'noerror)
  (yas/initialize)
  (yas/load-directory yasnippet-dir)
  (when (require 'dropdown-list nil 'noerror)
    (setq yas/prompt-functions '(yas/dropdown-prompt
				 yas/ido-prompt
				 yas/completing-prompt)))

  ;; hook for automatic reloading of changed snippets
  (defun local/update-yasnippets-on-save ()
    (when (string-match "/yasnippet/snippets" buffer-file-name)
      (yas/load-directory yasnippet-dir)))
  (add-hook 'after-save-hook 'local/update-yasnippets-on-save))
