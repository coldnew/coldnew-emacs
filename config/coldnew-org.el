;;; coldnew-org.el --- org-mode config.
(eval-when-compile (require 'cl))

(require 'org-install)

;;;;;;;; org-mode extensions
(add-to-list 'auto-mode-alist '("\\.txt$" . org-mode))
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-to-list 'auto-mode-alist '("\\.org_archive$" . org-mode))




(provide 'coldnew-org)
;; coldnew-org.el ends here.
