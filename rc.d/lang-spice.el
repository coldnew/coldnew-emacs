;;
(eval-when-compile (require 'cl))

;;;;;;;; Variable Setting
;; Use ng-spice-rework as spice simulator
(setq spice-simulator "ngspice")
;; Empty initial file
(setq spice-initialize-empty-file t)
;; Highlight keywords
(setq spice-highlight-keywords t)
;; Describe mode at startup
(setq spice-show-describe-mode t)



;;;;;;;; Auto-mode
(add-to-list 'auto-mode-alist '("\\.cir\\'" . spice-mode))
(add-to-list 'auto-mode-alist '("\\.sp\\'"  . spice-mode))
(add-to-list 'auto-mode-alist '("\\.spice\\'"  . spice-mode))



(provide 'lang-spice)
;; lang-spice.el ends here.
