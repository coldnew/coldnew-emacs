;;
(eval-when-compile (require 'cl))

;;;;;;;; Packages Import
(require 'coldnew-editor)
(require 'cc-mode)

;;;;;;;; spice-mode extensions
(add-to-list 'auto-mode-alist '("\\.sp\\'"  . spice-mode))
(add-to-list 'auto-mode-alist '("\\.spice\\'"  . spice-mode))


;; Use ng-spice-rework as spice simulator
(setq spice-simulator "ngspice")
;; Empty initial file
(setq spice-initialize-empty-file t)
;; Highlight keywords
(setq spice-highlight-keywords t)
;; Describe mode at startup
(setq spice-show-describe-mode t)
;; Use gwave as Waveform viewer
(setq spice-waveform-viewer "gwave")





(provide 'coldnew-lang-spice)
;; coldnew-lang-spice.el ends here.
