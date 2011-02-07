;;
(eval-when-compile (require 'cl))


;; enable ido-mode
(ido-mode t)

(setq ido-default-file-method 'samewindow)
(setq ido-default-buffer-method 'samewindow)
(setq ido-enable-prefix nil)
(setq ido-enable-case nil)



(provide '049-ido)
;; 049-ido.el ends here.
