;;;;;;;; ECB
;; ECB stands for "Emacs Code Browser". While Emacs already has good editing support for many modes,
;; its browsing support is somewhat lacking. That's where ECB comes in: it displays a number of
;; informational windows that allow for easy source code navigation and overview.
;;
(eval-when-compile (require 'cl))

;;;;;;;; Packages Import
(require 'coldnew-macro)
(require 'coldnew-functions)
(require 'coldnew-commands)
(require 'coldnew-variables)

(require* 'ecb)


(setq stack-trace-on-error t)

;; Disable ecb tips-of-day
(setq ecb-tip-of-the-day nil)

(setq ecb-expand-methods-switch-off-auto-expand t)

(add-hook 'ecb-deactivate-hook
	  (lambda () (modify-all-frames-parameters '((width . 80)))))
;; resize the ECB window to be default (order matters here)
(add-hook 'ecb-activate-hook (lambda () (ecb-redraw-layout)))
(add-hook 'ecb-activate-hook
	  (lambda () (modify-all-frames-parameters '((width . 120)))))



(provide 'coldnew-ecb)
;; coldnew-ecb.el ends here.
