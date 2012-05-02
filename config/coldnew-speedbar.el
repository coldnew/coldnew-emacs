;;; coldnew-speedbar.el ---
(eval-when-compile (require 'cl))

(require 'speedbar)
(setq speedbar-use-images nil)


(require 'sr-speedbar)
(setq sr-speedbar-right-side nil)
(setq sr-speedbar-refresh-turn-on t)

(require* 'nav)

(provide 'coldnew-speedbar)
;; coldnew-speedbar.el ends here.
