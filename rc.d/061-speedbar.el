;;
(eval-when-compile (require 'cl))



;; Make speedbar appear at left side
(setq sr-speedbar-right-side nil)

;; BUG: can't use
(vim:emap "sb" 'sr-speedbar-toggle)
(vim:emap "speedbar" 'sr-speedbar-toggle)



(provide '061-speedbar)
;; 061-speedbar.el ends here.
