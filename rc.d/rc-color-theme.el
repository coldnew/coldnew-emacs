;;;; initial color theme
(provide 'rc-color-theme)


(when (require 'color-theme nil 'noerror)

  (color-theme-initialize)
  (setq color-theme-is-global t)
  (setq color-theme-is-cumulative t)
  (setq color-theme-load-all-themes nil))

;;;; Use coldnew's personal theme default
(when (require 'color-theme-coldnew-night nil 'noerror)
  (eval-after-load "color-theme" '(color-theme-coldnew-night)))
