;;;; initial color theme
(provide 'init-color-theme)


(when (require 'color-theme nil 'noerror)
  (color-theme-initialize)
  (when (require 'color-theme-coldnew-night nil 'noerror)
    ;; use coldnew's theme
    (color-theme-coldnew-night)))
