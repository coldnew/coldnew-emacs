;;
(eval-when-compile (require 'cl))

;; set mmm-mode to load when necessary
(setq mmm-global-mode 'maybe)

;; Amount of coloring to use in submode regions
(setq mmm-submode-decoration-level 2)
;; define submode
(mmm-add-classes
 '(
   ;; HTML + PHP
   (html-php
    :submode php-mode
    :front "<\\?\\(php\\)?"
    :back "\\?>")
   ;; HTML + CSS
   (html-css
    :submode css-mode
    :front "<style [^>]*>?"
    :back "</style>?")
   ;; HTML + JavaScript
   (html-js
    :submode js2-mode
    :front "<script [^>]*>?"
    :back "</script>?")
   ))

;; (mmm-add-mode-ext-class 'html-helper-mode "\\.s?html?\\'" 'html-js)
;; (mmm-add-mode-ext-class 'html-helper-mode "\\.s?html?\\'" 'html-css)
;; (mmm-add-mode-ext-class 'html-helper-mode "\\.ctp?\\'" 'html-php)
;; (mmm-add-mode-ext-class 'html-helper-mode "\\.ctp?\\'" 'html-css)
;; (mmm-add-mode-ext-class 'html-helper-mode "\\.ctp?\\'" 'html-js)




(provide '047-mmm-mode)
;; 047-mmm-mode.el ends here.
