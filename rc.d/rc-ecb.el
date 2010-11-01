
(provide 'rc-ecb)

(when (require 'ecb nil 'noerror)
  (setq ecb-tip-of-the-day nil)
  (setq ecb-version-check nil)
  (setq ecb-options-version "2.40")
  (ecb-activate)

  ;;click mouse 1 to select ecb tree
  (setq ecb-primary-secondary-mouse-buttons 'mouse-1--C-mouse-1)

  ;;  (setq ecb-source-file-regexps '("*.elc$"))
  ;; 	'((".*"
  ;; 	   "\\(^\\(\\.\\|#\\)\\|\\(~$\\|\\.\\(elc\\|obj\\|o\\|class\\|lib\\|dll\\|a\\|so\\|cache\\)$\\)\\)"
  ;; 	   "^\\.\\(emacs\\|gnus\\)$")))
  ;;  (setq ecb-source-path '("~"))
  (setq ecb-auto-expand-directory-tree 'best)
  (setq ecb-auto-update-methods-after-save t)

  ;;TODO:??
  (setq ecb-directories-buffer-name " *ECB Directories*")

  (ecb-layout-define "ecb-default-layout"    left nil
		     (ecb-split-ver 0.7 t)
		     (ecb-set-directories-buffer)
		     (other-window    1)
		     (ecb-set-sources-buffer)
		     (select-window (next-window)))

  (ecb-layout-define "ecb-ide-common-layout" left-right nil
		     (ecb-split-ver 0.7 t)
		     (ecb-set-directories-buffer)
		     (other-window    1)
		     (ecb-set-sources-buffer)
		     (select-window (next-window))
		     (other-window  1)
		     (ecb-set-methods-buffer)
		     (ecb-split-ver 0.7 t)
		     (other-window    1)
		     (ecb-set-analyse-buffer))

  (setq ecb-layout-window-sizes '(("ecb-ide-common-layout"
				   (0.14705882352941177 . 0.68)
				   (0.14705882352941177 . 0.3)
				   (0.18823529411764706 . 0.68)
				   (0.18823529411764706 . 0.3))))

  (ecb-layout-switch "ecb-ide-common-layout")
  )
