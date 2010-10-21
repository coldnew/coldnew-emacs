

(provide 'rc-ecb)

(when (require 'ecb nil 'noerror)
  (setq ecb-tip-of-the-day nil)
  (setq ecb-version-check nil)
  (setq ecb-options-version "2.40")
  (ecb-activate)

  ;;click mouse 1 to select ecb tree
  (setq ecb-primary-secondary-mouse-buttons 'mouse-1--C-mouse-1)
  (ecb-layout-define "left19" left nil
		     (ecb-set-directories-buffer)
		     (ecb-split-ver 0.4 t)
		     (other-window 1)
		     (ecb-set-methods-buffer)
		     (ecb-split-ver 0.5 t)
		     (other-window 1)
		     (ecb-set-analyse-buffer)
		     (ecb-split-ver 0.6 t)
		     (other-window 1)
		     (ecb-set-sources-buffer)
		     (select-window (next-window)))
  )
