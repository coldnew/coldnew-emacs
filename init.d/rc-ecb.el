

(provide 'rc-ecb)

(when (require 'ecb nil 'noerror)
  (setq ecb-options-version "2.40")
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
