;;; Add all modules to path
;; This file will be load in my emacs config.

;; libvterm
(add-to-list 'load-path
	     (concat user-modules-directory "libvterm"))
(let (vterm-install)
  (require 'vterm))