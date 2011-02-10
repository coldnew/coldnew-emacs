;;
(eval-when-compile (require 'cl))


;; enable ido-mode
(ido-mode t)

(setq ido-default-file-method 'samewindow)
(setq ido-default-buffer-method 'samewindow)
(setq ido-save-directory-list-file "~/.emacs.d/var/cache/ido.cache")
(setq ido-create-new-buffer 'always)

;; Ignore following buffer in ido
(setq ido-ignore-buffers
      '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido" "^\*trace"
	"^\*compilation" "^\*GTAGS" "^session\.*" "^\*"))
(setq ido-use-filename-at-point nil)
(setq ido-use-url-at-point nil)
(setq ido-enable-flex-matching nil)
(setq ido-max-prospects 8)
(setq ido-enable-prefix nil)
(setq ido-enable-case nil)

;; when using ido, the confirmation is rather annoying...
(setq confirm-nonexistent-file-or-buffer nil)

;; increase minibuffer size when ido completion is active
(add-hook 'ido-minibuffer-setup-hook
	  (function
	   (lambda ()
	     (make-local-variable 'resize-minibuffer-window-max-height)
	     (setq resize-minibuffer-window-max-height 1))))



(provide '049-ido)
;; 049-ido.el ends here.
