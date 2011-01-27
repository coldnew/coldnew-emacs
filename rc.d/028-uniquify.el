

;; uniquify changes conflicting buffer names from file<2> etc
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

(provide '028-uniquify)
;; 028-uniquify.el ends here.
