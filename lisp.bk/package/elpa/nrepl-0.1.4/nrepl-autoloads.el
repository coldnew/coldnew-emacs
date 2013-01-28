;;; nrepl-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (nrepl nrepl-jack-in nrepl-enable-on-existing-clojure-buffers
;;;;;;  nrepl-interaction-mode) "nrepl" "nrepl.el" (20610 11577))
;;; Generated autoloads from nrepl.el

(autoload 'nrepl-interaction-mode "nrepl" "\
Minor mode for nrepl interaction from a Clojure buffer.

\(fn &optional ARG)" t nil)

(autoload 'nrepl-enable-on-existing-clojure-buffers "nrepl" "\


\(fn)" t nil)

(autoload 'nrepl-jack-in "nrepl" "\


\(fn PROMPT-PROJECT)" t nil)

(add-hook 'nrepl-connected-hook 'nrepl-enable-on-existing-clojure-buffers)

(autoload 'nrepl "nrepl" "\


\(fn HOST PORT)" t nil)

;;;***

;;;### (autoloads nil nil ("nrepl-pkg.el") (20610 11577 225365))

;;;***

(provide 'nrepl-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; nrepl-autoloads.el ends here
