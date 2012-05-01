;;; loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (rpm) "rpm" "rpm.el" (20383 64348))
;;; Generated autoloads from rpm.el

(autoload 'rpm "rpm" "\
Red Hat Package Management in Emacs.

\(fn)" t nil)

;;;***

;;;### (autoloads (Info-speedbar-buttons Info-speedbar-browser) "sb-info"
;;;;;;  "sb-info.el" (20383 64348))
;;; Generated autoloads from sb-info.el

(autoload 'Info-speedbar-browser "sb-info" "\
Initialize speedbar to display an info node browser.
This will add a speedbar major display mode.

\(fn)" t nil)

(autoload 'Info-speedbar-buttons "sb-info" "\
Create a speedbar display to help navigation in an Info file.
BUFFER is the buffer speedbar is requesting buttons for.

\(fn BUFFER)" nil nil)

(eval-after-load "info" '(require 'sb-info))

;;;***

;;;### (autoloads (rmail-speedbar-buttons) "sb-rmail" "sb-rmail.el"
;;;;;;  (20383 64348))
;;; Generated autoloads from sb-rmail.el

(autoload 'rmail-speedbar-buttons "sb-rmail" "\
Create buttons for BUFFER containing rmail messages.
Click on the address under Reply to: to reply to this person.
Under Folders: Click a name to read it, or on the <M> to move the
current message into that RMAIL folder.

\(fn BUFFER)" nil nil)

;;;***

;;;### (autoloads (speedbar-get-focus speedbar-frame-mode) "speedbar"
;;;;;;  "speedbar.el" (20383 64348))
;;; Generated autoloads from speedbar.el

(defalias 'speedbar 'speedbar-frame-mode)

(autoload 'speedbar-frame-mode "speedbar" "\
Enable or disable speedbar.  Positive ARG means turn on, negative turn off.
A nil ARG means toggle.  Once the speedbar frame is activated, a buffer in
`speedbar-mode' will be displayed.  Currently, only one speedbar is
supported at a time.
`speedbar-before-popup-hook' is called before popping up the speedbar frame.
`speedbar-before-delete-hook' is called before the frame is deleted.

\(fn &optional ARG)" t nil)

(autoload 'speedbar-get-focus "speedbar" "\
Change frame focus to or from the speedbar frame.
If the selected frame is not speedbar, then speedbar frame is
selected.  If the speedbar frame is active, then select the attached frame.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("dframe.el" "sb-ant.el" "sb-html.el" "sb-image.el"
;;;;;;  "sb-texinfo.el") (20383 64349 799057))

;;;***

(provide 'loaddefs)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; loaddefs.el ends here
