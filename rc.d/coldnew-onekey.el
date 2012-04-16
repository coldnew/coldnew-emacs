;;
(eval-when-compile (require 'cl))

;;;;;;;; Packages Import
(require 'coldnew-macro)
(require 'coldnew-functions)
(require 'coldnew-commands)
(require 'coldnew-variables)
(require 'coldnew-keybinding)

;;;;;;;; Loding libraries
(require 'one-key)

;; do not automatically show one-key menu, only show when type ?
(setq one-key-popup-window t)

(defun one-key-menu-VC ()
  "The `one-key' menu for VC."
  (interactive)
  (one-key-menu "VC"
		'((("+" . "Vc Update") . vc-update)
		  (("=" . "Vc Diff") . vc-diff)
		  (("a" . "Vc Update Change Log") . vc-update-change-log)
		  (("b" . "Vc Switch Backend") . vc-switch-backend)
		  (("c" . "Vc Rollback") . vc-rollback)
		  (("d" . "Vc Dir") . vc-dir)
		  (("g" . "Vc Annotate") . vc-annotate)
		  (("h" . "Vc Insert Headers") . vc-insert-headers)
		  (("i" . "Vc Register") . vc-register)
		  (("l" . "Vc Print Log") . vc-print-log)
		  (("m" . "Vc Merge") . vc-merge)
		  (("r" . "Vc Retrieve Tag") . vc-retrieve-tag)
		  (("s" . "Vc Create Tag") . vc-create-tag)
		  (("u" . "Vc Revert") . vc-revert)
		  (("v" . "Vc Next Action") . vc-next-action)
		  (("~" . "Vc Revision Other Window") . vc-revision-other-window)))
  )

;; (global-unset-key "\C-x v")
(global-set-key (kbd "C-x v") 'one-key-menu-VC)




;;;;;;;; windw

(defvar one-key-menu-window-navigation-alist nil
  "The `one-key' menu list for WINDOW_NAVIGATION.")

(setq one-key-menu-window-navigation-alist
      '(
	(("C-n"   . "Downward")                    . windmove-down)
	(("C-p"   . "Upward")                      . windmove-up)
	(("C-b"   . "Leftward")                    . windmove-left)
	(("C-f"   . "Rightward")                   . windmove-right)
	(("f"     . "Full Screen")                 . fullscreen-window)
	(("C-N"   . "Downward Fullscreen")         . windmove-down-fullscreen)
	(("C-P"   . "Upward Fullscreen")           . windmove-up-fullscreen)
	(("C-B"   . "Leftward Fullscreen")         . windmove-left-fullscreen)
	(("C-F"   . "Rightward Fullscreen")        . windmove-right-fullscreen)
	(("u"     . "Undo")                        . winner-undo)
	(("C-d"   . "Delete Window")               . delete-window)
	(("C-o"   . "Delete Other Windows")         . delete-other-windows)
	(("C-h"   . "Split Horizontally")          . split-window-horizontally)
	(("C-v"   . "Split Vertically")            . split-window-vertically)
	(("r"     . "Redo")                        . winner-redo)
	;; (("C-c"   . "Make Frame")                  . make-frame)
	;; (("C-d"   . "Delete Frame")                . delete-frame)
	;; (("C-D"   . "Delete Other Frame")          . delete-other-frames)
	;; (("C-o"   . "Other Frame")                 . other-frame)
	))

(defun one-key-menu-window-navigation ()
  "The `one-key' menu for WINDOW-NAVIGATION."
  (interactive)
  (one-key-menu "window-navigation" one-key-menu-window-navigation-alist t t))

(global-set-key (kbd "C-w") 'one-key-menu-window-navigation)


(define-key evil-emacs-state-map  (kbd "C-w C-s") 'sr-speedbar-toggle)


(provide 'coldnew-onekey)
;; coldnew-onekey.el ends here.
