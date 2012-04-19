;;; coldnew-menu.el ---
(eval-when-compile (require 'cl))


;;;; ---------------------------------------------------------------------------
;;;; one-key
;;;; ---------------------------------------------------------------------------
(require 'one-key)

;;;; Initial Setting
(setq one-key-popup-window t)

;; ------------------------------
;; window

(defun one-key-menu-window-navigation ()
  "The `one-key' menu for WINDOW-NAVIGATION."
  (interactive)
  (one-key-menu
   "window-navigation"
   '((("C-n"   . "Downward")                 . windmove-down)
     (("C-p"   . "Upward")                   . windmove-up)
     (("C-b"   . "Leftward")                 . windmove-left)
     (("C-f"   . "Rightward")                . windmove-right)
     (("f"     . "Full Screen")              . fullscreen-window)
     (("C-N"   . "Downward Fullscreen")      . windmove-down-fullscreen)
     (("C-P"   . "Upward Fullscreen")        . windmove-up-fullscreen)
     (("C-B"   . "Leftward Fullscreen")      . windmove-left-fullscreen)
     (("C-F"   . "Rightward Fullscreen")     . windmove-right-fullscreen)
     (("u"     . "Undo")                     . winner-undo)
     (("C-d"   . "Delete Window")            . delete-window)
     (("C-o"   . "Delete Other Windows")     . delete-other-windows)
     (("C-h"   . "Split Horizontally")       . split-window-horizontally)
     (("C-v"   . "Split Vertically")         . split-window-vertically)
     (("r"     . "Redo")                     . winner-redo)
     )))

;; ------------------------------
;; files
(defun one-key-menu-file-handle ()
  "The `one-key' menu for file-handle."
  (interactive)
  (one-key-menu
   "file-handle"
   '((("w"   . "Write File")                 . write-file)
     (("s"   . "Sudo Edit")                  . sudo-edit)
     )))




(provide 'coldnew-menu)
;; coldnew-menu.el ends here.
