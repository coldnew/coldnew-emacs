;;; highlight-system-commands.el --- highlight system commands

;; Copyright 2012 coldnew
;;
;; Author: coldnew <coldnew.tw@gmail.com>
;; Keywords: highlight
;; X-URL: http://www.emacswiki.org/cgi-bin/wiki/download/highlight-system-commands.el
(defconst highlight-system-commands-version "0.1")

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;;
;;

;;
;; Happy Coding !!
;;

;;; Usage:
;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'highlight-system-commands)

;;; Code:

(eval-when-compile (require 'cl))


(defgroup highlight-system-commands nil
  "Highlight system commands"
  :group 'faces
  :group 'matching)

(defcustom highlight-system-commands-modes
  '(sh-mode
    shell-mode)
  "Major modes `highlight-system-commands-mode' can run on."
  :group 'highlight-system-commands
  :type '(repeat symbol))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar highlight-system-commands-overlays nil
  "This buffers currently active overlays."
  (make-variable-buffer-local 'highlight-system-commands-overlays))

(defvar highlight-system-commands-last-point 0
  "The last point for which system-commands were highlighted.
This is used to prevent analyzing the same context over and over."
  (make-variable-buffer-local 'highlight-system-commands-last-point))


(defun highlight-system-commands-highlight ()
  "Highlight system commands in buffer."
  (let ((case-fold-search nil))
    (and  (search-forward-regexp "\\<[a-zA-Z\\-]+\\>" nil t)
	  (executable-find
	   (buffer-substring-no-properties (car (bounds-of-thing-at-point 'word))
					   (cdr (bounds-of-thing-at-point 'word)))
	   ))

    ))



(defun highlight-system-commands-overlays ())


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defface highlight-system-commands-face
  '((((class color)) (:foreground "red")))
  "I am comment"
  :group 'font-lock-faces
  )

(defvar font-lock-system-command-face 'font-lock-system-command-face)

;;;; Overlays

;; (defun highlight-system-commands-overlays ()
;;   (let (fg highlight-system-commands-)))






(provide 'highlight-system-commands)
;; highlight-system-commands.el ends here.
