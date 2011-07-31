;;; ac-gambit.el ---

;; Copyright 2011
;;
;; Author: coldnew@Fevia
;; Keywords:
;; X-URL: http://www.emacswiki.org/cgi-bin/wiki/download/ac-gambit.el
(defconst ac-gambit-version "0.1")

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

;;; Bug Report:
;;
;; If you have problems, send a bug report via M-x ac-gambit-send-bug-report.
;; I implemented bug report feature because I want to know your current state.
;; It helps me to solve problems easily.
;; The step is:
;;  0) Setup mail in Emacs, the easiest way is:
;;       (setq user-mail-address "your@mail.address")
;;       (setq user-full-name "Your Full Name")
;;       (setq smtpmail-smtp-server "your.smtp.server.tw")
;;       (setq mail-user-agent 'message-user-agent)
;;       (setq message-send-mail-function 'message-smtpmail-send-it)
;;  1) Be sure to use the LATEST version of anything.el.
;;  2) Enable debugger. M-x toggle-debug-on-error or (setq debug-on-error t)
;;  3) Use Lisp version instead of compiled one: (load "ac-gambit.el")
;;  4) Do it!
;;  5) If you got an error, please do not close *Backtrace* buffer.
;;  6) M-x ac-gambit-send-bug-report (outside)
;;     then M-x insert-buffer *Backtrace* (if you got error)
;;  7) Describe the bug using a precise recipe.
;;  8) Type C-c C-c to send.
;;  # If you are a Taiwanese, please write in Taiwanese :P

;;; Change Log:
;;
;;

;;; Usage:
;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'ac-gambit)

;;; Code:

(eval-when-compile (require 'cl))

;;;;##########################################################################
;;;;  User Options, Variables
;;;;##########################################################################


;;(##symbol-table)


;;;;;;;; faces
(defface ac-gambit-candidate-face
  '((t (:inherit ac-candidate-face)))
  "face for gambit candidate"
  :group 'auto-complete)

(defface ac-gambit-selection-face
  '((t (:inherit ac-selection-face)))
  "face for the gambit selected candidate."
  :group 'auto-complete)


;;;;;;;; local variables

(defvar ac-gambit-complete-list nil)

;;;;;;;; functions

(defun ac-gambit-init ()
  "Start inferior-gambit in background before use ac-gambit."
  (run-scheme "gsi -:d-"))


(defun ac-gambit-do-complete ()
  (interactive)
  (let* ((end (point))
	 (command (save-excursion
		   (skip-syntax-backward "w_")
		   (buffer-substring-no-properties (point) end))))

    (scheme-send-string
     (list (concat "\n")))

    (setq ac-octave-complete-list
	  (sort inferior-octave-output-list 'string-lessp))

    ;; remove dulpicates lists
    (delete-dups ac-octave-complete-list)

    ))


(defun ac-gambit-candidate ()
  (let (table)
    (ac-octave-do-complete)
    (dolist (s ac-octave-complete-list)
	    (push s table))
    table)
  )


(ac-define-source gambit
		  '((candidates . ac-gambit-candidate)
		    (candidate-face . ac-gambit-candidate-face)
		    (selection-face . ac-gambit-selection-face)
		    (init . ac-gambit-init)
		    (requires . 0)
		    (cache)
		    (symbol . "f")
		    ))







(provide 'ac-gambit)
;; ac-gambit.el ends here.
