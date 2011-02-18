;;; circuit-mode2.el --- A major mode for circuit_macro

;; Copyright 2011 Yen-Chin,Lee
;;
;; Author: coldnew coldnew.tw@gmail.com
;; Keywords: circuit
;; X-URL: http://www.emacswiki.org/cgi-bin/wiki/download/circuit-mode2.el
(defconst circuit-mode2-version "0.1")

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
;; If you have problems, send a bug report via M-x circuit-mode2-send-bug-report.
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
;;  3) Use Lisp version instead of compiled one: (load "circuit-mode2.el")
;;  4) Do it!
;;  5) If you got an error, please do not close *Backtrace* buffer.
;;  6) M-x circuit-mode2-send-bug-report (outside)
;;     then M-x insert-buffer *Backtrace* (if you got error)
;;  7) Describe the bug using a precise recipe.
;;  8) Type C-c C-c to send.
;;  # If you are a Taiwanese, please write in Taiwanese :P

;;; Change Log:
;;
;;

;;; Usage:
;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'circuit-mode2)

;;; Code:

(eval-when-compile (require 'cl))


;; define font-lock
;; TODO:only write to p.36
;; take a look at http://xahlee.org/emacs/elisp_syntax_coloring.html
(defvar circuit-mode-element
  '("AND_gate" "AND_gen" "BOX_gate" "BUFFER_gate" "BUFFER_gen" "Cos" "Cosine"
    "Darlington"  "Equidist3" "Fector" "FlipFlop" "FlipFlop6" "FlipFlopJK"
    "IOdefs" "Intersect_" "LH_symbol" "Loopover_" "LT_symbol" "Max" "Min"
    "Mux" "NAND_gate" "NOR_gate" "NOT_gate" "OR_gate" "OR_gen" "Point_"
    "Rect_" "Sin" "Vperp" "XOR_gate" "abs_" "adc" "amp" "along_" "antenna"

    ))
(defvar circuit-mode-macro
  '("AND_ht" "AND_wd" "BUF_ht" "BUF_wd" "E_" "FF_ht" "FF_wid" "G_hht" "HOMELIB_"
    "H_ht" "Int_" "L_unit" "Mux_ht" "Mux_wid" "Mx_pins" "NOT_circle" "NOT_rad"
    "OR_rad" "View3D" "XOR_off" "above_"
    ))

;; (defgroup circuit nil
;;   "circuit macro editing command for Emacs."
;;   :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
;;   :prefix "circuit-"
;;   :group 'languages)

;; (defvar circuit-font-lock-keywords
;;   `(
;;     ("left" . font-lock-variable-name-face))
;;   "Default font-lock-keywords for `circuit mode'.")


;; (defcustom circuit-mode-hook nil
;;   "*Hook called by `circuit-mode'."
;;   :type 'hook
;;   :group 'circuit)



;; ;;;###autoload
;; (defun circuit-mode ()
;;   "A major mode to edit circuit macro files."
;;   (interactive)
;;   (kill-all-local-variables)

;;   (make-local-variable 'comment-start)
;;   (setq comment-start "#")

;;   (make-local-variable 'parse-sexp-ignore-comments)
;;   (setq parse-sexp-ignore-comments t)

;;   (make-local-variable 'font-lock-defaults)
;;   (setq major-mode 'circuit-mode
;;	mode-name "circuit"
;;	font-lock-defaults '(circuit-font-lock-keywords nil))
;;   (run-mode-hooks 'circuit-mode-hook))





(provide 'circuit-mode2)
;; circuit-mode2.el ends here.
