;;; circuit-mode.el --- A major mode for circuit_macro

;; Copyright 2011 Yen-Chin,Lee
;;
;; Author: coldnew coldnew.tw@gmail.com
;; Keywords: circuit
;; X-URL: http://www.emacswiki.org/cgi-bin/wiki/download/circuit-mode.el
(defconst circuit-mode-version "0.1")

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

;;; Usage:
;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'circuit-mode)

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





(provide 'circuit-mode)
;; circuit-mode.el ends here.
