
;;; completion-ui-echo.el --- echo area user-interface for Completion-UI


;; Copyright (C) 2009 Toby Cubitt

;; Author: Toby Cubitt <toby-predictive@dr-qubit.org>
;; Version: 0.1.1
;; Keywords: completion, user interface, echo area, help-echo
;; URL: http://www.dr-qubit.org/emacs.php


;; This file is NOT part of Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301, USA.


;;; Change Log:
;;
;; Version 0.1.1
;; * fixed `completion-echo' to quote "%" characters in message text
;;
;; Version 0.1
;; * initial version (split off from completion-ui.el)


;;; Code:

(eval-when-compile (require 'cl))

(provide 'completion-ui-echo)
(require 'completion-ui)



;;; ============================================================
;;;                    Customization variables

(defgroup completion-ui-echo nil
  "Completion-UI echo-area user interface."
  :group 'completion-ui)


(defcustom completion-use-echo t
  "*Display completions in echo area."
  :group 'completion-ui-echo
  :type 'boolean)



;;; ============================================================
;;;                 Interface functions

(defun completion-echo (overlay)
  "Display completion candidates in the echo-area."
  (let ((message-log-max nil))
    (message (replace-regexp-in-string
	      "%" "%%" (completion-construct-echo-text overlay))))
  (overlay-put overlay 'help-echo 'completion-construct-help-echo-text))


(defun completion-construct-echo-text (overlay)
  "Function to return completion text for echo area."

  (let* ((prefix (overlay-get overlay 'prefix))
         (completions (overlay-get overlay 'completions))
         (text "") cmpl)

    (dotimes (i (length completions))
      (setq cmpl (nth i completions))
      (unless (stringp cmpl) (setq cmpl (car cmpl)))
      ;; if using hotkeys and one is assigned to current completion,
      ;; show it next to completion text
      (if (or (eq completion-use-hotkeys t)
	      (and (eq completion-use-hotkeys 'auto-show)
		   (overlay-get overlay 'auto-show)))
	  (if (< i (length completion-hotkey-list))
	      (setq cmpl
		    (concat
		     (format "(%s) "
			     (key-description
			      (vector (nth i completion-hotkey-list)))) cmpl))
	    (setq cmpl (concat "() " cmpl))))
      (setq text (concat text cmpl "  ")))

    ;; return constructed text
    text))


(defun completion-construct-help-echo-text (dummy1 overlay dummy2)
  "Function to return text for help-echo property
of completion overlay."

  (let* ((text "") str
         (prefix (overlay-get overlay 'prefix))
         (completions (overlay-get overlay 'completions))
         (num (overlay-get overlay 'completion-num)))

    ;; if `tooltip-mode' is enabled, construct text for tooltip
    (if tooltip-mode
        (dotimes (i (length completions))
          ;; if using hotkeys and one is assigned to current
          ;; completion, show it next to completion text
          (if (or (eq completion-use-hotkeys t)
		  (and (eq completion-use-hotkeys 'auto-show)
		       (overlay-get overlay 'auto-show)))
	      (if (< i (length completion-hotkey-list))
		  (setq str
			(format "(%s) "
				(key-description
				 (vector (nth i completion-hotkey-list)))))
		(setq str "     "))
	    (setq str ""))
          ;; add completion to text
          (setq str (concat str (nth i completions)))
          (setq text (concat text str "\n")))

      ;; otherwise, construct text for echo area
      (setq text (completion-construct-echo-text overlay)))

    ;; return constructed text
    text))



;;; =================================================================
;;;                    Register user-interface


(completion-ui-register-interface
 'completion-use-echo
 :activate 'completion-echo
 :deactivate t
 :auto-show-helper 'completion-echo)



;;; completion-ui-echo.el ends here
