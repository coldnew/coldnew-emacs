;;; circuit-mode.el --- Circuit Mode for GNU Emacs

;; Copyright (C) 2010 Yen-Chin,Lee

;; Author: Yen-Chin,Lee <coldnew.tw@gmail.com>
;; URL:
;; Keywords:
;; Version:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;

;;; Code:



(eval-when-compile
  (require 'cl))

(defgroup circuit nil
  "circuit macro editing command for Emacs."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :prefix "circuit-"
  :group 'languages)

(defvar circuit-font-lock-keywords
  `(
    ("left" . font-lock-variable-name-face))
  "Default font-lock-keywords for `circuit mode'.")


(defcustom circuit-mode-hook nil
  "*Hook called by `circuit-mode'."
  :type 'hook
  :group 'circuit)



;;;###autoload
(defun circuit-mode ()
  "A major mode to edit circuit macro files."
  (interactive)
  (kill-all-local-variables)

  (make-local-variable 'comment-start)
  (setq comment-start "#")

  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)

  (make-local-variable 'font-lock-defaults)
  (setq major-mode 'circuit-mode
	mode-name "circuit"
	font-lock-defaults '(circuit-font-lock-keywords nil))
  (run-mode-hooks 'circuit-mode-hook))

(provide 'circuit-mode)
;;; circuit-mode.el ends here
