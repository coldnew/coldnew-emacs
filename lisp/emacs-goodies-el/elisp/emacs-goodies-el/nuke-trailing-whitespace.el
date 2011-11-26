;;; whitespace.el --- strip trailing whitespace from buffers

;; Copyright (C) 1995, 1996, 1997, 2000 Noah S. Friedman

;; Author: Noah Friedman <friedman@splode.com>
;; Maintainer: friedman@splode.com
;; Keywords: extensions
;; Status: Works in Emacs 19 and XEmacs.

;; $Id: nuke-trailing-whitespace.el,v 1.2 2009-09-04 02:24:05 psg Exp $

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
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 51 Franklin Street, Fifth Floor; Boston, MA 02110-1301, USA.

;;; Commentary:

;; You may wish to do the following in your .emacs:
;;
;;     (autoload 'nuke-trailing-whitespace "whitespace" nil t)
;;     (add-hook 'mail-send-hook 'nuke-trailing-whitespace)
;;     (add-hook 'write-file-hooks 'nuke-trailing-whitespace)

;;; Code:

(defvar nuke-trailing-whitespace-p 'whitespace-check-mode
  "*Specify when stripping whitespace should be done.
This variable affects how the function `nuke-trailing-whitespace' behaves.
If `t', unreservedly strip trailing whitespace, including excess newlines.
If `nil', do nothing.
If a symbol \(not bound to a function\), query for each instance.

If a function or name of a function, call it to decide what to do.
This function is called once and should return `t', `nil', or the symbol
`query' to decide what to do.

This variable is made buffer-local when set in any fashion.")
(make-variable-buffer-local 'nuke-trailing-whitespace-p)

;; The regexp "\\s-+$" is too general, since form feeds (\n), carriage
;; returns (\r), and form feeds/page breaks (C-l) count as whitespace in
;; some syntaxes even though they serve a functional purpose in the file.
(defconst whitespace-regexp "[ \t]+$"
  "Regular expression which matches trailing whitespace.")

;; Match two or more trailing newlines at the end of the buffer; all but
;; the first newline will be deleted.
(defconst whitespace-eob-newline-regexp "\n\n+\\'"
  "Regular expression which matches newlines at the end of the buffer.")

(defvar nuke-trailing-whitespace-always-major-modes
  '(ada-mode
    c++-mode
    c-mode
    change-log-mode
    cperl-mode
    emacs-lisp-mode
    fortran-mode
    latex-mode
    lisp-interaction-mode
    lisp-mode
    makefile-mode
    nroff-mode
    perl-mode
    plain-tex-mode
    prolog-mode
    scheme-mode
    sgml-mode
    tcl-mode
    slitex-mode
    sml-mode
    texinfo-mode)
  "*Major modes for which `whitespace-check-mode' will return `t'.
These are major modes for which `nuke-trailing-whitespace' should
strip all trailing whitespace and excess newlines at the end of the buffer
without asking.")

(defvar nuke-trailing-whitespace-never-major-modes
  '(mail-mode
    rmail-mode
    vm-mode
    vm-summary-mode)
  "*Major modes for which `whitespace-check-mode' will return `nil'.
These are major modes for which `nuke-trailing-whitespace' should
never strip trailing whitespace automatically.")


;;;###autoload
(defun nuke-trailing-whitespace ()
  "Nuke all trailing whitespace in the buffer.
Whitespace in this case is just spaces or tabs.
This is a useful function to put on write-file-hooks.

Unless called interactively, this function uses
`nuke-trailing-whitespace-p' to determine how to behave.
However, even if this variable is `t', this function will query for
replacement if the buffer is read-only."
  (interactive)
  (cond ((interactive-p)
         (call-interactively 'whitespace-do-nuke-whitespace))
        (t
         (let ((flag nuke-trailing-whitespace-p))
           (and nuke-trailing-whitespace-p
                (symbolp nuke-trailing-whitespace-p)
                (fboundp nuke-trailing-whitespace-p)
                (setq flag (funcall nuke-trailing-whitespace-p)))

           (and flag
                (whitespace-do-nuke-whitespace flag)))))
  ;; always return nil, in case this is on write-file-hooks.
  nil)

(defun whitespace-do-nuke-whitespace (&optional flag)
  (interactive)
  (let ((buffer-orig-read-only buffer-read-only)
        (buffer-read-only nil))
    (save-excursion
      (save-restriction
        (save-match-data
          (widen)
          (goto-char (point-min))
          (cond
           ((or (and (eq flag t)
                     (not buffer-orig-read-only))
                (interactive-p))
            (while (re-search-forward whitespace-regexp (point-max) t)
              (delete-region (match-beginning 0) (match-end 0)))
            (goto-char (point-min))
            (and (re-search-forward whitespace-eob-newline-regexp nil t)
                 (delete-region (1+ (match-beginning 0)) (match-end 0))))
           (t
            (query-replace-regexp whitespace-regexp "")
            (goto-char (point-min))
            (and (re-search-forward whitespace-eob-newline-regexp nil t)
                 (save-match-data
                   (y-or-n-p
                    "Delete excess trailing newlines at end of buffer? "))
                 (delete-region (1+ (match-beginning 0)) (match-end 0))))))))))

(defun whitespace-check-mode (&optional mode)
  (or mode (setq mode major-mode))
  (cond ((memq mode nuke-trailing-whitespace-always-major-modes) t)
        ((memq mode nuke-trailing-whitespace-never-major-modes) nil)
        ;; Only query for visible buffers; invisible buffers are probably
        ;; managed by programs (e.g. w3 history list) and a query for them
        ;; is confusing.
        ((get-buffer-window (current-buffer) t) 'query)
        (t nil)))

(provide 'whitespace)

;;; whitespace.el ends here.
