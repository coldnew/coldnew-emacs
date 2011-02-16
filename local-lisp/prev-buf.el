;;; prev-buf.el ---

;; Copyright 2011
;;
;; Author: [yas] elisp error! Symbol's value as variable is void: user-nickname coldnew@Sara
;; Keywords:
;; X-URL: http://www.emacswiki.org/cgi-bin/wiki/download/prev-buf.el
(defconst prev-buf-version "0.1")

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
;; If you have problems, send a bug report via M-x prev-buf-send-bug-report.
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
;;  3) Use Lisp version instead of compiled one: (load "prev-buf.el")
;;  4) Do it!
;;  5) If you got an error, please do not close *Backtrace* buffer.
;;  6) M-x prev-buf-send-bug-report (outside)
;;     then M-x insert-buffer *Backtrace* (if you got error)
;;  7) Describe the bug using a precise recipe.
;;  8) Type C-c C-c to send.
;;  # If you are a Taiwanese, please write in Taiwanese :P

;;; Change Log:
;;
;;

;;; Usage:
;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'prev-buf)

;;; Code:

(eval-when-compile (require 'cl))

(defvar pre-buf-last-buf nil
  "")

(defun pre-buf-switch ()
  "switch to previous active buffer"
  (interactive)
  (if (not (boundp 'pre-buf-last-buf))
      (error "No previous buffer to switch"))
  (let ((buf (get-buffer pre-buf-last-buf)))
    (if (not buf)
	(error "Invalid buffer")
      (switch-to-buffer buf))))

(defadvice switch-to-buffer (before pre-buff-add-buf act)
  ""
  (setq pre-buf-last-buf (buffer-name)))

(vim:nmap (kbd "gb") 'pre-buf-switch)
;; ;;  see http://www.northbound-train.com/emacs/toggle-buffer.log


;; (eval-when-compile
;;   (defvar byte-compile-dynamic nil) ; silence the old byte-compiler
;;   (set (make-local-variable 'byte-compile-dynamic) t))

;; ;;; **************************************************************************
;; ;;; ***** customization routines
;; ;;; **************************************************************************
;; (defgroup joc-toggle-buffer nil
;;   "toggle-buffer package customization"
;;   :group 'tools)

;; ;; ---------------------------------------------------------------------------
;; (defun joc-toggle-buffer-customize ()
;;   "Customization of the group `joc-toggle-buffer'."
;;   (interactive)
;;   (customize-group "joc-toggle-buffer"))

;; ;; ---------------------------------------------------------------------------
;; (defcustom joc-toggle-buffer-swbuff-advice "P"
;;   "A hack to be compatable with the swbuff package.

;; Valid values are:
;;   o Never Advise - never advise the swbuff functions [nil]
;;   o Advise if Provided - only advise if swbuff already provided [P]
;;   o Always Advise - always define & activate the swbuff advise [A]

;; If you don't use the swbuff package, you can safely choose
;; Never Advise or Advise if Provided.  If you do use swbuff, you
;; may use Advise if Provided (in which case swbuff must be
;; `provide'd already) or Always Advise."
;;   :type `(choice
;;	  (const :tag "Never Advise" nil)
;;	  (const :tag "Advise if Provided" "P")
;;	  (const :tag "Always Advise" "A"))
;;   :group 'joc-toggle-buffer)

;; ;; ---------------------------------------------------------------------------
;; (defcustom toggle-buffer-load-hook nil
;;   "Hook to run when package is loaded."
;;   :type 'hook
;;   :group 'joc-toggle-buffer)

;; ;;; **************************************************************************
;; ;;; ***** version related routines
;; ;;; **************************************************************************
;; (defconst joc-toggle-buffer-version
;;   "$Revision: 1.3 $"
;;   "Version number for toggle-buffer package.")

;; ;; ---------------------------------------------------------------------------
;; (defun joc-toggle-buffer-version-number ()
;;   "Return `toggle-buffer' version number."
;;   (string-match "[0123456789.]+" joc-toggle-buffer-version)
;;   (match-string 0 joc-toggle-buffer-version))

;; ;; ---------------------------------------------------------------------------
;; (defun joc-toggle-buffer-display-version ()
;;   "Display `toggle-buffer' version."
;;   (interactive)
;;   (message "toggle-buffer version <%s>." (joc-toggle-buffer-version-number)))

;; ;;; **************************************************************************
;; ;;; ***** interactive functions
;; ;;; **************************************************************************
;; (defvar joc-toggle-buffer-last-buffer nil
;;   "Contains the name of the previous buffer.")

;; (defun joc-toggle-buffer ()
;;   "Switch to previous active buffer."
;;   (interactive)
;;   (if (not (boundp 'joc-toggle-buffer-last-buffer))
;;	  (error "No previous buffer to switch to (yet)"))
;;   (let ((buff (get-buffer joc-toggle-buffer-last-buffer)))
;;	(if (not buff)
;;		(error "Invalid buffer \"%s\"" joc-toggle-buffer-last-buffer)
;;	  (switch-to-buffer buff))))

;; ;;; **************************************************************************
;; ;;; ***** normal advice
;; ;;; **************************************************************************
;; (defadvice switch-to-buffer
;;   (before joc-toggle-buffer-setup-advice act)
;;   "Records active buffer (for possible later recall) before it's switched."
;;   (if (boundp 'joc-toggle-buffer-hack)
;;	  (setq joc-toggle-buffer-last-buffer joc-toggle-buffer-hack)
;;	(setq joc-toggle-buffer-last-buffer (buffer-name))))

;; ;;; **************************************************************************
;; ;;; ***** swbuff-specific advice
;; ;;; **************************************************************************
;; (let ((advise-swbuff-fns nil))
;;   (if joc-toggle-buffer-swbuff-advice
;;	  (if (eq joc-toggle-buffer-swbuff-advice "P")
;;		  (if (featurep 'swbuff)
;;			  (setq advise-swbuff-fns t))
;;		(setq advise-swbuff-fns t)))

;;   (if advise-swbuff-fns
;;	  (progn
;;		(defadvice swbuff-switch-to-next-buffer
;;		  (around joc-toggle-buffer-swbuf-next-advice act)
;;		  "hack for swbuff-users"
;;		  (setq joc-toggle-buffer-hack (buffer-name))
;;		  ad-do-it
;;		  (makunbound 'joc-toggle-buffer-hack))

;;		(defadvice swbuff-switch-to-previous-buffer
;;		  (around joc-toggle-buffer-swbuf-prev-advice act)
;;		  "hack for swbuff-users"
;;		  (setq joc-toggle-buffer-hack (buffer-name))
;;		  ad-do-it
;;		  (makunbound 'joc-toggle-buffer-hack))
;;		)))

;; ;;; **************************************************************************
;; ;;; ***** we're done
;; ;;; **************************************************************************
;; (provide 'toggle-buffer)
;; (run-hooks 'toggle-buffer-load-hook)

;; ;;; toggle-buffer.el ends here
;; ;;; **************************************************************************
;; ;;;; *****  EOF  *****  EOF  *****  EOF  *****  EOF  *****  EOF  *************





(provide 'prev-buf)
;; prev-buf.el ends here.
