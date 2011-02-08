;;; vim-elscreen.el ---

;; Copyright 2011 Yen-Chin,Lee
;;
;; Author: coldnew coldnew.tw@gmail.com
;; Keywords:
;; X-URL: http://www.emacswiki.org/cgi-bin/wiki/download/vim-elscreen.el
(defconst vim-elscreen-version "0.1")

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
;; If you have problems, send a bug report via M-x vim-elscreen-send-bug-report.
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
;;  3) Use Lisp version instead of compiled one: (load "vim-elscreen.el")
;;  4) Do it!
;;  5) If you got an error, please do not close *Backtrace* buffer.
;;  6) M-x vim-elscreen-send-bug-report (outside)
;;     then M-x insert-buffer *Backtrace* (if you got error)
;;  7) Describe the bug using a precise recipe.
;;  8) Type C-c C-c to send.
;;  # If you are a Taiwanese, please write in Taiwanese :P

;;; Change Log:
;;
;;

;;; Usage:
;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'vim-elscreen)

;;; Code:

(eval-when-compile (require 'cl))

(require 'vim)
(require 'elscreen)

;;;;##########################################################################
;;;;  User Options, Variables
;;;;##########################################################################


;; vim-> help tabpage

(vim:defcmd vim:cmd-tab-new ((argument:file file) nonrepeatable)
  "Open a new tab page and edit {file}, like with :edit."
  "If the {file} is nil, create a new tab page with empty window, after the current tab page."
  (if file
      (elscreen-find-file file)
    (progn
      (elscreen-create)
      (switch-to-buffer  (get-buffer-create (generate-new-buffer-name "[No Name]"))))))


(vim:defcmd vim:cmd-tab-find ((argument:file file) nonrepeatable)
  "Open a new tab page and edit {file} in 'path', like with :find. "
  (if file
      (elscreen-find-file file)
    (message "Error: need argument")
    ))

;; FIXME: need to add a function if command does not exist
;;        ex: vim:ex-command-exist
;;        maybe fix vim:ex-execute-command will do the work
(vim:defcmd vim:cmd-tab-command ((argument:text text) nonrepeatable)
  "Execute {cmd} and when it opens a new window open a new tab
   page instead.  Doesn't work for :diffsplit, :diffpatch,:execute and :normal.
   Examples: >
	     :tab split      'opens current buffer in new tab page'
	     :tab help gt    'opens tab page with help for \"gt\"'"
  (if text
      (progn
	(elscreen-create)
	(vim:ex-execute-command text)
	)))


;; TODO:
;; CTRL-W gf       Open a new tab page and edit the file name under the cursor.
;;                 See CTRL-W_gf.

;; CTRL-W gF       Open a new tab page and edit the file name under the cursor
;;                 and jump to the line number following the file name.
;;                 See CTRL-W_gF.



(vim:defcmd vim:cmd-tab-close (nonrepeatable)
  "Close current tab page.
   This command fails when:
     - There is only one tab page on the screen.
     - When 'hidden' is not set, [!] is not used, a buffer has
		  changes, and there is no other window on this buffer.
		Changes to the buffer are not written and won't get lost, so
		this is a \"safe\" command."
  (kill-buffer)
  (elscreen-kill)
  )

(vim:emap "tabnew" 'vim:cmd-tab-new)
(vim:emap "tabedit" "tabnew")
(vim:emap "tabe" "tabedit")
(vim:emap "tabfind" 'vim:cmd-tab-find)
(vim:emap "tabf" "tabfind")
(vim:emap "tab" 'vim:cmd-tab-command)
(vim:emap "tabclose" 'vim:cmd-tab-close)
(vim:emap "tabc" "tabclose")



(provide 'vim-elscreen)
;; vim-elscreen.el ends here.
