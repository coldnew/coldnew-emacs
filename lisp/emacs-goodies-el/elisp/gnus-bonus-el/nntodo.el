;;; nntodo.el --- Manage todo items with Gnus

;; Copyright (C) 1999 by Kai Grossjohann.

;; Authors: Kai.Grossjohann@CS.Uni-Dortmund.DE,
;;          John Wiegley <johnw@gnu.org>
;; Keywords: news, mail, calendar, convenience
;; Version: $Id: nntodo.el,v 1.1.1.1 2003-04-04 20:16:09 lolando Exp $

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Warning: this is alpha code!  Use at your own risk!  Don your
;; asbestos longjohns!  Might eat your mail for lunch!

;; This file provides a new Gnus backend, nntodo, for storing todo
;; items.  Each todo item is a message but has a special header
;; `X-Todo-Priority' for the priority of a todo item.  It is possible
;; to sort todo items by creation date and by priority.  Sorting by
;; due date hasn't been done yet.

;;; Kudos:

;; Dan Nicolaescu <dann@ics.uci.edu> for providing me with a first
;; implementation to look at and steal from.

;;; Code:

;;; Backend definition:

(require 'nnheader)
(require 'nnmail)
(require 'gnus-start)
(require 'nnmbox)
(require 'nnoo)
(require 'cl)

(nnoo-declare nntodo nnmbox)

;; If this variable isn't named nntodo-mbox-file, strange things
;; happen.  This seems to be a nnoo-ey problem.
(defvoo nntodo-mbox-file
    (expand-file-name (nnheader-concat gnus-home-directory ".nntodo"))
  "Name of the todo file in the user's home directory." nnmbox-mbox-file)
;; Similar.
(defvoo nntodo-active-file
    (expand-file-name (nnheader-concat gnus-home-directory ".nntodo-active"))
  "Name of the actile file for the todo file." nnmbox-active-file)

;; Can we protect better?
(defvoo nntodo-get-new-mail nil
  "Whether nntodo should get new mail.  MUST be nil!"
  nnmbox-get-new-mail)

(defvoo nntodo-current-group "" nil nnmbox-current-group)

(defconst nntodo-version "1.4")
(defvoo nntodo-status-string "" nil nnmbox-status-string)

(nnoo-define-basics nntodo)

;; Too bad that nnmbox-create-mbox-file isn't nnoo'd.
(deffoo nntodo-open-server (server &optional defs)
  (nnoo-change-server 'nntodo server defs)
  (nntodo-create-mbox-file)
  (cond
   ((not (file-exists-p nntodo-mbox-file))
    (nntodo-close-server)
    (nnheader-report 'nntodo "No such file: %s" nntodo-mbox-file))
   ((file-directory-p nntodo-mbox-file)
    (nntodo-close-server)
    (nnheader-report 'nntodo "Not a regular file: %s" nntodo-mbox-file))
   (t
    (nnheader-report 'nntodo "Opened server %s using mbox %s" server
		     nntodo-mbox-file)
    t)))

;; Copy of nnmbox-create-mbox-file, except for file name.
(defun nntodo-create-mbox-file ()
  (when (not (file-exists-p nntodo-mbox-file))
    (nnmail-write-region 1 1 nntodo-mbox-file t 'nomesg)))

;; When creating a group, it gets some special settings
(deffoo nntodo-request-create-group (group &optional server args)
  "Do whatever the nnmbox function does, then set a few group params."
  (nnoo-parent-function 'nntodo
			'nnmbox-request-create-group
			(list group server args))
  ;; Instead of setting these parameters for each group, isn't there a
  ;; way of somehow putting this into the server spec or into the
  ;; backend code?
  ;; Summary line looks different for todo items.
  (gnus-group-set-parameter
   (gnus-group-prefixed-name group (list "nntodo" server))
   'gnus-summary-line-format
   (list "%5N %U%R %7uT: %s\n"))
  ;; Why does the following not work?  `gnus-post-method' is nil or
  ;; something like this in the message buffer after hitting `a' in an
  ;; nntodo group.
  (gnus-group-set-parameter
   (gnus-group-prefixed-name group (list "nntodo" server))
   'gnus-post-method
   '('current))
  ;; Because the post method thing doesn't work, we need this.
  (gnus-group-set-parameter
   (gnus-group-prefixed-name group (list "nntodo" server))
   'gcc-self t)
  ;; default is to sort by priority, then by number
  (gnus-group-set-parameter
   (gnus-group-prefixed-name group (list "nntodo" server))
   'gnus-thread-sort-functions
   '('(gnus-thread-sort-by-number
       gnus-thread-sort-by-priority)))
  ;; Enter gnus-todo-mode in nntodo summaries.
  (gnus-group-set-parameter
   (gnus-group-prefixed-name group (list "nntodo" server))
   'dummy
   '( (gnus-todo-mode 1) )))

;; Ask for priority when entering articles.
(deffoo nntodo-request-accept-article (group &optional server last)
  "Add/modify priority header before storing the message."
  (let (prio)
    (save-restriction
      (message-narrow-to-headers-or-head)
      (unless (message-fetch-field "X-Todo-Priority")
	(setq prio (todo-gnus-get-priority prio))
	(message-remove-header "X-Todo-Priority" nil nil)
	(mail-position-on-field "X-Todo-Priority")
	(insert prio))))
  (nnoo-parent-function 'nntodo
			'nnmbox-request-accept-article
			(list group server last)))

(nnoo-import nntodo
  (nnmbox))

;;; Utility code:

;; Hook nntodo backend into Gnus

(unless (assoc "nntodo" gnus-valid-select-methods)
  (gnus-declare-backend "nntodo" 'post 'respool 'address))

;;; Creating todo items:

(defvar todo-gnus-priority-alist
  '(("high" . 0) ("medium" . 1) ("low" . 2))
  "Association between prio names and values.")

(defun todo-gnus-get-priority (&optional prio)
  "Read a priority from the minibuffer."
  (interactive)
  (unless prio (setq prio "medium"))
  (completing-read "Priority (medium): "
		   todo-gnus-priority-alist
		   nil                  ;predicate
		   t                    ;require-match
		   nil nil prio))

;; The following section is gross.  Isn't there a better way to do
;; this?  Maybe specify a special sending function for nntodo groups?
;; But how?
(defun todo-gnus-message-send-hook ()
  "Inserts required headers in todo item."
  (when (and (boundp 'gnus-message-group-art)
	     gnus-message-group-art
	     (car gnus-message-group-art)
	     (string-match "^nntodo\\>"
			   (car gnus-message-group-art)))
    (message-remove-header "Newsgroups")))

(add-hook 'message-send-hook 'todo-gnus-message-send-hook)

(add-to-list 'gnus-extra-headers 'X-Todo-Priority)
(add-to-list 'nnmail-extra-headers 'X-Todo-Priority)

;;; Summary buffer:

;; This function is used in nntodo-request-create-group to set
;; `gnus-summary-line-format'.
(defun gnus-user-format-function-T (head)
  (let* ((extra-headers (mail-header-extra head)))
    (cdr (assoc 'X-Todo-Priority extra-headers))))

;; Sorting by priority.  Code pretty much gleaned from gnus-sum.el
;; without any deeper understanding at all.
(defun gnus-article-sort-by-priority (h1 h2)
  (let* ((e1 (mail-header-extra h1))
	 (e2 (mail-header-extra h2))
	 (p1 (cdr (assoc 'X-Todo-Priority e1)))
	 (p2 (cdr (assoc 'X-Todo-Priority e2)))
	 (n1 (cdr (assoc p1 todo-gnus-priority-alist)))
	 (n2 (cdr (assoc p2 todo-gnus-priority-alist))))
    (unless n1
      (error "Unknown priority: %s" p1))
    (unless n2
      (error "Unknown priority: %s" p2))
    (if (= n1 n2)
	(< (mail-header-number h1) (mail-header-number h2))
      (< n1 n2))))

(defun gnus-thread-sort-by-priority (h1 h2)
  (gnus-article-sort-by-priority
   (gnus-thread-header h1) (gnus-thread-header h2)))

(defun gnus-summary-sort-by-priority (&optional reverse)
  "Sort the summary buffer by priority.
Argument REVERSE means reverse order."
  (interactive "P")
  (gnus-summary-sort 'priority reverse))

;; Todo minor mode.

;; Gee, this seems to be simple with easy-mmode!
(require 'easy-mmode)

(defvar gnus-todo-mode-map
  (easy-mmode-define-keymap
   (list (cons (kbd "i n")
	       (cons "Sort by number" 'gnus-summary-sort-by-number))
	 (cons (kbd "i p")
	       (cons "Sort by priority" 'gnus-summary-sort-by-priority))
	 (cons (kbd "i i")
	       (cons "Add new todo item" 'gnus-summary-post-news))
	 (cons (kbd "i d")
	       (cons "Delete todo item" 'gnus-summary-delete-article)))
   "Todo"))

(easy-mmode-define-minor-mode
 gnus-todo-mode
 "Minor mode for nntodo summary buffers.
Without ARG, toggle gnus-todo-mode.
With ARG, turn on iff ARG is positive, else turn off."
 nil
 " Todo"
 gnus-todo-mode-map)

;;; Epilog:

(provide 'nntodo)

;;; nntodo.el ends here
