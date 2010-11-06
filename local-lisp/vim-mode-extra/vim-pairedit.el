;;; vim-pairedit.el --- 

;; Copyright 2010 Yen-Chin,Lee
;;
;; Author: Yen-Chin,Lee <coldnew.tw@gmail.com>
;; Version: $Id: vim-pairedit.el,v 0.0 2010/08/31 05:00:07 coldnew Exp $
;; Keywords: 
;; X-URL: not distributed yet

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

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'vim-pairedit)

;;; Code:

(provide 'vim-pairedit)
(eval-when-compile
  (require 'cl))


(require 'vim-modes)
(require 'paredit)

;;;;##########################################################################
;;;;  User Options, Variables
;;;;##########################################################################


(vim:defcmd  vim:paredit-insert-open-parenthesis (nonrepeatable)
  "Insert open parenthesis."
  (paredit-open-parenthesis))

(vim:defcmd  vim:paredit-insert-close-parenthesis (nonrepeatable)
  "Insert close parenthesis."
  (paredit-close-parenthesis))

(vim:defcmd  vim:paredit-insert-open-brace (nonrepeatable)
  "Insert close brace."
  (paredit-open-brace))

(vim:defcmd  vim:paredit-insert-close-brace (nonrepeatable)
  "Insert close brace."
  (paredit-close-brace))

(vim:defcmd  vim:paredit-insert-open-bracket (nonrepeatable)
  "Insert close bracket."
  (paredit-open-bracket))

(vim:defcmd  vim:paredit-insert-close-bracket (nonrepeatable)
  "Insert close bracket."
  (paredit-close-bracket))

(vim:defcmd  vim:paredit-insert-doublequote (nonrepeatable)
  "Insert double quote."
  (paredit-doublequote))

(vim:defcmd  vim:paredit-backspace (nonrepeatable)
  "."
  (paredit-backward-delete))

(vim:defcmd  vim:paredit-delete (nonrepeatable)
  "."
  (paredit-forward-delete))



(vim:imap (kbd "(")  'vim:paredit-insert-open-parenthesis)
(vim:imap (kbd ")")  'vim:paredit-insert-close-parenthesis)
(vim:imap (kbd "[")  'vim:paredit-insert-open-bracket)
(vim:imap (kbd "]")  'vim:paredit-insert-close-bracket)
(vim:imap (kbd "{")  'vim:paredit-insert-open-brace)
(vim:imap (kbd "}")  'vim:paredit-insert-close-brace)
(vim:imap (kbd "\"") 'vim:paredit-insert-doublequote)
(vim:imap (kbd "<backspace>") 'vim:paredit-backspace)
(vim:imap (kbd "<delete>")    'vim:paredit-delete)





;;; vim-pairedit.el ends here
