;;; semantic/db-mozrepl.el --- Semantic database extensions for mozrepl

;;; Copyright (C) 2012 David Engster

;; This file is not part of GNU Emacs.

;; Semanticdb is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;
;;; Commentary:
;;
;; In a nutshell:
;;   - Install mozrepl add-on for Firefox:
;;         https://addons.mozilla.org/de/firefox/addon/mozrepl/
;;   - Activate mozrepl in Firefox
;;   In Emacs:
;;   - Put (require 'semantic/db-mozrepl) in your .emacs
;;   - M-x semanticdb-mozrepl-activate
;;   - Enter URL of the page you're currently working on (you can
;;     set it permanently through `semanticdb-mozrepl-URL'.
;;
;; Semantic will now be able to provide completions based on what it
;; can gather from mozrepl.  Use semanticdb-set-URL to change URL.

;;; Code:
(require 'semantic/db)
(require 'semantic/db-find)

(defvar semanticdb-mozrepl-port 4242
  "Port for mozrepl.")

(defvar semanticdb-mozrepl-host "localhost"
  "Host for mozrepl.")

(defvar semanticdb-mozrepl-URL nil
  "URL mozrepl should connect to.")

;;; Internal variables

(defvar semanticdb-mozrepl-proc nil
  "Current mozrepl connection process.")

(defvar semanticdb-mozrepl-buffer " *MOZREPL*"
  "Buffer name used for mozrepl connection.")

(defvar semanticdb-mozrepl-maxwait 0.3
  "Timeout when calling `accept-process-output'.")

(defvar semanticdb-mozrepl-object nil
  "Current mozrepl object name.")

;;;###autoload
(defun semanticdb-mozrepl-activate ()
  "Activate mozrepl database for Javascript.
Connect to `semanticdb-mozrepl-host' on port
`semanticdb-mozrepl-port'.  If `semanticdb-mozrepl-URL' is
defined it will automatically open that location, otherwise it
will ask the user."
  (interactive)
  (unless semanticdb-mozrepl-URL
    (setq semanticdb-mozrepl-URL (read-from-minibuffer "mozrepl URL: ")))
  (setq semanticdb-mozrepl-proc
	(open-network-stream "mozrepl" semanticdb-mozrepl-buffer
			     semanticdb-mozrepl-host semanticdb-mozrepl-port))
  (when (null semanticdb-mozrepl-proc)
    (error "Could not open connection to mozrepl on %s:%s."
	   semanticdb-mozrepl-host semanticdb-mozrepl-port))
  (accept-process-output semanticdb-mozrepl-proc semanticdb-mozrepl-maxwait)
  (with-current-buffer semanticdb-mozrepl-buffer
    (goto-char (point-max))
    (beginning-of-line)
    (if (looking-at "\\(.+\\)>")
	(setq semanticdb-mozrepl-object (match-string 1))
      (error "Could not parse mozrepl prompt.")))
  ;; Open page
  (process-send-string
   semanticdb-mozrepl-proc
   (concat "content.location.href='" semanticdb-mozrepl-URL "'\n"))
  (accept-process-output semanticdb-mozrepl-proc semanticdb-mozrepl-maxwait)

  (message "Activated mozrepl database for %s." semanticdb-mozrepl-URL))

(defun semanticdb-mozrepl-send (msg)
  "Send MSG to mozrepl object.
Returns string with output from mozrepl."
  (with-current-buffer semanticdb-mozrepl-buffer
    (goto-char (point-max))
    (let ((cur (point)))
      (process-send-string
       semanticdb-mozrepl-proc
       (concat semanticdb-mozrepl-object "." msg "\n"))
      (accept-process-output semanticdb-mozrepl-proc semanticdb-mozrepl-maxwait)
      ;; Wait till we have a prompt
      (while (not (progn
		    (goto-char (point-max))
		    (beginning-of-line)
		    (looking-at (concat semanticdb-mozrepl-object ">"))))
	(accept-process-output semanticdb-mozrepl-proc semanticdb-mozrepl-maxwait))
      (goto-char (point-max))
      (buffer-substring-no-properties cur (point)))))

(defun semanticdb-mozrepl-home-and-check-state ()
  "Send 'home()' to mozrepl and check if it is still running properly.
Will return non-nil if everything is OK."
  (let ((res (semanticdb-mozrepl-send "home()")))
    ;; Try to revive mozrepl when stuck
    (when (string-match "^....>" res)
      (process-send-string semanticdb-mozrepl-proc ";\n")
      (accept-process-output semanticdb-mozrepl-proc 1)
      (setq res (semanticdb-mozrepl-send "home()")))
    (string-match "object ChromeWindow" res)))


(defun semanticdb-mozrepl-reconnect ()
  "Kill current mozrepl connection and reconnect."
  (interactive)
  (when (null semanticdb-mozrepl-proc)
    (error "No mozrepl connection available."))
  (delete-process semanticdb-mozrepl-proc)
  (with-current-buffer semanticdb-mozrepl-buffer
    (erase-buffer))
  (semanticdb-mozrepl-activate))

(defun semanticdb-mozrepl-set-URL (url)
  "Change URL for mozrepl database."
  (interactive "sURL: ")
  (setq semanticdb-mozrepl-URL url)
  (semanticdb-mozrepl-reconnect))

;; Omniscient semanticdb interface

(defclass semanticdb-table-mozrepl (semanticdb-search-results-table)
  ((major-mode :initform javascript-mode))
  "A table for returning search results from mozrepl.")

(defclass semanticdb-project-database-mozrepl (semanticdb-project-database)
  ((new-table-class :initform semanticdb-table-mozrepl
		    :type class
		    :documentation
		    "New tables created for this database are of this class."))
  "Database representing mozrepl.")

;; Create the database, and add it to searchable databases for mozrepl mode.
(defvar-mode-local javascript-mode semanticdb-project-system-databases
  (list
   (semanticdb-project-database-mozrepl "mozrepl"))
  "Search mozrepl for symbols.")

;; NOTE: Be sure to modify this to the best advantage of your
;;       language.
(defvar-mode-local javascript-mode semanticdb-find-default-throttle
  '(local project unloaded system recursive omniscience)
  "Search project files, then search this omniscience database.")

;;; Filename based methods
;;
(defmethod semanticdb-get-database-tables ((obj semanticdb-project-database-mozrepl))
  "For a mozrepl database, there are no explicit tables.
Create one of our special tables that can act as an intermediary."
  ;; NOTE: This method overrides an accessor for the `tables' slot in
  ;;       a database.  You can either construct your own (like 'mozrepl' here)
  ;;       or you can manage any number of tables.

  ;; We need to return something since there is always the "master table".
  (when (not (slot-boundp obj 'tables))
    (let ((newtable (semanticdb-table-mozrepl "mozrepl")))
      (oset obj tables (list newtable))
      (oset newtable parent-db obj)
      (oset newtable tags nil)))
  (call-next-method))

(defmethod semanticdb-file-table ((obj semanticdb-project-database-mozrepl) filename)
  "From OBJ, return FILENAME's associated table object."
  ;; NOTE: See note for `semanticdb-get-database-tables'.
  (car (semanticdb-get-database-tables obj)))

(defmethod semanticdb-get-tags ((table semanticdb-table-mozrepl ))
  "Return the list of tags belonging to TABLE."
  ;; NOTE: Omniscient databases probably don't want to keep large tables
  ;;       lolly-gagging about.  Keep internal tables empty and
  ;;       refer to alternate databases when you need something.
  nil)

(defmethod semanticdb-equivalent-mode ((table semanticdb-table-mozrepl) &optional buffer)
  "Return non-nil if TABLE's mode is equivalent to BUFFER.
Equivalent modes are specified by by `semantic-equivalent-major-modes'
local variable."
  (with-current-buffer buffer
    (eq (or mode-local-active-mode major-mode) 'javascript-mode)))

;;; Usage
;;
;; Unlike other tables, an omniscent database does not need to
;; be associated with a path.  Use this routine to always add ourselves
;; to a search list.
(define-mode-local-override semanticdb-find-translate-path javascript-mode
  (path brutish)
  "Return a list of semanticdb tables asociated with PATH.
If brutish, do the default action.
If not brutish, do the default action, and append the system
database (if available.)"
  (let ((default
	  ;; When we recurse, disable searching of system databases
	  ;; so that our mozrepl database only shows up once when
	  ;; we append it in this iteration.
	  (let ((semanticdb-search-system-databases nil))
	    (semanticdb-find-translate-path-default path brutish))))
    ;; Don't add anything if BRUTISH is on (it will be added in that fcn)
    ;; or if we aren't supposed to search the system.
    (if (or brutish (not semanticdb-search-system-databases))
	default
      (let ((tables (apply #'append
			   (mapcar
			    (lambda (db) (semanticdb-get-database-tables db))
			    semanticdb-project-system-databases))))
	(append default tables)))))


;;; Search Overrides
;;
;; NOTE WHEN IMPLEMENTING: Be sure to add doc-string updates explaining
;; how your new search routines are implemented.
;;
(defmethod semanticdb-find-tags-by-name-method
  ((table semanticdb-table-mozrepl) name &optional tags)
  "Find all tags named NAME in TABLE.
Return a list of tags by calling 'inspect' on NAME through mozrepl."
  (if tags
      (call-next-method)
    (when semanticdb-mozrepl-proc
      (let ((start 0)
	    members res)
	(unless (semanticdb-mozrepl-home-and-check-state)
	  (semanticdb-mozrepl-reconnect))
	(semanticdb-mozrepl-send "enter(content)")
	(setq  res (semanticdb-mozrepl-send (concat "inspect(" name ")")))
	(while (string-match "<\\(object\\|function\\)>\\.\\(.+\\)=" res start)
	  (setq start (match-end 2))
	  (push (semantic-tag-new-type (match-string 2 res) nil nil nil) members))
	(when members
	  (list
	   (semantic-tag-new-type name nil members nil)))))))


(defmethod semanticdb-find-tags-by-name-regexp-method
  ((table semanticdb-table-mozrepl) regex &optional tags)
  "Find all tags with name matching REGEX in TABLE.
Optional argument TAGS is a list of tags to search.
Return a list of tags."
  (if tags (call-next-method)
    ;; YOUR IMPLEMENTATION HERE
    ))

(defmethod semanticdb-find-tags-for-completion-method
  ((table semanticdb-table-mozrepl) prefix &optional tags)
  "In TABLE, find all occurances of tags matching PREFIX.
Optional argument TAGS is a list of tags to search.  Returns a
table of all matching tags by calling 'search' through mozrepl."
  (if tags
      (call-next-method)
    (when semanticdb-mozrepl-proc
      (let ((start 0)
	    str results)
	(unless (semanticdb-mozrepl-home-and-check-state)
	  (semanticdb-mozrepl-reconnect))
	(setq str (semanticdb-mozrepl-send (concat "search(/^" prefix "/i)")))
	(while (string-match "^\\(.+\\)$" str start)
	  (setq start (match-end 1))
	  (push (semantic-tag-new-type
		 (match-string 1 str)
		 nil nil nil)
		results))
	results))))

;;; Deep Searches
;;
;; If your language does not have a `deep' concept, these can be left
;; alone, otherwise replace with implementations similar to those
;; above.
;;
(defmethod semanticdb-deep-find-tags-by-name-method
  ((table semanticdb-table-mozrepl) name &optional tags)
  "Find all tags name NAME in TABLE.
Optional argument TAGS is a list of tags t
Like `semanticdb-find-tags-by-name-method' for mozrepl."
  (semanticdb-find-tags-by-name-method table name tags))

(defmethod semanticdb-deep-find-tags-by-name-regexp-method
  ((table semanticdb-table-mozrepl) regex &optional tags)
  "Find all tags with name matching REGEX in TABLE.
Optional argument TAGS is a list of tags to search.
Like `semanticdb-find-tags-by-name-method' for mozrepl."
  (semanticdb-find-tags-by-name-regexp-method table regex tags))

(defmethod semanticdb-deep-find-tags-for-completion-method
  ((table semanticdb-table-mozrepl) prefix &optional tags)
  "In TABLE, find all occurances of tags matching PREFIX.
Optional argument TAGS is a list of tags to search.
Like `semanticdb-find-tags-for-completion-method' for mozrepl."
  (semanticdb-find-tags-for-completion-method table prefix tags))

(provide 'semantic/db-mozrepl)

;; Local variables:
;; generated-autoload-file: "loaddefs.el"
;; generated-autoload-load-name: "semantic/db-mozrepl"
;; End:

;;; semantic/db-mozrepl.el ends here
