;;; org-contacts.el --- Contacts management

;; Copyright (C) 2010, 2011 Julien Danjou <julien@danjou.info>

;; Author: Julien Danjou <julien@danjou.info>
;; Keywords: outlines, hypermedia, calendar
;;
;; This file is NOT part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This file contains the code for managing your contacts into Org-mode.

;; To enter new contacts, you can use `org-capture' and a template just like
;; this:

;;         ("c" "Contacts" entry (file "~/Org/contacts.org")
;;          "* %(org-contacts-template-name)
;; :PROPERTIES:
;; :EMAIL: %(org-contacts-template-email)
;; :END:")))
;;
;;; Code:

(defgroup org-contacts nil
  "Options concerning contacts management."
  :group 'org)

(defcustom org-contacts-files nil
  "List of Org files to use as contacts source.
If set to nil, all your Org files will be used."
  :type '(repeat file)
  :group 'org-contacts)

(defcustom org-contacts-email-property "EMAIL"
  "Name of the property for contact email address."
  :type 'string
  :group 'org-contacts)

(defcustom org-contacts-birthday-property "BIRTHDAY"
  "Name of the property for contact birthday date."
  :type 'string
  :group 'org-contacts)

(defcustom org-contacts-last-read-mail-property "LAST_READ_MAIL"
  "Name of the property for contact last read email link storage."
  :type 'string
  :group 'org-contacts)

(defcustom org-contacts-completion-ignore-case t
  "Ignore case when completing contacts."
  :type 'boolean
  :group 'org-contacts)

(defcustom org-contacts-group-prefix "+"
  "Group prefix."
  :type 'string
  :group 'org-contacts)

(defcustom org-contacts-matcher (concat org-contacts-email-property "<>\"\"")
  "Matching rule for finding heading that are contacts.
This can be a tag name, or a property check."
  :type 'string
  :group 'org-contacts)

(defcustom org-contacts-email-link-description-format "%s (%d)"
  "Format used to store links to email.
This overrides `org-email-link-description-format' if set."
  :group 'org-contacts
  :type 'string)

(defun org-contacts-files ()
  "Return list of Org files to use for contact management."
  (or org-contacts-files (org-agenda-files t 'ifmode)))

(defun org-contacts-filter (&optional name-match tags-match)
  "Search for a contact maching NAME-MATCH and TAGS-MATCH.
If both match values are nil, return all contacts."
  (let ((tags-matcher
         (if tags-match
             (cdr (org-make-tags-matcher tags-match))
           t))
        (name-matcher
         (if name-match
             '(org-string-match-p name-match (org-get-heading t))
           t))
        (contacts-matcher
         (cdr (org-make-tags-matcher org-contacts-matcher)))
        markers result)
    (dolist (file (org-contacts-files))
      (org-check-agenda-file file)
      (with-current-buffer (org-get-agenda-file-buffer file)
        (unless (org-mode-p)
          (error "File %s is no in `org-mode'" file))
        (org-scan-tags
         '(add-to-list 'markers (set-marker (make-marker) (point)))
         `(and ,contacts-matcher ,tags-matcher ,name-matcher))))
    (dolist (marker markers result)
      (org-with-point-at marker
        (add-to-list 'result
                     (list (org-get-heading t) marker (org-entry-properties marker 'all)))))))

(when (not (fboundp 'completion-table-case-fold))
  ;; That function is new in Emacs 24...
  (defun completion-table-case-fold (table string pred action)
    (let ((completion-ignore-case t))
      (complete-with-action action table string pred))))

(defun org-contacts-complete-name (&optional start)
  "Complete text at START with a user name and email."
  (let* ((end (point))
         (start (or start
                    (save-excursion
                      (re-search-backward "\\(\\`\\|[\n:,]\\)[ \t]*")
                      (goto-char (match-end 0))
                      (point))))
         (orig (buffer-substring start end))
         (completion-ignore-case org-contacts-completion-ignore-case)
         (group-completion-p (org-string-match-p (concat "^" org-contacts-group-prefix) orig))
         (completion-list
          (if group-completion-p
              (mapcar (lambda (group) (propertize (concat org-contacts-group-prefix group) 'org-contacts-group group))
                      (org-uniquify
                       (loop for contact in (org-contacts-filter)
                             with group-list
                             nconc (org-split-string
                                    (or (cdr (assoc-string "ALLTAGS" (caddr contact))) "") ":"))))
            (loop for contact in (org-contacts-filter)
                  ;; The contact name is always the car of the assoc-list
                  ;; returned by `org-contacts-filter'.
                  for contact-name = (car contact)
                  ;; Build the list of the user email addresses.
                  for email-list = (split-string (or
                                                  (cdr (assoc-string org-contacts-email-property (caddr contact)))
                                                  ""))
                  ;; If the user has email addresses…
                  if email-list
                  ;; … append a list of USER <EMAIL>.
                  nconc (loop for email in email-list
                              collect (concat contact-name " <" email ">")))))
         (completion-list (all-completions orig completion-list)))
    ;; If we are completing a group, and that's the only group, just return
    ;; the real result.
    (when (and group-completion-p
               (= (length completion-list) 1))
      (setq completion-list
            (list (concat (car completion-list) ";: "
                          (mapconcat 'identity
                                     (loop for contact in (org-contacts-filter
                                                           nil
                                                           (get-text-property 0 'org-contacts-group (car completion-list)))
                                           ;; The contact name is always the car of the assoc-list
                                           ;; returned by `org-contacts-filter'.
                                           for contact-name = (car contact)
                                           ;; Grab the first email of the contact
                                           for email = (car (split-string (or
                                                                           (cdr (assoc-string org-contacts-email-property (caddr contact)))
                                                                           "")))
                                           ;; If the user has an email address, append USER <EMAIL>.
                                           if email collect (concat contact-name " <" email ">"))
                                     ", ")))))
    (list start end (if org-contacts-completion-ignore-case
			(apply-partially #'completion-table-case-fold completion-list)
		      completion-list))))

(defun org-contacts-message-complete-function ()
  "Function used in `completion-at-point-functions' in `message-mode'."
  (let ((mail-abbrev-mode-regexp
         "^\\(Resent-To\\|To\\|B?Cc\\|Reply-To\\|From\\|Mail-Followup-To\\|Mail-Copies-To\\|Disposition-Notification-To\\|Return-Receipt-To\\):"))
        (when (mail-abbrev-in-expansion-header-p)
          (org-contacts-complete-name))))

(add-hook 'message-mode-hook
          (lambda ()
            (add-to-list 'completion-at-point-functions
                         'org-contacts-message-complete-function)))

(defun org-contacts-gnus-get-name-email ()
  "Get name and email address from Gnus message."
  (ignore-errors
    (gnus-with-article-headers
      (mail-extract-address-components
       (or (mail-fetch-field "From") "")))))

(defun org-contacts-gnus-article-from-get-marker ()
  "Return a marker for a contact based on From."
  (let* ((address (org-contacts-gnus-get-name-email))
         (name (car address))
         (email (cadr address)))
    (cadar (or (org-contacts-filter
                nil
                (concat org-contacts-email-property "={" (regexp-opt (list email)) "}"))
               (when name
                 (org-contacts-filter name))))))

(defun org-contacts-gnus-article-from-goto ()
  "Go to contact in the From address of current Gnus message."
  (interactive)
  (let ((marker (org-contacts-gnus-article-from-get-marker)))
    (when marker
      (switch-to-buffer-other-window (marker-buffer marker))
      (goto-char marker)
      (when (org-mode-p)
        (org-show-context 'agenda)
        (save-excursion
          (and (outline-next-heading)
               ;; show the next heading
               (org-flag-heading nil)))))))

(require 'gnus)
(define-key gnus-summary-mode-map ";" 'org-contacts-gnus-article-from-goto)

(defun org-contacts-anniversaries (&optional field format)
  "Compute FIELD anniversary for each contact, returning FORMAT.
Default FIELD value is \"BIRTHDAY\".

Format is a string matching the following format specification:

  %h - Heading name
  %l - Link to the heading
  %y - Number of year
  %Y - Number of year (ordinal)"
  (let ((calendar-date-style 'american)
        (entry ""))
    (loop for contact in (org-contacts-filter)
          for anniv = (let ((anniv (cdr (assoc-string
                                         (or field org-contacts-birthday-property)
                                         (caddr contact)))))
                        (when anniv
                          (calendar-gregorian-from-absolute
                           (org-time-string-to-absolute anniv))))
          ;; Use `diary-anniversary' to compute anniversary.
          if (and anniv (apply 'diary-anniversary anniv))
          collect (format-spec (or format "Birthday: %l (%Y)")
                               `((?l . ,(org-with-point-at (cadr contact) (org-store-link nil)))
                                 (?h . ,(car contact))
                                 (?y . ,(- (calendar-extract-year date)
                                           (calendar-extract-year anniv)))
                                 (?Y . ,(let ((years (- (calendar-extract-year date)
                                                        (calendar-extract-year anniv))))
                                          (format "%d%s" years (diary-ordinal-suffix years)))))))))

(defun org-completing-read-date (prompt collection
                                        &optional predicate require-match initial-input
                                        hist def inherit-input-method)
  "Like `completing-read' but reads a date.
Only PROMPT and DEF are really used."
  (org-read-date nil nil nil prompt nil def))

(add-to-list 'org-property-set-functions-alist
             `(,org-contacts-birthday-property . org-completing-read-date))

(defun org-contacts-template-name (&optional return-value)
  "Try to return the contact name for a template.
If not found return RETURN-VALUE or something that would ask the user."
  (or (car (org-contacts-gnus-get-name-email))
      return-value
      "%^{Name}"))

(defun org-contacts-template-email (&optional return-value)
  "Try to return the contact email for a template.
If not found return RETURN-VALUE or something that would ask the user."
  (or (cadr (org-contacts-gnus-get-name-email))
      return-value
      (concat "%^{" org-contacts-email-property "}p")))

(defun org-contacts-gnus-store-last-mail ()
  "Function called in `gnus-article-prepare-hook'.
Store a link between mails and contacts.

You can enable this when Gnus open a new article with:

  (add-hook 'gnus-article-prepare-hook 'org-contacts-gnus-store-last-mail)"
  (let ((marker (org-contacts-gnus-article-from-get-marker)))
    (when marker
      (save-excursion
        (set-buffer (marker-buffer marker))
        (goto-char marker)
        (let* ((org-email-link-description-format (or org-contacts-email-link-description-format
                                                      org-email-link-description-format))
               (link (gnus-with-article-buffer (org-store-link nil))))
          (org-set-property org-contacts-last-read-mail-property link))))))

(require 'gnus-art)
(add-hook 'gnus-article-prepare-hook 'org-contacts-gnus-store-last-mail)

;;;###autoload
(defun org-contacts (name)
  "Create agenda view for contacts matching NAME."
  (interactive (list (read-string "Name: ")))
  (let ((org-agenda-files (org-contacts-files))
        (org-agenda-skip-function
         (lambda () (org-agenda-skip-if nil `(notregexp ,name))))
        (org-agenda-overriding-header
         (or org-agenda-overriding-header
             (concat "List of contacts matching `" name "':"))))
    (setq org-agenda-skip-regexp name)
    (org-tags-view nil org-contacts-matcher)
    (with-current-buffer org-agenda-buffer-name
      (setq org-agenda-redo-command
            (list 'org-contacts name)))))

(provide 'org-contacts)
