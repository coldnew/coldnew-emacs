;;; gnus-eyecandy.el --- add some eyecandy to Gnus
;; Copyright (C) 1999 BrYan P. Johnson

;; Author: BrYan P. Johnson <bilko@onebabyzebra.com>
;; Keywords: news, mail, gnus

;; Modified 2003-10-15 to work with GNU Emacs
;; by Johan Bockgård <bojohan@dd.chalmers.se>

;; gnus-eyecandy.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.

;; gnus-eyecandy.el is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along
;; with GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;;; Commentary:

;; in your .gnus file:

;; (require 'gnus-eyecandy)
;; (add-hook 'gnus-group-update-hook 'gnus-group-line-add-icon)

;; Try something like:
;; (setq gnus-group-glyph-directory "~/groupicons")
;; (setq gnus-group-icon-list
;;       '(((and (string-match "spam\\|junk" group) (> unread 0)) . "minispam.xpm")
;;         ((and (> unread 0)(> ticked 0)) . "mini-exclam.xpm")
;;         ((> unread 0) . "mini-mail.xpm")
;;         ((> ticked 0) . "mini.checkmark.xpm")
;;         ((and (equal group "gnu.emacs.gnus")(= unread 0)) . "FaceSad.xpm")
;;         ))

;; Then just M-x customize gnus-group-icons

; History:

; 11/16 First release

;Todo:

; + Have it search mulitple glyph directories
; + Glyphs for Topics.

;;; Code:

;; Straight variables

(defvar gnus-group-icon-cache nil)
(defvar gnus-group-running-xemacs (string-match "XEmacs" emacs-version))


(if gnus-group-running-xemacs
    (require 'gnus-xmas)
  (defvar  gnus-xmas-glyph-directory nil))

;; Customizable variables

(defgroup gnus-group-icons nil
  "Add Icons to your group buffer.  "
  :group 'gnus-group-visual)

(defcustom gnus-group-icon-list
  nil
  "*Controls the insertion of icons into group buffer lines.

Below is a list of `Form'/`File' pairs.  When deciding how a
particular group line should be displayed, each form is evaluated.
The icon from the file field after the first true form is used.  You
can change how those group lines are displayed by editing the file
field.  The File will either be found in the
`gnus-group-glyph-directory' or by designating absolute path to the
file.

It is also possible to change and add form fields, but currently that
requires an understanding of Lisp expressions.  Hopefully this will
change in a future release.  For now, you can use the following
variables in the Lisp expression:

group: The name of the group.
unread: The number of unread articles in the group.
method: The select method used.
mailp: Whether it's a mail group or not.
newsp: Whether it's a news group or not
level: The level of the group.
score: The score of the group.
ticked: The number of ticked articles."
  :group 'gnus-group-icons
  :type '(repeat (cons (sexp :tag "Form") file)))

(defcustom gnus-group-icon-at-end t
  "*Controls the positioning of the group icons.
If non-nil, icons will be placed at end of the group line."
  :group 'gnus-group-icons
  :type 'boolean)

(defcustom gnus-group-glyph-directory gnus-xmas-glyph-directory
  "*Directory where gnus group icons are located.
Defaults to `gnus-xmas-glyph-directory'."
  :group 'gnus-group-icons
  :type 'directory
  )

;; Our functions. What wasn't blatantly stolen from smiley.el was
;; blatantly stolen from gnus-group.el.

(defun gnus-group-line-add-icon ()
  "Highlight the current line according to `gnus-group-icon-list'."
  (let* ((p (point))
	 (end (progn (end-of-line) (point)))
	 ;; now find out where the line starts and leave point there.
	 (beg (progn (beginning-of-line) (point)))
	 (group (gnus-group-group-name))
	 (entry (gnus-group-entry group))
	 (unread (if (numberp (car entry)) (car entry) 0))
	 (active (gnus-active group))
	 (total (if active (1+ (- (cdr active) (car active))) 0))
	 (info (nth 2 entry))
	 (method (gnus-server-get-method group (gnus-info-method info)))
	 (marked (gnus-info-marks info))
	 (mailp (memq 'mail (assoc (symbol-name
				    (car (or method gnus-select-method)))
				   gnus-valid-select-methods)))
	 (level (or (gnus-info-level info) gnus-level-killed))
	 (score (or (gnus-info-score info) 0))
	 (ticked (gnus-range-length (cdr (assq 'tick marked))))
	 (group-age (gnus-group-timestamp-delta group))
	 (inhibit-read-only t)
	 (list gnus-group-icon-list))
    (progn
      (goto-char beg)
      (if gnus-group-icon-at-end
	  (progn
	    (goto-char end)
	    (princ "   ")))
      (let* ((mystart (if gnus-group-icon-at-end end beg))
	     (myend (if gnus-group-icon-at-end mystart (+ mystart 1))))
	;; When myend = mystart and glyph is at start, should it be supposed to
	;; show up next to a group that's the first group in a topic, it ends up
	;; next to the topic.
	(goto-char beg)
	(while (and list
		    (not (eval (caar list))))
	  (setq list (cdr list)))
	(if list
	    (let* ((file (cdar list))
		   (glyph (gnus-group-icon-create-glyph (buffer-substring mystart myend)
							file)))
	      (when glyph
		(if gnus-group-running-xemacs
		    (progn
		      (mapcar 'delete-annotation (annotations-at myend))
		      (let ((ext (make-extent mystart myend))
			    (ant (make-annotation glyph myend 'text)))
			;; set text extent params
			(set-extent-property ext 'end-open t)
			(set-extent-property ext 'start-open t)))
		  ;; should we do something more clever if inserting at front?
		  (goto-char mystart)
		  (insert-image glyph)))))))
    (goto-char p)))

(defun gnus-group-icon-create-glyph (substring pixmap)
  (or
   (cdr-safe (assoc pixmap gnus-group-icon-cache))
   (let* ((glyph
	   (if gnus-group-running-xemacs
	       (make-glyph (list
			    (cons 'x (expand-file-name pixmap gnus-group-glyph-directory))
			    (cons 'mswindows
				  (expand-file-name pixmap gnus-group-glyph-directory))
			    (cons 'tty substring)))
	     (create-image (expand-file-name pixmap gnus-group-glyph-directory)))))
     (setq gnus-group-icon-cache (cons (cons pixmap glyph) gnus-group-icon-cache))
     (if gnus-group-running-xemacs
	 (set-glyph-face glyph 'default)
       ;; (set-glyph-contrib-p glyph nil)
       )
     glyph)))


(provide 'gnus-eyecandy)

;;; gnus-eyecandy.el ends here

