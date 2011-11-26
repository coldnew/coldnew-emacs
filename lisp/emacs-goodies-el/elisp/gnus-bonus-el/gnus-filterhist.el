;;; gnus-filterhist.el --- Gnus Filter Histories -- parse nnmail-split-history to provide reports of mail splits.
;; Copyright (C) 1999 BrYan P. Johnson

;; Author: BrYan P. Johnson <bilko@onebabyzebra.com>
;; Keywords: news, mail

;; gnus-filterhist.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.

;; gnus-pers.el is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along
;; with GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;; Commentary:
;; Creates a buffer with a summary of the number of messages you've received per mailbox.
;; This summary is cleared every time you check mail. The buffer name is *Filter History*
;; To run:

;; in your .gnus:
;; (require 'gnus-filterhist)
;; (add-hook 'gnus-group-mode-hook 'gnus-filter-history)
;; (add-hook 'gnus-after-getting-new-news-hook 'gnus-filter-history)
;; I also have set up a window configuration like so:

;; (gnus-add-configuration
;;  '(group
;;    (horizontal 1.0
;;    (vertical 0.25
;;		     (group 0.85 point)
;;		     ("*Filter History*" 1.0)
;;		     )
;;    ("*scratch*" 1.0)
;;    )))

;; If you set gnus-filter-history-popup to t then the *Filter History*
;; buffer will only pop up when there is new mail.

;; Todo:

;;+ buttonize group names to open that group.
;;+ perhaps put current-split and session-split each in their own column. You can try this by setting gnus-filter-history-column-view to t



;;; Commentary:
;; 

;;; History:

;; 2001/05/10 BrYan P. Johnson <bilko@onebabyzebra.com> 
;; Added ability to only pop up if there's new mail. Check
;; gnus-filter-history-popup for more.
;;
;; 1999/12/07 BrYan P. Johnson <bilko@onebabyzebra.com>
;; Made this whole deal non-destructive with regards to nnmail variables.
;; Added a special font for the buffer so that if you want to change it you can (I like mine much smaller than the default font).
;;
;; 1999/11/08 BrYan P. Johnson <bilko@onebabyzebra.com>
;; Added session history. Added ability to clear session history.
;;
;; 1999/10/05  aaron .'. culich <aaron@math.umass.edu>
;; General cleanup to make things more concise.
;;
;; 1999/10/04  BrYan P. Johnson <bilko@onebabyzebra.com>
;; Created.

;;; Code:

(eval-when-compile (require 'cl))

(require 'nnmail)

(defvar gnus-filter-history-popup nil
  "Only popup the buffer when there is new mail.")

(defvar gnus-filter-history-show-session-history t
  "Show a session history.")

(defvar gnus-filter-history-show-current-split-history t
  "Show the current split.")

(defvar nnmail-session-split-history nil
  "Session history for mail splits.")

(defvar nnmail-current-split-history nil
  "Split history used by `gnus-filter-history'.")

(defvar gnus-filter-history-column-view nil
  "Show filter history in two columns rather than one.  This is untested.")

(defface gnus-filterhist-face-1 nil
  "Face used in gnus-filterhist buffer.")

(defun gnus-filter-history-clear-session ()
  "Clear the current gnus-filter-history-session."
  (interactive)
  (setq nnmail-session-split-history nil))

(defun gnus-filter-history-get-current-split ()
  "Determine which portions of `nnmail-split-history' are the current split."
  (let* ((current-nnmail-split-history nnmail-split-history)
	 my-current-splits my-current-split)
    (progn
      (setq my-current-splits (or (mapcar '(lambda (x)
					     (let ((y (if (and x (not (member x nnmail-current-split-history)))
							  x)))
					       (if y
						   y)))
					  current-nnmail-split-history)
				  current-nnmail-split-history))
      (setq my-current-split (delete nil my-current-splits))
      (if nnmail-current-split-history
	  (setq nnmail-current-split-history (append nnmail-current-split-history my-current-split))
	(setq nnmail-current-split-history my-current-split))
      my-current-split)))

(defun gnus-filter-history ()
  "Create a buffer *Filter History* with the results of the latest nnmail split."
  (interactive)
  (let ((buf (current-buffer)))
    (let* ((my-split-history (gnus-filter-history-get-current-split))
	   (my-session-split-history (append nnmail-session-split-history my-split-history))
	   (my-current-split-shown nil))
      (progn
	(if (and gnus-filter-history-popup (not(and gnus-filter-history-show-current-split-history my-split-history)))
	    (progn
	      (if (buffer-live-p (get-buffer "*Filter History*"))
		  (kill-buffer "*Filter History*")))
	  (progn
	    (with-output-to-temp-buffer "*Filter History*"
	      (save-excursion
		(switch-to-buffer "*Filter History*")
		(princ (current-time-string))
		(if (and gnus-filter-history-show-current-split-history my-split-history)
		    (progn
		      (princ (concat
			      "\n -- Current Split -- \n"
			      (mapconcat (lambda (elem) (if elem (concat (car elem) " : " (number-to-string (cdr elem)))))
					 (let ((history (sort (mapcar 'caar my-split-history)
							      'string<)))
					   (mapcar (lambda (x) (cons x (count x history :test 'string=)))
						   (let (new (tail history))
						     (while tail
						       (or (member (car tail) new)
							   (setq new (cons (car tail) new)))
						       (setq tail (cdr tail)))
						     (nreverse new))))
					 "\t\n")
			      "\n"))
		      (setq my-current-split-shown t))
		  (princ "\n -- No Current Split -- "))
		(if gnus-filter-history-show-session-history
		    (progn
		      (let ((beg (point)))
			(progn
			  (princ (concat
				  "\n -- Session Split -- \n"
				  (mapconcat (lambda (elem) (if elem (concat (car elem) " : " (number-to-string (cdr elem)))))
					     (let ((history (sort (mapcar 'caar my-session-split-history)
								  'string<)))
					       (mapcar (lambda (x) (cons x (count x history :test 'string=)))
						       (let (new (tail history))
							 (while tail
							   (or (member (car tail) new)
							       (setq new (cons (car tail) new)))
							   (setq tail (cdr tail)))
							 (nreverse new))))
					     "\n")))
			  (if (and my-current-split-shown gnus-filter-history-column-view)
			      (let* ((max (point-max))
				     (end (+ max 25))
				     )
				(progn
				  (goto-char max)
				  (princ "\n                     ")
				  (kill-rectangle beg end)
				  (goto-char (point-min))
				  (end-of-line)
				  (yank-rectangle)
				  )
				)
			    )))
		      (setq nnmail-session-split-history my-session-split-history)
		  ))))
	(add-text-properties (point-min) (point-max)
			     '(face gnus-filterhist-face-1))
	(switch-to-buffer buf)
	(if gnus-filter-history-popup
	    (pop-to-buffer "*Filter History*")
	  )
      ))))))

(provide 'gnus-filterhist)

;;; gnus-filterhist.el ends here
