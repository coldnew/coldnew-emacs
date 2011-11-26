;;; gnus-outlook-deuglify.el --- deuglify broken Outlook (Express) articles

;; Copyright (C) 2001,2002 Raymond Scholz

;; Author: Raymond Scholz <rscholz@zonix.de>
;;         Thomas Steffen (unwrapping algorithm,
;;                         based on an idea of Stefan Monnier)
;; Keywords: mail, news
;; Version: $Id: gnus-outlook-deuglify.el,v 1.1.1.1 2003-04-04 20:16:02 lolando Exp $
;; X-URL: http://www.zonix.de/div/el/deuglify/gnus-outlook-deuglify.el

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

;; This file enables Gnus to repair broken citations produced by
;; common user agents like MS Outlook (Express).  It may repair
;; articles of other user agents too.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Outlook sometimes wraps cited lines before sending a message as
;; seen in this example:
;;
;; Example #1
;; ----------
;;
;; John Doe wrote:
;;
;; > This sentence no verb.  This sentence no verb.  This sentence
;; no
;; > verb.  This sentence no verb.  This sentence no verb.  This
;; > sentence no verb.
;;
;; The function `gnus-outlook-unwrap-lines' tries to recognize those
;; erroneously wrapped lines and will unwrap them.  I.e. putting the
;; wrapped parts ("no" in this expample) back where they belong (at the
;; end of the cited line above).
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Note that some people not only use broken user agents but also
;; practise a bad citation style by omitting blank lines between the
;; cited text and their own text.
;:
;; Example #2
;; ----------
;;
;; John Doe wrote:
;;
;; > This sentence no verb.  This sentence no verb.  This sentence no
;; You forgot in all your sentences.
;; > verb.  This sentence no verb.  This sentence no verb.  This
;; > sentence no verb.
;;
;; Unwrapping "You forgot in all your sentences." would be illegal as
;; this part wasn't intended to be cited text.
;; `gnus-outlook-unwrap-lines' will only unwrap lines if the resulting
;; citation line will be of a certain maximum length.  You can control
;; this by adjusting `gnus-outlook-deuglify-unwrap-max'.  Also
;; unwrapping will only be done if the line above the (possibly)
;; wrapped line has a minumum length of `gnus-outlook-deuglify-unwrap-min'.
;;
;; Furthermore no unwrapping will be undertaken if the last character
;; is one of the chars specified in
;; `gnus-outlook-deuglify-unwrap-stop-chars'.  Setting this to ".?!"
;; inhibits unwrapping if the cited line ends with a full stop,
;; question mark or exclamation mark.  Note that this variable
;; defaults to `nil', triggering a few false positives but generally
;; giving you better results.
;;
;; Unwrapping works on every level of citation.  Thus you will be able
;; repair broken citations of broken user agents citing broken
;; citations of broken user agents citing broken citations...
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Citations are commonly introduced with an attribution line
;; indicating who wrote the cited text.  Outlook adds superflous
;; information that can be found in the header of the message to this
;; line and often wraps it.
;;
;; If that weren't enough, lots of people write their own text above
;; the cited text and cite the complete original article below.
;;
;; Example #3
;; ----------
;;
;; Hey, John.  There's no in all your sentences!
;;
;; John Doe <john.doe@some.domain> wrote in message
;; news:a87usw8$dklsssa$2@some.news.server...
;; > This sentence no verb.  This sentence no verb.  This sentence
;; no
;; > verb.  This sentence no verb.  This sentence no verb.  This
;; > sentence no verb.
;; >
;; > Bye, John
;;
;; Repairing the attribution line will be done by function
;; `gnus-outlook-repair-attribution' which calls other function that
;; try to recognize and repair broken attribution lines.  See variable
;; `gnus-outlook-deuglify-attrib-cut-regexp' for stuff that should be
;; cut off from the beginning of an attribution line and variable
;; `gnus-outlook-deuglify-attrib-verb-regexp' for the verbs that are
;; required to be found in an attribution line.  These function return
;; the point where the repaired attribution line starts.
;;
;; Rearranging the article so that the cited text appears above the
;; new text will be done by function
;; `gnus-outlook-rearrange-citation'.  This function calls
;; `gnus-outlook-repair-attribution' to find and repair an attribution
;; line.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Well, and that's what the message will look like after appliying
;; deuglification:
;;
;; Example #3 (deuglified)
;; -----------------------
;;
;; John Doe <john.doe@some.domain> wrote:
;;
;; > This sentence no verb.  This sentence no verb.  This sentence no
;; > verb.  This sentence no verb.  This sentence no verb.  This
;; > sentence no verb.
;; >
;; > Bye, John
;;
;; Hey, John.  There's no in all your sentences!
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Usage
;; -----
;;
;; Put this in your .gnus:
;;
;; (require 'gnus-outlook-deuglify)
;;
;; and you're enabled to press `W k' in the Summary Buffer.
;;
;; Non recommended usage :-)
;; ---------------------
;;
;; To automatically invoke deuglification on every article you read,
;; put something like that in your .gnus:
;;
;; (add-hook 'gnus-article-decode-hook 'gnus-outlook-unwrap-lines)
;;
;; or _one_ of the following lines:
;;
;; ;; repair broken attribution lines
;; (add-hook 'gnus-article-decode-hook 'gnus-outlook-repair-attribution)
;;
;; ;; repair broken attribution lines and citations
;; (add-hook 'gnus-article-decode-hook 'gnus-outlook-rearrange-citation)
;;
;; Note that there always may be some false positives, so I suggest
;; using the manual invokation.  After deuglification you may want to
;; refill the whole article using `W w'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Limitations
;; -----------
;;
;; As I said before there may (or will) be a few false positives on
;; unwrapping cited lines with `gnus-outlook-unwrap-lines'.
;;
;; `gnus-outlook-repair-attribution' will only fix the first
;; attribution line found in the article.  Furthermore it fixed to
;; certain kinds of attributions.  And there may be horribly many
;; false positives, vanishing lines and so on -- so don't trust your
;; eyes.  Again I recommend manual invocation.
;;
;; `gnus-outlook-rearrange-citation' carries all the limitations of
;; `gnus-outlook-repair-attribution'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; $Log: gnus-outlook-deuglify.el,v $
;; Revision 1.1.1.1  2003-04-04 20:16:02  lolando
;; Initial import, based on version 19.2-1 currently in unstable.
;;
;; Revision 1.2  2001/12/30 20:14:34  rscholz
;; Clean up source.
;;
;; Revision 1.1  2001/12/30 20:13:32  rscholz
;; Initial revision
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:

(require 'gnus-art)
(require 'gnus-sum)

(defconst gnus-outlook-deuglify-version "$Revision: 1.1.1.1 $"
  "Version of gnus-outlook-deuglify.")

(defvar gnus-outlook-deuglify-debug nil
  "Enable debugging information.")

;;; User Customizable Variables:

(defgroup gnus-outlook-deuglify nil
  "Deuglify articles generated by broken user agents like MS 
Outlook (Express).")

(defcustom gnus-outlook-deuglify-unwrap-min 45
  "Minimum length of the cited line above the (possibly) wrapped line."
  :type 'number
  :group 'gnus-outlook-deuglify)

(defcustom gnus-outlook-deuglify-unwrap-max 95
  "Maximum length of the cited line after unwrapping."
  :type 'number
  :group 'gnus-outlook-deuglify)

(defcustom gnus-outlook-deuglify-unwrap-stop-chars nil ;; ".?!"
  "Characters that inhibit unwrapping if they are the last one on the
cited line above the possible wrapped line."
  :type 'string
  :group 'gnus-outlook-deuglify)

(defcustom gnus-outlook-deuglify-cite-marks ">|#%"
  "Characters that indicate cited lines."
  :type 'string
  :group 'gnus-outlook-deuglify)

(defcustom  gnus-outlook-deuglify-attrib-cut-regexp
  "\\(On \\)?\\(Mon\\|Tue\\|Wed\\|Thu\\|Fri\\|Sat\\|Sun\\),[^,]+, "
  "Regular expression matching the beginning of an attribution line
that should be cut away."
  :type 'string
  :group 'gnus-outlook-deuglify)

(defcustom gnus-outlook-deuglify-attrib-verb-regexp
  "wrote\\|writes\\|schrieb\\|schreibt\\|skrev\\|a écrit\\|schreef"
  "Regular expression matching the verb used in an attribution line."
  :type 'string
  :group 'gnus-outlook-deuglify)

(defcustom  gnus-outlook-deuglify-attrib-end-regexp
  ":\\|\\.\\.\\."
  "Regular expression matching the end of an attribution line."
  :type 'string
  :group 'gnus-outlook-deuglify)


;; Functions

(defun gnus-outlook-unwrap-lines ()
  "Unwrap lines that appear to be wrapped citation lines.  You can
control what lines will be unwrapped by frobbing
`gnus-outlook-deuglify-unwrap-min' and
`gnus-outlook-deuglify-unwrap-max', indicating the miminum and maximum
length of an unwrapped citation line."
  (interactive)
  (let ((case-fold-search nil)
        (inhibit-read-only t)
	(cite-marks gnus-outlook-deuglify-cite-marks)
	(stop-chars gnus-outlook-deuglify-unwrap-stop-chars))
    (with-current-buffer (or gnus-article-buffer (current-buffer))
      (article-goto-body)
      (while (re-search-forward
	      (concat
	       "^\\([ "
	       cite-marks
	       "]*\\)\\(["
	       cite-marks
	       "].*[^\n "
	       stop-chars
	       "]\\)\n\\1\\([^\n "
	       cite-marks
	       "]+.*\\)$")
              nil t)
	(let ((len12 (- (match-end 2) (match-beginning 1)))
	      (len3 (- (match-end 3) (match-beginning 3))))
	  (if (and (> len12 gnus-outlook-deuglify-unwrap-min)
		   (< (+ len12 len3) gnus-outlook-deuglify-unwrap-max))
	      (progn 
		(replace-match "\\1\\2 \\3")
		(goto-char (match-beginning 0)))))))))

(defun gnus-outlook-rearrange-article (from-where)
  "Put the text from `from-where' to the end of buffer at the top of
the article buffer."
  (let ((inhibit-read-only t))
    (with-current-buffer (or gnus-article-buffer (current-buffer))
      (kill-region from-where (point-max))
      (article-goto-body)
      (yank)
      (insert "\n"))))

(defun gnus-outlook-repair-attribution-outlook ()
  "Repair a broken attribution line (Outlook)."
  (interactive)
  (let ((case-fold-search nil)
        (inhibit-read-only t)
	(cite-marks gnus-outlook-deuglify-cite-marks))
    (with-current-buffer (or gnus-article-buffer (current-buffer))
      (article-goto-body)
      (if (re-search-forward
	   (concat "^\\([^" cite-marks "].+\\)"
		   "\\(" gnus-outlook-deuglify-attrib-verb-regexp "\\)"
		   "\\(.*\n[^\n" cite-marks "].+\\)?.*"
		   "\\(" gnus-outlook-deuglify-attrib-end-regexp "\\)$")
	   nil t)
	  (progn
	    (replace-match "\\1\\2\\4\n")
	    (match-beginning 0))))))

(defun gnus-outlook-repair-attribution-other ()
  "Repair a broken attribution line (other user agents than Outlook)."
  (interactive)
  (let ((case-fold-search nil)
        (inhibit-read-only t)
	(cite-marks gnus-outlook-deuglify-cite-marks))
    (with-current-buffer (or gnus-article-buffer (current-buffer))
      (article-goto-body)
      (if (re-search-forward
	   (concat "^\\("gnus-outlook-deuglify-attrib-cut-regexp"\\)?"
		   "\\([^" cite-marks "].+\\)\n\\(.+\\)?"
		   "\\(" gnus-outlook-deuglify-attrib-verb-regexp "\\).*"
		   "\\(" gnus-outlook-deuglify-attrib-end-regexp "\\)$")
	   nil t)
	  (progn
	    (replace-match "\\4 \\5\\6\\7\n")
	    (match-beginning 0))))))

(defun gnus-outlook-repair-attribution ()
  "Repair a broken attribution line."
  (or
   (gnus-outlook-repair-attribution-other)
   (gnus-outlook-repair-attribution-outlook)))

(defun gnus-outlook-rearrange-citation ()
  "Repair broken citations."
  (let ((attrib-start (gnus-outlook-repair-attribution)))
    ;; rearrange citations
    (if attrib-start
	(gnus-outlook-rearrange-article attrib-start))))

(defun gnus-outlook-deuglify-article ()
  "Deuglify broken Outlook (Express) articles."
  (interactive)
  ;; apply treatment of dumb quotes
  (gnus-article-treat-dumbquotes)
  ;; repair wrapped cited lines
  (gnus-outlook-unwrap-lines)
  ;; repair attribution line
  (gnus-outlook-rearrange-citation)
  ;; finally redisplay the article buffer
  (with-current-buffer (or gnus-article-buffer (current-buffer))
    (gnus-article-prepare-display)))

;; luckily `W k' (mnemonic "Wash outlooK") isn't bound in Gnus :-)
(define-key gnus-summary-wash-map "k" 'gnus-outlook-deuglify-article)

(provide 'gnus-outlook-deuglify)

;;; gnus-outlook-deuglify.el ends here
