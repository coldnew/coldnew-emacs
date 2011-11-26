;;; gnus-junk.el --- a response to junk e-mails

;; Copyright (C) 1996,1997 Robert Bihlmeyer

;; Author: Robert Bihlmeyer <robbe@orcus.priv.at>
;; Version: $Revision: 1.1.1.1 $
;; Keywords: mail unsolicited commercial junk spam
;; X-URL: http://stud2.tuwien.ac.at/~e9426626/gnus-junk.html

;; LCD Archive Entry:
;; gnus-junk|Robert Bihlmeyer|robbe@orcus.priv.at|
;; Semiautomatic replys to junk e-mails for Gnus|
;; $Date: 2003-04-04 20:16:01 $|$Revision: 1.1.1.1 $|~/misc/gnus-junk.el.gz|

;; This file is not yet part of anything.

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along
;; with GNU Emacs; see the file COPYING. If not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;;; Commentary:

;; This code somewhat automates complaints about unsolicited commercial e-mail
;; (uce). Essentially, it prepares a message based on some standard text and
;; (parts of) the offending mail, and addresses it to a number of mailboxes.
;; All you have to do is check the message (especially the list of recipients)
;; for sanity, and send it.

;; gnus-junk is intended for Gnus and Message (which is included in the Gnus
;; distribution). But you guessed this from the name, didn't you, you
;; clever-dick? In fact, the dependencies are not dramatic, and
;; conversion/generalization to other Emacs-based MUAs is possible. But who
;; would want to read mail by other means than with Gnus? And, no, I won't
;; write a version for Pine. Feel free to do it yourself!

;; The main function is gnus-junk-complain, which does all the things
;; described above. It is intended to be bound to a key in gnus-summary-map.

;; A few ideas were taken from Stanislav Shalunov's `uce.el'.


;; Compatibility:

;; This version of gnus-junk should work with Gnus 5.3 and above. These Gnusii
;; in turn need Emacs 19.32 (and better), or XEmacs 19.14 (and better) to
;; work. The newest version of Gnus is availiable from
;; <http://www.ifi.uio.no/~larsi/gnus.tar.gz>.

;; gnus-junk supports Customization, if you have a recent version of `custom'.
;; A sufficient version comes with Gnus 5.4 and above (or Red Gnus >=0.36). So
;; you can upgrade by getting the newest Gnus (see above) or fetching custom
;; separately from <http://www.dina.kvl.dk/~abraham/custom/>. Or get Emacs
;; 19.35 (not availiable at time of writing), XEmacs 20.2, or XEmacs 19.15.


;; Installation:

;; Get it! The newest version of gnus-junk is availiable at the above URL.

;; Put gnus-junk somewhere into your loadpath (for example into the directory
;; where Gnus resides).

;; Insert the following into your .emacs or .gnus file:

;; (autoload 'gnus-junk-complain "gnus-junk" "(not loaded yet)" t)
;; (add-hook 'gnus-sum-load-hook (lambda nil
;;				(define-key gnus-summary-mode-map [(?$)]
;;				  'gnus-junk-complain)))


;; Future Plans:

;; Select from a couple of complaint-texts. For example, the legal text cited
;; below applies only to US-spammers. What are the criterias?

;; Additionally, MMF/pyramid scams by US-citizens should probably be cc'ed to
;; the IRS and/or FBI. Again: How do you recognize US-addresses?

;; Should we read more headers?

;; Be more intelligent about `Received:'

;; Use `whois' or `traceroute' if possible, to gain more information. For
;; example one could find if the ISP is in the USA, which would help with the
;; above issues.

;; If tm is availiable, generate MIME-messages.

;; Incooperate other types of complaints or automatic replys: EMP, ECP,
;; perhaps even vacation-like features. This would necessiate a change of
;; name.


;; Final words:

;; If you have any comments or bug-reports, just send them to the
;; address mentioned above.


;;; Code:

(require 'gnus)
(require 'message)
(require 'mail-utils)
(require 'cl)

(eval-and-compile
  (condition-case nil (require 'custom)
    (file-error nil))
  ;; if custom is not there or way old, define some replacement macros
  (if (not (fboundp 'defgroup))
      (progn 
	(defmacro defgroup (symbol members doc &rest args)
	  "Stub definition used if a recent custom is not availiable."
	  (list 'quote symbol))

	(defmacro defcustom (symbol value doc &rest args)
	  "Stub definition used if a recent custom is not availiable.
Simply `defvar's the SYMBOL with VALUE, and DOC, ignoring ARGS."
	  (list 'defvar symbol value doc)))))

;;; Customization.

(defgroup gnus-junk nil
  "Gnus' semi-automatic flamethrower to roast junk e-mail out of existence."
  :group 'gnus)

(defcustom gnus-junk-recipients '((from t "postmaster" "abuse")
			      (reply-to t "postmaster" "abuse")
			      (sender t "postmaster" "abuse")
			      (received "postmaster" "abuse")
			      (message-id "postmaster" "abuse"))
  "Defines the mailboxes that will receive the complaint.
This is a list of lists, the car of each being one of: `from', `sender',
`reply-to', `received', or `message-id'. The cdr lists the recipients on the
host computed from the appropriate header. The special recipient `t' is
replaced by the whole address, when processing `from' or `sender'.

During address collection, each of the relevant headers is processed. If
a list with an appropriate carr is found, a hostname is extracted from the
header and used in conjunction with the list's cdr to construct the complete
recipient-addresses. This is done by appending `@' and the hostname to each
recipient. If the recipient is not a string, but rather the symbol `t', the
complete mail-address from the header is taken instead. Using this with
`received' or `message-id' (which contain no mail-address) yields undefined
results."
  :group 'gnus-junk
  :type '(repeat (list :format "%v"
		       (choice :tag "Header" :value from (const from)
			       (const reply-to)
			       (const sender)
			       (const received)
			       (const message-id))
		       (repeat :inline t :tag "Recipients"
			       (choice (string :format "%v") (const t))))))

(defcustom gnus-junk-trim-domains t
  "*Non-nil enables trimming of domains in the list of recipients.
I.e. mailboxes on more specialized domains will not be recipients,
if the same mailbox is already adressed on a domain that is a
substring of this one.

For example: `abuse@foo.bar.baz.com' will be dropped, if one or more of
`abuse@bar.baz.com', `abuse@baz.com', or `abuse@com' is present in the list of
recipients."
  :group 'gnus-junk
  :type 'boolean)

(defcustom gnus-junk-forbidden-recipients '("abuse@msn.com"
					    "postmaster@mci.net"
					    "postmaster@aol.com")
  "*A list of mail-addresses that should never be recipients.
This is to avoid penetrating innocent people that only have a misleading
address."
  :group 'gnus-junk
  :type '(repeat (string :format "%v")))

(defcustom gnus-junk-subject-format "E-mail abuse report (was: %s)"
  "*Format of the subject line for complaints.
%s is replaced by the original subject."
  :group 'gnus-junk
  :type 'string)

(defcustom gnus-junk-default-headers
  "Errors-To: nobody@localhost\nPrecedence: bulk\n"
  "*Default headers to include into the complaint.
Each header in this string should be on a seperate line (i.e. have a `\\n' at
the end."
  :group 'gnus-junk
  :type '(string :format "%[%t%]:\n%v"))

;; If you want to get formal, the following text could just work with US
;; Spammers:

; By US Code Title 47, Sec. 227(a)(2)(B), a computer/modem/printer meets the
; definition of a telephone fax machine.  By Sec. 227(b)(1)(C), it is
; unlawful to send any unsolicited advertisement to such equipment.  By
; Sec. 227(b)(3)(C), a violation of the aforementioned section is punishable
; by action to recover actual monetary loss, or $500, whichever is greater,
; for each violation.

; Further violations from your site will be reported to abuse@postoffice.us.

(defcustom gnus-junk-complaint-text
  '(format "Hi,

the following unsolicited junk e-mail (%sincluded below)
was sent to me - apparently from your host. Please stop this
practice, as it is an abuse of the Internet mail system!

" (if gnus-junk-include-body "" "only headers "))
  "The main text of the complaint.
If t, the file `gnus-junk-complaint-file' will be used instead.
If a function, the result of the function will be used instead.
If a form, the result of the form will be used instead."
  :group 'gnus-junk
  :type '(choice (const :tag "A file" t) string function sexp))

(defcustom gnus-junk-complaint-file "~/.complaint"
  "The name of a file where the complaint text is stored.
If a function, the result of the function will be used instead.
If a form, the result of the form will be used instead.
This variable is consulted only when `gnus-junk-complaint-text' is t."
  :group 'gnus-junk
  :type '(choice (file :must-match t) function sexp))

(defcustom gnus-junk-original-seperator
  '(if gnus-junk-include-body
      "\n--------------------original--message--follows---------------------\n"
    "\n--------------------original's--headers--follow--------------------\n")
  "The text to put before the original message
If a function, the result of the function will be used instead.
If a form, the result of the form will be used instead."
  :group 'gnus-junk
  :type '(choice string function sexp))

(defcustom gnus-junk-original-end-seperator ""
  "The text to put after the original message
If a function, the result of the function will be used instead.
If a form, the result of the form will be used instead."
  :group 'gnus-junk
  :type '(choice string function sexp))

(defcustom gnus-junk-signature 'normal
  "*The signature used in complaints.
See the documentation of `message-signature' for an explanation of possible
values.
Additionally, setting this to the special symbol `normal', uses the normal
value of `message-signature'."
  :group 'gnus-junk
  :type '(choice (const normal) string function sexp))

(defcustom gnus-junk-original-header-replacement '(("^X-From-Line:" . "From")
						   ("^Xref: .*\n" . ""))
  "*An alist which pairs headers of the original with their replacements.
The original mail usually contains some headers added by your MUA. These can
be deleted or transformed to original form with these alist. Each cons-cell
lists a regular expression to search for, and a replacement."
  :group 'gnus-junk
  :type '(repeat (cons :format "%v"
		       (regexp :tag "match") (string :tag "replace with"))))

(defcustom gnus-junk-include-body 10
  "*Says how many lines of the original's body will be included.
T means that the whole original will be included verbatim.
Nil means that only the headers will be included.
An integer value means that this many lines will be included at max." 
  :group 'gnus-junk
  :type '(choice (const :tag "All" t)
		 (integer :tag "Max Lines")
		 (const :tag "None" nil)))

(defcustom gnus-junk-elide-text
  "[Note: rest of original removed for brevity]\n"
  "The text to put after a shortened message.
If a function, the result of the function will be used instead.
If a form, the result of the form will be used instead.
Will be used only in cases where the original is shortened (i.e.
`gnus-junk-include-body' is not t)."
  :group 'gnus-junk
  :type '(choice string function sexp))


;;; Interface.

;;;###autoload
(defun gnus-junk-complain (n)
  "Mail a complaint about next N messages to (hopefully) relevant people.
If N is negative, whine about the previous N messages."
  (interactive "P")
  (let (buf lastbuf
	    (articles (gnus-summary-work-articles n)))
    (if (equal articles (reverse gnus-newsgroup-processable))
	(gnus-summary-unmark-all-processable))
    (while articles
      (setq lastbuf buf)
      (gnus-summary-goto-article (pop articles))
      (gnus-junk-complain-about-current-message)
      (setq buf (current-buffer))
      (message-add-action (if lastbuf
			      (list 'switch-to-buffer lastbuf) 
			    '(gnus-configure-windows article))
			  'exit 'kill 'postpone))))

(defun gnus-junk-complain-about-current-message nil
  "Prepares a complaint about the currently displayed message.
A message buffer is set up and you are free to edit, send, or abort the
complaint."
  (let ((headers (gnus-eval-in-buffer-window gnus-original-article-buffer
		    (gnus-junk-get-headers)))
	 to addr recp)
    (if (null headers) (error "No headers found in article!"))
    (mapcar
     (lambda (header)
       (and (setq recp (cdr (assq header gnus-junk-recipients)))
	    (setq addr (gnus-mail-strip-quoted-names (mail-header header)))
	    (setq to (append to (gnus-junk-make-recipients-from-address
				 addr recp)))))
     '(from reply-to sender message-id))
    (and (setq recp (cdr (assq 'received gnus-junk-recipients)))
	 (setq to (append to (gnus-junk-make-recipients-from-received
			       (reverse headers) recp))))
    (let ((message-default-mail-headers
	   (concat
	    (and message-default-mail-headers
		 (if (string-match "\\<[GF]cc: .*\n"
				   message-default-mail-headers)
		     (replace-match "" t t message-default-mail-headers)
		   message-default-mail-headers)
		 gnus-junk-default-headers)))
	  (message-signature
	   (if (eq gnus-junk-signature 'normal) message-signature
	     gnus-junk-signature)))
      (message-mail (mapconcat 'identity
			       (set-difference
				(gnus-junk-maybe-trim-domains
				 (remove-duplicates to :test
						    'string-caseless-equal))
				gnus-junk-forbidden-recipients :test
				'string-caseless-equal)
			       ", ")
		    (format gnus-junk-subject-format (or
						      (mail-header 'subject)
						      "(no subject)"))))
    (if (eq gnus-junk-complaint-text t)
	(insert-file-contents (gnus-junk-var-eval gnus-junk-complaint-file))
      (insert (gnus-junk-var-eval gnus-junk-complaint-text)))
    (goto-char (point-max))
    (insert (gnus-junk-var-eval gnus-junk-original-seperator))
    (insert-buffer gnus-original-article-buffer)
    (mapcar (lambda (x) (if (re-search-forward (car x) nil t)
			    (replace-match (cdr x))))
	    gnus-junk-original-header-replacement)
    (if (and (not (eq gnus-junk-include-body t))
	     (search-forward-regexp "^$" nil t))
	(progn
	  (if gnus-junk-include-body
	      (forward-line gnus-junk-include-body))
	  (insert gnus-junk-elide-text)
	  (delete-region (point) (point-max))))
    (goto-char (point-max))
    (insert (gnus-junk-var-eval gnus-junk-original-end-seperator))
    (set-buffer-modified-p nil)
    (message-goto-body)))

;;; Internal functions:

(defun string-caseless-equal (s1 s2)
  "Just like `string-equal', but ignore case."
  (string-equal
   (downcase (if (symbolp s1) (symbol-name s1) s1))
   (downcase (if (symbolp s2) (symbol-name s2) s2))))

(defun gnus-junk-get-headers nil
  ;; extract headers from an article
  (goto-char (point-min))
  (if (looking-at "From ")
      (forward-line))
  (mail-header-extract))

(defun gnus-junk-var-eval (foo)
  ;; if FOO is: a function, call it; a form, eval it; otherwise return FOO
  (cond
   ((message-functionp foo) (funcall foo))
   ((listp foo) (eval foo))
   (t foo)))

(defun gnus-junk-check-hostname (hostname)
  "Check if HOSTNAME looks valid. Return it if valid, nil otherwise."
  (if (and hostname
	   (string-match "^[0-9a-zA-Z-]+\\(\\.[0-9a-zA-Z-]+\\)+$" hostname))
      hostname))

(defun gnus-junk-host-from-mail-address (mail-address)
  "Parse the hostname from MAIL-ADDRESS and return it."
  (if (string-match "^[^@]+@\\([^ ,>]+\\)" mail-address)
      (gnus-junk-check-hostname (match-string 1 mail-address))))

(defun gnus-junk-make-recipients-from-address (address recipients)
  "Returns a list of recipients (based on RECIPIENTS) on ADDRESS's host."
  (let ((host (gnus-junk-host-from-mail-address address)))
    (if host (mapcar (lambda (user)
		       (if (eq user t) address (concat user "@" host)))
		     recipients))))

(defun gnus-junk-make-recipients-from-received (headers recipients)
  "Returns a list of recipients on the host mentioned in the received header."
  (let (host)
    (while headers
      (if (and (eq (caar headers) 'received)
	       (string-match
		"\\bfrom\\s +\\[?\\([@.0-9a-zA-Z-]+\\)\\]?\\s *\\(([^)]+)\\)?\\b"
		(cdar headers))
	       (setq host (or (gnus-junk-check-hostname
			       (match-string 1 (cdar headers)))
			      (gnus-junk-check-hostname
			       (match-string 2 (cdar headers))))))
	  (progn
	    (setq headers nil))
	(setq headers (cdr headers))))
    (if host
	(mapcar (lambda (user)
		  (if (eq user t) (cdar headers) (concat user "@" host)))
		recipients))))

(defun gnus-junk-maybe-trim-domains (addresses)
  "Returns a trimmed-down version of the ADDRESSES list. 
See the documentation of `gnus-junk-trim-domains' for an explanation of
trimming."
  (if gnus-junk-trim-domains
      (mapc (lambda (adr)
	      (let* ((at (1+ (string-match "@" adr)))
		     (domain (concat "^" (regexp-quote (substring adr 0 at))
				     ".*\\." (regexp-quote (substring adr at))
				     "$")))
		(mapc (lambda (a)
			(if (string-match domain a)
			    (setq addresses (delete a addresses))))
		      addresses)))
	    (copy-sequence addresses)))
  addresses)

(provide 'gnus-junk)

;; Local Variables:
;; fill-column: 78
;; End:

;;; gnus-junk.el ends here
