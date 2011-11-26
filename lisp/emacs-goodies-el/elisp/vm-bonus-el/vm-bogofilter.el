;;; vm-bogofilter.el version 1.1.4
;;
;; An interface between the VM mail reader and the bogofilter spam filter.
;;
;; Copyright (C) 2003-2006 by Bjorn Knutsson
;;
;; Home page: http://www.cis.upenn.edu/~bjornk/
;;
;; Bjorn Knutsson, CIS, 3330 Walnut Street, Philadelphia, PA 19104-6389, USA
;;
;;
;; Based on vm-spamassassin.el v1.1, Copyright (C) 2002 by Markus Mohnen
;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;;; Version history:
;; v 1.1.4: Change in the way bogofilter is called
;;        * No longer uses formail to process mails
;;        * Slightly improved error handling
;; v 1.1.3: Minor edits
;;        * Documentation updates
;;        * Error checking for bogofilter calls.
;;        * vm-bogofilter-delete-spam variable.
;;          Set to cause spam to be automatically deleted.
;;        * vm-bogofilter-setup function.
;;          Automatically called on loading, but can be called again
;;          to re-initialize the vm-bogofilter setup
;; v 1.1.2: Borg assimilation version (12-Sep-2003)
;;        * Great minds think alike. Olivier Cappe independently 
;;          created his own version of vm-bogofilter.el based on
;;          vm-spamassassin.el with the same basic functions.
;;          He submitted a patch to my version to harmonize them.
;;        * Added comment about vm-delete-after-archiving, as suggested
;;          by Olivier.
;; v 1.1.1: minor edits
;;        * Chris McMahan submitted a patch that disables running
;;          bogfilter on incoming mail. While at first potentially
;;          confusing, this means that you can run bogofilter via
;;          e.g. procmail filters, and then use vm-bogofilter.el to
;;          (re-)educate bogofilter about false positives/negatives.
;;        * Documentation of a folder problem added
;; v 1.1: functional update
;;        * Changed re-training functions to also re-tag the the message
;;          in the VM folder, thus making the tag on the message in VM
;;          be consistent with bogofilter's opinion about the message.
;;          Notice!! If you use the tag in the message, you should be
;;          aware that a message re-classified as spam may still not
;;          be tagged as spam by bogofilter, and vice versa, if the
;;          bogofilter database contains too many counter-examples.
;;          The old re-training functions are still present, if you
;;          prefer not to muck around with your inbox. They've been
;;          renamed vm-bogofilter-is-spam-old/vm-bogofilter-is-clean-old
;;          and works as before.
;; v 1.0.1: update
;;        * Very minor edits of texts, no functional changes.
;; v 1.0: initial release
;;        * First release, based on Markus Mohnen's vm-spamassassin
;;
;;
;; To use this program, you need reasonably recent versions of VM from
;; http://www.wonderworks.com/vm) and bogofilter from
;; http://sourceforge.net/projects/bogofilter/
;;
;; This version of the interface has been developed for, and tested
;; with, VM version 7.17 and later, and bogofilter version 0.17.4 and
;; later. Some features used /require/ bogofilter version 0.15.0 and
;; later but no testing of versions earlier than 0.17.4 has been done.
;; It has been tested with bogofilter versions up to 0.93.2
;;
;; (Former RMAIL-users should read the BUGS-note about the BABYL-format)
;;
;;; Installation:
;;
;;  Put this file on your Emacs-Lisp load path and add following into your
;;  ~/.vm startup file
;;
;;      (require 'vm-bogofilter)
;;
;;
;;; Usage:
;;
;; Whenever you get new mail bogofilter will be invoked on them. Mail
;; detected as spam will be tagged by bogofilter, and you can use
;; existing mechanisms to dispose of them.
;;
;; For example, if you append this line to your .vm (or modify your
;; existing auto-folder-alist), you could then have messages tagged as
;; spam automatically saved in a separate 'spam' folder:
;;
;; (setq vm-auto-folder-alist '(("^X-Bogosity: " ("Yes," . "spam"))))
;;
;; If you want your auto-folder to be used every time you've received
;; new mail, just add the following to your .vm:
;;
;; (add-hook 'vm-arrived-messages-hook 'vm-auto-archive-messages)
;;
;; You can also set (setq 'vm-delete-after-archiving t) to make VM
;; automatically delete archived spams from the main folder.
;;
;;
;; If a message is tagged as spam incorrectly, you can re-train
;; bogofilter by calling the function vm-bogofilter-is-clean on that
;; message. Similarly, calling vm-bogofilter-is-spam will re-train
;; bogofilter to recognize a clean-marked message as spam.
;;
;; These functions can be bound to keys in your .vm, for example:
;;
;; (define-key vm-mode-map "K" 'vm-bogofilter-is-spam)
;; (define-key vm-mode-map "C" 'vm-bogofilter-is-clean)
;;
;; would define K (shift-k) as the key to declare the current message
;; as spam, while C (shift-c) as the key to declare the current
;; message as clean.
;;
;; Re-training with the old functions (still available) would not
;; re-tag messages, while the new ones will. Re-training may or may
;; not change the spam-status of a message. Because of the way
;; bogofilter works, even a message explicitly declared as spam may
;; not be tagged as spam if there are enough similar non-spam
;; messages. Remember, bogofilter is not trained to recognize
;; individual messages, but rather patterns. You may have to train
;; bogofilter on a number of spam messages before it recognizes any of
;; them as spam. See the documentation for bogofilter. Notice also
;; that even if the tag changes, this will not undo actions previously
;; taken based on the tag, e.g. moving spam to a spamfolder with
;; auto-folders.
;;
;; If you have a small database, running bogofilter without '-u' may
;; be better in the beginning. If you want to run without '-u', it
;; can easily be accomplished. Just:
;; 
;; M-x customize<return> vm-bogofilter<return>
;;
;; Then change the Program Options to just '-p -e' and the Unspam to
;; '-n' and Spam to '-s'.
;;
;; Now, bogofilter will not auto-train, and you must instead use the
;; vm-bogofilter-is-spam and vm-bogofilter-is-clean to manually tag
;; messages. (If you've bound them to keys, it will be quite simple.)
;;
;;; BUGS:
;;
;; One know bug is that formail will not like it if the input is not
;; in the format it expects and knows. Even though it's supposed to
;; know BABYL, this does not work.
;;
;; A related problem is that if you have the wrong folder type
;; selected, then sometimes, VM will merge messages. You can check the
;; raw folder to see if you have a blank line before the "From "-line
;; separating messages. See the documentation for vm-default-folder-type
;;
;; vm-bogofilter is not very smart about errors. If an error occurs
;; during any operation that tags or re-tags messages, the message(s)
;; being processed will be *lost*. If errors occur during initial
;; processing, the lost mails can sometimes be recovered since VM will
;; save the folder *after* receiving new mails, but *before*
;; processing hooks, e.g. vm-bogofilter. If you notice the errors
;; before saving the folder, you can copy the old file, close VM,
;; rename your copy to the original folder name and then start VM
;; again. Naturally, anything that happened to the folder after
;; fetching new mail will be lost, e.g. bogofilter tagging etc.
;;
;;; Customization:
;;
;;  M-x customize RET vm-bogofilter

;;; Code:

(eval-when-compile (require 'vm))

;;; Customisation:

(defgroup vm-bogofilter nil
  "VM Spam Filter Options"
  :group 'vm)

(defcustom vm-bogofilter-program "bogofilter"
  "*Name of the bogofilter program."
  :group 'vm-bogofilter
  :type 'string)

(defcustom vm-bogofilter-program-options "-u -p -e"
  "*Options for the bogofilter program. Since we use bogofilter as a
filter, '-p' must be one of the options, while '-e' tells bogofilter
that it is embedded, and thus should not signal spam/ham with return
values.
* The flag '-u' controls if bogofilter automatically learns from its own
classification. You may not want to use this flag if bogofilter still is
learning to classify, or if you do not have the discipline to correct every
mis-classification."
  :group 'vm-bogofilter
  :type 'string)

(defcustom vm-bogofilter-program-mbox "-M"
  "*Options for the bogofilter program. This flags tells bogofilter
how to process mailboxes, i.e., multiple messages."
  :group 'vm-bogofilter
  :type 'string)

(defcustom vm-bogofilter-program-options-unspam "-Sn"
  "*Options for the bogofilter program when declaring a spam-marked
message as clean. The default, '-Sn', assumes that bogofilter already
has trained itself on the message, e.g. by running it with '-u' during
classification. If this is the initial training, use '-n' instead."
  :group 'vm-bogofilter
  :type 'string)

(defcustom vm-bogofilter-program-options-spam "-Ns"
  "*Options for the bogofilter program when declaring a clean-marked
message as spam. The default, '-Ns', assumes that bogofilter already
has trained itself on the message, e.g. by running it with '-u' during
classification. If this is the initial training, use '-s' instead."
  :group 'vm-bogofilter
  :type 'string)

(defcustom vm-bogofilter-program-options-reclassify "-p -e"
  "*Options for the bogofilter program when declaring a clean-marked
message as spam.
*See vm-bogofilter-program-options for a discussion of the options."
  :group 'vm-bogofilter
  :type 'string)

(defcustom vm-bogofilter-formail-program "formail"
  "*Name of the program used to split a sequence of mails."
  :group 'vm-bogofilter
  :type 'string)

(defcustom vm-bogofilter-formail-program-options "-s"
  "*Options for the 'vm-bogofilter-formail-program'. After this
arguments, the name of the bogofilter program will be passed."
  :group 'vm-bogofilter
  :type 'string)

(defcustom vm-bogofilter-invoke-through-vm t
  "*When true, bogofilter will be invoked through the
vm-retrieved-spooled-mail-hook. If you have procmail or some other
MTA configured to filter through bogofilter already, then set this to
nil to speed vm-startup.
*NOTE: This variable is only consulted on startup, so if you change
it, it will take effect the next time vm-bogofilter is loaded, or
vm-bogofilter-setup is called."
  :group 'vm-bogofilter
  :type 'boolean)

(defcustom vm-bogofilter-delete-spam nil
  "*When true, mark messages for deletion when reclassifying as spam.
*NOTE: This does not affect the initial classification, only when messages
are explicitly marked as spams by the vm-bogofilter-is-spam function."
  :group 'vm-bogofilter
  :type 'boolean)

(defun vm-bogofilter-arrived-message ()
  "The function used to do the actual filtering. It is used as a value for
vm-retrieved-spooled-mail-hook."
  (save-excursion
    (vm-save-restriction
     (let ((tail-cons (vm-last vm-message-list))
	   (buffer-read-only nil))
       (widen)
       (if (null tail-cons)
	   (goto-char (point-min))
	 (goto-char (vm-text-end-of (car tail-cons)))
	 (beginning-of-line)
	 (forward-line)
	 )
       (message "Filtering new messages... ")
       (let ((res (call-process-region (point) (point-max)
				       (or shell-file-name "sh")
				       t t nil shell-command-switch
				       (concat vm-bogofilter-program " "
					       vm-bogofilter-program-options " "
					       vm-bogofilter-program-mbox))))

	 (if (and res (not (and (integerp res) (zerop res))))
	     (error "Something went wrong filtering new messages (exit %s)"
		    res)
	   (delete-region (point) (point-max))))
       (message "Filtering new messages... done.")
       )
     )
    )
  )

(defun vm-bogofilter-is-spam-old ()
  "Declare that a clean-marked message is spam"
  (interactive)
  (vm-follow-summary-cursor)
  (vm-pipe-message-to-command
   (concat vm-bogofilter-program " " vm-bogofilter-program-options-spam) nil)
  )

(defun vm-bogofilter-is-clean-old ()
  "Declare that a spam-marked message is clean"
  (interactive)
  (vm-follow-summary-cursor)
  (vm-pipe-message-to-command
   (concat vm-bogofilter-program " " vm-bogofilter-program-options-unspam) nil)
  )

(defun vm-bogofilter-is-spam ()
  "Declare that a clean-marked message is spam, and re-tag message"
  (interactive)
  (vm-bogofilter-retag "spam" vm-bogofilter-program-options-reclassify vm-bogofilter-program-options-spam)
  (if vm-bogofilter-delete-spam
      (vm-delete-message 1)) 
  )

(defun vm-bogofilter-is-clean ()
  "Declare that a spam-marked message is clean, and re-tag message"
  (interactive)
  (vm-bogofilter-retag "clean" vm-bogofilter-program-options-reclassify vm-bogofilter-program-options-unspam)
  )

;; Based on vm-pipe-message-to-command
(defun vm-bogofilter-retag (text switch &optional switch2)
  "Workhorse function for re-tagging of messages."
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-read-only)
  (vm-error-if-folder-empty)
  (save-excursion
    (let ((message (vm-real-message-of (car vm-message-pointer)))
	  (buffer (get-buffer-create "*Shell Command Output*"))
	  )
          
      (save-excursion
	(set-buffer buffer)
	(erase-buffer))
      (set-buffer (vm-buffer-of message))
      (vm-save-restriction
       (vm-save-buffer-excursion
	(widen)
	(goto-char (vm-headers-of message))
	(narrow-to-region (point) (vm-text-end-of message))
	(message "Re-classifying message as %s." text)
	(if (not (eq switch2 nil))
	    (progn
	      (call-process-region (point-min) (point-max)
				   (or shell-file-name "sh")
				   nil buffer nil shell-command-switch
				   (concat vm-bogofilter-program " "
					   switch2)
				   )
	      (message "Message re-classified as %s, updating tag."
		       text)
	      ))
	(let ((buffer-read-only nil)
	      (buffer (get-buffer-create "*Shell Command Output*")))
	  (call-process-region (point-min) (point-max)
			       (or shell-file-name "sh")
			       nil t nil shell-command-switch
			       (concat vm-bogofilter-program " "
				       switch)
			       )
	  (delete-region (point) (vm-text-end-of message)))
	(vm-discard-cached-data)
	(message "Message re-classified and tagged as %s." text)
	(vm-preview-current-message)
	(vm-update-summary-and-mode-line)
	)))))

;;; Hooking into VM

(defun vm-bogofilter-setup ()
  "Initialize vm-bogofilter."
  (interactive)
  (if vm-bogofilter-invoke-through-vm
      (add-hook 'vm-retrieved-spooled-mail-hook 'vm-bogofilter-arrived-message)
      (remove-hook 'vm-retrieved-spooled-mail-hook 'vm-bogofilter-arrived-message)))

(vm-bogofilter-setup)
  
(provide 'vm-bogofilter)

;;; vm-bogofilter.el ends here
