;;; cedet-m3.el --- A CEDET mode for binding mouse-3 convenience menu.
;;
;; Copyright (C) 2010 Eric M. Ludlam
;;
;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; X-RCS: $Id: cedet-m3.el,v 1.10 2010-07-27 00:18:24 zappo Exp $
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; CEDET M3 is a generic minor mode for CEDET, the collection that
;; puts useful information into a context menu.  This context menu
;; is designed to be bound to mouse-3, and maximize a user's efficiency
;; by figuring out what the "best thing to do" might be, and collecting
;; those concepts together in the menu.

(require 'semantic)
(require 'ede)
(require 'srecode)

;;; Code:
(if (featurep 'xemacs)
    (progn
      ;; XEmacs support
      (defalias 'cedet-event-window 'event-window)
      )

  ;; Emacs
  (defun cedet-event-window (event)
    "Extract the window from EVENT."
    (car (car (cdr event))))
  )

(defcustom global-cedet-m3-minor-mode nil
  "Non-nil in buffers with Semantic Recoder macro keybindings."
  :group 'cedet-m3
  :type 'boolean
  :require 'cedet-m3
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
         (global-cedet-m3-minor-mode (if val 1 -1))))

(defvar cedet-m3-minor-mode nil
  "Non-nil in buffers with Semantic Recoder macro keybindings.")
(make-variable-buffer-local 'cedet-m3-minor-mode)

(defcustom cedet-m3-minor-mode-hook nil
  "Hook run at the end of the function `cedet-m3-minor-mode'."
  :group 'cedet-m3
  :type 'hook)

(defvar cedet-m3-prefix-key (if (featurep 'xemacs) [ button3 ] [ mouse-3 ])
  "The common prefix key in cedet-m3 minor mode.")

(defvar cedet-m3-minor-menu nil
  "Menu keymap build from `cedet-m3-menu-bar'.")

(defvar cedet-m3-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km cedet-m3-prefix-key 'cedet-m3-menu)
    ;;(define-key km "\C-x," 'cedet-m3-menu-kbd)
    km)
  "Keymap for cedet-m3 minor mode.")

(defvar cedet-m3-hack-map (make-sparse-keymap)
  "Keymap where we hide our context menu.")

;;;###autoload
(defun cedet-m3-minor-mode (&optional arg)
  "Toggle cedet-m3 minor mode, a mouse 3 context menu.
With prefix argument ARG, turn on if positive, otherwise off.  The
minor mode can be turned on only if semantic feature is available and
the current buffer was set up for parsing.  Return non-nil if the
minor mode is enabled.

\\{cedet-m3-mode-map}"
  (interactive
   (list (or current-prefix-arg
             (if cedet-m3-minor-mode 0 1))))
  ;; Flip the bits.
  (setq cedet-m3-minor-mode
        (if arg
            (>
             (prefix-numeric-value arg)
             0)
          (not cedet-m3-minor-mode)))
  ;; Run hooks if we are turning this on.
  (when cedet-m3-minor-mode
    (run-hooks 'cedet-m3-minor-mode-hook))
  cedet-m3-minor-mode)

;;;###autoload
(defun global-cedet-m3-minor-mode (&optional arg)
  "Toggle global use of cedet-m3 minor mode.
If ARG is positive, enable, if it is negative, disable.
If ARG is nil, then toggle."
  (interactive "P")
  (setq global-cedet-m3-minor-mode
        (semantic-toggle-minor-mode-globally
         'cedet-m3-minor-mode arg)))

;;; KEYBINDING COMMANDS
;;
(defun cedet-m3-menu (event)
  "Popup a menu that can help a user figure out what is under the mouse.
Argument EVENT describes the event that caused this function to be called."
  (interactive "e")
  (let* ((startwin (selected-window))
	 (win (cedet-event-window event))
	 (startloc (point-marker))
	 (winloc (window-start startwin))
	 (menuloc nil)
	 )
    (select-window win t)
    (mouse-set-point event)
    (setq menuloc (point-marker))
    (cedet-m3-create-menu)
    (sit-for 0)
    (semantic-popup-menu cedet-m3-minor-menu)
    ;; Once this is done, decide if the mouse should go back to where
    ;; it came from.
    (when (and (eq (point) menuloc)
	       (eq (window-start) winloc))
      ;; Go back
      (goto-char startloc)
      (select-window startwin))
    ))

(defun cedet-m3-menu-kbd ()
  "Popup a menu at the cursor to help a user figure out what is at that point."
  (interactive)
  (cedet-m3-create-menu)
  (sit-for 0)
  (semantic-popup-menu cedet-m3-minor-menu (senator-completion-menu-point-as-event))
  )

;;; WHATISIT?
;;
(defun cedet-m3-whatisit ()
  "Try and explain what this is.
The what is under the cursor."
  (interactive)
  (semanticdb-without-unloaded-file-searches
      (let* ((ctxt (semantic-analyze-current-context))
	     (pf (when ctxt (oref ctxt prefix)))
	     (rpf (reverse pf))
	     )
	(with-output-to-temp-buffer (help-buffer)
	  (with-current-buffer standard-output
	    (if (not ctxt)
		(progn
		  ;; @TODO - what might we say about that location?
		  (princ "You have selected some uninteresting location:\n")
		  (princ "   whitespace, punctuation, comment, or string content.")
		  )

	      ;; Found something
	      (princ "You have found ")
	      (cond

	       ;; RAW String - unknown symbol
	       ((stringp (car rpf))
		(let ((comp (save-excursion
			      (set-buffer (oref ctxt :buffer))
			      (condition-case nil
				  (semantic-analyze-possible-completions ctxt)
				(error nil)))))
		  (princ "the text ")
		  (princ (car rpf))
		  (princ "\n\n")
		  (if (not comp)
		      (princ "There are no known completions.")
		    (if (cdr comp)
			(progn
			  (princ "There are ")
			  (prin1 (length comp))
			  (princ " possible completions:\n"))
		      (princ "There is one possible completion:\n"))

		    (dolist (C comp)
		      (princ "   ")
		      (princ (semantic-format-tag-summarize C))
		      (princ "\n"))
		    )))

	       ;; A Semantic Tag
	       ((semantic-tag-p (car rpf))
		(princ "the symbol:\n  ")
		(princ (semantic-format-tag-summarize (car rpf)
						      (car (cdr rpf))
						      t))
		(princ "\n\n")

		 
		;; Filename
		(when (semantic-tag-file-name (car rpf))
		  (princ "This tag is found in:\n  ")
		  (princ (semantic-tag-file-name (car rpf)))
		  (let ((line (semantic-tag-get-attribute (car rpf) :line))
			(start (when (semantic-tag-with-position-p (car rpf))
				 (semantic-tag-start (car rpf)))))
		    (cond (line
			   (princ "\n  on Line: ")
			   (princ line))
			  (start
			   (princ "\n  at character: ")
			   (princ start))
			  ))
		  (princ "\n\n"))
	    
		;; Raw Tag Data
		(princ "The Raw Tag Data Structure is:\n\n")
		(prin1 (car rpf))
		)

	       ;; Something else?
	       (t

		(princ "absolutely nothing...")

		))))))))

;;; UTILITIES
;;
;;; Menu Item compatibility
;;
(defun cedet-m3-menu-item (itemname function &rest attributes)
  "Build an easymenu compatible menu item.
Provides a menu item compatible with Emacs or XEmacs.
XEmacs is different in that :help is removed.
The name is ITEMNAME.  It will call FUNCTION.
ATTRIBUTES are easymenu compatible attributes."
  (when (featurep 'xemacs)
    (attributes (plist-remprop attr :help)))
  (apply #'vector itemname function attributes))

(defun cedet-m3-create-menu ()
  "Create a menu custom to this location."
  (semanticdb-without-unloaded-file-searches
      (let ((baseitems (list
			(cedet-m3-menu-item
			 "What is this?"
			 'cedet-m3-whatisit
			 :active t)))
	    (context (cedet-m3-context-items))
	    (refs (cedet-m3-ref-items))
	    (recode (cedet-m3-srecode-items))
	    (project (cedet-m3-ede-items))
	    (cogre (cedet-m3-cogre-items))
	    (easy nil)
	    )
	(setq easy (cons "CEDET"
			 (append baseitems
				 context
				 refs
				 recode
				 cogre
				 project)))
    
	(easy-menu-define cedet-m3-minor-menu
	  cedet-m3-hack-map
	  "Cedet-M3 Minor Mode Menu"
	  easy)
	)))

(defun cedet-m3-context-items ()
  "Return a list of menu items if the cursor is on some useful code constrct."
  (save-excursion
    (let* ((ctxt (semantic-analyze-current-context))
	   (prefix (when ctxt (oref ctxt :prefix)))
	   (sym (car (reverse prefix)))
	   (completions (when (and ctxt (semantic-idle-summary-useful-context-p))
			  (condition-case nil
			      (semantic-analyze-possible-completions ctxt)
			    (error nil))))
	   (tag (semantic-current-tag))
	   (items nil)
	   )
      ;; If there is a context and bounds, then pulse the symbol
      (when (and ctxt (oref ctxt :bounds))
	(let ((pulse-flag nil))
	  (pulse-momentary-highlight-region (car (oref ctxt :bounds))
					    (cdr (oref ctxt :bounds))))

	;; If there are completions, then add some in.
	;; Don't use completions if there is only one, and SYM
	;; is a tag.
	(when (and completions (or (> (length completions) 1)
				   (stringp sym)))
	  (dolist (T (reverse completions))
	    (push (cedet-m3-menu-item
		   (concat "==> " (semantic-format-tag-name T))
		   `(lambda () (interactive)
		      (cedet-m3-complete-from-menu (quote ,T)))
		   :active t)
		  items)))
	;; If there were no completions, do non-completion like things
	(when (or (not completions) (= (length completions) 1))

	  ;; If this symbol is purely local, we can do a mini refactor.
	  ;; with semantic-symref-rename-local-variable
	  (when (and (semantic-tag-p sym)
		     (semantic-tag-of-class-p sym 'variable)
		     (semantic-tag-with-position-p sym)
		     ;; within this tag
		     (or (> (semantic-tag-start sym) (semantic-tag-start tag))
			 (< (semantic-tag-end sym) (semantic-tag-end tag)))
		     ;; within this buffer
		     (or (not (semantic-tag-buffer sym))
			 (eq (semantic-tag-buffer sym) (current-buffer)))
		     )
	    (push (cedet-m3-menu-item
		   (concat "Rename local variable: " (semantic-format-tag-name sym))
		   'semantic-symref-rename-local-variable
		   :active t
		   :help "Rename the local value using field edits.")
		  items))
	  
	  ;; Symref lookups
	  (let ((str (semantic-format-tag-name-from-anything sym)))
	    (push (cedet-m3-menu-item
		   (concat "Symref Lookup: " str)
		   `(lambda () (interactive)
		      (semantic-symref-symbol ,str))
		   :active t)
		  items))

	  ;; Offer the JUMP TO option iff there is a thing to jump to
	  (when (semantic-tag-with-position-p sym)
	    (let ((fn (semantic-tag-file-name sym)))
	      (push (cedet-m3-menu-item
		     (concat "Jump to symbol: " (semantic-format-tag-name sym))
		     'semantic-ia-fast-jump
		     :active t
		     :help "Jump to the current symbol.")
		    items)))

	  ))
      items)))

(defun cedet-m3-ref-items ()
  "Return a list of menu items for dealing with analyzer refs."
  (save-excursion
    (let* ((sym (semantic-current-tag))
	   (sar (if sym
		    (semantic-analyze-tag-references sym)
		  nil))
	   (target nil)
	   (items nil))
      (when sar
	(setq target
	      (if (semantic-tag-prototype-p sym)
		  (car (semantic-analyze-refs-impl sar t))
		(car (semantic-analyze-refs-proto sar t))))
	(when target
	  ;; We have something, so offer to go there.
	  (push (cedet-m3-menu-item
		 (concat "Jump to "
			 (if (semantic-tag-prototype-p target)
			     "prototype" "impl")
			 ": " (semantic-format-tag-name sym))
		 'semantic-analyze-proto-impl-toggle
		 :active t
		 :help "Jump to the current symbol.")
		items)
	  )))))


(defun cedet-m3-complete-from-menu (tag)
  "Complete the item under point with TAG."
  ;; getting the context here may seem like extra work, but buffer
  ;; caching prevents this from being a big problem. easy
  (let* ((ctxt (semantic-analyze-current-context))
	 (bounds (when ctxt (oref ctxt :bounds)))
	 )
    (when (and ctxt bounds)
      (delete-region (car bounds) (cdr bounds))
      (goto-char (car bounds))
      (insert (semantic-format-tag-name tag)))
    ))

(defun cedet-m3-srecode-items ()
  "Return a list of menu items based on SRecode features."
  (save-excursion
    (let ((sr-ctxt (srecode-calculate-context))
	  (sym (semantic-current-tag))
	  (items nil)
	  )
      (when (and sym (semantic-tag-of-class-p sym 'function))
	(push (cedet-m3-menu-item
	       (concat "Comment " (semantic-tag-name sym))
	       'srecode-document-insert-function-comment
	       :active t
	       :help "Write/replace a comment for this tag.")
	 items)
	)
      
      )))

(defun cedet-m3-ede-items ()
  "Return a list of menu items based on EDE project stats."
  ;; Only create items if EDE is active.
  (when ede-object
    (let ((objs (if (eieio-object-p ede-object)
		    (list ede-object)
		  ede-object))
	  (items nil))
      ;; Do this for every target.
      (dolist (OBJ objs)
	;; If the active item is a PROJECT, provide a project level compile.
	(if (ede-project-child-p OBJ)
	    (setq items
		  (cons (cedet-m3-menu-item
			 (concat "Compile Project: (" (ede-name OBJ) ")")
			 'ede-compile-project
			 :active t
			 :help "Compile the current project with EDE.")
			items))
	  ;; Else, we can compile a target.
	  (setq items
		(cons (cedet-m3-menu-item
		       (concat "Compile Target: (" (ede-name OBJ) ")")
		       `(lambda () (interactive) (project-compile-target ,OBJ))
		       :active t
		       :help "Compile the current target with EDE.")
		      items))))
      items)))

(defun cedet-m3-cogre-items ()
  "Return a list of menu items based on COGRE features."
  (when (locate-library "cogre")
    (let* ((ctxt (semantic-analyze-current-context))
	   (pt (if ctxt (reverse (oref ctxt :prefixtypes)) nil))
	   (items nil)
	   )
      (dolist (DT pt)
	(when (and (semantic-tag-p DT)
		   (semantic-tag-of-class-p DT 'type)
		   (semantic-tag-of-type-p DT "class"))
	  (push
	   (cedet-m3-menu-item
	    (concat "Browse UML: (" (semantic-format-tag-name DT) ")")
	    `(lambda ()
	       (interactive)
	       (cogre-uml-quick-class (quote , DT)))
	    :active t
	    :help "Browse a UML diagram with COGRE.")
	   items)
	  ))
      items)))

;; Use the semantic minor mode magic stuff.
(semantic-add-minor-mode 'cedet-m3-minor-mode
			 "" 
			 cedet-m3-mode-map)

(provide 'cedet-m3)

;;; cedet-m3.el ends here
