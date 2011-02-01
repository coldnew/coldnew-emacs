;;; vim-commands.el - Implementation of VIM commands.

;; Copyright (C) 2009, 2010 Frank Fischer

;; Author: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;;
;; This file is not part of GNU Emacs.

;;; Commentary:

;; In general there are two types of commands: those operating on a
;; motion and those not taking a motion. Examples of the first one are
;; the vim-commands c, d, y, =, examples of the second one are dd, D,
;; p, x.
;;
;; Commands are defined using the `vim:defcmd' macro and have the
;; following form:
;;
;;   (vim:defcmd name (count 
;;                     motion[:optional]
;;                     argument[:{char,file,buffer}] 
;;                     [nonrepeatable]
;;                     [keep-visual])
;;      body ...)
;;
;; Each of the arguments is optional. The names of the arguments must
;; be exactly as in the definition above (but see 'Argument-renaming'
;; below).
;;
;; The COUNT argument (if given) takes the count of the command which
;; is usually the number how often the command should be repeated.
;; This argument may be nil if no count is given. If the command takes
;; a MOTION argument, no COUNT argument is allowed (will always be
;; nil).
;;
;; The MOTION argument defines the range where the command should work
;; on. It's always of type `vim:motion'. Usually, a command should
;; respect the of the motion, i.e. charwise, linewise or block, but
;; there are commands that behave indepently of the motion type (e.g.
;; `vim:cmd-shift-left' always works linewise). If the MOTION
;; parameter has the form motion:optional, the MOTION parameter may be
;; nil, which can only happen if the command is bound in ex-mode (e.g.
;; the command `vim:cmd-substitute' is bound to :s may be called
;; without a motion, in which case it works only on the current line).
;; If the command is bound in normal-mode, the MOTION argument will
;; usually be created by some motion-command bound in
;; operator-pending-mode.
;;
;; The ARGUMENT argument is an aditional text-argument to be given and
;; may be nil, too. If it is specified as ARGUMENT:CHAR, the argument
;; is a one-character argument (see `vim:cmd-replace-char' usually
;; bound to 'r' for an example). If it specified as ARGUMENT:FILE it
;; takes a file-name as argument, ARGUMENT:BUFFER takes a buffer-name
;; as argument and a single ARGUMENT takes a string as argument. Only
;; the type ARGUMENT:CHAR has an effect in normal-mode, the others are
;; only important if bound in ex-mode. In this case the type of the
;; argument determines how minibuffer-completion is done. The argument
;; may be nil in which case the command should have a default
;; behaviour (e.g. the command `vim:cmd-write' bound to :write takes
;; an ARGUMENT:FILE argument and saves the current buffer to the given
;; file or to the buffer's own file if ARGUMENT is nil).
;;
;; The pseudo-argument NONREPEATABLE means, the command will not be
;; recorded to the repeat command (usually bound to '.'). This is
;; useful for non-editing commands, e.g. all window and scrolling
;; commands have this behaviour.
;;
;; The pseudo-argument KEEP-VISUAL means the command should not exit
;; visual-mode and go back to normal-mode when called in visual-mode.
;; This is useful for scrolling-commands which stay in visual-mode but
;; are no regular motions (scrolling commands move the (point) but are
;; no real motions since they can't in operating-pending mode), or
;; some visual-mode specific command like
;; `vim:visual-exchange-point-and-mark', usually bound to 'o').
;; 
;; If you do not like the default argument names, they may be renamed
;; by using (ARG NEWNAME) instead of ARG, e.g.
;;
;;   (vim:defcmd vim:cmd-replace-char (count (argument:char arg))
;;
;; defines a simple command with a COUNT argument but renames the
;; character-argument to ARG.
;;
;; Each command should place (point) at the correct position after the
;; operation.
;;

;;; Code:

(defcustom vim:shift-width 8
  "The number of columns for shifting commands like < or >."
  :type 'integer
  :group 'vim-mode)

(vim:defcmd vim:cmd-insert (count)
  "Switches to insert-mode before point."
  (vim:activate-insert-mode))

(vim:defcmd vim:cmd-append (count)
  "Switches to insert-mode after point."
  (unless (eolp) (forward-char))
  (vim:activate-insert-mode))

(vim:defcmd vim:cmd-Insert (count)
  "Moves the cursor to the beginning of the current line
and switches to insert-mode."
  (vim:motion-first-non-blank)
  (vim:cmd-insert :count count))

(vim:defcmd vim:cmd-Append (count)
  "Moves the cursor to the end of the current line
and switches to insert-mode."
  (end-of-line)
  (vim:cmd-append :count count))

(vim:defcmd vim:cmd-insert-line-above (count)
  "Inserts a new line above the current one and goes to insert mode."
  (vim:motion-beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-according-to-mode)
  (vim:cmd-Insert))

(vim:defcmd vim:cmd-insert-line-below (count)
  "Inserts a new line below the current one and goes to insert mode."
  (vim:motion-end-of-line)
  (newline)
  (indent-according-to-mode)
  (vim:cmd-insert))

(vim:defcmd vim:cmd-replace (count)
  "Goes to replace-mode."
  (vim:activate-insert-mode)
  (vim:insert-mode-toggle-replace))

(vim:defcmd vim:insert-mode-exit (nonrepeatable)
  "Deactivates insert-mode, returning to normal-mode."
  (vim:activate-normal-mode)
  (goto-char (max (line-beginning-position) (1- (point)))))


(vim:defcmd vim:cmd-delete-line (count register)
  "Deletes the next count lines."
  (vim:cmd-yank-line :count count :register register)
  (let ((beg (line-beginning-position))
        (end (save-excursion
               (forward-line (1- (or count 1)))
               (line-end-position))))
    (if (= beg (point-min))
        (if (= end (point-max))
            (erase-buffer)
          (delete-region beg (save-excursion
                               (goto-char end)
                               (forward-line)
                               (line-beginning-position))))
      (delete-region (save-excursion
                       (goto-char beg)
                       (forward-line -1)
                       (line-end-position))
                     end))
    (goto-char beg)
    (vim:motion-first-non-blank)))


(vim:defcmd vim:cmd-delete (motion register)
  "Deletes the characters defined by motion."
  (case (vim:motion-type motion)
    ('linewise
     (goto-line (vim:motion-first-line motion))
     (vim:cmd-delete-line :count (vim:motion-line-count motion)
                          :register register))

    ('block
     (vim:cmd-yank :motion motion :register register)
     (delete-rectangle (vim:motion-begin-pos motion)
		       (vim:motion-end-pos motion)))

    (t
     (vim:cmd-yank :motion motion :register register)
     (delete-region (vim:motion-begin-pos motion) (vim:motion-end-pos motion))
     (goto-char (vim:motion-begin-pos motion)))))


(vim:defcmd vim:cmd-delete-char (count register)
  "Deletes the next count characters."
  (vim:cmd-delete :motion (vim:motion-right :count (or count 1))
                  :register register))


(vim:defcmd vim:cmd-change (motion register)
  "Deletes the characters defined by motion and goes to insert mode."
  (case (vim:motion-type motion)
    ('linewise
     (goto-line (vim:motion-first-line motion))
     (vim:cmd-change-line :count (vim:motion-line-count motion)
                          :register register))

    ('block
        (let ((insert-info (vim:make-visual-insert-info :first-line (vim:motion-first-line motion)
                                                        :last-line (vim:motion-last-line motion)
                                                        :column (vim:motion-first-col motion))))
          (vim:cmd-delete :motion motion :register register)
          (vim:visual-start-insert insert-info)))

    (t
     ;; deal with cw and cW
     (when (and vim:current-motion
                (not (member (char-after) '(?  ?\r ?\n ?\t))))
       (cond
        ((eq vim:current-motion 'vim:motion-fwd-word)
         (let* ((cnt (* (or vim:current-cmd-count 1)
                        (or vim:current-motion-count 1)))
                (pos
                (save-excursion
                  (dotimes (i cnt)
                    (while
                        (not
                         (or (and (looking-at (concat "[^ \t\r\n]"
                                                      "[ \t\r\n]")))
                             (and (looking-at (concat "[" vim:word "]"
                                                      "[^ \t\r\n" vim:word "]")))
                             (and (looking-at (concat "[^ \t\r\n" vim:word "]"
                                                      "[" vim:word "]")))))
                      (forward-char))
                    (when (< i (1- cnt))
                      (forward-char)))
                  (point))))
           (setq motion (vim:make-motion :begin (point) :end pos :type 'inclusive))))
        
        ((eq vim:current-motion 'vim:motion-fwd-WORD)
         (let* ((cnt (* (or vim:current-cmd-count 1)
                        (or vim:current-motion-count 1)))
                (pos
                 (save-excursion
                   (dotimes (i cnt)
                     (while
                         (not (looking-at (concat "[^ \t\r\n]"
                                                  "[ \t\r\n]")))
                       (forward-char))
                     (when (< i (1- cnt))
                       (forward-char)))
                   (point))))
           (setq motion (vim:make-motion :begin (point) :end pos :type 'inclusive))))))
        
     (vim:cmd-delete :motion motion :register register)
     (if (eolp)
         (vim:cmd-append :count 1)
       (vim:cmd-insert :count 1)))))


(vim:defcmd vim:cmd-change-line (count register)
  "Deletes count lines and goes to insert mode."
  (let ((pos (line-beginning-position)))
    (vim:cmd-delete-line :count count :register register)
    (if (< (point) pos)
        (progn
          (end-of-line)
          (newline))
      (progn
        (beginning-of-line)
        (newline)
        (forward-line -1)))
    (indent-according-to-mode)
    (if (eolp)
        (vim:cmd-append :count 1)
      (vim:cmd-insert :count 1))))


(vim:defcmd vim:cmd-change-rest-of-line (register)
  "Deletes the rest of the current line."
  (vim:cmd-delete :motion (vim:make-motion :begin (point)
                                           :end (1- (line-end-position))
                                           :type 'inclusive)
                  :register register)
  (vim:cmd-append :count 1))
                                


(vim:defcmd vim:cmd-change-char (count register)
  "Deletes the next count characters and goes to insert mode."
  (let ((pos (point)))
    (vim:cmd-delete-char :count count :register register)
    (if (< (point) pos)
        (vim:cmd-append)
      (vim:cmd-insert))))


(vim:defcmd vim:cmd-replace-char (count (argument:char arg))
  "Replaces the next count characters with arg."
  (unless (vim:char-p arg)
    (error "Expected a character."))
  (when (< (- (line-end-position) (point))
           (or count 1))
    (error "Too few characters to end of line."))
  (delete-region (point) (+ (point) (or count 1)))
  (insert-char arg (or count 1))
  (backward-char))


(vim:defcmd vim:cmd-replace-region (motion (argument:char arg))
   "Replace the complete region with `arg'"
   (case (vim:motion-type motion)
     ('block
      ;; replace in block
      (let ((begrow (vim:motion-first-line motion))
            (begcol (vim:motion-first-col motion))
            (endrow (vim:motion-last-line motion))
            (endcol (1+ (vim:motion-last-col motion))))
        (goto-line begrow)
        (dotimes (i (1+ (- endrow begrow)))
          ;; TODO does it work with \r\n at the end?
          (let ((maxcol (save-excursion
                          (end-of-line)
                          (current-column))))
            (when (> maxcol begcol)
              (delete-region (save-excursion
                               (move-to-column begcol t)
                               (point))
                             (save-excursion
                               (move-to-column (min endcol maxcol) t)
                               (point)))
              (move-to-column begcol t)
              (insert-char arg (- (min endcol maxcol) begcol))))
          (forward-line 1))
        (goto-line begrow)
        (move-to-column begcol)))
       
     (t ;; replace in linewise and normal
      (let ((begrow (vim:motion-first-line motion))
            (endrow (vim:motion-last-line motion)))
        (goto-line begrow)
        (do ((r begrow (1+ r)))
            ((> r endrow))
          (goto-line r)
          (let ((begcol
                 (if (and (= r begrow)
                          (not (eq (vim:motion-type motion) 'linewise)))
                     (save-excursion
                       (goto-char (vim:motion-begin-pos motion))
                       (current-column))
                   0))
                (endcol
                 (if (and (= r endrow)
                          (not (eq (vim:motion-type motion) 'linewise)))
                     (save-excursion
                       (goto-char (vim:motion-end-pos motion))
                       (current-column))
                   ;; TODO does it work with \r\n at the end?
                   (save-excursion
                     (end-of-line)
                     (current-column)))))

	    (delete-region (save-excursion
			     (move-to-column begcol t)
			     (point))
			   (save-excursion
			     (move-to-column endcol t)
			     (point)))
	    (move-to-column begcol t)
	    (insert-char arg (- endcol begcol)))))
      
      (goto-char (vim:motion-begin-pos motion)))))


(vim:defcmd vim:cmd-yank (motion register nonrepeatable)
  "Saves the characters in motion into the kill-ring."
  (case (vim:motion-type motion)
    ('block (vim:cmd-yank-rectangle :motion motion :register register))
    ('linewise (goto-line (vim:motion-first-line motion))
	       (vim:cmd-yank-line :count (vim:motion-line-count motion)
                                  :register register))
    (t
     (let ((text (buffer-substring
                  (vim:motion-begin-pos motion)
                  (vim:motion-end-pos motion))))
       (if register
           (set-register register text)
         (kill-new text))))))
  

(vim:defcmd vim:cmd-yank-line (count register nonrepeatable)
  "Saves the next count lines into the kill-ring."
  (let ((beg (line-beginning-position))
	end
        (linenr (line-number-at-pos (point))))
    (setq count (or count 1))
    (save-excursion
      (forward-line (or count 1))
      (setq end (point)))
    (if register
	(let ((txt (buffer-substring beg end)))
	  (put-text-property 0 (- end beg)
			     'yank-handler
			     (list #'vim:yank-line-handler txt)
			     txt)
	  (set-register register txt))
      (kill-new (buffer-substring-no-properties beg end)
		nil
		(list #'vim:yank-line-handler
		      (buffer-substring beg end))))))


(vim:defcmd vim:cmd-yank-rectangle (motion register nonrepeatable)
  "Stores the rectangle defined by motion into the kill-ring."
  (unless (eq (vim:motion-type motion) 'block)
    (error "Motion must be of type block"))
  ;; TODO: yanking should not insert spaces or expand tabs.
  (let ((begrow (vim:motion-first-line motion))
	(begcol (vim:motion-first-col motion))
	(endrow (vim:motion-last-line motion))
	(endcol (vim:motion-last-col motion))
	(parts nil))
    (goto-line endrow)
    (dotimes (i (1+ (- endrow begrow)))
      (let ((beg (save-excursion (move-to-column begcol) (point)))
            (end (save-excursion (move-to-column (1+ endcol)) (point))))
        (push (cons (save-excursion (goto-char beg)
                                    (- (current-column) begcol))
                    (buffer-substring beg end))
              parts)
        (forward-line -1)))
    (let ((txt (apply #'concat (cdr (mapcan
				     #'(lambda (part) (list "\n" (cdr part)))
				     parts)))))
      ;; `txt' contains the block as single lines
      (if register
          (progn
            (put-text-property 0 (length txt)
                               'yank-handler
                               (list #'vim:yank-block-handler
                                     (cons (- endcol begcol -1) parts)
                                     nil
                                     #'delete-rectangle) 
                               txt)
            (set-register register txt))
        (kill-new txt nil (list #'vim:yank-block-handler
                                (cons (- endcol begcol -1) parts)
                                nil
                                #'delete-rectangle))))
    (goto-line begrow)
    (move-to-column begcol)))

(defun vim:yank-line-handler (text)
  "Inserts the current text linewise."
  (beginning-of-line)
  (set-mark (point))
  (insert text))



(defun vim:yank-block-handler (text)
  "Inserts the current text as block."
  ;; TODO: yank-pop with count will not work for blocks, because
  ;; it's difficult to place (point) (or (mark)) at the correct
  ;; position since they may no exist.
  (let ((ncols (car text))
        (parts (cdr text))
        (col (current-column))
	(current-line (line-number-at-pos (point)))
        (last-pos (point)))
    
    (set-mark (point))
    (dolist (part parts)
      
      (let* ((offset (car part))
             (txt (cdr part))
             (len (length txt)))
	
	;; maybe we have to insert a new line at eob
	(when (< (line-number-at-pos (point))
		 current-line)
          (goto-char (point-max))
          (newline))
	(incf current-line)
        
        (unless (and (< (current-column) col)   ; nothing in this line
                     (<= offset 0) (zerop len)) ; and nothing to insert
          (move-to-column (+ col (max 0 offset)) t)
          (insert txt)
          (unless (eolp)
            ;; text follows, so we have to insert spaces
            (insert (make-string (- ncols len) ? ))))
        (setq last-pos (point))
	(forward-line 1)))
    (goto-char last-pos)
    (exchange-point-and-mark)))


(defvar vim:last-paste nil
  "Information (beg end count) of the latest paste.")


(vim:defcmd vim:cmd-paste-pop (count)
  "Cycles through the kill-ring like yank-pop."
  (unless (and (member last-command '(vim:cmd-paste-pop
                                      vim:cmd-paste-pop-next
                                      vim:cmd-paste-before
                                      vim:cmd-paste-behind))
               vim:last-paste)
    (error "Previous command was not a vim-mode paste"))
  (when vim:last-paste
    (save-excursion
      (funcall (or yank-undo-function #'delete-region)
               (car vim:last-paste)
               (cadr vim:last-paste))
      (goto-char (car vim:last-paste))
      (current-kill (or count 1))
      (vim:cmd-paste-before :count (caddr vim:last-paste)))))


(vim:defcmd vim:cmd-paste-pop-next (count)
  "Cycles through the kill-ring like yank-pop."
  (vim:cmd-paste-pop :count (- (or count 1))))
  
(vim:defcmd vim:cmd-paste-before (count register)
  "Pastes the latest yanked text before the cursor position."
  (let (beg end)
    (save-excursion
      (dotimes (i (or count 1))
        (if register
            (insert-for-yank (vim:get-register register))
          (set-mark (point))
	  (yank)
          (setq beg (min (point) (mark t) (or beg (point)))
                end (max (point) (mark t) (or end (point)))))))
    (setq vim:last-paste (list beg end (or count 1)))))


(vim:defcmd vim:cmd-paste-behind (count register)
  "Pastes the latest yanked text behind point."
  (let ((txt (if register (vim:get-register register) (current-kill 0))))
    (unless txt
      (error "Kill-ring empty"))
    (let ((yhandler (get-text-property 0 'yank-handler txt)))
      (case (car-safe yhandler)
	(vim:yank-line-handler
	 (end-of-line)
	 (if (eobp)
	     (progn
	       (newline)
	       (save-excursion
		 (vim:cmd-paste-before :count count :register register)
		 (goto-char (point-max))
		 (delete-backward-char 1)))
	   (forward-line)
	   (vim:cmd-paste-before :count count :register register))
	 (vim:motion-first-non-blank))
	
	(vim:yank-block-handler
	 (forward-char)
	 (vim:cmd-paste-before :count count :register register))
	
	(t
	 (unless (and (bolp) (eolp)) (forward-char))
	 (set-mark (point))
	 (dotimes (i (or count 1))
	   (if register
	       (insert-for-yank (vim:get-register register))
	     (insert-for-yank (car kill-ring-yank-pointer))))
	 (setq vim:last-paste (list (mark t) (point) (or count 1)))
	 (backward-char))))))


(vim:defcmd vim:cmd-join-lines (count)
  "Join `count' lines with a minimum of two lines."
  (dotimes (i (max 1 (1- (or count 1))))
    (when (re-search-forward "\\(\\s-*\\)\\(\n\\s-*\\)\\()?\\)")
      (delete-region (match-beginning 2)
                     (match-end 2))
      (when (and (= (match-beginning 1) (match-end 1))
                 (= (match-beginning 3) (match-end 3)))
        (insert-char ?  1))
      (backward-char))))


(vim:defcmd vim:cmd-join (motion)
  "Join the lines covered by `motion'."
  (goto-line (vim:motion-first-line motion))
  (vim:cmd-join-lines :count (vim:motion-line-count motion)))


(vim:defcmd vim:cmd-indent (motion)
  "Reindent the lines covered by `motion'."
  (goto-line (vim:motion-first-line motion))
  (indent-region (line-beginning-position)
                 (line-end-position (vim:motion-line-count motion))))
  

(vim:defcmd vim:cmd-shift-left (motion)
  "Shift the lines covered by `motion' leftwards."
  (goto-line (vim:motion-first-line motion))
  (indent-rigidly (line-beginning-position)
                  (line-end-position (vim:motion-line-count motion))
                  (- vim:shift-width)))


(vim:defcmd vim:cmd-shift-right (motion)
  "Shift the lines covered by `motion' rightwards."
  (goto-line (vim:motion-first-line motion))
  (indent-rigidly (line-beginning-position)
                  (line-end-position (vim:motion-line-count motion))
                  vim:shift-width))
  

(vim:defcmd vim:cmd-toggle-case (motion)
  "Toggles the case of all characters defined by `motion'."
  (vim:change-case motion
                   #'(lambda (beg end)
                       (save-excursion
                         (goto-char beg)
                         (while (< beg end)
                           (let ((c (following-char)))
                             (delete-char 1 nil)
                             (insert-char (if (eq c (upcase c)) (downcase c) (upcase c)) 1)
                             (setq beg (1+ beg))))))))


(vim:defcmd vim:cmd-make-upcase (motion)
  "Upcases all characters defined by `motion'."
  (vim:change-case motion #'upcase-region))


(vim:defcmd vim:cmd-make-downcase (motion)
  "Downcases all characters defined by `motion'."
  (vim:change-case motion #'downcase-region))


(defun vim:change-case (motion case-func)
  (case (vim:motion-type motion)
    ('block
        (do ((l (vim:motion-first-line motion) (1+ l)))
            ((> l (vim:motion-last-line motion)))
          (funcall case-func
                   (save-excursion
                     (goto-line l)
                     (move-to-column (vim:motion-first-col motion))
                     (point))
                   (save-excursion
                     (goto-line l)
                     (move-to-column (vim:motion-last-col motion))
                     (1+ (point))))))
    ('linewise
     (save-excursion
       (funcall case-func (vim:motion-begin-pos motion) (vim:motion-end-pos motion))))
    (t
     (funcall case-func (vim:motion-begin-pos motion) (vim:motion-end-pos motion))
     (goto-char (vim:motion-end-pos motion)))))


(vim:defcmd vim:cmd-repeat (nonrepeatable)
  "Repeats the last command."
  (unless vim:repeat-events
    (error "Nothing to repeat"))
  (vim:reset-key-state)
  ;;(dotimes (i (or count 1))
    (let ((repeat-events vim:repeat-events)
          (vim:repeat-events nil))
      (execute-kbd-macro repeat-events)))


(vim:defcmd vim:cmd-emacs (nonrepeatable)
   "Switches to Emacs for the next command."
   (let (message-log-max) (message "Switch to Emacs for the next command."))
   (vim:escape-to-emacs nil))

(vim:defcmd vim:cmd-write-and-close (nonrepeatable)
   "Saves the current buffer and closes the window."
   (save-buffer)
   (condition-case nil
       (delete-window)
     (error (condition-case nil
                (delete-frame)
              (error (save-buffers-kill-emacs))))))

(vim:defcmd vim:cmd-set-mark ((argument:char mark-char) nonrepeatable)
  "Sets the mark `mark-char' at point."
  (vim:set-mark mark-char))

(vim:defcmd vim:cmd-show-marks (nonrepeatable (argument marks))
  "Shows all currently defined marks."
  (let ((all-marks (append vim:local-marks-alist vim:global-marks-alist)))
    (when marks
      (setq all-marks (remove-if-not #'(lambda (x) (find (car x) marks)) all-marks)))

    (setq all-marks (sort all-marks #'(lambda (x y) (< (car x) (car y)))))
    (setq all-marks (apply #'concat
                           (mapcar
                            #'(lambda (m)
                                (format "%3c  %5d %3d %s\n"
                                        (car m)
                                        (line-number-at-pos (cdr m))
                                        (save-excursion
                                          (goto-char (cdr m))
                                          (current-column))
                                        (buffer-substring-no-properties
                                         (save-excursion
                                           (goto-char (cdr m))
                                           (line-beginning-position))
                                         (+ 20
                                            (save-excursion
                                              (goto-char (cdr m))
                                              (line-beginning-position))))))
                            all-marks)))
    (let (message-truncate-lines message-log-max)
      (message "%4s %5s %3s %s\n%s" "Mark" "Line" "Col" "File/Text"
               all-marks))))

(vim:deflocalvar vim:current-macro nil
  "The name of the currently recorded macro.")

(vim:defcmd vim:cmd-toggle-macro-recording ((argument:char reg) nonrepeatable)
  "Toggles recording of a keyboard macro."
  (if reg
      (progn
        (put 'vim:cmd-toggle-macro-recording 'argument nil)
        (vim:cmd-start-macro reg))
    (progn
      (put 'vim:cmd-toggle-macro-recording 'argument 'char)
      (vim:cmd-stop-macro))))

(defun vim:cmd-start-macro (reg)
  "Starts recording a macro in register `reg'."
  (setq vim:current-macro reg)
  (start-kbd-macro nil)
  (let (message-log-max)
    (message "Start recording keyboard macro in register '%c'" reg)))

(defun vim:cmd-stop-macro ()
  "Stops recording of a macro."
  (end-kbd-macro)
  (let (message-log-max)
    (message "Stop recording keyboard macro in register '%c'" vim:current-macro))
  (set-register vim:current-macro last-kbd-macro))

(vim:defcmd vim:cmd-execute-macro (count nonrepeatable (argument:char reg))
  "Executes the keyboard-macro in register `reg.'"
  (vim:reset-key-state)
  (execute-kbd-macro (vim:get-register reg) count))

(provide 'vim-commands)

;;; vim-commands.el ends here 
