;;; vim-motions.el - Implementation of VIM motions.

;; Copyright (C) 2009, 2010 Frank Fischer

;; Author: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;;
;; This file is not part of GNU Emacs.

;;; Commentary:

;; Motions describe functions moving the cursor or representing an
;; argument for an operator. There are three types of motions:
;; character-wise, line-wise and block-wise. Usually only the first
;; two types are represented by motion-commands while the last one is
;; implicitly used by visual-block-mode.
;;
;; A motion is defined using the macro 'vim:defmotion' which has the
;; following form:
;; 
;; (vim:defmotion name (count 
;;                      argument[:{char}] 
;;                      {inclusive,exclusive,linewise,block}) 
;;   body...)
;;
;; Each of the arguments is optional. The names of the arguments must
;; be exactly as in the definition above (but see 'Argument-renaming'
;; below).
;;
;; The COUNT argument (if given) takes the count of the motion which
;; is usually the number how often the motion should be repeated. This
;; argument may be nil if no count is given.
;;
;; The ARGUMENT argument is an aditional text-argument to be given and
;; may be nil, too. If it is specified as ARGUMENT:CHAR, the argument
;; is a one-character argument (see `vim:motion-find' usually bound to
;; 'f' for an example), otherwise it's a string-argument. Currently all
;; motions taking an argument take a character-argument.
;;
;; One if the pseudo-arguments INCLUSIVE, EXCLUSIVE, LINEWISE and BLOCK
;; must be given and specifies the type of the motion. See the Vim-manual
;; for an explanation of motion-types.
;;
;; If you do not like the default argument names, they may be renamed by using
;; (ARG NEWNAME) instead of ARG, e.g.
;;
;;   (vim:defmotion vim:motion-find (inclusive count (argument:char arg))
;;
;; defines an inclusive motion with a count-argument but renames the 
;; character-argument to ARG.
;;
;; Each motion should return an object of type `vim:motion'. This may happen
;; in one of two ways: explicit or implicit. 
;;
;; Explicit: The function creates an object of type `vim:motion' using
;; `vim:make-motion' specifing the begin position, the end position
;; and the type of the motion (overriding the motion-type specified in
;; the argument list). If the motion is a usual motion, the vim:motion
;; parameter :has-begin should be nil, if it's a text-objects it
;; should be t. The difference is that text-objects actively define a
;; range from the begin-position to the end-position, while
;; conventional motions define only the end-position placing begin at
;; (point). The motion should also change (point) usually to the
;; end-position of the returned motion.
;;
;; Implicit: Creating an explicit `vim:motion' object is overkill for
;; most simple motions. If the motion does not return a `vim:motion'
;; object, its created implicitly with the following rules: 
;;   - the begin-position is set to (point) before the execution of motion's 
;;     body
;;   - the end-position is set to (point) after the execution of motion's
;;     body
;;   - :has-begin is nil 
;;   - the type is the type defined in the motion's argument list
;; Almost all motions defined in this file are implicit.
;;
;; Note that, independently on whether the motion is defined
;; implicitly or explicitly, calling a motion always returns a
;; `vim:motion' object, i.e. (vim:motion-p (vim:motion-left)) would
;; return t.
;;
;; Motions can be bound to some key-sequence as any other interactive
;; Emacs function, but they work only in vim-mode. Ususally motions
;; are bound to the operator-pending-mode keymap using `vim:omap'.

;;; Code:

(vim:deflocalvar vim:this-column nil
  "The resulting column of the current motion.")

(vim:deflocalvar vim:last-column nil
  "The resulting column of the previous motion.")

(vim:deflocalvar vim:last-find nil
  "The previous find command (command . arg).")

(defcustom vim:word "[:word:]_"
  "Regexp-set matching a word."
  :type 'string
  :group 'vim-mode)

(defcustom vim:whitespace " \t\r\n"
  "Regexp-set matching a whitespace."
  :type 'string
  :group 'vim-mode)

(defun vim:adjust-point ()
  "Adjust the pointer after a command."
  ;; TODO: should we check modes directly?
  (when (and (not (vim:insert-mode-p))
             )				;(not vim:replace-mode))
    
    (when vim:this-column
      (move-to-column vim:this-column))
    ;; always stop at the last character (not the newline)
    (when (and (not (vim:visual-mode-p))
               (eolp) (not (bolp)))
      (backward-char)))
  
  (setq vim:last-column (or vim:this-column
                            (current-column)))
  (setq vim:this-column nil))


(defun vim:use-last-column ()
  "This function should by called by a motion not changing the column."
  (setq vim:this-column vim:last-column))


;; This structure is passed to operators taking a motion.
;; It should *not* be returned by motions.
(defstruct (vim:motion
            (:constructor vim:make-motion-struct))
  has-begin		  ; t iff the motion defined an explicit begin
  begin			  ; first point in this motion
  end			  ; last point in this motion
  type			  ; 'inclusive, 'exclusive, 'linewise
  )

(defun* vim:make-motion (&key
                         has-begin
			 (begin (point))
			 (end (point))
			 type)
  "Creates a new motion with `begin' and `end' always 
positions within (point-min) and (point-max) and not at 
 (line-end-position) (if possible)."
  (unless type
    (setq type (if (<= begin end) 'inclusive 'exclusive)))
  
  (labels 
      ((shrink-to (pos lower upper)
                  (max lower (min upper pos)))
       
       (normalize-pos (pos)
                      (let ((pos (shrink-to pos (point-min) (point-max))))
                        (shrink-to pos 
                                   (save-excursion
                                     (goto-char pos)
                                     (line-beginning-position))
                                   (save-excursion
                                     (goto-char pos)
                                     (- (line-end-position)
                                        (if (eq type 'inclusive) 1 0)))))))
    
    (vim:make-motion-struct :has-begin has-begin
                            :begin (normalize-pos begin)
                            :end (normalize-pos end)
                            :type type)))


(defun vim:motion-line-count (motion)
  "Returns the number of lines the `motion' covers."
  (1+ (- (vim:motion-last-line motion)
	 (vim:motion-first-line motion))))

(defun vim:motion-first-line (motion)
  "Returns the first line covered by `motion'."
  (min (line-number-at-pos (vim:motion-begin motion))
       (line-number-at-pos (vim:motion-end motion))))

(defun vim:motion-last-line (motion)
  "Returns the last line covered by `motion'."
  (max (line-number-at-pos (vim:motion-begin motion))
       (line-number-at-pos (vim:motion-end motion))))

(defun vim:motion-first-col (motion)
  "Returns the first column covered by `motion'."
  (min (save-excursion 
	 (goto-char (vim:motion-begin motion))
	 (current-column))
       (save-excursion 
	 (goto-char (vim:motion-end motion))
	 (current-column))))

(defun vim:motion-last-col (motion)
  "Returns the last column covered by `motion'."
  (max (save-excursion 
	 (goto-char (vim:motion-begin motion))
	 (current-column))
       (save-excursion 
	 (goto-char (vim:motion-end motion))
	 (current-column))))

(defun vim:motion-begin-pos (motion)
  "Returns the smaller position covered by `motion'.
The result is modified depending on the motion type to
return the correct start-position of emacs-ranges, i.e.
  - if motion is inclusive or exclusive, nothing is changed
  - if motion is line-wise, is always bol of the first line in the motion,
  - if motion is block 1 is added if and only if the begin column
    is larget than the end column."
  (case (vim:motion-type motion)
    (linewise
     (save-excursion
       (goto-line (vim:motion-first-line motion))
       (line-beginning-position)))
    ('block
        (let ((b (min (vim:motion-begin motion) (vim:motion-end motion)))
              (e (max (vim:motion-begin motion) (vim:motion-end motion))))
          (if (> (save-excursion (goto-char b) (current-column))
                 (save-excursion (goto-char e) (current-column)))
              (1+ b)
            b)))
    (t (min (vim:motion-begin motion) (vim:motion-end motion)))))


(defun vim:motion-end-pos (motion)
  "Returns the larger position covered by `motion'.
The result is modified depending on the motion type to
return the correct end-position of emacs-ranges, i.e.
  - if motion is inclusive, 1 is added,
  - if motion is exclusive, nothing is change,
  - if motion is line-wise, is always eol of the last line in the motion,
  - if motion is block 1 is added if and only if the end column
    is larger than or equal to the begin column."
  (case (vim:motion-type motion)
    (linewise
     (save-excursion
       (goto-line (vim:motion-last-line motion))
       (line-end-position)))
    ('block
        (let ((b (min (vim:motion-begin motion) (vim:motion-end motion)))
              (e (max (vim:motion-begin motion) (vim:motion-end motion))))
          (if (>= (save-excursion (goto-char e) (current-column))
                  (save-excursion (goto-char b) (current-column)))
              (1+ e)
            e)))
    (inclusive
     (1+ (max (vim:motion-begin motion) (vim:motion-end motion))))
    (t (max (vim:motion-begin motion) (vim:motion-end motion)))))


(defmacro vim:do-motion (type expression)
  "Executes a motion body, ensuring the return of a valid vim:motion object."
  (let ((current-pos (gensym))
        (motion (gensym)))
    `(let* ((,current-pos (point))
            (,motion ,expression))
       (if (vim:motion-p ,motion)
           ,motion
         (vim:make-motion :has-begin nil
                          :begin ,current-pos
                          :end (point)
                          :type ,type)))))
(font-lock-add-keywords 'emacs-lisp-mode '("vim:do-motion"))


(vim:deflocalvar vim:local-marks-alist nil
  "Local marks for this buffer.")

(defvar vim:global-marks-alist nil
  "Global marks.")

(defun vim:local-mark-p (mark-char)
  "Returns t if `mark-char' is a local mark."
  (or (and (>= mark-char ?a) (<= mark-char ?z))
      (member mark-char '(?^ ?. ?< ?>))))

(defun vim:global-mark-p (mark-char)
  "Returns t if `mark-char' is a global mark."
  (and (>= mark-char ?A) (<= mark-char ?z)))

(defun vim:set-mark (mark-char &optional pos)
  "Sets the mark `mark-char' to `pos' or (point)."
  (let (m)
    (cond
     ((vim:local-mark-p mark-char)
      (setq m (or (cdr-safe (assoc mark-char vim:local-marks-alist))))
      (unless m
        (setq m (make-marker))
        (push (cons mark-char m) vim:local-marks-alist)))
     
     ((vim:global-mark-p mark-char)
      (setq m (or (cdr-safe (assoc mark-char vim:global-marks-alist))))
      (unless m
        (setq m (make-marker))
        (push (cons mark-char m) vim:global-marks-alist)))
     (t (error "Unknown mark '%c'" mark-char)))
    (set-marker m (or pos (point)))))

(defun vim:get-local-mark (mark-char)
  "Returns the marker of `mark-char' if it's in the current buffer."
  (cond
   ((vim:local-mark-p mark-char)
    (let ((m (cdr-safe (assoc mark-char vim:local-marks-alist))))
      (if m m
        (error "No mark '%c' defined." mark-char))))
   ((vim:global-mark-p mark-char)
    (let ((m (cdr-safe (assoc mark-char vim:global-marks-alist))))
      (if m
          (if (eq (marker-buffer m) (current-buffer))
              m
            (error "Global mark '%c' not in current buffer." mark-char))
        (error "No mark '%c' defined." mark-char))))
   (t
    (error "Unknown mark: '%c'" mark-char))))

(add-hook 'before-change-functions 'vim:set-change-mark)
(defun vim:set-change-mark (beg end)
  "Sets the change mark . to `beg'."
  (vim:set-mark ?. beg))

(defun vim:adjust-end-of-line-position (pos)
  "If pos is an end-of-line returns pos - 1 and pos otherwise."
  (save-excursion
    (goto-char pos)
    (max (line-beginning-position)
         (min (1- (line-end-position)) pos))))

(vim:defmotion vim:motion-left (exclusive count)
  "Move the cursor count characters left."
  (goto-char (max (line-beginning-position)
                  (- (point) (or count 1)))))

(vim:defmotion vim:motion-right (exclusive count)
  "Move the cursor count characters right."
  (goto-char
   (min (line-end-position)
        (+ (point) (or count 1)))))

(vim:defmotion vim:motion-up (linewise count)
  "Move the cursor count lines up."
  (vim:use-last-column)
  (forward-line (- (or count 1))))

(vim:defmotion vim:motion-down (linewise count)
  "Move the cursor count lines down."
  (vim:use-last-column)
  (forward-line (or count 1)))

(vim:defmotion vim:motion-lines (linewise count)
  "Moves count - 1 lines down."
  (vim:use-last-column)
  (forward-line (1- (or count 1))))


(defun vim:motion-beginning-of-line-or-digit-argument ()
  "Feeds a 0 count or moves the cursor to the beginning of the line."
  (interactive)
  (if (and current-prefix-arg
           (not (zerop (prefix-numeric-value current-prefix-arg))))
      (call-interactively 'digit-argument)
    (call-interactively 'vim:motion-beginning-of-line)))


(vim:defmotion vim:motion-beginning-of-line (exclusive)
  "Move the cursor to the beginning of the current line."
  (beginning-of-line))

(vim:defmotion vim:motion-first-non-blank (exclusive)
  "Move the cursor to the first non-blank character of the current line."
  (back-to-indentation))

(vim:defmotion vim:motion-end-of-line (inclusive count)
  "Move the cursor to the end of the current line."
  (end-of-line count))

(vim:defmotion vim:motion-last-non-blank (inclusive count)
  "Move the cursor to the last non-blank charactor of the current line."
  (goto-char
   (save-excursion
     (beginning-of-line count)
     (re-search-forward "[ \t]*$")
     (max (line-beginning-position)
          (1- (match-beginning 0))))))

(vim:defmotion vim:motion-go-to-first-non-blank-beg (linewise count)
  "Moves the cursor to the first non-blank charactor of line count."
  (if count
      (goto-line count)
    (goto-char (point-min)))
  (vim:motion-first-non-blank))

(vim:defmotion vim:motion-go-to-first-non-blank-end (linewise count)
  "Moves the cursor to the first non-blank charactor of line count."
  (if count
      (goto-line count)
    (goto-char (point-max)))
  (vim:motion-first-non-blank))


(defun vim:boundary-chars (direction chars)
  "A boundary selector for a sequence of `chars'."
  (save-excursion
    (case direction
      (fwd
       (when (re-search-forward (concat "[" chars "]+") nil t)
         (1- (match-end 0))))
      (bwd
       (unless (looking-at (concat "[" chars "]"))
         (skip-chars-backward (if (= (aref chars 0) ?^)
                                  (substring chars 1)
                                (concat "^" chars))))
       (skip-chars-backward chars)
       (when (looking-at (concat "[" chars "]"))
         (point))))))


(defun vim:boundary-lines (direction predicate)
  "A boundary selector for lines identified by an predicate.
The begin-boundary is placed at the first character of the first
line, the end-boundary is placed at the last character before the
newline character of the last line."
  (save-excursion
    (let ((dir (case direction
                 (fwd +1)
                 (bwd -1))))
      ;; The last newline on a non-empty line does not count as part
      ;; of the current line.
      (when (and (not (bolp)) (looking-at "\n")) (forward-char))
      (forward-line 0)
      ;; skip unmatched lines
      (while (and (not (funcall predicate)) (zerop (forward-line dir))))
      ;; skip matched lines
      (when (funcall predicate)
        (while (save-excursion
                 (and (zerop (forward-line dir))
                      (funcall predicate)))
          (forward-line dir))
        (case direction
          (fwd (end-of-line)
               (when (and (not (bolp)) (looking-at "\n"))
                 (backward-char)))
          (bwd (forward-line 0)))
        (point)))))
  

(defun vim:boundary-empty-line (direction)
  "A boundary selector for a single empty line."
  (save-excursion
    (let ((dir (case direction
                 (fwd +1)
                 (bwd -1))))
      (while (and (not (and (bolp) (eolp))) (zerop (forward-line dir))))
      (when (and (bolp) (eolp))
        (point)))))


(defun vim:boundary-ws (direction)
  "A boundary selector for whitespaces excluding newlines."
  (vim:boundary-chars direction " \r\t"))


(defun vim:boundary-wl (direction)
  "A boundary selector for whitespaces."
  (vim:boundary-lines direction #'(lambda () (and (bolp) (eolp)))))


(defun vim:boundary-wsnl (direction)
  "A boundary selector for all whitespaces. A newline at the
beginning or end of the object (except empty lines) is not
counted."
  (save-excursion
    (catch 'end
      (case direction
        (fwd (while (let ((pos (vim:boundary-chars 'fwd " \t\r\n")))
                      (unless pos (throw 'end nil))
                      (goto-char pos)
                      (when (and (not (bolp))
                                 (looking-at "\n")
                                 (not (looking-back "[ \t\r\n]")))
                        (forward-char) t))))
        (bwd (let ((start (point)))
               (while (let ((pos (vim:boundary-chars 'bwd " \t\r\n")))
                        (unless pos (throw 'end nil))
                        (goto-char pos)
                        (when (and (not (bolp))
                                   (looking-at "\n"))
                          (if (and (looking-at "\n[ \t\r\n]")
                                   (< (point) start))
                              (progn (forward-char) nil)
                            (backward-char) t)))))))
      (point))))
         

(defun vim:boundary-word (direction)
  "A boundary selector for words."
  (funcall (vim:union-boundary #'(lambda (dir) (vim:boundary-chars dir vim:word))
                               #'(lambda (dir) (vim:boundary-chars dir (concat "^ \t\r\n" vim:word)))
                               #'(lambda (dir) (vim:boundary-empty-line dir)))
           direction))


(defun vim:boundary-WORD (direction)
  "A boundary selector for WORDs."
  (funcall (vim:union-boundary #'(lambda (dir) (vim:boundary-chars dir "^ \t\r\n"))
                               #'(lambda (dir) (vim:boundary-empty-line dir)))
           direction))
                 

(defun vim:boundary-sentence (direction)
  "A boundary selector for sentences."
  (save-excursion
    (case direction
      (fwd (when (re-search-forward "\\([.!?][])\"']*\\)\\(?:[ \t\r\n]+\\|\\'\\)" nil t)
             (1- (match-end 1))))
      (bwd (let ((start (point))
                 dot)
             ;; search the final char of the previous sentence, check
             ;; if it is really the end of a sentence up to the
             ;; beginning of the next sentence, and ensure that this
             ;; beginning is not behind the start position
             (while (and (setq dot (re-search-backward "[.!?]" nil t))
                         (not (bobp))
                         (or (not (re-search-forward "\\=[.!?][])\"']*[ \t\r\n]+" nil t))
                             (> (match-end 0) start)))
               (goto-char (1- dot)))
             (when dot (point)))))))


(defun vim:boundary-paragraph (direction)
  "A boundary selector for paragraphs.
A paragraph is a non-empty sequence of non-empty lines."
  (vim:boundary-lines direction #'(lambda () (not (and (bolp) (eolp))))))


(defun vim:union-boundary (&rest boundaries)
  "A boundary selector returning the nearest bound out of a set
of bounds."
  (lexical-let ((boundaries boundaries))
    #'(lambda (direction)
        (let ((positions (mapcan #'(lambda (bnd)
                                     (let ((pos (funcall bnd direction)))
                                       (when pos (list pos))))
                                 boundaries)))
          (when positions
            (apply (case direction
                     (fwd #'min)
                     (bwd #'max))
                   positions))))))
                       

(defun vim:union-selector (&rest boundaries)
  "A selector returns a pair of coordinates of the next (or
previous) object described by one of the given `boundaries'."
  (lexical-let ((boundaries boundaries))
    (labels
        ((find-best (get-object first-better)
                    (reduce #'(lambda (obj1 obj2)
                                (multiple-value-bind (b1 e1) obj1
                                  (multiple-value-bind (b2 e2) obj2
                                    (cond
                                     ((null obj1) obj2)
                                     ((null obj2) obj1)
                                     ((funcall first-better b1 e1 b2 e2) obj1)
                                     (t obj2)))))
                            (mapcar get-object boundaries))))
    #'(lambda (direction)
        (case direction
          (fwd (find-best #'(lambda (bnd)
                              (let ((end (funcall bnd 'fwd)))
                                (when end
                                  (let ((beg (save-excursion
                                               (goto-char end)
                                               (funcall bnd 'bwd))))
                                    (values beg end)))))
                          #'(lambda (b1 e1 b2 e2)
                              (or (< b1 b2) (and (= b1 b2) (> e1 e2))))))
          
          (bwd (find-best #'(lambda (bnd)
                              (let ((beg (funcall bnd 'bwd)))
                                (when beg
                                  (let ((end (save-excursion
                                               (goto-char beg)
                                               (funcall bnd 'fwd))))
                                    (values beg end)))))
                          #'(lambda (b1 e1 b2 e2)
                              (or (> e1 e2) (and (= e1 e2) (< b1 b2)))))))))))


(defun vim:move-fwd-beg (n boundary &optional linewise)
  "Moves the cursor to the beginning of the `n'-th text-object
forward given by `boundary'. A boundary is a function taking one
parameter `direction' which is either 'fwd or 'bwd. If the
paramter is 'fwd the function should return the last position
contained in the first text-object after or at point. If the
parameter is 'bwd the function should return the first position
contained in the first text-object before or at point."
  (catch 'end
    (when (> n 0)
      (let ((start (point)))
        ;; can't move further if already at the end of buffer
        (when (>= start (1- (point-max))) (signal 'end-of-buffer))
        ;; go to the end of the (possibly) current object
        (let ((pos (funcall boundary 'fwd)))
          (if pos (goto-char pos)
            ;; no such object
            (goto-char (point-max))
            (throw 'end nil)))
        ;; check if this object is really the current one
        (when (< start (or (funcall boundary 'bwd) (point-min)))
          ;; if not, count this object
          (decf n))
        ;; search the end of the next objects
        (dotimes (i n)
          (if linewise (forward-line) (forward-char))
          (let ((next (funcall boundary 'fwd)))
            (unless next (goto-char (point-max)) (throw 'end nil))
            (goto-char next)))
        ;; found the end of the object, go to its beginning
        (goto-char (or (funcall boundary 'bwd) (point-min)))))))


(defun vim:move-fwd-end (n boundary &optional linewise)
  "Moves the cursor to the end of the `n'-th text-object forward
given by `boundary'. A boundary is a function taking one
parameter `direction' which is either 'fwd or 'bwd. If the
paramter is 'fwd the function should return the last position
contained in the first text-object after or at point. If the
parameter is 'bwd the function should return the first position
contained in the first text-object before or at point."
  (when (> n 0)
    (when (>= (point) (1- (point-max))) (signal 'end-of-buffer)))
    (dotimes (i n)
      (if linewise (forward-line) (forward-char))
      (goto-char (or (funcall boundary 'fwd) (point-max)))))
          

(defun vim:move-bwd-beg (n boundary &optional linewise)
  "Moves the cursor to the beginning of the `n'-th text-object
backward given by `boundary'. A boundary is a function taking one
parameter `direction' which is either 'fwd or 'bwd. If the
paramter is 'fwd the function should return the last position
contained in the first text-object after or at point. If the
parameter is 'bwd the function should return the first position
contained in the first text-object before or at point."
  (when (> n 0)
    (when (bobp) (signal 'beginning-of-buffer))
    (dotimes (i n)
      (if linewise (forward-line -1) (backward-char))
      (goto-char (or (funcall boundary 'bwd) (point-min))))))


(defun vim:move-bwd-end (n boundary &optional linewise)
  "Moves the cursor to the end of the `n'-th text-object backward
given by `boundary'. A boundary is a function taking one
parameter `direction' which is either 'fwd or 'bwd. If the
paramter is 'fwd the function should return the last position
contained in the first text-object after or at point. If the
parameter is 'bwd the function should return the first position
contained in the first text-object before or at point."
  (catch 'end
    (when (> n 0)
      (let ((start (point)))
        ;; can't move further if already at the beginning of buffer
        (when (eobp) (signal 'beginning-of-buffer))
        ;; go to the beginning of the (possibly) current object
        (let ((pos (funcall boundary 'bwd)))
          (if pos (goto-char pos)
            ;; no such object
            (goto-char (point-min))
            (throw 'end nil)))
        ;; check if this object is really the current one
        (when (> start (or (funcall boundary 'fwd) (point-min)))
          ;; if not, count this object
          (decf n))
        (dotimes (i n)
          (if linewise (forward-line -1) (forward-char -1))
          (let ((next (funcall boundary 'bwd)))
            (unless next (goto-char (point-min)) (throw 'end nil))
            (goto-char next)))
        (goto-char (or (funcall boundary 'fwd) (point-min)))))))
    

(defun vim:inner-motion (n boundary ws-boundary type)
  "Selects or extends an inner text-object given by `boundary'.
`n' is the number of text-objects to be selected (or by which the
selection should be extended), `ws-boundary' selects the
whitespace object, `type' is the type of the motion to be
returned. A boundary is a function taking one parameter
`direction' which is either 'fwd or 'bwd. If the paramter is 'fwd
the function should return the last position contained in the
first text-object after or at point. If the parameter is 'bwd the
function should return the first position contained in the first
text-object before or at point."
  (let* ((linewise (eq type 'linewise))
         (forward (if linewise #'forward-line #'forward-char))
         (sel (vim:union-selector ws-boundary boundary))
         beg end pnt)
    (if (and (vim:visual-mode-p)
             (/= (point) (mark)))
        ;; extend visual range
        (if (< (point) (mark))
            ;; extend backward
            (progn
              (dotimes (i n)
                (funcall forward -1)
                (multiple-value-bind (b e) (funcall sel 'bwd)
                  (goto-char (or b (point-min)))))
              (setq end (mark)
                    pnt (point)))
          ;; extend forward
          (dotimes (i n)
            (funcall forward +1)
            (multiple-value-bind (b e) (funcall sel 'fwd)
              (goto-char (or e (1- (point-max))))))
          (setq end (point)
                pnt (point)))
      
      ;; select current ...
      (multiple-value-bind (b e) (funcall sel 'fwd)
        (dotimes (i (1- n))
          (goto-char (or e (1- (point-max))))
          (funcall forward +1)
          (multiple-value-bind (nb ne) (funcall sel 'fwd)
            (setq e (or ne (1- point-max)))))
        (setq beg b
              end e
              pnt e)))

    (goto-char pnt)
    (if beg
        (vim:make-motion :has-begin t
                         :begin beg
                         :end end
                         :type type)
      (vim:make-motion :has-begin nil
                       :end end
                       :type type))))


(defun vim:outer-motion (n boundary ws-boundary type)
  "Selects or extends an outer text-object given by `boundary'.
`n' is the number of text-objects to be selected (or by which the
selection should be extended), `ws-boundary' selects the
whitespace object, `type' is the type of the motion to be
returned. A boundary is a function taking one parameter
`direction' which is either 'fwd or 'bwd. If the paramter is 'fwd
the function should return the last position contained in the
first text-object after or at point. If the parameter is 'bwd the
function should return the first position contained in the first
text-object before or at point."
  (let* ((linewise (eq type 'linewise))
         (forward (if linewise #'forward-line #'forward-char))
         (sel (vim:union-selector boundary))
         (ws-sel (vim:union-selector ws-boundary))
         beg end pnt)
    (if (and (vim:visual-mode-p) (/= (point) (mark)))
        ;; extend visual range
        (if (< (point) (mark))
            ;; extend backward
            (progn
              (dotimes (i n)
                (multiple-value-bind (wsb wse) (save-excursion
                                                 (funcall forward -1)
                                                 (funcall ws-sel 'bwd))
                  (vim:move-bwd-beg 1 boundary linewise)
                  (when (and wsb (< wsb (point))
                             (save-excursion
                               (funcall forward -1)
                               (>= wse (point))))
                    (goto-char wsb))))
              (setq end (point)
                    pnt (point)))
          
          ;; extend forward
          (dotimes (i n)
            (multiple-value-bind (wsb wse) (save-excursion
                                             (funcall forward +1)
                                             (funcall ws-sel 'fwd))
              (vim:move-fwd-end 1 boundary linewise)
              (when (and wsb (> wse (point))
                         (save-excursion
                           (funcall forward +1)
                           (<= wsb (point))))
                (goto-char wse))))
          (setq end (point)
                pnt (point)))
      
      ;; select current ...
      (save-excursion
        (multiple-value-bind (b e) (funcall sel 'fwd)
          (unless b (signal 'no-such-object '("No such text object")))
          (dotimes (i (1- n))
            (goto-char e)
            (funcall forward +1)
            (multiple-value-bind (nb ne) (funcall sel 'fwd)
              (when ne (setq e ne))))
          (setq beg b
                end e)))

      ;; check whitespace before object
      (cond
       ;; started at white-space
       ((multiple-value-bind (wsb wse) (funcall ws-sel 'fwd)
          (if (and wsb (<= wsb (point)))
              (setq beg wsb))))
       
       ;; whitespace behind
       ((save-excursion
          (when (< end (point-max))
            (goto-char end)
            (funcall forward +1)
            (multiple-value-bind (wsb wse) (funcall ws-sel 'fwd)
              (if (and wsb (<= wsb (point)))
                  (setq end wse))))))

       ;; no whitespace behind
       ((> beg (point-min))
        (goto-char beg)
        (funcall forward -1)
        (multiple-value-bind (wsb wse) (funcall ws-sel 'bwd)
          (if (and wse (>= wse (point)))
              (setq beg wsb)))))
      
      (setq pnt end))

    (goto-char pnt)
    (if beg
        (vim:make-motion :has-begin t
                         :begin beg
                         :end end
                         :type type)
      (vim:make-motion :end end :type type))))


(defun vim:block-select (open-re close-re match-test open-pos close-pos n)
  "Returns the position of an enclosing block."
  (labels
      ((find-at-point (re pos begin)
                      (goto-char pos)
                      ;; start searching the object in the current
                      ;; line to see if it's at point
                      (forward-line 0)
                      (while (and (re-search-forward re
                                                     (line-end-position) t)
                                  (< (match-end 0) pos)))
                      (if (and (match-beginning 0)
                               (<= (match-beginning 0) open-pos)
                               (>= (match-end 0) open-pos))
                          ;; found object at cursor
                          (if begin
                              (goto-char (match-beginning 0))
                            (goto-char (match-end 0)))
                        (goto-char pos))))
    (catch 'end
      (save-excursion
        (let ((combined-re (concat "\\(" open-re "\\)\\|\\(" close-re "\\)"))
              op-beg op-end cl-beg cl-end
              (cnt n)
              found-stack)
          ;; set default match-test
          (unless match-test (setq match-test #'(lambda (a b) t)))
          ;; search the opening object
          (find-at-point open-re open-pos nil)
          (while (> cnt 0)
            (unless (re-search-backward combined-re nil t) (throw 'end nil))
            (if (match-beginning 1)
                (if found-stack
                    (if (funcall match-test
                                 (cons (match-beginning 1)
                                       (match-end 1))
                                 (car found-stack))
                        ;; found matching opening object
                        (pop found-stack)
                      ;; found object does not match
                      (throw 'end nil))
                  ;; found enclosing opening object
                  (decf cnt))
              (push (cons (match-beginning 2) (match-end 2)) found-stack)))
                  
          ;; found the opening object
          (setq op-beg (match-beginning 0)
                op-end (1- (match-end 0)))
          
          ;; search the closing object
          (push (cons op-beg (1+ op-end)) found-stack)
          (goto-char (1+ op-end))
          (while found-stack
            (unless (re-search-forward combined-re nil t) (throw 'end nil))
            (if (match-beginning 2)
                (if (funcall match-test
                             (car found-stack)
                             (cons (match-beginning 2)
                                   (match-end 2)))
                    ;; found matching closing object
                    (pop found-stack)
                  ;; found object does not match
                  (throw 'end nil))
              ;; found opening object
              (push (cons (match-beginning 1) (match-end 1)) found-stack)))
                  
          ;; found the closing object
          (setq cl-beg (match-beginning 0)
                cl-end (1- (match-end 0)))
          (when (>= cl-end close-pos)
            (values op-beg op-end cl-beg cl-end)))))))


(defun vim:inner-block (open-re close-re match-test n)
  "Selects the next `n' enclosing blocks excluding the delimiters."
  (let (open-pos close-pos)
    (if (vim:visual-mode-p)
        (setq open-pos (min (point) (mark))
              close-pos (max (point) (mark)))
      (setq open-pos (point)
            close-pos (point)))

    ;; check if we the current inner tag is selected completely
    (multiple-value-bind (op-beg op-end cl-beg cl-end)
        (vim:block-select open-re close-re match-test open-pos close-pos 1)
      (when (and op-beg
                 (= (1+ op-end) open-pos)
                 (= (1- cl-beg) close-pos))
        (incf n)))
    
    (multiple-value-bind (op-beg op-end cl-beg cl-end)
        (vim:block-select open-re close-re match-test open-pos close-pos n)
      (when op-beg
        (goto-char (if (< (point) (mark)) (1+ op-end) (1- cl-beg)))
        (vim:make-motion :has-begin t
                         :begin (1+ op-end)
                         :end (1- cl-beg)
                         :type 'inclusive)))))


(defun vim:outer-block (open-re close-re match-test n)
  "Selects the next `n' enclosing blocks including the delimiters."
  (let (open-pos close-pos)
    (if (vim:visual-mode-p)
        (setq open-pos (min (point) (mark))
              close-pos (max (point) (mark)))
      (setq open-pos (point)
            close-pos (point)))

    ;; check if we the current inner tag is selected completely
    (multiple-value-bind (op-beg op-end cl-beg cl-end)
        (vim:block-select open-re close-re match-test open-pos close-pos 1)
      (when (and op-beg
                 (= op-beg open-pos)
                 (= cl-end close-pos))
        (incf n)))
    
    (multiple-value-bind (op-beg op-end cl-beg cl-end)
        (vim:block-select open-re close-re match-test open-pos close-pos n)
      (when op-beg
        (goto-char (if (< (point) (mark)) op-beg cl-end))
        (vim:make-motion :has-begin t
                         :begin op-beg
                         :end cl-end
                         :type 'inclusive)))))


(vim:defmotion vim:motion-fwd-word (exclusive count)
  "Moves the cursor beginning of the next word."
  (vim:move-fwd-beg (or count 1) #'vim:boundary-word)
  
  ;; in operator-pending mode, if we reached the beginning of a new
  ;; line, go back to the end of the previous line
  (when (and (vim:operator-pending-mode-p)
             (vim:looking-back "^[ \t]*")
             (not (save-excursion
                    (forward-line -1)
                    (and (bolp) (eolp)))))
    (forward-line -1)
    (end-of-line)))


(vim:defmotion vim:motion-bwd-word (exclusive count)
  "Moves the cursor beginning of the previous word."
  (vim:move-bwd-beg (or count 1) #'vim:boundary-word))


(vim:defmotion vim:motion-fwd-word-end (inclusive count)
  "Moves the cursor to the end of the next word."            
  (vim:move-fwd-end (or count 1) #'vim:boundary-word))


(vim:defmotion vim:motion-bwd-word-end (inclusive count)
  "Moves the cursor to the end of the previous word."            
  (vim:move-bwd-end (or count 1) #'vim:boundary-word))


(vim:defmotion vim:motion-inner-word (inclusive count)
  "Select `count' inner words."
  (vim:inner-motion (or count 1) #'vim:boundary-word #'vim:boundary-ws 'inclusive))


(vim:defmotion vim:motion-outer-word (inclusive count)
  "Select `count' outer words."
  (vim:outer-motion (or count 1) #'vim:boundary-word #'vim:boundary-ws 'inclusive))


(vim:defmotion vim:motion-fwd-WORD (exclusive count)
  "Moves the cursor to beginning of the next WORD."
  (vim:move-fwd-beg (or count 1) #'vim:boundary-WORD)
  
  ;; in operator-pending mode, if we reached the beginning of a new
  ;; line, go back to the end of the previous line
  (when (and (vim:operator-pending-mode-p)
             (vim:looking-back "^[ \t]*")
             (not (save-excursion
                    (forward-line -1)
                    (and (bolp) (eolp)))))
    (forward-line -1)
    (end-of-line)))


(vim:defmotion vim:motion-bwd-WORD (exclusive count)
  "Moves the cursor to beginning of the previous WORD."
  (vim:move-bwd-beg (or count 1) #'vim:boundary-WORD))


(vim:defmotion vim:motion-fwd-WORD-end (inclusive count)
  "Moves the cursor to the end of the next WORD."            
  (vim:move-fwd-end (or count 1) #'vim:boundary-WORD))


(vim:defmotion vim:motion-bwd-WORD-end (inclusive count)
  "Moves the cursor to the end of the next WORD."            
  (vim:move-bwd-end (or count 1) #'vim:boundary-WORD))


(vim:defmotion vim:motion-inner-WORD (inclusive count)
  "Select `count' inner WORDs."
  (vim:inner-motion (or count 1) #'vim:boundary-WORD #'vim:boundary-ws 'inclusive))


(vim:defmotion vim:motion-outer-WORD (inclusive count)
  "Select `count' outer WORDs."
  (vim:outer-motion (or count 1) #'vim:boundary-WORD #'vim:boundary-ws 'inclusive))


(vim:defmotion vim:motion-fwd-sentence (exclusive count)
  "Move the cursor `count' sentences forward."
  (dotimes (i (or count 1))
    (goto-char (min (save-excursion
                      (vim:move-fwd-beg 1 #'vim:boundary-sentence)
                      (point))
                    (save-excursion
                      (vim:motion-fwd-paragraph)
                      (point))))))
    

(vim:defmotion vim:motion-bwd-sentence (exclusive count)
  "Move the cursor `count' sentences backward."
  (vim:move-bwd-beg (or count 1)
                    (vim:union-boundary #'vim:boundary-sentence #'vim:boundary-paragraph)))


(vim:defmotion vim:motion-inner-sentence (inclusive count)
  "Select `count' inner words."
  (vim:inner-motion (or count 1)
                    (vim:union-boundary #'vim:boundary-sentence #'vim:boundary-paragraph)
                    #'vim:boundary-wsnl 'inclusive))


(vim:defmotion vim:motion-outer-sentence (inclusive count)
  "Select `count' outer words."
  (vim:outer-motion (or count 1)
                    (vim:union-boundary #'vim:boundary-sentence #'vim:boundary-paragraph)
                    #'vim:boundary-wsnl 'inclusive))


(vim:defmotion vim:motion-fwd-paragraph (exclusive count)
  "Move the cursor `count' paragraphs forward."
  (if (eobp) (signal 'end-of-buffer)
    (dotimes (i (or count 1))
      (goto-char (or (vim:boundary-paragraph 'fwd) (point-max)))
      (forward-line))))


(vim:defmotion vim:motion-bwd-paragraph (exclusive count)
  "Move the cursor `count' paragraphs backward."
  (if (bobp) (signal 'beginning-of-buffer)
    (dotimes (i (or count 1))
      (goto-char (or (vim:boundary-paragraph 'bwd) (point-min)))
      (forward-line -1))))


(vim:defmotion vim:motion-inner-paragraph (inclusive count)
  "Select `count' inner words."
  (vim:inner-motion (or count 1) #'vim:boundary-paragraph #'vim:boundary-wl 'linewise))


(vim:defmotion vim:motion-outer-paragraph (inclusive count)
  "Select `count' outer words."
  (vim:outer-motion (or count 1) #'vim:boundary-paragraph #'vim:boundary-wl 'linewise))


(vim:defmotion vim:motion-inner-parentheses (inclusive count)
  "Select `count' enclosing pairs of () exclusive."
  (vim:inner-block "(" ")" nil (or count 1)))


(vim:defmotion vim:motion-outer-parentheses (inclusive count)
  "Select `count' enclosing pairs of () inclusive."
  (vim:outer-block "(" ")" nil (or count 1)))


(vim:defmotion vim:motion-inner-brackets (inclusive count)
  "Select `count' enclosing pairs of [] exclusive."
  (vim:inner-block "\\[" "\\]" nil (or count 1)))


(vim:defmotion vim:motion-outer-brackets (inclusive count)
  "Select `count' enclosing pairs of [] inclusive."
  (vim:outer-block "[" "]" nil (or count 1)))


(vim:defmotion vim:motion-inner-braces (inclusive count)
  "Select `count' enclosing pairs of {} exclusive."
  (vim:inner-block "{" "}" nil (or count 1)))


(vim:defmotion vim:motion-outer-braces (inclusive count)
  "Select `count' enclosing pairs of {} inclusive."
  (vim:outer-block "{" "}" nil (or count 1)))


(vim:defmotion vim:motion-inner-angles (inclusive count)
  "Select `count' enclosing pairs of <> exclusive."
  (vim:inner-block "<" ">" nil (or count 1)))


(vim:defmotion vim:motion-outer-angles (inclusive count)
  "Select `count' enclosing pairs of <> inclusive."
  (vim:outer-block "<" ">" nil (or count 1)))


(vim:defmotion vim:motion-inner-xml-tags (inclusive count)
  "Select `count' enclosing pairs of <tag> </tag> exclusive."
  (vim:inner-block "<[^/>]+?>" "</[^/>]+?>"
                   #'(lambda (tag1 tag2)
                       (zerop (compare-buffer-substrings nil
                                                         (1+ (car tag1))
                                                         (1- (cdr tag1))
                                                         nil
                                                         (+ 2 (car tag2))
                                                         (1- (cdr tag2)))))
                   (or count 1)))


(vim:defmotion vim:motion-outer-xml-tags (inclusive count)
  "Select `count' enclosing pairs of <tag> </tag> inclusive."
  (vim:outer-block "<[^/>]+?>" "</[^/>]+?>"
                   #'(lambda (tag1 tag2)
                       (zerop (compare-buffer-substrings nil
                                                         (1+ (car tag1))
                                                         (1- (cdr tag1))
                                                         nil
                                                         (+ 2 (car tag2))
                                                         (1- (cdr tag2)))))
                   (or count 1)))


(vim:defmotion vim:motion-find (inclusive count (argument:char arg))
  "Move the cursor to the next count'th occurrence of arg."
  (forward-char)
  (let ((case-fold-search nil))
    (unless (search-forward (char-to-string arg)
                            nil t (or count 1))
      (backward-char)
      (error (format "Can't find %c" arg)))
    (setq vim:last-find (cons 'vim:motion-find arg))
    (backward-char)))


(vim:defmotion vim:motion-find-back (exclusive count (argument:char arg))
  "Move the cursor to the previous count'th occurrence of arg."
  (let ((case-fold-search nil))
    (unless (search-backward (char-to-string arg)
                             nil t (or count 1))
      (error (format "Can't find %c" arg)))
    (setq vim:last-find (cons 'vim:motion-find-back arg))))


(vim:defmotion vim:motion-find-to (inclusive count (argument:char arg))
  "Move the cursor to the character before the next count'th\
   occurence of arg."
  (vim:motion-find :count count :argument arg)
  (backward-char)
  (setq vim:last-find (cons 'vim:motion-find-to arg)))


(vim:defmotion vim:motion-find-back-to (exclusive count (argument:char arg))
  "Move the cursor to the character after the previous count'th\
   occurence of arg."
  (vim:motion-find-back :count count :argument arg)
  (forward-char)
  (setq vim:last-find (cons 'vim:motion-find-to arg)))


(vim:defmotion vim:motion-repeat-last-find (inclusive count)
  "Repeats the last find command."
  (unless vim:last-find
    (error "No previous find command."))
  (funcall (car vim:last-find)
           :count count
           :argument (cdr vim:last-find)))


(vim:defmotion vim:motion-repeat-last-find-opposite (inclusive count)
  "Repeats the last find command."
  (unless vim:last-find
    (error "No previous find command."))
  (let ((func (case (car vim:last-find)
                ('vim:motion-find 'vim:motion-find-back)
                ('vim:motion-find-back 'vim:motion-find)
                ('vim:motion-find-to 'vim:motion-find-back-to)
                ('vim:motion-find-back-to 'vim:motion-find-to)
                (t (error (format "Unexpected find command %s"
                                  (car vim:last-find))))))
        (arg (cdr vim:last-find)))
    (let ((vim:last-find nil))
      (funcall func :count count :argument arg))))


(vim:defmotion vim:motion-jump-item (inclusive)
  "Find the next item in this line after or under the cursor and
jumps to the corresponding one."
  (let ((next-open
         (condition-case err
             (1- (scan-lists (point) 1 -1))
           (error
            (point-max))))
        (next-close
         (condition-case nil
             (1- (scan-lists (point) 1 +1))
           (error (point-max)))))
    (let ((pos (min next-open next-close)))
      (when (>= pos (line-end-position))
        (error "No matching item found on the current line."))
      (if (= pos next-open)
          (progn
            (goto-char pos)
            (forward-list)
            (backward-char))
        (progn
          (goto-char (1+ pos))
          (backward-list))))))


(vim:defmotion vim:motion-mark (exclusive (argument:char mark-char))
  "Moves to the position of `mark-char'."
  (goto-char (vim:get-local-mark mark-char)))

(vim:defmotion vim:motion-mark-line (linewise (argument:char mark-char))
  "Moves to the first non-blank char in the line of `mark-char'."
  (goto-char (vim:get-local-mark mark-char))
  (vim:motion-first-non-blank)
  t)

(provide 'vim-motions)

;;; vim-motions.el ends here
