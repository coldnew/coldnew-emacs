;;;; Operator-Pending mode

;; This provides a framework for combining "motions" and "operators".
;; A motion is any command moving point. An operator is a command
;; acting on the text moved over by a motion.
;;
;; Defining operator commands is similar to defining commands acting
;; on the region. That is, both must have two arguments, BEG and END,
;; and an `interactive' specification that stores the relevant range
;; in those arguments. The `vimpulse-define-operator' macro takes care
;; of this. (If you like, you can also convert any region command to
;; an operator with `vimpulse-convert-to-operator'.)
;;
;; When an operator command is run in vi state, it queries the user
;; for a motion and determines the resulting range to store in BEG and
;; END. In Visual mode, it skips the querying and uses the selection
;; boundaries.
;;
;; While a motion is read from the keyboard, a temporary Viper state,
;; Operator-Pending mode, is entered. This state inherits bindings
;; from the regular vi state, but it may also define its own, for
;; instance text objects. Text objects are like motions, but define a
;; starting point as well as an ending point. They are implemented
;; simply as selection commands.
;;
;; As in Vim, a motion may specify a motion type, such as `line',
;; stored in the `motion-type' symbol property:
;;
;;   * `line': the motion range is extended to whole lines.
;;   * `inclusive': the ending character is included.
;;   * `exclusive' (default): the ending character is excluded.
;;
;; For example, (put 'foo 'motion-type 'line) gives `foo' a type of
;; `line'. If unspecified, the motion is considered `exclusive'.
;; You can override the type with v, V and C-v: for instance,
;; dvj will delete an exclusive range rather than a linewise.
;;
;; The benefit of a dedicated state when an "operator" is "pending" is
;; code separation. In the original scheme, every Viper motion had to
;; manually do the work of deleting/changing/yanking the text moved
;; over, making that action repeatable, etc. The new framework handles
;; everything automatically and orthogonally, enabling the use of
;; plain Emacs movement commands (like S-exp navigation) as motions.
;;
;; A smattering of compatibility macros ensure that certain Viper
;; motions are repeated correctly. In the long run, Viper's motions
;; should be rewritten; I'll have to contact Michael Kifer and hear
;; what he thinks about this. For what it's worth, the following code
;; addresses "TODO item #1" in viper.el.

(require 'vimpulse-visual-mode)
(eval-when-compile (require 'vimpulse-viper-function-redefinitions)) ; vimpulse-define-state

(vimpulse-define-state operator
  "Operator-pending mode is when an operator is pending,
awaiting a motion (after \"d\", \"y\", \"c\", etc.)."
  :id "<OP> "
  :hook '(vimpulse-set-operator-cursor-type)
  :enable '(vimpulse-operator-remap-minor-mode
            (viper-vi-kbd-minor-mode nil)
            vi-state vimpulse-careful-minor-mode)
  (cond
   ((eq viper-current-state 'operator-state)
    (vimpulse-careful-minor-mode 1))
   (t
    (vimpulse-careful-minor-mode -1))))

;; This is a short-lived state, only used for calculating
;; motion ranges. If anything goes wrong and we enter the
;; command loop, exit to vi state immediately.
(defun vimpulse-operator-exit-hook ()
  "Exit Operator-Pending mode."
  (when (eq viper-current-state 'operator-state)
    (let (abbrev-mode)
      (save-excursion
        (viper-change-state-to-vi)))))

(add-hook 'pre-command-hook 'vimpulse-operator-exit-hook)
(add-hook 'post-command-hook 'vimpulse-operator-exit-hook)

;; We place all remap bindings in a keymap of their own.
;; This enables Visual mode only to inherit text object
;; bindings from Operator-Pending mode, not any remapping.
(define-minor-mode vimpulse-operator-remap-minor-mode
  "Minor mode of bindings overwritten by `vimpulse-map' et al."
  :keymap vimpulse-operator-remap-map)

(put 'vimpulse-operator-remap-map
     'remap-alist 'vimpulse-operator-remap-alist)

(defun vimpulse-operator-remap (from to)
  "Remap FROM to TO in Operator-Pending mode."
  (vimpulse-remap vimpulse-operator-remap-map from to))

(defun vimpulse-operator-remapping (cmd)
  "Return Operator-Pending remapping for CMD."
  (if (featurep 'xemacs)
      (or (cdr (assq cmd vimpulse-operator-remap-alist)) cmd)
    (or (command-remapping cmd) cmd)))

(when (featurep 'xemacs)
  ;; XEmacs shows the tag before the modes, so truncate it to a
  ;; constant length to avoid excessive flickering
  (setq vimpulse-operator-state-id "<OP>") ; 4 characters
  ;; XEmacs lacks a horizontal bar cursor option
  (setq vimpulse-want-operator-pending-cursor nil))

(defun vimpulse-set-operator-cursor-type ()
  "Change cursor appearance in Operator-Pending mode."
  (when vimpulse-want-operator-pending-cursor
    (vimpulse-half-height-cursor)))

;; The half-height "Operator-Pending cursor" cannot be specified
;; as a static `cursor-type' value, since its height depends on
;; the current font size; a function is needed.
(defun vimpulse-half-height-cursor ()
  "Change cursor to a half-height box.
\(This is really just a thick horizontal bar.)"
  (unless (featurep 'xemacs)
    (condition-case nil
        (let (height)
          ;; make `window-line-height' reliable
          (redisplay)
          (setq height (window-line-height))
          (setq height (+ (nth 0 height) (nth 3 height)))
          ;; cut cursor height in half
          (setq height (/ height 2))
          (setq cursor-type (cons 'hbar height))
          ;; ensure the cursor is redisplayed
          (force-window-update (selected-window))
          (redisplay))
      (error nil))))

(defvar vimpulse-operators nil
  "Operator commands defined with `vimpulse-define-operator'.")

(viper-deflocalvar vimpulse-inhibit-operator nil
  "Inhibit current operator.
If an operator calls a motion and the motion sets this variable
to t, the operator code is not executed.")

(defmacro vimpulse-define-operator (operator args &rest body)
  "Define an operator command OPERATOR.
ARGS is the argument list, which must contain at least two
arguments: the beginning and end of the range. It is followed by
an optional docstring and optional keywords:

:repeat BOOLEAN         Let \\[viper-repeat] repeat the command (default).
:move-point BOOLEAN     Move to beg. of range in vi state (default).
:whole-lines BOOLEAN    Extend the range to include whole lines.
:keep-visual BOOLEN     Don't disable Visual selection.
:motion MOTION          Predefined motion to use in vi state.
:keys KEYS              A key or a list of keys to bind the command to.
:map MAP                Keymap to bind :keys in, defaults to
                        `viper-vi-basic-map'.

The keywords are followed by the operator's body. Thus, a simple
example may look like:

    (vimpulse-define-operator test (beg end)
      \"Test operator.\"
      :repeat nil
      :whole-lines t
      (delete-region beg end))

When this command is called interactively, a motion is read from
the keyboard and the resulting range is stored in BEG and END.
In Visual mode, the beginning and end of the selection are used.
The command then proceeds to do whatever it wants to do on the
text between those buffer positions, like delete it in this case.

If :motion is specified, the operator will use that motion
instead of reading one from the keyboard. This has no effect
in Visual mode."
  (declare (indent defun)
           (debug (&define name lambda-list
                           [&optional stringp]
                           [&rest keywordp sexp]
                           def-body)))
  (let ((repeat t) (move-point t)
        (map 'viper-vi-basic-map)
        beg end doc keep-visual keys keyword motion whole-lines)
    ;; collect BEG and END arguments
    (setq beg (or (pop args) 'beg)
          end (or (pop args) 'end))
    ;; collect docstring, if any
    (when (stringp (car body))
      (setq doc (pop body)))
    ;; collect keywords
    (while (keywordp (setq keyword (car body)))
      (setq body (cdr body))
      (cond
       ((eq keyword :repeat)
        (setq repeat (vimpulse-unquote (pop body))))
       ((eq keyword :move-point)
        (setq move-point (vimpulse-unquote (pop body))))
       ((eq keyword :whole-lines)
        (setq whole-lines (vimpulse-unquote (pop body))))
       ((eq keyword :keep-visual)
        (setq keep-visual (vimpulse-unquote (pop body))))
       ((eq keyword :motion)
        (setq motion (vimpulse-unquote (pop body))))
       ((eq keyword :map)
        (setq map (vimpulse-unquote (pop body))))
       ((eq keyword :keys)
        (setq keys (vimpulse-unquote (pop body))))
       (t
        (pop body))))
    (unless (listp keys)
      (setq keys (list keys)))
    ;; macro expansion: define key bindings and define command
    `(progn
       (add-to-list 'vimpulse-operators ',operator)
       (dolist (key ',keys)
         (define-key ,map key ',operator))
       (defun ,operator (,beg ,end ,@args)
         ,doc
         (interactive
          (vimpulse-range
           ,(not repeat) ,(not move-point)
           ,whole-lines ,keep-visual ',motion))
         (if (and vimpulse-inhibit-operator
                  (vimpulse-called-interactively-p))
             (setq vimpulse-inhibit-operator nil)
           ,@body)))))

(when (fboundp 'font-lock-add-keywords)
  (font-lock-add-keywords
   'emacs-lisp-mode
   '(("(\\(vimpulse-define-operator\\)\\>[ \f\t\n\r\v]*\\(\\sw+\\)?"
      (1 font-lock-keyword-face)
      (2 font-lock-function-name-face nil t)))))

(defun vimpulse-range
  (&optional no-repeat dont-move-point whole-lines keep-visual custom-motion)
  "Read a motion and return a range (BEG END).
Belongs in the `interactive' form of a command. Don't use this
function directly; see `vimpulse-define-operator' instead."
  (let ((range (list (point) (point)))
        (type-alist '((vimpulse-visual-toggle-char . inclusive)
                      (vimpulse-visual-toggle-line . line)
                      (vimpulse-visual-toggle-block . block)))
        (type (when whole-lines 'line))
        ;; For restoration of the echo area. We bind `message-log-max' to nil
        ;; to prevent `oldmsg' from messing up the *Messages* buffer.
        (oldmsg (current-message))
        message-log-max)
    (setq vimpulse-this-motion-type nil
          vimpulse-this-count nil
          vimpulse-this-motion nil
          vimpulse-this-operator this-command
          vimpulse-inhibit-operator nil)
    (cond
     ;; if text is selected, use selection boundaries as range
     ((or vimpulse-visual-mode (region-active-p))
      (when (and whole-lines
                 (not (eq vimpulse-visual-mode 'line)))
        (vimpulse-visual-activate 'line)
        (vimpulse-set-visual-dimensions))
      ;; determine range and go to beginning
      (setq range (vimpulse-visual-range))
      (setq vimpulse-this-motion-type (vimpulse-motion-type range)
            range (vimpulse-motion-range range))
      (setq vimpulse-this-motion 'vimpulse-visual-reselect)
      (if keep-visual
          (if (eq vimpulse-visual-mode 'line)
              (vimpulse-visual-restore)
            (vimpulse-visual-contract-region))
        (if (eq vimpulse-this-motion-type 'block)
            (vimpulse-visual-block-rotate
             'upper-left
             (vimpulse-range-beginning range)
             (vimpulse-range-end range))
          (goto-char (vimpulse-range-beginning range))
          (set-mark  (vimpulse-range-end range)))
        ;; disable selection
        (if (and vimpulse-visual-mode
                 (fboundp 'vimpulse-visual-mode))
            (vimpulse-visual-mode -1)
          (vimpulse-deactivate-region))))
     ;; Not in Visual mode: use CUSTOM-MOTION if specified,
     ;; or read motion and return motion range.
     (t
      (if custom-motion
          (setq vimpulse-this-motion custom-motion)
        (vimpulse-change-state-to-operator)
        (while (progn
                 (setq vimpulse-this-motion
                       (vimpulse-keypress-parser t))
                 (setq vimpulse-this-count
                       (if vimpulse-this-count
                           (if (numberp (cadr vimpulse-this-motion))
                               (string-to-number
                                (concat (number-to-string
                                         vimpulse-this-count)
                                        (number-to-string
                                         (cadr vimpulse-this-motion))))
                             vimpulse-this-count)
                         (cadr vimpulse-this-motion))
                       vimpulse-this-motion
                       (car vimpulse-this-motion))
                 (when (assq vimpulse-this-motion type-alist)
                   (setq type (cdr (assq vimpulse-this-motion
                                         type-alist))))))
        ;; motion reading done: restore the echo area
        (if oldmsg (message "%s" oldmsg)
          (message nil))
        ;; with doubled operator ("gqgq" or "gqq"), set motion to current line
        (if (or (eq vimpulse-this-motion vimpulse-this-operator)
                (member (vimpulse-strip-prefix (this-command-keys) t)
                        '("g??" "gUU" "gqq" "guu" "gww" "g~~")))
            (setq vimpulse-this-motion 'vimpulse-line)
          (setq vimpulse-this-motion
                (vimpulse-operator-remapping vimpulse-this-motion))))
      (cond
       ;; quit if motion reading failed
       ((or (not vimpulse-this-motion)
            (memq vimpulse-this-motion '(viper-nil keyboard-quit))
            (vimpulse-operator-cmd-p vimpulse-this-motion))
        (let (abbrev-mode)
          (save-excursion
            (viper-change-state-to-vi)))
        (setq quit-flag t))
       (t
        ;; multiply operator count and motion count together
        (when (or current-prefix-arg vimpulse-this-count)
          (setq vimpulse-this-count
                (* (prefix-numeric-value current-prefix-arg)
                   (prefix-numeric-value vimpulse-this-count))))
        ;; determine type to use for type conversion
        (when (and (eq type 'inclusive)
                   (memq (vimpulse-motion-type vimpulse-this-motion)
                         '(line inclusive)))
          (setq type 'exclusive))
        (setq range (vimpulse-calculate-motion-range
                     vimpulse-this-count vimpulse-this-motion type))
        (setq vimpulse-this-motion-type (vimpulse-motion-type range)
              range (vimpulse-motion-range range))
        (unless dont-move-point
          (goto-char (vimpulse-range-beginning range))
          (when (and viper-auto-indent
                     (looking-back "^[ \f\t\v]*"))
            (back-to-indentation)))
        (let (abbrev-mode)
          (save-excursion
            (viper-change-state-to-vi)))))))
    ;; set up repeat
    (unless no-repeat
      (setq vimpulse-last-operator vimpulse-this-operator
            vimpulse-last-motion vimpulse-this-motion
            vimpulse-last-motion-type
            (when type vimpulse-this-motion-type))
      (viper-set-destructive-command
       (list 'vimpulse-operator-repeat
             vimpulse-this-count nil viper-use-register nil nil)))
    range))

(defun vimpulse-calculate-motion-range (count motion &optional type refresh)
  "Derive motion range (TYPE BEG END) from MOTION and COUNT.
MOTION can move point or select some text (a text object).
TYPE may specify the motion type for normalizing the resulting
range. If REFRESH is t, this function changes point,
`viper-com-point' and `vimpulse-this-motion-type'."
  (cond
   ;; REFRESH is nil, so bind global variables
   ((not refresh)
    (let ((opoint   (point))
          (omark    (mark t))
          (omactive (and (boundp 'mark-active) mark-active))
          (obuffer  (current-buffer))
          viper-com-point vimpulse-this-motion-type)
      (unwind-protect (vimpulse-calculate-motion-range count motion type t)
        ;; restore point and mark like `save-excursion',
        ;; but only if the motion hasn't disabled the operator
        (unless vimpulse-inhibit-operator
          (set-buffer obuffer)
          (let (mark-active) (set-mark omark))
          (and (boundp 'mark-active) (setq mark-active omactive))
          (goto-char opoint)))))
   (t
    (let ((current-prefix-arg count)
          (viper-intermediate-command 'viper-command-argument)
          (viper-current-state 'operator-state)
          (vimpulse-operator-basic-minor-mode t)
          (motion-type (vimpulse-motion-type motion t))
          (already-selection (or vimpulse-visual-mode
                                 (region-active-p)))
          (range (list 'exclusive (point) (point)))
          vimpulse-visual-vars-alist)
      (setq vimpulse-this-motion-type
            (or type motion-type 'exclusive))
      (viper-move-marker-locally 'viper-com-point (point))
      ;; enable Transient Mark mode so we can reliably
      ;; detect selection commands
      (vimpulse-transient-mark)
      ;; Whatever happens next, we must restore Transient Mark mode
      ;; to its original state afterwards!
      (unwind-protect
          ;; `vimpulse-visual-vars-alist' is used for restoring,
          ;; so protect it
          (let (vimpulse-visual-vars-alist)
            (if (commandp motion)
                (call-interactively motion)
              (funcall motion count))
            (cond
             ;; if text has been selected (i.e., it's a text object),
             ;; return the selection
             ((and (not already-selection)
                   (or vimpulse-visual-mode (region-active-p)))
              (setq range (vimpulse-visual-range))
              (cond
               ((and motion-type (not (eq (car range) motion-type)))
                (setcar range motion-type))
               ((and type (not (eq (car range) type)))
                (setcar range type)
                (setq range (vimpulse-normalize-motion-range range))))
              ;; deactivate Visual mode/region
              (if (and vimpulse-visual-mode
                       (fboundp 'vimpulse-visual-mode))
                  (vimpulse-visual-mode -1)
                (vimpulse-deactivate-region)))
             ;; otherwise, range is defined by `viper-com-point'
             ;; and point (Viper type motion)
             (t
              (setq range (vimpulse-make-motion-range
                           (marker-position viper-com-point)
                           (point)
                           (or type vimpulse-this-motion-type) t)))))
        (vimpulse-transient-restore))
      range))))

;; A keypress parser of some kind is unavoidable because we need to
;; know what we are executing beforehand (like when multiplying the
;; counts in "2d3w"). We try to avoid hard-coding where possible by
;; inspecting commands rather than the keys themselves.
(defun vimpulse-keypress-parser (&optional no-remap)
  "Read from keyboard and build a command description.
Returns (CMD COUNT), where COUNT is the numeric prefix argument
of CMD. Both COUNT and CMD may be nil."
  (let ((inhibit-quit t)
        (echo-keystrokes 0.01)
        char digit keys cmd count)
    (while (progn
             ;; read a keypress, respecting Emacs version,
             ;; and convert it to ASCII representation
             (if (featurep 'xemacs)
                 (setq char (event-to-character
                             (next-command-event) nil t))
               (setq char (read-event))
               (when (symbolp char)
                 (setq char (or (get char 'ascii-character) char))))
             ;; this trick from simple.el's `digit-argument'
             ;; converts keystrokes like C-0 and C-M-1 to digits
             (if (or (characterp char) (integerp char))
                 (setq digit (- (logand char ?\177) ?0))
               (setq digit nil))
             (if (keymapp cmd)
                 (setq keys (vconcat keys (vector char)))
               (setq keys (vector char)))
             (if no-remap              ; XEmacs doesn't have remapping
                 (setq cmd (key-binding keys t))
               (setq cmd (key-binding keys t t)))
             ;; this `cond' form determines whether
             ;; the reading loop will continue
             (cond
              ;; if calling itself ("cc"), return current command
              ((eq (vimpulse-strip-prefix
                    (vconcat (this-command-keys))) keys)
               (setq cmd this-command)
               nil)
              ;; if CMD is a keymap, we need to read more
              ((keymapp cmd)
               t)
              ;; numeric prefix argument
              ((or (memq cmd '(viper-digit-argument digit-argument))
                   ;; the 0 key runs `viper-beginning-of-line',
                   ;; so ignore it unless preceded by other digits
                   (and (eq (length keys) 1)
                        (not (keymapp cmd))
                        count
                        ;; probably overkill: only 0 bound this way
                        (memq digit '(0 1 2 3 4 5 6 7 8 9))))
               ;; store digits in a string, which is easily converted
               ;; to a number afterwards
               (setq count (concat (or count "")
                                   (number-to-string digit)))
               t)
              ;; catch middle digits like "da2w"
              ((and (not cmd)
                    (> (length keys) 1)
                    (memq digit '(0 1 2 3 4 5 6 7 8 9)))
               (setq count (concat (or count "")
                                   (number-to-string digit)))
               ;; remove the digit from the key sequence
               ;; so we can see if the previous one goes anywhere
               (setq keys (vimpulse-truncate keys -1))
               (setq cmd (key-binding keys))
               t)
              ;; We might as well accept negative numbers using
              ;; Emacs' C--. Best of both worlds, right?
              ((eq cmd 'negative-argument)
               (unless count
                 (setq count "-")))
              ;; user pressed C-g, so return nil for CMD
              ((eq cmd 'keyboard-quit)
               (setq cmd nil))
              ;; we are done, exit the `while' loop
              (t
               nil))))
    ;; determine COUNT
    (when (stringp count)
      (if (string= count "-")
          (setq count nil)
        (setq count (string-to-number count))))
    ;; return command description
    (list cmd count)))

(defmacro vimpulse-with-operator-message (beg end template &rest body)
  "Echo an operator message after executing BODY.
BEG and END specify the text range acted upon.
TEMPLATE is a string like \"Deleted <N>\", where <N>
is substituted with the amount of characters or lines,
which is determined before executing BODY. The range type
is read from `vimpulse-this-motion-type'.

This macro respects `viper-change-notification-threshold'."
  (declare (indent defun)
           (debug (sexp sexp sexp body)))
  `(let* ((range (vimpulse-make-motion-range
                  ,beg ,end vimpulse-this-motion-type))
          (height (vimpulse-range-height range t))
          (width  (vimpulse-range-width range t))
          (template (replace-regexp-in-string
                     "<N>"
                     (apply 'format
                            (cond
                             ((eq vimpulse-this-motion-type 'block)
                              (list "%s row%s and %s column%s"
                                    height
                                    (if (/= (abs height) 1) "s" "")
                                    width
                                    (if (/= (abs width) 1) "s" "")))
                             ((eq vimpulse-this-motion-type 'line)
                              (list "%s line%s"
                                    height
                                    (if (/= (abs height) 1) "s" "")))
                             (t
                              (list "%s character%s"
                                    width
                                    (if (/= (abs width) 1) "s" "")))))
                     ,template)))
     (prog1 (progn ,@body)
       (when (and template
                  (not (viper-is-in-minibuffer))
                  (or (> (or height width)
                         viper-change-notification-threshold)
                      (> (or width height)
                         viper-change-notification-threshold)))
         (message "%s" template)))))

(when (fboundp 'font-lock-add-keywords)
  (font-lock-add-keywords
   'emacs-lisp-mode
   '(("(\\(vimpulse-with-operator-message\\)\\>" 1 font-lock-keyword-face))))

;; utility macro for converting region commands to operators
(defmacro vimpulse-convert-to-operator (region-cmd &rest args)
  "Convert a region command to an operator command.
Defines a new command with the name REGION-CMD-operator.
ARGS is passed to `vimpulse-range'."
  `(vimpulse-define-operator
     ,(intern (concat (symbol-name region-cmd) "-operator"))
     (beg end)
     ,(format "Operator-wrapper for `%s'.\n\n%s"
              region-cmd (documentation region-cmd t))
     (,region-cmd beg end)))

(defun vimpulse-operator-cmd-p (cmd)
  "Return non-nil if CMD is an operator command."
  (memq cmd vimpulse-operators))

;;; Repeat an operator/motion combination

;; this is used in `viper-d-com' (read by `viper-repeat')
(defun vimpulse-operator-repeat (arg)
  "Repeat an operator-motion combination.
ARG is a list of the form (COUNT . COM).
COM is discarded."
  (let ((val (viper-P-val arg)))
    (cond
     ((region-active-p)
      (funcall vimpulse-last-operator
               (region-beginning) (region-end)))
     (t
      (vimpulse-operator-apply
       vimpulse-last-operator vimpulse-last-motion val
       vimpulse-last-motion-type)))))

(defun vimpulse-operator-apply (operator motion count &optional type)
  "Apply OPERATOR on MOTION. COUNT is the motion count.
TYPE is the motion type."
  (let ((vimpulse-this-operator operator)
        (vimpulse-this-motion motion)
        (vimpulse-this-motion-type (or type vimpulse-this-motion-type))
        vimpulse-inhibit-operator beg end range)
    (setq range (vimpulse-calculate-motion-range count motion type)
          beg   (vimpulse-range-beginning range)
          end   (vimpulse-range-end range)
          vimpulse-this-motion-type (vimpulse-motion-type range))
    (unless vimpulse-inhibit-operator
      (funcall operator beg end))))

;;; Registers

(defun vimpulse-store-in-register (register start end)
  "Store text from START to END in REGISTER."
  (cond
   ((viper-valid-register register '(Letter))
    (viper-append-to-register
     (downcase register) start end))
   (t
    (copy-to-register register start end))))

(defun vimpulse-store-in-current-register (start end)
  "Store text from START to END in current register, if any.
  Resets `viper-use-register'."
  (when viper-use-register
    (vimpulse-store-in-register viper-use-register start end)
    (setq viper-use-register nil)))

;;; Operators

;; yank, delete, change
(vimpulse-define-operator vimpulse-yank (beg end)
  "Yank text from BEG to END."
  :repeat nil
  :move-point nil
  (let (last-command)
    (vimpulse-with-operator-message beg end "Saved <N>"
      (cond
       ((eq vimpulse-this-motion-type 'block)
        (setq killed-rectangle (extract-rectangle beg end))
        ;; associate the rectangle with the last entry in the kill-ring
        (unless kill-ring
          (copy-region-as-kill beg end))
        (put 'killed-rectangle 'previous-kill (current-kill 0))
        (vimpulse-visual-block-rotate 'upper-left beg end))
       (t
        (vimpulse-store-in-current-register beg end)
        (copy-region-as-kill beg end)
        (unless (eq vimpulse-this-motion-type 'line)
          (goto-char beg))
        (when (and (eolp) (not (bolp)))
          (backward-char)))))))

(vimpulse-define-operator vimpulse-delete (beg end &optional dont-save)
  "Delete text from BEG to END.
If DONT-SAVE is non-nil, don't store the deleted text on `kill-ring'."
  (let (last-command)
    (vimpulse-with-operator-message beg end "Deleted <N>"
      (cond
       (dont-save
        (if (eq vimpulse-this-motion-type 'block)
            (delete-rectangle beg end)
          (delete-region beg end)))
       ((eq vimpulse-this-motion-type 'block)
        (let ((orig (make-marker)))
          ;; associate the rectangle with the last entry in the kill-ring
          (viper-move-marker-locally
           'orig (vimpulse-visual-block-position 'upper-left beg end))
          (unless kill-ring
            (copy-region-as-kill beg end))
          (kill-rectangle beg end)
          (put 'killed-rectangle 'previous-kill (current-kill 0))
          (goto-char orig)
          (set-marker orig nil)))
       (t
        (vimpulse-store-in-current-register beg end)
        (kill-region beg end)
        (when (and (eolp) (not (bolp)))
          (backward-char)))))))

(vimpulse-define-operator vimpulse-change (beg end &optional dont-save)
  "Change text from BEG to END.
If DONT-SAVE is non-nil, don't store the deleted text on `kill-ring'."
  (when vimpulse-want-change-undo
    (vimpulse-start-undo-step))
  (cond
   ((eq vimpulse-this-motion-type 'block)
    (vimpulse-delete beg end dont-save)
    (goto-char
     (vimpulse-visual-create-coords
      'block ?i
      (min vimpulse-visual-point vimpulse-visual-mark)
      (1+ (max vimpulse-visual-point vimpulse-visual-mark))))
    (viper-insert nil))
   ((eq viper-intermediate-command 'viper-repeat)
    (if dont-save
        (delete-region beg end)
      (kill-region beg end))
    (goto-char beg)
    (when (eq vimpulse-this-motion-type 'line)
      (save-excursion (newline))
      (when viper-auto-indent
        (indent-according-to-mode)))
    (viper-yank-last-insertion))
   ((eq vimpulse-this-motion-type 'line)
    (setq viper-began-as-replace t)
    (if dont-save
        (delete-region beg end)
      (vimpulse-store-in-current-register beg end)
      (kill-region beg end))
    (save-excursion (newline))
    (when viper-auto-indent
      (indent-according-to-mode))
    (viper-change-state-to-insert))
   (t
    (if dont-save
        (progn
          (delete-region beg end)
          (viper-change-state-to-insert))
      (vimpulse-store-in-current-register beg end)
      (viper-change beg end)))))

;; r, J, =, >, <
(vimpulse-define-operator vimpulse-replace (beg end)
  "Replace all selected characters with ARG."
  :move-point nil
  :keep-visual t
  :motion 'forward-char
  (let (endpos length visual-p)
    (setq endpos (max beg (1- end)))
    (unless (and (eq viper-intermediate-command 'viper-repeat)
                 viper-d-char)
      (unwind-protect
          (progn
            (vimpulse-set-replace-cursor-type)
            (save-excursion
              (viper-special-read-and-insert-char))
            (setq viper-d-char (char-after))
            (delete-char 1))
        (viper-restore-cursor-type)
        (when vimpulse-visual-mode
          (vimpulse-visual-mode -1)
          (setq endpos beg))))
    (cond
     ((eq vimpulse-this-motion-type 'block)
      (setq length (abs (- (save-excursion
                             (goto-char beg)
                             (current-column))
                           (save-excursion
                             (goto-char end)
                             (current-column)))))
      (vimpulse-apply-on-block
       (lambda (beg end)
         (goto-char beg)
         (delete-region beg end)
         (insert (make-string length viper-d-char)))
       beg end))
     (t
      (goto-char beg)
      (while (< (point) end)
        (if (looking-at "\n")
            (forward-char)
          (delete-char 1)
          (insert-char viper-d-char 1)))
      (goto-char endpos)))))

(vimpulse-define-operator vimpulse-join (beg end)
  "Join the selected lines."
  :whole-lines t
  :motion 'vimpulse-line
  (let ((num (count-lines beg end)))
    (unless (> num 2)
      (setq num 2))
    (viper-join-lines num)))

(vimpulse-define-operator vimpulse-indent (beg end)
  "Indent text according to mode."
  :repeat nil
  :whole-lines t
  (indent-region beg end nil)
  (when viper-auto-indent
    (back-to-indentation)))

(vimpulse-define-operator vimpulse-shift-left (beg end)
  "Shift all selected lines to the left."
  (let ((nlines (count-lines beg end)))
    (viper-next-line (cons (1- nlines) ?<))))

(vimpulse-define-operator vimpulse-shift-right (beg end)
  "Shift all selected lines to the right."
  (let ((nlines (count-lines beg end)))
    (viper-next-line (cons (1- nlines) ?>))))

;; gq, gu, gU
(vimpulse-define-operator vimpulse-fill (beg end)
  "Fill text."
  :repeat nil
  :move-point nil
  (setq end (save-excursion
              (goto-char end)
              (skip-chars-backward " ")
              (point)))
  (save-excursion
    (fill-region beg end)))

(vimpulse-define-operator vimpulse-downcase (beg end)
  "Convert text to lower case."
  (if (eq vimpulse-this-motion-type 'block)
      (vimpulse-apply-on-block 'downcase-region beg end)
    (downcase-region beg end))
  (when (and viper-auto-indent
             (looking-back "^[ \f\t\v]*"))
    (back-to-indentation)))

(vimpulse-define-operator vimpulse-upcase (beg end)
  "Convert text to upper case."
  (if (eq vimpulse-this-motion-type 'block)
      (vimpulse-apply-on-block 'upcase-region beg end)
    (upcase-region beg end)
    (when (and viper-auto-indent
               (looking-back "^[ \f\t\v]*"))
      (back-to-indentation))))

(vimpulse-define-operator vimpulse-invert-case (beg end)
  "Convert text to inverted case."
  (let (char)
    (save-excursion
      (cond
       ((eq vimpulse-this-motion-type 'block)
        (let (vimpulse-this-motion-type)
          (vimpulse-apply-on-block 'vimpulse-invert-case beg end)))
       (t
        (goto-char beg)
        (while (< beg end)
          (setq char (following-char))
          (delete-char 1 nil)
          (if (eq (upcase char) char)
              (insert-char (downcase char) 1)
            (insert-char (upcase char) 1))
          (setq beg (1+ beg))))))
    (when (and viper-auto-indent
               (looking-back "^[ \f\t\v]*"))
      (back-to-indentation))))

(vimpulse-define-operator vimpulse-invert-char (beg end)
  "Invert case of character."
  :keep-visual t
  :motion 'forward-char
  (vimpulse-invert-case beg end)
  (cond
   (vimpulse-visual-mode
    (goto-char beg)
    (vimpulse-visual-mode -1))
   (t
    (goto-char end))))

(vimpulse-define-operator vimpulse-rot13 (beg end)
  "ROT13 encrypt text."
  (rot13-region beg end))

;;; Compatibility code allowing old-style Viper motions to work

;; Postpone operator execution by disabling `viper-execute-com'.
;; In the old scheme, the operator was executed inside the motion
;; (by a call to this function), rather than after it; the following
;; advice removes this behavior. However, some motions, like f and /,
;; need to access `viper-d-com' for negative count and command-keys
;; while repeating, so certain parts must be carefully retained.
(defadvice viper-execute-com (around vimpulse-operator activate)
  "Disable in Operator-Pending mode."
  (cond
   ((eq viper-current-state 'operator-state)
    ;; ?r is Viper's "dummy operator", associated with
    ;; `viper-exec-dummy' in `viper-exec-array'
    (setq com ?r)
    ad-do-it
    ;; while repeating, put needed values in `viper-d-com'
    (unless (or (eq this-command 'viper-repeat)
                (eq viper-intermediate-command 'viper-repeat))
      (unless viper-d-com
        (setq viper-d-com (list nil nil nil nil nil nil)))
      (unless (eq vimpulse-this-motion
                  (vimpulse-operator-remapping m-com))
        (setq vimpulse-this-motion (vimpulse-operator-remapping m-com))
        (setcar (nthcdr 2 viper-d-com) com))
      (setq vimpulse-this-count val)
      (setcar (nthcdr 5 viper-d-com)
              (viper-array-to-string
               (if (arrayp viper-this-command-keys)
                   viper-this-command-keys
                 (this-command-keys))))))
   (t
    ad-do-it)))

;; this separates the operator-pending part of a Viper motion from the
;; rest, defining a new command called vimpulse-operator-MOTION
(defmacro vimpulse-operator-map-define
  (viper-motion &optional type &rest body)
  "Define a new command for the Operator-Pending part of VIPER-MOTION.
The new command is named VIMPULSE-OPERATOR-MOTION and has motion
type TYPE. A custom function body may be specified via BODY."
  (declare (indent 2))
  `(let* ((viper-motion ',viper-motion)
          (type ,type)
          (body ',body)
          (motion-name (symbol-name viper-motion))
          (docstring (documentation viper-motion t)))
     (setq type (or type (vimpulse-motion-type viper-motion)))
     (unless (memq type '(inclusive line block))
       (setq type 'exclusive))
     (setq motion-name (replace-regexp-in-string
                        "^viper-\\\|^vimpulse-" "" motion-name))
     (setq motion-name
           (concat "vimpulse-operator-" motion-name))
     (setq motion-name (intern motion-name))
     (add-to-list 'vimpulse-movement-cmds motion-name)
     (vimpulse-operator-remap viper-motion motion-name)
     (eval `(defun ,motion-name (arg)
              ,(format "Operator-pending %s part of `%s'.\n\n%s"
                       type viper-motion (or docstring ""))
              ,@(if body body
                  `((interactive "P")
                    (let (com com-alist)
                      (setq com-alist
                            '((vimpulse-change . ?c)
                              (vimpulse-delete . ?d)
                              (vimpulse-yank . ?y)))
                      (setq com
                            (or (cdr (assq vimpulse-this-operator
                                           com-alist))
                                ?r))
                      (,viper-motion (if (region-active-p)
                                         arg
                                       (cons arg com)))
                      ,(unless (eq type 'exclusive)
                         '(viper-backward-char-carefully)))))))
     (put motion-name 'motion-type type)
     `(quote ,motion-name)))

;; d%: when point is before the parenthetical expression,
;; include it in the resulting range
(vimpulse-operator-map-define viper-paren-match 'inclusive
  (interactive "P")
  (let ((orig (point)))
    (viper-paren-match arg)
    (viper-move-marker-locally 'viper-com-point orig)
    (when (integerp arg)
      (setq vimpulse-this-motion-type 'line))))

;; Viper quirk: cw only deletes a single character when at whitespace,
;; dw deletes all of it. Use the latter behavior in both cases.
(vimpulse-operator-map-define viper-forward-word 'exclusive
  (interactive "P")
  (let ((com-alist '((vimpulse-change . ?c)
                     (vimpulse-delete . ?d)
                     (vimpulse-yank . ?y))) com)
    (if (looking-at "[[:space:]]")
        (setq com ?d)
      (setq com (or (cdr (assq vimpulse-this-operator com-alist)) ?r)))
    (viper-forward-word (if (region-active-p)
                            arg
                          (cons arg com)))))

(vimpulse-operator-map-define viper-forward-Word 'exclusive
  (interactive "P")
  (let ((com-alist '((vimpulse-change . ?c)
                     (vimpulse-delete . ?d)
                     (vimpulse-yank . ?y))) com)
    (if (looking-at "[[:space:]]")
        (setq com ?d)
      (setq com (or (cdr (assq vimpulse-this-operator com-alist)) ?r)))
    (viper-forward-Word (if (region-active-p)
                            arg
                          (cons arg com)))))

;;; remap non-motion commands to `viper-nil'
(vimpulse-operator-remap 'undo 'viper-nil)
(vimpulse-operator-remap 'undo-tree-redo 'viper-nil)
(vimpulse-operator-remap 'redo 'viper-nil)
(vimpulse-operator-remap 'vimpulse-put-and-indent 'viper-nil)
(vimpulse-operator-remap 'vimpulse-Put-and-indent 'viper-nil)
(vimpulse-operator-remap 'viper-Put-back 'viper-nil)
(vimpulse-operator-remap 'viper-put-back 'viper-nil)
(vimpulse-operator-remap 'viper-delete-backward-char 'viper-nil)
(vimpulse-operator-remap 'viper-delete-char 'viper-nil)
(vimpulse-operator-remap 'viper-insert 'viper-nil)
(vimpulse-operator-remap 'viper-intercept-ESC-key 'viper-nil)
(vimpulse-operator-remap 'viper-line-to-bottom 'viper-nil)
(vimpulse-operator-remap 'viper-line-to-middle 'viper-nil)
(vimpulse-operator-remap 'viper-line-to-top 'viper-nil)
(vimpulse-operator-remap 'viper-repeat 'viper-nil)
(vimpulse-operator-remap 'viper-substitute 'viper-nil)

;; these motions need wrapper functions to repeat correctly
(vimpulse-operator-map-define viper-end-of-Word 'inclusive)
(vimpulse-operator-map-define viper-end-of-word 'inclusive)
(vimpulse-operator-map-define viper-find-char-backward 'exclusive)
(vimpulse-operator-map-define viper-find-char-forward 'inclusive)
(vimpulse-operator-map-define viper-forward-char 'inclusive)
(vimpulse-operator-map-define viper-goto-char-backward 'exclusive)
(vimpulse-operator-map-define viper-goto-char-forward 'inclusive)
(vimpulse-operator-map-define viper-search-backward 'exclusive)
(vimpulse-operator-map-define viper-search-forward 'exclusive)

;; set up motion types for remaining Viper motions
(put 'vimpulse-goto-first-line 'motion-type 'line)
(put 'vimpulse-goto-mark-and-skip-white 'motion-type 'line)
(put 'vimpulse-end-of-visual-line 'motion-type 'inclusive)
(put 'viper-backward-Word 'motion-type 'exclusive)
(put 'viper-backward-char 'motion-type 'exclusive)
(put 'viper-backward-paragraph 'motion-type 'exclusive)
(put 'viper-backward-sentence 'motion-type 'exclusive)
(put 'viper-backward-word 'motion-type 'exclusive)
(put 'viper-beginning-of-line 'motion-type 'exclusive)
(put 'viper-forward-paragraph 'motion-type 'exclusive)
(put 'viper-forward-sentence 'motion-type 'exclusive)
(put 'viper-goto-eol 'motion-type 'inclusive)
(put 'viper-goto-line 'motion-type 'line)
(put 'viper-goto-mark 'motion-type 'exclusive)
(put 'viper-goto-mark-and-skip-white 'motion-type 'line)
(put 'viper-next-line 'motion-type 'line)
(put 'viper-previous-line 'motion-type 'line)
(put 'viper-search-Next 'motion-type 'exclusive)
(put 'viper-search-next 'motion-type 'exclusive)
(put 'viper-window-bottom 'motion-type 'line)
(put 'viper-window-middle 'motion-type 'line)
(put 'viper-window-top 'motion-type 'line)
(put 'next-line 'motion-type 'line)
(put 'previous-line 'motion-type 'line)

(provide 'vimpulse-operator)
