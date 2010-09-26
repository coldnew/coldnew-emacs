;;;; Visual mode

;; Visual mode is defined as another Viper state, just like vi state,
;; Insert state, Replace state etc. It inherits keybindings from
;; vi state (movement), but defines some bindings of its own
;; on top of that.
;;
;; Text selection in Emacs and Vim differs subtly by that in Vim, the
;; character under the cursor is always included in the selection,
;; while Emacs' region excludes it when point follows mark. Vimpulse
;; solves the problem by "translating" a Visual selection to the
;; equivalent Emacs region when a command is about to be executed.
;; Likewise, a Line selection is translated to an Emacs region of
;; whole lines.
;;
;; This is pretty transparent, except that we don't wish to do any
;; translating when the user is just moving around in the buffer.
;; To that end, the variable `vimpulse-movement-cmds' lists all of
;; Viper's movement commands, so that translation can be postponed
;; until the user executes a non-movement command.
;;
;; Block selections are rectangle compatible. This means Emacs'
;; rectangular commands are applicable on the selection, and you can
;; write your own utilities using the rect.el library. Alternatively,
;; use the `vimpulse-apply-on-block' function.

(eval-when-compile (require 'vimpulse-viper-function-redefinitions)) ; vimpulse-define-state
(eval-when-compile (require 'vimpulse-utils)) ; vimpulse-remap

(declare-function vimpulse-delete "vimpulse-operator" (beg end &optional dont-save))
(declare-function vimpulse-mark-range "vimpulse-text-object-system" (range &optional widen type))
(declare-function vimpulse-operator-cmd-p "vimpulse-operator" (cmd))

(defgroup vimpulse-visual nil
  "Visual mode for Viper."
  :prefix "vimpulse-visual-"
  :group  'vimpulse)

(defvar vimpulse-visual-remap-alist nil
  "Association list of command remappings in Visual mode.")

(viper-deflocalvar vimpulse-visual-global-vars nil
  "List of variables that were global.") ; FIXME when? what for?

(viper-deflocalvar vimpulse-visual-local-vars
  '(cua-mode
    mark-active
    transient-mark-mode
    zmacs-regions)
  "System variables that are reset for each Visual session.")

(viper-deflocalvar vimpulse-visual-vars-alist nil
  "Alist of old variable values.")

(viper-deflocalvar vimpulse-visual-last nil
  "Last active Visual mode.
May be `char', `line', `block' or nil.")

(viper-deflocalvar vimpulse-visual-previous-state 'viper-state
  "Previous state before enabling Visual mode.
This lets us revert to Emacs state in non-vi buffers.")

(viper-deflocalvar vimpulse-visual-region-expanded nil
  "Whether region is expanded to the Visual selection.")

(viper-deflocalvar vimpulse-visual-point nil
  "Last expanded `point' in Visual mode.")

(viper-deflocalvar vimpulse-visual-mark nil
  "Last expanded `mark' in Visual mode.")

(viper-deflocalvar vimpulse-visual-overlay nil
  "Overlay for Visual selection.
In XEmacs, this is an extent.")

(viper-deflocalvar vimpulse-visual-block-overlays nil
  "Overlays for Visual Block selection.")

(viper-deflocalvar vimpulse-visual-whitespace-overlay nil
  "Overlay encompassing text inserted into the buffer
to make Block selection at least one column wide.")

(defvar vimpulse-visual-height nil
  "Height of last Visual selection.")

(defvar vimpulse-visual-width nil
  "Width of last Visual selection.")

(defvar vimpulse-visual-insert-coords nil
  "List of the form (I-COM UL-POS COL NLINES), where
I-COM is the insert command (?i, ?a, ?I or ?A),
UL-POS is the position of the upper left corner of the region,
COL is the column of insertion, and
NLINES is the number of lines in the region.")

;;; Key bindings

(defvar vimpulse-visual-basic-map
  (let ((map (make-sparse-keymap)))
    (define-key map "v" 'vimpulse-visual-toggle-char)
    (define-key map "V" 'vimpulse-visual-toggle-line)
    (define-key map "\C-v" 'vimpulse-visual-toggle-block)
    (define-key map "x" 'vimpulse-delete)
    (define-key map "D" 'vimpulse-delete)
    (define-key map "Y" 'vimpulse-yank)
    (define-key map "R" 'vimpulse-change)
    (define-key map "C" 'vimpulse-change)
    (define-key map "s" 'vimpulse-change)
    (define-key map "S" 'vimpulse-change)
    (define-key map "o" 'exchange-point-and-mark)
    (define-key map "O" 'vimpulse-visual-exchange-corners)
    (define-key map "I" 'vimpulse-visual-insert)
    (define-key map "A" 'vimpulse-visual-append)
    (define-key map "U" 'vimpulse-upcase)
    (define-key map "u" 'vimpulse-downcase)
    (define-key map ":" 'vimpulse-visual-ex)
    map)
  "Vimpulse Visual mode keymap.")

(put 'vimpulse-visual-basic-map
     'remap-alist 'vimpulse-visual-remap-alist)

;; FIXME single use below
(defun vimpulse-visual-remap (from to)
  "Remap FROM to TO in Visual mode."
  (vimpulse-remap vimpulse-visual-basic-map from to))

;; Keys that have no effect in Visual mode.
(vimpulse-visual-remap 'viper-repeat 'viper-nil)

(viper-deflocalvar vimpulse-visual-mode nil
  "Current Visual mode: may be nil, `char', `line' or `block'.")

;; Visual mode comprises three "submodes": characterwise, linewise
;; and blockwise selection. We implement this by setting the mode
;; variable `vimpulse-visual-mode' to either `char', `line'
;; or `block'.
(define-minor-mode vimpulse-visual-mode
  "Toggles Visual mode in Viper."
  :initial-value nil
  :keymap vimpulse-visual-basic-map
  :group 'vimpulse-visual
  (cond
   (vimpulse-visual-mode
    (unless (memq vimpulse-visual-mode '(char line block))
      (vimpulse-visual-activate 'char)))
   (t
    ;; This is executed when we do (vimpulse-visual-mode -1).
    ;; It must run without error even if Visual mode is not active.
    (vimpulse-visual-highlight -1)
    ;; clean up local variables
    (dolist (var vimpulse-visual-local-vars)
      (when (assq var vimpulse-visual-vars-alist)
        (set var (cdr (assq var vimpulse-visual-vars-alist))))
      (when (memq var vimpulse-visual-global-vars)
        (kill-local-variable var)))
    (setq vimpulse-visual-region-expanded nil)
    ;; deactivate mark
    (when vimpulse-visual-vars-alist
      (vimpulse-deactivate-mark t))
    (vimpulse-transient-restore)
    (kill-local-variable 'vimpulse-visual-vars-alist)
    (kill-local-variable 'vimpulse-visual-global-vars)
    ;; if Viper state is not already changed,
    ;; change it to vi (command) state
    (when (eq viper-current-state 'visual-state)
      (cond
       ((eq vimpulse-visual-previous-state 'emacs-state)
        (viper-change-state-to-emacs))
       (t
        (let (abbrev-mode)
          (save-excursion
            (viper-change-state-to-vi))))))
    (kill-local-variable 'vimpulse-visual-previous-state))))

(vimpulse-define-state visual
  "Visual mode is a flexible and easy way to select text.
To use Visual mode, press v in vi (command) mode. Then use the
motion commands to expand the selection. Press d to delete, c to
change, r to replace, or y to copy. You can use p to paste.
For Line selection, press V instead of v; then you can copy and
paste whole lines. For Block selection, press C-v; now you can
copy and paste the selected rectangle. In Block selection, you
may use I or A to insert or append text before or after the
selection on each line."
  :id "<VIS> "
  :basic-minor-mode 'vimpulse-visual-mode
  :enable '((vimpulse-visual-mode (or vimpulse-visual-mode t))
            (vimpulse-operator-remap-minor-mode nil)
            operator-state
            vi-state)
  (cond
   ((eq new-state 'visual-state)
    (unless (memq vimpulse-visual-mode '(char line block))
      (vimpulse-visual-mode 1)))
   (t
    (vimpulse-visual-mode -1))))

;;; Activation

(eval-and-compile
  (defalias 'viper-deactivate-mark 'vimpulse-deactivate-mark)
  (defalias 'vimpulse-activate-mark 'vimpulse-activate-region))

(defun vimpulse-visual-activate (&optional mode)
  "Activate Visual mode. MODE is `char', `line' or `block'.
May also be used to change the Visual mode."
  (unless (memq vimpulse-visual-mode '(char line block))
    ;; we are activating Visual mode for the first time
    (kill-local-variable 'vimpulse-visual-vars-alist)
    (kill-local-variable 'vimpulse-visual-global-vars)
    (setq vimpulse-visual-previous-state viper-current-state)
    ;; make global variables buffer-local
    (setq vimpulse-visual-vars-alist nil)
    (vimpulse-visual-block-cleanup-whitespace)
    (vimpulse-transient-remember)
    (dolist (var vimpulse-visual-local-vars)
      (when (and (boundp var)
                 (not (assq var vimpulse-visual-vars-alist)))
        ;; remember old value
        (add-to-list 'vimpulse-visual-vars-alist
                     (cons var (eval var))))
      (unless (assoc var (buffer-local-variables))
        (make-local-variable var)
        (add-to-list 'vimpulse-visual-global-vars var)))
    (setq vimpulse-visual-region-expanded nil)
    ;; re-add hooks in case they were cleared
    (add-hook 'pre-command-hook 'vimpulse-visual-pre-command)
    (add-hook 'post-command-hook 'vimpulse-visual-post-command)
    (if (featurep 'xemacs)
        (add-hook 'zmacs-deactivate-region-hook
                  'vimpulse-visual-deactivate-hook)
      (add-hook 'deactivate-mark-hook 'vimpulse-visual-deactivate-hook))
    ;; activate mark at point
    (cond
     ((eq mode 'block)
      (set-mark (point))
      (vimpulse-deactivate-mark t)     ; `set-mark' activates the mark
      (vimpulse-transient-mark -1))
     (t
      (vimpulse-transient-mark 1)
      ;; convert active Emacs region to Visual selection, if any
      (cond
       ((region-active-p)
        (vimpulse-visual-contract-region
         (not viper-ESC-moves-cursor-back)))
       (t
        (vimpulse-activate-mark (point))))
      (vimpulse-visual-highlight))))
  ;; set the Visual mode
  (setq mode (or mode 'char))
  (setq vimpulse-visual-mode mode
        vimpulse-visual-last mode)
  (viper-change-state 'visual-state)
  (viper-restore-cursor-type)           ; use vi cursor
  ;; reactivate mark
  (cond
   ((eq mode 'block)
    (vimpulse-deactivate-mark t)
    (vimpulse-transient-mark -1))
   (t
    (vimpulse-transient-mark 1)
    (vimpulse-activate-mark))))

(defun vimpulse-visual-toggle (mode)
  "Enable Visual MODE if this is not the current mode.
Otherwise disable Visual mode."
  (if (eq mode vimpulse-visual-mode)
      (vimpulse-visual-mode -1)
    (vimpulse-visual-activate mode)))

(defun vimpulse-visual-activate-char ()
  "Enable Visual Character selection."
  (interactive)
  (let (message-log-max)
    (vimpulse-visual-activate 'char)
    (message "-- VISUAL --")))

(defun vimpulse-visual-activate-line ()
  "Enable Visual Line selection."
  (interactive)
  (let (message-log-max)
    (vimpulse-visual-activate 'line)
    (message "-- VISUAL LINE --")))

(defun vimpulse-visual-activate-block ()
  "Enable Visual Block selection."
  (interactive)
  (let (message-log-max)
    (vimpulse-visual-activate 'block)
    (message "-- VISUAL BLOCK --")))

(defun vimpulse-visual-toggle-char ()
  "Toggle Visual Character selection."
  (interactive)
  (let (message-log-max)
    (vimpulse-visual-toggle 'char)
    (when vimpulse-visual-mode
      (message "-- VISUAL --"))))

(defun vimpulse-visual-toggle-line ()
  "Toggle Visual Line selection."
  (interactive)
  (let (message-log-max)
    (vimpulse-visual-toggle 'line)
    (when vimpulse-visual-mode
      (message "-- VISUAL LINE --"))))

(defun vimpulse-visual-toggle-block ()
  "Toggle Visual Block selection."
  (interactive)
  (let (message-log-max)
    (vimpulse-visual-toggle 'block)
    (when vimpulse-visual-mode
      (message "-- VISUAL BLOCK --"))))

;;; UUOE contest
;; Should be replaced with something more readable,
;; like (vimpulse-visual-historical-value 'transient-mark-mode).
(defmacro vimpulse-visual-before (&rest body)
  "Evaluate BODY with original system values from before Visual mode.
This is based on `vimpulse-visual-vars-alist'."
  ;; This needs to be expanded at runtime, obviously.
  `(eval `(let ,(mapcar (lambda (elt)
                          `(,(car elt) (quote ,(cdr elt))))
                        vimpulse-visual-vars-alist)
            ,',@body)))

;;; Visualization

(defun vimpulse-deactivate-mark (&optional now)
  "Don't deactivate mark in Visual mode."
  (cond
   ((and vimpulse-visual-mode
         (not (eq vimpulse-visual-mode 'block)))
    nil)
   (t
    (vimpulse-deactivate-region now))))

(defun vimpulse-transient-mark (&optional arg)
  "Enable Transient Mark mode (and Cua mode) if not already enabled.
Enable forcefully with positive ARG. Disable with negative ARG.
Saves the previous state of Transient Mark mode in
`vimpulse-visual-vars-alist', so it can be restored with
`vimpulse-transient-restore'."
  (setq deactivate-mark nil)
  (and (boundp 'mark-active)
       (setq mark-active (region-active-p)))
  (let (deactivate-mark)
    (cond
     ;; disable Transient Mark/Cua
     ((and (integerp arg) (< arg 1))
      (and (fboundp 'cua-mode)
           cua-mode
           (cua-mode -1))
      (and (fboundp 'transient-mark-mode)
           transient-mark-mode
           (transient-mark-mode -1))
      (and (boundp 'zmacs-regions)
           (setq zmacs-regions nil)))
     ;; enable Transient Mark/Cua
     (t
      (vimpulse-transient-remember)
      (cond
       ((and (fboundp 'cua-mode)
             (vimpulse-visual-before (eq cua-mode t))
             (or (not cua-mode) (numberp arg)))
        (cua-mode 1))
       ((and (fboundp 'transient-mark-mode)
             (or (not transient-mark-mode) (numberp arg)))
        (transient-mark-mode 1))
       ((and (boundp 'zmacs-regions)
             (or (not zmacs-regions) (numberp arg)))
        (setq zmacs-regions t)))))))

(defun vimpulse-transient-remember ()
  "Remember Transient Mark mode state in `vimpulse-visual-vars-alist'."
  (when (and (boundp 'transient-mark-mode)
             (not (assq 'transient-mark-mode
                        vimpulse-visual-vars-alist)))
    (add-to-list 'vimpulse-visual-vars-alist
                 (cons 'transient-mark-mode
                       (when (eq transient-mark-mode t)
                         transient-mark-mode))))
  (when (and (boundp 'cua-mode)
             (not (assq 'cua-mode vimpulse-visual-vars-alist)))
    (add-to-list 'vimpulse-visual-vars-alist
                 (cons 'cua-mode cua-mode))))

(defun vimpulse-transient-restore ()
  "Restore Transient Mark mode to what is was before Visual mode.
 Also restores Cua mode."
  (when vimpulse-visual-vars-alist
    (when (boundp 'transient-mark-mode)
      (if (vimpulse-visual-before transient-mark-mode)
          (transient-mark-mode 1)
        (transient-mark-mode -1)))
    (when (boundp 'cua-mode)
      ;; prevent Cua mode from setting `deactivate-mark' to t
      (let (deactivate-mark)
        (if (vimpulse-visual-before cua-mode)
            (cua-mode 1)
          (cua-mode -1))))
    (when (boundp 'zmacs-regions)
      (let ((oldval (vimpulse-visual-before zmacs-regions)))
        (setq zmacs-regions oldval)))))

(defun vimpulse-visual-beginning (&optional mode force)
  "Return beginning of Visual selection.
See `vimpulse-visual-range'."
  (vimpulse-range-beginning (vimpulse-visual-range mode force)))

(defun vimpulse-visual-end (&optional mode force)
  "Return end of Visual selection.
See `vimpulse-visual-range'."
  (vimpulse-range-end (vimpulse-visual-range mode force)))

(defun vimpulse-visual-range (&optional mode force)
  "Return a Visual motion range (TYPE BEG END).
TYPE is the Visual mode.

The range depends on `point', `mark' and `vimpulse-visual-mode'.
The Visual mode may be specified explicitly with MODE, which must
be one of `char', `line' and `block'.

In Character mode, returns region plus one character.
In Line mode, returns region as whole lines.
In Block mode, returns rectangle plus one column.

If the Visual selection is already translated to Emacs' region,
returns the region as-is. This can be overridden with FORCE.

See also `vimpulse-visual-beginning' and `vimpulse-visual-end'."
  (let ((mark  (or (mark t) 1))
        (point (point)))
    (setq mode (or mode vimpulse-visual-mode))
    (unless (memq mode '(line block))
      (setq mode (if vimpulse-visual-mode 'inclusive 'exclusive)))
    (cond
     ((and (not force)
           (or (not vimpulse-visual-mode)
               vimpulse-visual-region-expanded))
      (vimpulse-make-motion-range mark point mode))
     ((eq mode 'block)
      (vimpulse-block-range mark point))
     ((eq mode 'line)
      (vimpulse-line-range mark point))
     (t
      (vimpulse-inclusive-range mark point)))))

(defun vimpulse-visual-select (beg end &optional widen)
  "Visually select text inclusively from BEG to END.
Return nil if selection is unchanged. If WIDEN is non-nil, only
modify selection if it does not already encompass BEG and END.

Under the hood, this function changes Emacs' `point' and `mark'.
The boundaries of the Visual selection are deduced from these and
the current Visual mode via `vimpulse-visual-beginning' and
`vimpulse-visual-end'."
  (cond
   ;; in Visual mode, protect the value of `mark-active'
   (vimpulse-visual-mode
    (let (mark-active)
      (vimpulse-set-region
       (min beg end)
       (if vimpulse-visual-region-expanded
           (max beg end)
         (max (min beg end) (1- (max beg end))))
       widen)))
   (t
    (vimpulse-set-region
     (min beg end) (max beg end) widen))))

;;; Functions for Visual selection <=> Emacs region transformation

;; In Vim, Visual-mode selection always includes the character position under
;; the cursor (i.e., "at point" or "following point" in Emacs-speak), so the
;; former is invariably larger than the latter -- thus "expand" and "contract".
(defun vimpulse-visual-expand-region (&optional mode no-trailing-newline)
  "Transform the current Emacs region to the equivalent Visual selection.
If NO-TRAILING-NEWLINE is t and the selection ends with a newline,
exclude that newline from the region.
Cf. `vimpulse-visual-contract-region' for the reverse operation."
  (let* ((range (vimpulse-visual-range mode))
         (type  (vimpulse-motion-type range))
         (beg   (vimpulse-range-beginning range))
         (end   (vimpulse-range-end range))
         mark-active)
    (when no-trailing-newline
      (save-excursion
        (goto-char end)
        (when (and (bolp) (not (bobp)))
          (setq range (vimpulse-make-motion-range
                       beg (max beg (1- (point))) type)))))
    (setq vimpulse-visual-region-expanded t)
    (vimpulse-mark-range range)))

(defun vimpulse-visual-contract-region (&optional keep-point)
  "Transform the current Visual selection to the equivalent Emacs region.
If KEEP-POINT is t, do not move point (transformation may be incomplete
if mark < point).
Return nil if selection is unchanged.
Cf. `vimpulse-visual-expand-region' for the reverse operation."
  (let ((opoint (point)) (omark (mark t)))
    (setq vimpulse-visual-region-expanded nil)
    (vimpulse-visual-select (region-beginning) (region-end))
    ;; KEEP-POINT?
    (when keep-point
      (goto-char opoint))
    ;; Was selection changed?
    (not (and (= (point)  opoint)
              (= (mark t) omark)))))

;; While there is a one-to-one relationship between Vim-like, "inclusive"
;; selections and Emacs-like, "exclusive" regions, line selection is a
;; one-way operation -- multiple selections can produce the same number
;; of lines. Line "contraction" is therefore based on memory.
(defun vimpulse-visual-restore ()
  "Restore previous selection.
This selects a specific range of text in the buffer.
See also `vimpulse-visual-reselect'."
  (interactive)
  (setq vimpulse-visual-region-expanded nil)
  (let ((last vimpulse-visual-last))
    (cond
     ;; if no previous selection, try a quick C-x C-x
     ((or (not vimpulse-visual-point)
          (not vimpulse-visual-mark))
      (vimpulse-activate-mark nil)
      (vimpulse-visual-mode 1))
     (t
      (unless vimpulse-visual-mode
        ;; protect the previous values of `vimpulse-visual-mark'
        ;; and `vimpulse-visual-point'
        (let (vimpulse-visual-mark vimpulse-visual-point)
          (cond
           ((eq last 'line)
            (vimpulse-visual-activate-line))
           ((eq last 'block)
            (vimpulse-visual-activate-block))
           (t                           ; char
            (vimpulse-visual-activate-char)))))
      (set-mark vimpulse-visual-mark)
      (goto-char vimpulse-visual-point)
      (unless (save-excursion
                (goto-char (max vimpulse-visual-mark
                                vimpulse-visual-point))
                (bolp))
        (vimpulse-visual-contract-region))
      (vimpulse-visual-highlight)))))

(defun vimpulse-visual-reselect (&optional mode height width pos)
  "Create a Visual MODE selection of dimensions HEIGHT and WIDTH.
When called interactively, uses dimensions of previous selection.
If POS is specified, selects about POS; otherwise about point.
See also `vimpulse-visual-restore'."
  (interactive)
  (when pos
    (goto-char pos))
  (setq mode (or mode vimpulse-visual-mode vimpulse-visual-last)
        height (or height vimpulse-visual-height 1)
        width (or width vimpulse-visual-width 1))
  (unless vimpulse-visual-mode
    (vimpulse-visual-activate mode))
  (cond
   ((eq mode 'block)
    (viper-next-line-carefully (1- height))
    (setq width (+ (1- width) (current-column)))
    (vimpulse-move-to-column width)
    (setq height (count-lines (vimpulse-visual-beginning mode)
                              (vimpulse-visual-end mode)))
    (while (and (not (eq (current-column) width))
                (> height 1))
      (viper-next-line-carefully -1)
      (setq height (1- height))
      (move-to-column width)))
   ((eq mode 'line)
    (viper-next-line-carefully (1- height)))
   (t                                   ; char
    (viper-forward-char-carefully (1- width)))))

(defun vimpulse-set-visual-markers (&optional point mark)
  "Refresh `vimpulse-visual-point' and `vimpulse-visual-mark'."
  (setq mark  (vimpulse-visual-beginning 'char)
        point (vimpulse-visual-end 'char))
  (when (< (point) (mark t))
    (setq mark (prog1 point
                 (setq point mark))))
  (viper-move-marker-locally 'vimpulse-visual-point point)
  (viper-move-marker-locally 'vimpulse-visual-mark  mark)
  (set-marker-insertion-type vimpulse-visual-point
                             (<= point mark))
  (set-marker-insertion-type vimpulse-visual-mark
                             (> point mark)))

(defun vimpulse-set-visual-dimensions (&optional beg end mode)
  "Refresh `vimpulse-visual-height' and `vimpulse-visual-width'."
  (vimpulse-set-visual-markers beg end)
  (let* ((range (vimpulse-visual-range mode))
         (beg   (or beg (vimpulse-range-beginning range)))
         (end   (or end (vimpulse-range-end range)))
         (type  (vimpulse-motion-type range))
         (range (vimpulse-make-motion-range beg end type)))
    (setq vimpulse-visual-height (vimpulse-range-height range t)
          vimpulse-visual-width  (vimpulse-range-width range t))))

(defun vimpulse-visual-highlight (&optional arg)
  "Highlight Visual selection, depending on region and Visual mode.
With negative ARG, removes highlighting."
  (cond
   ((and (numberp arg) (< arg 1))
    (when (viper-overlay-live-p vimpulse-visual-overlay)
      (vimpulse-delete-overlay vimpulse-visual-overlay))
    (mapc 'vimpulse-delete-overlay vimpulse-visual-block-overlays)
    (setq vimpulse-visual-block-overlays nil)
    ;; clean up unreferenced overlays
    (dolist (overlay (vimpulse-overlays-at (point)))
      (when (eq (viper-overlay-get overlay 'face) (vimpulse-region-face))
        (vimpulse-delete-overlay overlay))))
   ((eq vimpulse-visual-mode 'block)
    ;; remove any char/line highlighting
    (when (viper-overlay-live-p vimpulse-visual-overlay)
      (vimpulse-delete-overlay vimpulse-visual-overlay))
    ;; block highlighting isn't perfect
    (condition-case nil
        (vimpulse-visual-highlight-block
         (vimpulse-visual-beginning)
         (vimpulse-visual-end))
      (error nil)))
   (vimpulse-visual-mode                ; char or line
    (let ((beg (vimpulse-visual-beginning))
          (end (vimpulse-visual-end)))
      ;; remove any block highlighting
      (mapc 'vimpulse-delete-overlay vimpulse-visual-block-overlays)
      (setq vimpulse-visual-block-overlays nil)
      ;; reuse overlay if possible
      (if (viper-overlay-live-p vimpulse-visual-overlay)
          (viper-move-overlay vimpulse-visual-overlay beg end)
        (setq vimpulse-visual-overlay
              (vimpulse-make-overlay beg end nil t))
        (viper-overlay-put vimpulse-visual-overlay
                           'face (vimpulse-region-face))
        (viper-overlay-put vimpulse-visual-overlay
                           'priority 99))))))

(defun vimpulse-visual-highlight-block (beg end)
  "Highlight rectangular region from BEG to END.
We do this by putting an overlay on each line within the
rectangle. Each overlay extends across all the columns of the
rectangle. We try to reuse overlays where possible because this
is more efficient and results in less flicker.

Adapted from: `rm-highlight-rectangle' in rect-mark.el."
  (let ((opoint (point))                ; remember point
        (omark  (mark t))               ; remember mark
        (old vimpulse-visual-block-overlays)
        beg-col end-col new nlines overlay window-beg window-end)
    ;; Calculate the rectangular region represented by BEG and END,
    ;; but put BEG in the north-west corner and END in the south-east
    ;; corner if not already there.
    (save-excursion
      (setq beg-col (save-excursion (goto-char beg)
                                    (current-column))
            end-col (save-excursion (goto-char end)
                                    (current-column)))
      (when (>= beg-col end-col)
        (if (= beg-col end-col)
            (setq end-col (1+ end-col))
          (setq beg-col (prog1 end-col
                          (setq end-col beg-col))))
        (setq beg (save-excursion (goto-char beg)
                                  (vimpulse-move-to-column beg-col)
                                  (point))
              end (save-excursion (goto-char end)
                                  (vimpulse-move-to-column end-col 1)
                                  (point))))
      ;; force a redisplay so we can do reliable
      ;; windows BEG/END calculations
      (sit-for 0)
      (setq window-beg (max (window-start) beg)
            window-end (min (window-end) (1+ end))
            nlines (count-lines window-beg
                                (min window-end (point-max))))
      ;; iterate over those lines of the rectangle which are
      ;; visible in the currently selected window
      (goto-char window-beg)
      (dotimes (i nlines)
        (let (row-beg row-end bstring astring)
          ;; beginning of row
          (vimpulse-move-to-column beg-col)
          (when (< (current-column) beg-col)
            ;; prepend overlay with virtual spaces if we are unable to
            ;; move directly to the first column
            (setq bstring
                  (propertize
                   (make-string
                    (- beg-col (current-column)) ?\ )
                   'face
                   (or (get-text-property (1- (point)) 'face)
                       'default))))
          (setq row-beg (point))
          ;; end of row
          (vimpulse-move-to-column end-col)
          (when (< (current-column) end-col)
            ;; append overlay with virtual spaces if we are unable to
            ;; move directly to the last column
            (setq astring
                  (propertize
                   (make-string
                    (if (= (point) row-beg)
                        (- end-col beg-col)
                      (- end-col (current-column)))
                    ?\ ) 'face (vimpulse-region-face)))
            ;; place cursor on one of the virtual spaces
            ;; (only works in GNU Emacs)
            (if (= opoint row-beg)
                (put-text-property
                 0 (min (length astring) 1)
                 'cursor t astring)
              (put-text-property
               (max 0 (1- (length astring))) (length astring)
               'cursor t astring)))
          (setq row-end (min (point) (line-end-position)))
          ;; XEmacs bug: zero-length extents display
          ;; end-glyph before start-glyph
          (and (featurep 'xemacs)
               bstring astring
               (= row-beg row-end)
               (setq bstring (prog1 astring
                               (setq astring bstring))))
          ;; trim old leading overlays
          (while (and old
                      (setq overlay (car old))
                      (< (viper-overlay-start overlay) row-beg)
                      (/= (viper-overlay-end overlay) row-end))
            (vimpulse-delete-overlay overlay)
            (setq old (cdr old)))
          ;; reuse an overlay if possible, otherwise create one
          (cond
           ((and old (setq overlay (car old))
                 (or (= (viper-overlay-start overlay) row-beg)
                     (= (viper-overlay-end overlay) row-end)))
            (viper-move-overlay overlay row-beg row-end)
            (vimpulse-overlay-before-string overlay bstring)
            (vimpulse-overlay-after-string overlay astring)
            (setq new (cons overlay new)
                  old (cdr old)))
           (t
            (setq overlay (vimpulse-make-overlay row-beg row-end))
            (vimpulse-overlay-before-string overlay bstring)
            (vimpulse-overlay-after-string overlay astring)
            (viper-overlay-put overlay 'face (vimpulse-region-face))
            (viper-overlay-put overlay 'priority 99)
            (setq new (cons overlay new)))))
        (forward-line 1))
      ;; trim old trailing overlays
      (mapc 'vimpulse-delete-overlay old)
      (setq vimpulse-visual-block-overlays (nreverse new)))))

(defun vimpulse-visual-pre-command ()
  "Run before each command in Visual mode."
  (when vimpulse-visual-mode
    ;; refresh Visual restore markers and marks
    (vimpulse-set-visual-dimensions)
    (cond
     ;; movement command: don't expand region
     ((vimpulse-movement-cmd-p this-command)
      (setq vimpulse-visual-region-expanded nil))
     (t
      ;; add whitespace if necessary for making a rectangle
      (and (eq vimpulse-visual-mode 'block)
           (vimpulse-visual-block-add-whitespace))
      (vimpulse-visual-expand-region
       ;; if in Line mode, don't include trailing newline
       ;; unless the command has real need of it
       nil (and (eq vimpulse-visual-mode 'line)
                (not (vimpulse-needs-newline-p this-command))))))))

(defun vimpulse-visual-post-command ()
  "Run after each command in Visual mode."
  (cond
   (vimpulse-visual-mode
    ;; quitting: exit to vi (command) mode
    (cond
     (quit-flag                         ; C-g
      (vimpulse-visual-mode -1))
     ((eq this-command 'keyboard-quit)
      (vimpulse-visual-mode -1))
     ((and (not (region-active-p))
           (not (eq vimpulse-visual-mode 'block)))
      (vimpulse-visual-mode -1))
     ;; region was expanded, so contract it
     (vimpulse-visual-region-expanded
      (when (eq vimpulse-visual-mode 'block)
        (vimpulse-visual-block-cleanup-whitespace))
      (if (eq vimpulse-visual-mode 'line)
          (vimpulse-visual-restore)
        (vimpulse-visual-contract-region))
      (vimpulse-visual-highlight))
     (t
      (vimpulse-visual-highlight))))
   ;; Not in the Visual state, but maybe the mark
   ;; was activated in vi (command) state?
   ((and (region-active-p)
         (eq viper-current-state 'vi-state)
         (if (boundp 'deactivate-mark) (not deactivate-mark) t))
    (vimpulse-visual-mode 1))))

(defun vimpulse-visual-deactivate-hook ()
  "Hook run when mark is deactivated in Visual mode."
  (when vimpulse-visual-mode
    (and (not (region-active-p))
         (not (vimpulse-movement-cmd-p this-command))
         (vimpulse-visual-mode -1))))

(add-hook 'pre-command-hook 'vimpulse-visual-pre-command)
(add-hook 'post-command-hook 'vimpulse-visual-post-command)
(if (featurep 'xemacs)
    (add-hook 'zmacs-deactivate-region-hook
              'vimpulse-visual-deactivate-hook)
  (add-hook 'deactivate-mark-hook 'vimpulse-visual-deactivate-hook))

;; advise viper-intercept-ESC-key to exit Visual mode with ESC
(defadvice viper-intercept-ESC-key
  (around vimpulse-ESC-exit-visual-mode activate)
  "Exit Visual mode with ESC."
  (let ((viper-ESC-moves-cursor-back (unless (region-active-p)
                                       viper-ESC-moves-cursor-back))
        deactivate-mark)
    (if (and vimpulse-visual-mode
             (not (input-pending-p)))
        (vimpulse-visual-mode -1)
      ad-do-it)))

(defadvice viper-Put-back (around vimpulse-visual activate)
  "Delete selection before pasting in Visual mode."
  (let (inserted-text replaced-text mode)
    (setq yank-window-start (window-start))
    (cond
     (vimpulse-visual-mode
      (setq mode vimpulse-visual-mode)
      (unless (eq mode 'block)
        ;; add replaced text to the kill-ring before the current kill
        (setq inserted-text (current-kill 0))
        (setq replaced-text
              (buffer-substring (region-beginning) (region-end)))
        (kill-new replaced-text t)
        (kill-new inserted-text))
      (vimpulse-delete (region-beginning) (region-end) t)
      (when (and (eq mode 'char)
                 (not (bolp))
                 (viper-end-with-a-newline-p inserted-text))
        (newline))
      (when (and (eq mode 'line)
                 (not (viper-end-with-a-newline-p inserted-text)))
        (save-excursion (newline))))
     ((region-active-p)
      (delete-region (region-beginning) (region-end))))
    (if (and killed-rectangle
             kill-ring
             (eq (get 'killed-rectangle 'previous-kill)
                 (current-kill 0)))
        (save-excursion
          (yank-rectangle))
      ad-do-it)
    (when vimpulse-visual-mode
      (vimpulse-visual-mode -1))))

(defadvice viper-put-back (around vimpulse-visual activate)
  "Delete selection before pasting in Visual mode."
  (setq yank-window-start (window-start))
  (cond
   (vimpulse-visual-mode
    (viper-Put-back arg))
   ((region-active-p)
    (viper-Put-back arg))
   ((and killed-rectangle
         kill-ring
         (eq (get 'killed-rectangle 'previous-kill)
             (current-kill 0)))
    (unless (eolp)
      (viper-forward-char-carefully))
    (save-excursion
      (yank-rectangle)))
   (t
    ad-do-it))
  (when vimpulse-visual-mode
    (vimpulse-visual-mode -1)))

;; Viper's larger movement commands use the mark to store the previous
;; position, which is fine and useful when the mark isn't active. When
;; it is, however, it has the effect of remaking the region.
(defadvice push-mark (around vimpulse-visual-mode activate)
  (unless (and vimpulse-visual-mode
               ;; Note: if you really need to call `push-mark'
               ;; in proximity with these commands (e.g., in a hook),
               ;; do (let (this-command) (push-mark)).
               (memq this-command
                     '(vimpulse-goto-first-line
                       vimpulse-goto-line
                       viper-backward-paragraph
                       viper-backward-sentence
                       viper-forward-paragraph
                       viper-forward-sentence
                       viper-goto-line
                       viper-search-next
                       viper-search-Next
                       viper-window-bottom
                       viper-window-middle
                       viper-window-top)))
    ad-do-it))

;; block selection disables Transient Mark mode
(defadvice deactivate-mark (after vimpulse-visual activate)
  "Deactivate Visual Block mode."
  (when (eq vimpulse-visual-mode 'block)
    (vimpulse-visual-mode -1)))

(defmacro vimpulse-visual-mouse-advice (cmd)
  "Advise mouse command CMD to enable Visual mode."
  `(defadvice ,cmd (around vimpulse-visual activate)
     "Enable Visual mode in vi (command) state."
     (let ((w (posn-window (event-start (ad-get-arg 0)))))
       (cond
        ;; if Visual mode is enabled in the window clicked in,
        ;; adjust region afterwards
        ((with-selected-window w
           vimpulse-visual-mode)
         (vimpulse-visual-highlight -1)
         ad-do-it
         (when (eq (selected-window) w)
           (vimpulse-visual-contract-region t)
           (vimpulse-visual-highlight)))
        ;; otherwise, if in vi (command) state, enable Visual mode
        ((with-selected-window w
           (eq viper-current-state 'vi-state))
         ad-do-it
         (when (eq (selected-window) w)
           (cond
            (vimpulse-visual-mode
             (vimpulse-visual-contract-region t))
            ((region-active-p)
             (vimpulse-visual-mode 1)
             (setq vimpulse-visual-region-expanded nil)
             (vimpulse-visual-contract-region t)))))
        (t
         ad-do-it)))))

(vimpulse-visual-mouse-advice mouse-drag-region)
(vimpulse-visual-mouse-advice mouse-save-then-kill)

(defadvice mouse-show-mark (before vimpulse-visual activate)
  "Refresh highlighting of Visual selection."
  (when vimpulse-visual-mode
    (vimpulse-visual-highlight)))

(defun vimpulse-movement-cmd-p (command)
  "Whether COMMAND is a \"movement\" command.
That is, whether it is listed in `vimpulse-movement-cmds'."
  ;; we use `member' rather than `memq' to allow lambdas
  (member command vimpulse-movement-cmds))

(defun vimpulse-needs-newline-p (command)
  "Whether COMMAND needs trailing newline in Visual Line mode.
In most cases (say, when wrapping the selection in a skeleton),
it is more useful to exclude the last newline from the region."
  (or (member command vimpulse-newline-cmds)
      (vimpulse-operator-cmd-p command)))


;;; Ex

(defun vimpulse-visual-ex (arg)
  "Call `viper-ex' on region."
  (interactive "p")
  (viper-ex arg))

;;; Insert/append

(defun vimpulse-visual-insert (beg end &optional arg)
  "Enter Insert state at beginning of Visual selection."
  (interactive "r\nP")
  (let (deactivate-mark)
    (cond
     ((eq vimpulse-visual-mode 'block)
      (vimpulse-visual-block-rotate 'upper-left beg end)
      (setq beg (vimpulse-visual-beginning)
            end (vimpulse-visual-end))
      (vimpulse-visual-mode -1)
      (goto-char
       (vimpulse-visual-create-coords 'block ?i beg end))
      (viper-insert arg))
     (t
      (vimpulse-visual-mode -1)
      (push-mark end t t)
      (goto-char beg)
      (viper-insert arg))
     (t
      (error "Not in Visual mode")))))

(defun vimpulse-visual-append (beg end &optional arg)
  "Enter Insert state at end of Visual selection."
  (interactive "r\nP")
  (let (deactivate-mark)
    (cond
     ((eq vimpulse-visual-mode 'block)
      (vimpulse-visual-block-rotate 'upper-left beg end)
      (setq beg (vimpulse-visual-beginning)
            end (vimpulse-visual-end))
      (setq vimpulse-visual-whitespace-overlay nil)
      (vimpulse-visual-mode -1)
      (goto-char
       (vimpulse-visual-create-coords 'block ?a beg end))
      (viper-append arg))
     (t
      (vimpulse-visual-mode -1)
      (push-mark beg t t)
      (goto-char end)
      (viper-insert arg))
     (t
      (error "Not in Visual mode")))))

;;; Block selection

(defun vimpulse-apply-on-block (func &optional beg end &rest args)
  "Call FUNC for each line of Visual Block selection.
The selection may be specified explicitly with BEG and END.
FUNC must take at least two arguments, the beginning and end of
each line. Extra arguments to FUNC may be passed via ARGS."
  (let (beg-col end-col)
    (save-excursion
      (setq beg (or beg (vimpulse-visual-beginning))
            end (or end (vimpulse-visual-end)))
      ;; ensure BEG < END
      (setq beg (prog1 (min beg end)
                  (setq end (max beg end))))
      ;; calculate columns
      (goto-char end)
      (setq end-col (current-column))
      (goto-char beg)
      (setq beg-col (current-column))
      ;; ensure BEG-COL < END-COL
      (when (> beg-col end-col)
        (setq beg-col (prog1 end-col
                        (setq end-col beg-col)))
        (setq end (save-excursion
                    (goto-char end)
                    (move-to-column end-col)
                    (point))))
      ;; apply FUNC on each line
      (while (< (point) end)
        (apply func
               (save-excursion
                 (move-to-column beg-col)
                 (point))
               (save-excursion
                 (move-to-column end-col)
                 (point))
               args)
        (forward-line 1)))))

(defun vimpulse-visual-block-position (corner &optional beg end)
  "Return position of Visual Block CORNER.
CORNER may be one of `upper-left', `upper-right', `lower-left'
and `lower-right', or a clockwise number from 0 to 3:

        0---1        upper-left +---+ upper-right
        |   |                   |   |
        3---2        lower-left +---+ lower-right

The rectangle is defined by mark and point, or BEG and END
if specified. The CORNER values `upper', `left', `lower'
and `right' return one of the defining corners.

        upper P---+                    +---M upper
         left |   | lower        lower |   | right
              +---M right         left P---+

Corners 0 and 3 are returned by their left side, corners 1 and 2
by their right side. To place point in one of the corners, use
`vimpulse-visual-block-rotate'.

To go the other way, use `vimpulse-visual-block-corner'."
  (save-excursion
    (setq beg (or beg (vimpulse-visual-beginning 'block))
          end (or end (vimpulse-visual-end 'block)))
    (when (> beg end) (setq beg (prog1 end (setq end beg))))
    (let ((beg-col (progn (goto-char beg)
                          (current-column)))
          (end-col (progn (goto-char end)
                          (current-column)))
          (upper beg) (left beg) (lower end) (right end)
          (upper-left 0) (upper-right 1)
          (lower-left 3) (lower-right 2))
      (when (> beg-col end-col)
        (setq beg-col (prog1 end-col
                        (setq end-col beg-col)))
        (setq left (prog1 right
                     (setq right left))))
      (if (memq corner '(upper left lower right))
          (eval corner)
        (setq corner (mod (eval corner) 4))
        (if (memq corner '(0 1))
            (goto-char beg)
          (goto-char end))
        (if (memq corner '(0 3))
            (vimpulse-move-to-column beg-col)
          (vimpulse-move-to-column end-col))
        (point)))))

(defun vimpulse-visual-block-corner (&optional symbolic pos)
  "Return the current Visual Block corner as a number from 0 to 3.
Corners are numbered clockwise, starting with the upper-left corner.
Return as one of `upper-left', `upper-right', `lower-left' and
`lower-right' if SYMBOLIC is non-nil.

        0---1        upper-left +---+ upper-right
        |   |                   |   |
        3---2        lower-left +---+ lower-right

Specify POS to compare that position, rather than point,
against the corners. The result can be passed to functions
like `vimpulse-visual-block-position' and
`vimpulse-visual-block-rotate'."
  (let ((upper-left 0)
        (upper-right 1)
        (lower-left 3)
        (lower-right 2)
        corner)
    (setq pos (or pos (point)))
    (or (dolist (i '(upper-left lower-left) corner)
          (when (eq (vimpulse-visual-block-position i) pos)
            (setq corner i)))
        (progn
          (unless vimpulse-visual-region-expanded
            (setq pos (1+ pos)))
          (dolist (i '(upper-right lower-right) corner)
            (when (eq (vimpulse-visual-block-position i) pos)
              (setq corner i)))))
    (if symbolic
        corner
      (eval corner))))

(defun vimpulse-visual-block-rotate (corner &optional beg end)
  "In Visual Block selection, rotate point and mark clockwise.
When called non-interactively, CORNER specifies the corner to
place point in; mark is placed in the opposite corner.

        0---1        upper-left +---+ upper-right
        |   |                   |   |
        3---2        lower-left +---+ lower-right

Corners are numbered clockwise from 0. For better readability,
you may use the symbolic values `upper-left', `upper-right',
`lower-left' and `lower-right'.

This function updates `vimpulse-visual-point' and
`vimpulse-visual-mark' so that \\[vimpulse-visual-restore]
restores the selection with the same rotation."
  (interactive
   (list (if (< (prefix-numeric-value current-prefix-arg) 0)
             (1- (vimpulse-visual-block-corner))
           (1+ (vimpulse-visual-block-corner)))))
  (let ((upper-left 0) (upper-right 1) (lower-left 3) (lower-right 2)
        newmark newpoint newmark-marker newpoint-marker mark-active)
    (setq corner (mod (eval corner) 4))
    (setq newpoint (vimpulse-visual-block-position corner beg end))
    (setq newmark (vimpulse-visual-block-position
                   (mod (+ 2 corner) 4) beg end))
    (if (memq corner '(0 3))
        (setq newmark-marker (1- newmark)
              newpoint-marker newpoint)
      (setq newpoint-marker (1- newpoint)
            newmark-marker newmark))
    (unless vimpulse-visual-region-expanded
      (setq newpoint newpoint-marker
            newmark  newmark-marker))
    (set-mark newmark)
    (goto-char newpoint)
    (vimpulse-set-visual-dimensions beg end 'block)))

(defun vimpulse-visual-exchange-corners ()
  "Rearrange corners in Visual Block mode.

        M---+          +---M
        |   |    =>    |   |
        +---P          P---+

For example, if mark is in the upper left corner and point
in the lower right (see fig.), this function puts mark in
the upper right corner and point in the lower left."
  (interactive)
  (cond
   ((memq vimpulse-visual-mode '(char line))
    (exchange-point-and-mark))
   ((eq vimpulse-visual-mode 'block)
    (let ((mark-col (save-excursion
                      (goto-char (mark t))
                      (forward-char)
                      (1- (current-column))))
          (point-col (current-column)))
      (set-mark (save-excursion
                  (goto-char (mark t))
                  (vimpulse-move-to-column
                   point-col (< (current-column) point-col))
                  (point)))
      (vimpulse-move-to-column
       mark-col (< (current-column) mark-col))
      (and (eolp) (not (bolp)) (backward-char))))
   (t
    (error "Not in Visual mode"))))

;; Insert whitespace into buffer to handle zero-width rectangles.
;; This isn't ideal and should be replaced with something else.
(defun vimpulse-visual-block-add-whitespace ()
  "Ensure rectangle is at least one column wide.
If the Block selection starts and ends on blank lines, the
resulting rectangle has width zero even if intermediate lines
contain characters. This function inserts a space after mark
so that a one-column rectangle can be made. The position of the
space is stored in `vimpulse-visual-whitespace-overlay' so it can be
removed afterwards with `vimpulse-visual-block-cleanup-whitespace'."
  (save-excursion
    (when (and (eq vimpulse-visual-mode 'block)
               (/= (vimpulse-visual-beginning)
                   (vimpulse-visual-end))
               (save-excursion
                 (goto-char (vimpulse-visual-beginning))
                 (and (bolp) (eolp)))
               (save-excursion
                 (goto-char (vimpulse-visual-end))
                 (and (bolp) (eolp))))
      (goto-char (mark t))
      (insert " ")
      (setq vimpulse-visual-whitespace-overlay
            (vimpulse-make-overlay (mark t) (1+ (mark t))
                                   nil t nil)))))

(defun vimpulse-visual-block-cleanup-whitespace ()
  "Clean up whitespace inserted by `vimpulse-visual-block-add-whitespace'."
  (when (viper-overlay-live-p vimpulse-visual-whitespace-overlay)
    (when (= (- (viper-overlay-end
                 vimpulse-visual-whitespace-overlay)
                (viper-overlay-start
                 vimpulse-visual-whitespace-overlay))
             1)
      (delete-region
       (viper-overlay-start vimpulse-visual-whitespace-overlay)
       (viper-overlay-end   vimpulse-visual-whitespace-overlay)))
    (vimpulse-delete-overlay vimpulse-visual-whitespace-overlay)
    (setq vimpulse-visual-whitespace-overlay nil)))

(defun vimpulse-visual-create-coords
  (mode i-com upper-left lower-right)
  "Update the list of block insert coordinates with current rectangle.
I-COM should be ?c, ?i, ?a, ?I or ?A; the column for the
insertion will be chosen according to this command.
Returns the insertion point."
  (setq vimpulse-visual-insert-coords nil)
  (let ((nlines (count-lines upper-left lower-right))
        (col 0))                 ; for ?I and ?A, trivial: column is 0
    (when (memq i-com '(?a ?c ?i))
      ;; for ?i and ?a, choose the left (the right) rectangle column
      (let ((beg-col (save-excursion
                       (goto-char upper-left)
                       (current-column)))
            (end-col (save-excursion
                       (goto-char lower-right)
                       (current-column))))
        ;; decide if we use the left or right column
        (setq col (max 0 (if (memq i-com '(?c ?i))
                             beg-col
                           (1- end-col))))))
    ;; save the information
    (setq vimpulse-visual-insert-coords
          (list mode i-com upper-left col nlines))
    (save-excursion
      (goto-char upper-left)
      (vimpulse-move-to-column col)
      (point))))

;; Redefinitions of Viper functions to handle Visual block selection,
;; that is, the "update all lines when we hit ESC" part.
;; This function is not in viper-functions-redefinitions.el
;; because its code is closely related to Visual mode.
(defun vimpulse-exit-insert-state ()
  (interactive)
  (viper-move-marker-locally 'vimpulse-exit-point (point))
  (viper-change-state-to-vi)
  (when vimpulse-visual-insert-coords
    ;; get the saved info about the Visual selection
    (let ((mode   (nth 0 vimpulse-visual-insert-coords))
          (i-com  (nth 1 vimpulse-visual-insert-coords))
          (pos    (nth 2 vimpulse-visual-insert-coords))
          (col    (nth 3 vimpulse-visual-insert-coords))
          (nlines (nth 4 vimpulse-visual-insert-coords)))
      (goto-char pos)
      (save-excursion
        (dotimes (i (1- nlines))
          (forward-line 1)
          (let ((cur-col (vimpulse-move-to-column col)))
            ;; if we are in Block mode, this line, but do not hit the
            ;; correct column, we check if we should convert tabs
            ;; and/or append spaces
            (if (and (eq mode 'block)
                     (or (/= col cur-col) ; wrong column or
                         (eolp)))         ; end of line
                (cond ((> cur-col col)    ; we are inside a tab
                       (move-to-column (1+ col) t) ; convert to spaces
                       (move-to-column col t) ; this is needed for ?a
                       (viper-repeat nil))
                      ((and (>= col cur-col) ; we are behind the end
                            (eq i-com ?a))   ; and I-COM is ?a
                       (move-to-column (1+ col) t) ; append spaces
                       (viper-repeat nil)))
              (viper-repeat nil)))))
      (setq vimpulse-visual-insert-coords nil)))
  ;; update undo-list
  (vimpulse-end-undo-step))

(defalias 'viper-exit-insert-state 'vimpulse-exit-insert-state)

(defadvice viper-goto-eol (after vimpulse-visual activate)
  "Move to end of line in Visual mode."
  (when vimpulse-visual-mode
    (end-of-line 1)))

(defadvice viper-forward-char (around vimpulse-activate activate)
  "Move to end of line in Visual mode."
  (cond
   (vimpulse-visual-mode
    (let ((val (viper-p-val arg)))
      (forward-char val)
      (when (and viper-ex-style-motion (bolp))
        (backward-char))))
   (t
    ad-do-it)))

(provide 'vimpulse-visual-mode)
