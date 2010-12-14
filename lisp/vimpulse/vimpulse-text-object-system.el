;;;; Text objects support

;; The following code implements support for text objects and commands
;; like diw, daw, ciw, caw. Currently, the most common objects are
;; supported:
;;
;;   - bracket-blocks: b B { [ ( < > ) ] }
;;   - sentences: s
;;   - paragraphs: p
;;   - quoted expressions: " and '
;;   - words: w and W
;;
;; Vimpulse's text objects are fairly close to Vim's, and are based on
;; Viper's movement commands. More objects are easily added with
;; `vimpulse-define-text-object'.

(eval-when-compile (require 'vimpulse-utils)) ; vimpulse-unquote
(require 'vimpulse-visual-mode)         ; v-v-{activate,expand-region,select}

(declare-function vimpulse-calculate-motion-range "vimpulse-operator" (count motion &optional type refresh))

(defvar vimpulse-operator-basic-map)    ; defined programmatically by `v-define-state'

(defmacro vimpulse-define-text-object (object args &rest body)
  "Define a text object OBJECT.
ARGS is the argument list, which must contain at least one argument:
the count. It is followed by an optional docstring and optional
keywords:

:keys KEYS      A key or a list of keys to bind the command to.
:map MAP        Keymap to bind :keys in, defaults to
                `vimpulse-operator-basic-map'.
:type TYPE      The object's motion type.

The keywords are followed by the object's body, which must return
a pure range (BEG END) or a motion range (TYPE BEG END). Thus,
a simple example may look somewhat like:

    (vimpulse-define-text-object test (arg)
      \"Test object.\"
      :keys \"t\"
      (list 'exclusive (point) (+ arg (point))))

Here, the count is stored in ARG. Note that the body must be able
to handle a negative value, which specifies reverse direction."
  (declare (indent defun)
           (debug (&define name lambda-list
                           [&optional stringp]
                           [&rest keywordp sexp]
                           def-body)))
  (let ((map 'vimpulse-operator-basic-map)
        count doc keys keyword type)
    ;; collect COUNT argument
    (setq args (or args (list 'arg))
          count (car args))
    ;; collect docstring, if any
    (when (stringp (car body))
      (setq doc  (car body)
            body (cdr body)))
    ;; collect keywords
    (while (keywordp (setq keyword (car body)))
      (setq body (cdr body))
      (cond
       ((eq keyword :keys)
        (setq keys (vimpulse-unquote (pop body))))
       ((eq keyword :map)
        (setq map (vimpulse-unquote (pop body))))
       ((eq keyword :type)
        (setq type (vimpulse-unquote (pop body))))
       (t
        (pop body))))
    (unless (listp keys)
      (setq keys (list keys)))
    (when type
      (setq type `(',type)))
    ;; macro expansion: define key bindings, set motion type
    ;; and define command
    `(progn
       (dolist (key ',keys)
         (define-key ,map key ',object))
       ,(when type
          `(put ',object 'motion-type ,@type))
       (defun ,object ,args
         ,doc
         (interactive "p")
         (let ((,count (if (numberp ,count) ,count 1))
               range)
           (cond
            ((region-active-p)
             (when (< (point) (mark t))
               (setq ,count (- ,count)))
             (when (memq vimpulse-visual-mode '(line block))
               (vimpulse-visual-activate 'char))
             (when (and vimpulse-visual-mode
                        (not vimpulse-visual-region-expanded))
               (vimpulse-visual-expand-region))
             (setq range (progn ,@body))
             (unless (vimpulse-mark-range range t ,@type)
               ;; Are we stuck (unchanged region)?
               ;; Move forward and try again.
               (viper-forward-char-carefully (if (< ,count 0) -1 1))
               (setq range (progn ,@body))
               (vimpulse-mark-range range t ,@type)))
            (t
             (setq range (progn ,@body))
             (vimpulse-mark-range range nil ,@type))))))))

(defun vimpulse-mark-range (range &optional widen type)
  "Mark RANGE, which has the form (BEG END) or (TYPE BEG END).
If WIDEN is non-nil, expands existing region. If the TYPE
argument is specified, it overrides the type of RANGE."
  (let* ((type  (or type (vimpulse-motion-type range)))
         (range (vimpulse-motion-range range))
         (beg (vimpulse-range-beginning range))
         (end (vimpulse-range-end range)))
    (cond
     ((eq type 'exclusive)
      (vimpulse-visual-select beg end widen))
     (t
      (when vimpulse-visual-mode
        (unless (memq type '(line block))
          (setq type 'char))
        (unless (eq type vimpulse-visual-mode)
          (vimpulse-visual-activate type)))
      (vimpulse-visual-select beg end widen)))))

;;; Text object range functions

;; word-like expressions (words, sentences, paragraphs)
(defun vimpulse-object-range
  (count backward-func forward-func &optional type)
  "Return a text object range (TYPE BEG END).
BACKWARD-FUNC moves point to the object's beginning,
FORWARD-FUNC moves to its end. Schematically,

\(vimpulse-object-range <num> <beg-of-object> <end-of-object>)

COUNT is the number of objects. If positive, go forwards and
then backwards; if negative, go backwards and then forwards.

The type of the object (`exclusive', `inclusive' or `line')
may be specified with TYPE. Otherwise, the type is inferred
from the motion types of BACKWARD-FUNC and FORWARD-FUNC."
  (let ((types '(exclusive inclusive line block))
        beg end forward-range backward-range
        viper-com-point
        vimpulse-visual-vars-alist
        vimpulse-this-motion
        vimpulse-this-motion-type)
    (save-excursion
      (setq count (or (if (eq count 0) 1 count) 1))
      (if (< count 0)
          (setq backward-range
                (vimpulse-calculate-motion-range
                 (abs count) backward-func type t)
                forward-range
                (vimpulse-calculate-motion-range
                 (abs count) forward-func type t))
        (setq forward-range
              (vimpulse-calculate-motion-range
               (abs count) forward-func type t)
              backward-range
              (vimpulse-calculate-motion-range
               (abs count) backward-func type t)))
      (setq beg (vimpulse-range-beginning backward-range)
            end (vimpulse-range-end forward-range))
      (unless type
        (setq type 'exclusive)
        (dolist (elt types)
          (when (or (eq elt (vimpulse-motion-type backward-range))
                    (eq elt (vimpulse-motion-type forward-range)))
            (setq type elt))))
      (list type beg end))))

(defun vimpulse-an-object-range
  (count backward-func forward-func &optional include-newlines regexp)
  "Return a text object range (BEG END) with whitespace.
Unless INCLUDE-NEWLINES is t, whitespace inclusion is restricted
to the line(s) the object is on. REGEXP is a regular expression
for matching whitespace; the default is \"[ \\f\\t\\n\\r\\v]+\".
See `vimpulse-object-range' for more details."
  (let (range beg end line-beg line-end mark-active-p)
    (save-excursion
      (setq count (or (if (eq count 0) 1 count) 1))
      (setq regexp (or regexp "[ \f\t\n\r\v]+"))
      (setq range (vimpulse-motion-range
                   (vimpulse-object-range
                    count backward-func forward-func)))
      ;; let `end' be the boundary furthest from point,
      ;; based on the direction we are going
      (if (< count 0)
          (setq beg (cadr range)
                end (car range))
        (setq beg (car range)
              end (cadr range)))
      ;; if INCLUDE-NEWLINES is nil, never move past
      ;; the line boundaries of the text object
      (unless include-newlines
        (setq line-beg (line-beginning-position)
              line-end (line-end-position))
        (when (> (* count beg)
                 (max (* count line-beg) (* count line-end)))
          (setq count (- count))
          (setq range (vimpulse-motion-range
                       (vimpulse-object-range
                        count backward-func forward-func)))
          (if (< count 0)
              (setq beg (cadr range)
                    end (car range))
            (setq beg (car range)
                  end (cadr range))))
        (setq line-beg (save-excursion
                         (goto-char (min beg end))
                         (line-beginning-position))
              line-end (save-excursion
                         (goto-char (max beg end))
                         (line-end-position))))
      ;; Generally only include whitespace at one side (but see below).
      ;; If we are before the object, include leading whitespace;
      ;; if we are inside the object, include trailing whitespace.
      ;; If trailing whitespace inclusion fails, include leading.
      (setq count (if (< count 0) -1 1))
      (when (or (< (* count (point)) (* count beg))
                (eq end (setq end (save-excursion
                                    (goto-char end)
                                    (vimpulse-skip-regexp
                                     regexp count line-beg line-end)))))
        (setq beg (save-excursion
                    (goto-char beg)
                    (if (and (not include-newlines)
                             (looking-back "^[ \t]*"))
                        beg
                      (vimpulse-skip-regexp
                       regexp (- count) line-beg line-end))))
        ;; Before/after adjustment for whole lines: if the object is
        ;; followed by a blank line, include that as trailing
        ;; whitespace and subtract a line from the leading whitespace.
        (when include-newlines
          (goto-char end)
          (forward-line count)
          (when (looking-at "[ \t]*$")
            (setq end (line-beginning-position))
            (goto-char beg)
            (when (looking-at "[ \t]*$")
              (forward-line count)
              (setq beg (line-beginning-position))))))
      ;; return the range
      (list (min beg end) (max beg end)))))

(defun vimpulse-inner-object-range
  (count backward-func forward-func)
  "Return a text object range (BEG END) including point.
If point is outside the object, it is included in the range.
To include whitespace, use `vimpulse-an-object-range'.
See `vimpulse-object-range' for more details."
  (let (range beg end line-beg line-end)
    (setq count (or (if (eq count 0) 1 count) 1))
    (setq range (vimpulse-motion-range
                 (vimpulse-object-range
                  count backward-func forward-func)))
    (setq beg (car range)
          end (cadr range))
    (setq line-beg (line-beginning-position)
          line-end (line-end-position))
    (when (> (min (* count beg) (* count end))
             (max (* count line-beg) (* count line-end)))
      (setq count (- count))
      (setq range (vimpulse-motion-range
                   (vimpulse-object-range
                    count backward-func forward-func))
            beg (car range)
            end (cadr range)))
    ;; return the range, including point
    (list (min beg (point)) (max end (point)))))

;; parenthetical expressions
(defun vimpulse-paren-range (count &optional open close include-parentheses)
  "Return a parenthetical expression range (BEG END).
The type of parentheses may be specified with OPEN and CLOSE,
which must be characters. INCLUDE-PARENTHESES specifies
whether to include the parentheses in the range."
  (let ((beg (point)) (end (point))
        line-beg line-end)
    (setq count (if (eq count 0) 1 (abs count)))
    (save-excursion
      (setq open  (if (characterp open)
                      (regexp-quote (string open)) "")
            close (if (characterp close)
                      (regexp-quote (string close)) ""))
      (when (and (not (string= open ""))
                 (looking-at open))
        (forward-char))
      ;; find opening and closing paren with Emacs' S-exp facilities
      (while (progn
               (vimpulse-backward-up-list 1)
               (not (when (looking-at open)
                      (when (save-excursion
                              (forward-sexp)
                              (when (looking-back close)
                                (setq end (point))))
                        (if (>= count 0)
                            (setq beg (point))
                          (setq count (1- count)) nil))))))
      (if include-parentheses
          (list beg end)
        (setq beg (prog1 (min (1+ beg) end)
                    (setq end (max (1- end) beg))))
        (if (<= (count-lines beg end) 1)
            (list beg end)
          ;; multi-line inner range: select whole lines
          (goto-char beg)
          (when (looking-at "[ \f\t\n\r\v]*$")
            (forward-line)
            ;; Include indentation?
            (if (and viper-auto-indent
                     (not (eq vimpulse-this-operator
                              'vimpulse-delete)))
                (back-to-indentation)
              (beginning-of-line))
            (setq beg (point)))
          (goto-char end)
          (when (and (looking-back "^[ \f\t\n\r\v]*")
                     (not (eq vimpulse-this-operator
                              'vimpulse-delete)))
            (setq end (line-end-position 0))
            (goto-char end))
          (list (min beg end) (max beg end)))))))

;; quoted expressions
(defun vimpulse-quote-range (count &optional quote include-quotes)
  "Return a quoted expression range (BEG END).
QUOTE is a quote character (default ?\\\"). INCLUDE-QUOTES
specifies whether to include the quote marks in the range."
  (let ((beg (point)) (end (point))
        regexp)
    (save-excursion
      (setq count (if (eq count 0) 1 (abs count)))
      (setq quote (or quote ?\"))
      (setq quote (if (characterp quote)
                      (regexp-quote (string quote)) "")
            regexp (concat "\\([^\\\\]\\|^\\)" quote))
      (when (and (not (string= quote ""))
                 (looking-at quote))
        (forward-char))
      ;; search forward for a closing quote
      (while (and (> count 0)
                  (re-search-forward regexp nil t))
        (setq count (1- count))
        (setq end (point))
        ;; find the matching opening quote
        (condition-case nil
            (progn
              (setq beg (scan-sexps end -1))
              ;; Emacs' S-exp logic doesn't work in text mode
              (save-excursion
                (goto-char beg)
                (unless (looking-at quote)
                  (re-search-backward regexp)
                  (unless (looking-at quote)
                    (forward-char))
                  (setq beg (point)))))
          ;; Finding the opening quote failed. Maybe we're already at
          ;; the opening quote and should look for the closing instead?
          (error (condition-case nil
                     (progn
                       (viper-backward-char-carefully)
                       (setq beg (point))
                       (setq end (scan-sexps beg 1))
                       (unless (looking-back quote)
                         (re-search-forward regexp)
                         (unless (looking-back quote)
                           (backward-char))
                         (setq end (point))))
                   (error (setq end beg))))))
      (if include-quotes
          (list beg end)
        (list (min (1+ beg) end) (max (1- end) beg))))))

;;; Text object definitions

(vimpulse-define-text-object vimpulse-line (arg)
  "Select ARG lines."
  :type 'line
  (setq arg (1- arg))
  (vimpulse-line-range
   (point)
   (save-excursion
     (when (> arg 0)
       (viper-next-line-carefully arg))
     (point))))

(vimpulse-define-text-object vimpulse-a-word (arg)
  "Select a word."
  :keys "aw"
  (vimpulse-an-object-range
   arg
   (lambda (arg)
     (vimpulse-limit (line-beginning-position) (line-end-position)
       (viper-backward-word (cons arg ?r))))
   (lambda (arg)
     (vimpulse-limit (line-beginning-position) (line-end-position)
       (viper-end-of-word (cons arg ?r))))))

(vimpulse-define-text-object vimpulse-inner-word (arg)
  "Select inner word."
  :keys "iw"
  (vimpulse-inner-object-range
   arg
   (lambda (arg)
     (vimpulse-limit (line-beginning-position) (line-end-position)
       (viper-backward-word (cons arg ?r))))
   (lambda (arg)
     (vimpulse-limit (line-beginning-position) (line-end-position)
       (backward-char)
       (viper-end-of-word (cons arg ?r))))))

(vimpulse-define-text-object vimpulse-a-Word (arg)
  "Select a Word."
  :keys "aW"
  (vimpulse-an-object-range
   arg
   (lambda (arg)
     (vimpulse-limit (line-beginning-position) (line-end-position)
       (viper-backward-Word (cons arg ?r))))
   (lambda (arg)
     (vimpulse-limit (line-beginning-position) (line-end-position)
       (viper-end-of-Word (cons arg ?r))))))

(vimpulse-define-text-object vimpulse-inner-Word (arg)
  "Select inner Word."
  :keys "iW"
  (vimpulse-inner-object-range
   arg
   (lambda (arg)
     (vimpulse-limit (line-beginning-position) (line-end-position)
       (viper-backward-Word (cons arg ?r))))
   (lambda (arg)
     (vimpulse-limit (line-beginning-position) (line-end-position)
       (viper-end-of-Word (cons arg ?r))))))

(vimpulse-define-text-object vimpulse-a-sentence (arg)
  "Select a sentence."
  :keys "as"
  (vimpulse-an-object-range
   arg
   (lambda (arg)
     (viper-backward-sentence arg)
     (vimpulse-skip-regexp "[ \f\t\n\r\v]+" 1))
   (lambda (arg)
     (viper-forward-sentence arg)
     (vimpulse-skip-regexp "[ \f\t\n\r\v]+" -1))))

(vimpulse-define-text-object vimpulse-inner-sentence (arg)
  "Select inner sentence."
  :keys "is"
  (vimpulse-inner-object-range
   arg
   (lambda (arg)
     (viper-backward-sentence arg)
     (vimpulse-skip-regexp "[ \f\t\n\r\v]+" 1))
   (lambda (arg)
     (viper-forward-sentence arg)
     (vimpulse-skip-regexp "[ \f\t\n\r\v]+" -1))))

(vimpulse-define-text-object vimpulse-a-paragraph (arg)
  "Select a paragraph."
  :keys "ap"
  (vimpulse-an-object-range
   arg
   (lambda (arg)
     (vimpulse-skip-regexp "[ \f\t\n\r\v]+" -1)
     (viper-backward-paragraph arg)
     (vimpulse-skip-regexp "[ \f\t\n\r\v]+" 1))
   (lambda (arg)
     (vimpulse-skip-regexp "[ \f\t\n\r\v]+" 1)
     (viper-forward-paragraph arg)
     (vimpulse-skip-regexp "[ \f\t\n\r\v]+" -1)) t))

(vimpulse-define-text-object vimpulse-inner-paragraph (arg)
  "Select inner paragraph."
  :keys "ip"
  (vimpulse-inner-object-range
   arg
   (lambda (arg)
     (vimpulse-skip-regexp "[ \f\t\n\r\v]+" -1)
     (viper-backward-paragraph arg)
     (vimpulse-skip-regexp "[ \f\t\n\r\v]+" 1))
   (lambda (arg)
     (vimpulse-skip-regexp "[ \f\t\n\r\v]+" 1)
     (viper-forward-paragraph arg)
     (vimpulse-skip-regexp "[ \f\t\n\r\v]+" -1))))

(vimpulse-define-text-object vimpulse-a-paren (arg)
  "Select a parenthesis."
  :keys '("ab" "a(" "a)")
  (vimpulse-paren-range arg ?\( nil t))

(vimpulse-define-text-object vimpulse-inner-paren (arg)
  "Select inner parenthesis."
  :keys '("ib" "i(" "i)")
  (vimpulse-paren-range arg ?\())

(vimpulse-define-text-object vimpulse-a-bracket (arg)
  "Select a square bracket."
  :keys '("a[" "a]")
  (vimpulse-paren-range arg ?\[ nil t))

(vimpulse-define-text-object vimpulse-inner-bracket (arg)
  "Select inner square bracket."
  :keys '("i[" "i]")
  (vimpulse-paren-range arg ?\[))

(vimpulse-define-text-object vimpulse-a-curly (arg)
  "Select a curly bracket (\"brace\")."
  :keys '("aB" "a{" "a}")
  (vimpulse-paren-range arg ?{ nil t))

(vimpulse-define-text-object vimpulse-inner-curly (arg)
  "Select inner curly bracket (\"brace\")."
  :keys '("iB" "i{" "i}")
  (vimpulse-paren-range arg ?{))

(vimpulse-define-text-object vimpulse-an-angle (arg)
  "Select an angle bracket."
  :keys '("a<" "a>")
  (vimpulse-paren-range arg ?< nil t))

(vimpulse-define-text-object vimpulse-inner-angle (arg)
  "Select inner angle bracket."
  :keys '("i<" "i>")
  (vimpulse-paren-range arg ?<))

(vimpulse-define-text-object vimpulse-a-single-quote (arg)
  "Select a single-quoted expression."
  :keys "a'"
  (vimpulse-quote-range arg ?' t))

(vimpulse-define-text-object vimpulse-inner-single-quote (arg)
  "Select inner single-quoted expression."
  :keys "i'"
  (vimpulse-quote-range arg ?'))

(vimpulse-define-text-object vimpulse-a-double-quote (arg)
  "Select a double-quoted expression."
  :keys "a\""
  (vimpulse-quote-range arg ?\" t))

(vimpulse-define-text-object vimpulse-inner-double-quote (arg)
  "Select inner double-quoted expression."
  :keys "i\""
  (vimpulse-quote-range arg ?\"))

(provide 'vimpulse-text-object-system)
