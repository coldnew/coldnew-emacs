;;;; Keybindings

(require 'vimpulse-dependencies)
(require 'vimpulse-visual-mode)         ; vimpulse-apply-on-block, vimpulse-visual-mode

(declare-function vimpulse-range "vimpulse-operator" (&optional no-repeat dont-move-point whole-lines keep-visual custom-motion))
(declare-function vimpulse-set-replace-cursor-type "vimpulse-viper-function-redefinitions" nil)

;;; C-u

(unless vimpulse-want-C-u-like-Vim
  (define-key viper-vi-basic-map "\C-u" 'universal-argument))

;;; vi (command) mode keys

(define-key viper-vi-basic-map "c" 'vimpulse-change)
(define-key viper-vi-basic-map "d" 'vimpulse-delete)
(define-key viper-vi-basic-map "g" nil) ; delete `viper-nil' binding
(define-key viper-vi-basic-map "g?" 'vimpulse-rot13)
(define-key viper-vi-basic-map "gU" 'vimpulse-upcase)
(define-key viper-vi-basic-map "gb" 'vimpulse-end-of-previous-word)
(define-key viper-vi-basic-map "gd" 'vimpulse-goto-definition)
(define-key viper-vi-basic-map "gf" 'find-file-at-point)
(define-key viper-vi-basic-map "gg" 'vimpulse-goto-first-line)
(define-key viper-vi-basic-map "gh" 'backward-char)
(define-key viper-vi-basic-map "gi" 'vimpulse-resume-insert)
(define-key viper-vi-basic-map "gj" 'next-line)
(define-key viper-vi-basic-map "gk" 'previous-line)
(define-key viper-vi-basic-map "gl" 'forward-char)
(define-key viper-vi-basic-map "gq" 'vimpulse-fill)
(define-key viper-vi-basic-map "gu" 'vimpulse-downcase)
(define-key viper-vi-basic-map "gw" 'vimpulse-fill)
(define-key viper-vi-basic-map "g~" 'vimpulse-invert-case)
(define-key viper-vi-basic-map "g0" 'vimpulse-beginning-of-visual-line)
(define-key viper-vi-basic-map "g$" 'vimpulse-end-of-visual-line)
(define-key viper-vi-basic-map "gJ" 'vimpulse-Join)
(define-key viper-vi-basic-map "J" 'vimpulse-join)
(define-key viper-vi-basic-map "K" 'woman)
(define-key viper-vi-basic-map "m" 'vimpulse-mark-point)
(define-key viper-vi-basic-map "`" 'vimpulse-goto-mark)
(define-key viper-vi-basic-map "'" 'vimpulse-goto-mark-and-skip-white)
(define-key viper-vi-basic-map "r" 'vimpulse-replace)
(define-key viper-vi-basic-map "y" 'vimpulse-yank)
(define-key viper-vi-basic-map "zb" 'viper-line-to-bottom)
(define-key viper-vi-basic-map "zh" 'scroll-right)
(define-key viper-vi-basic-map "zl" 'scroll-left)
(define-key viper-vi-basic-map "zt" 'viper-line-to-top)
(define-key viper-vi-basic-map "zz" 'viper-line-to-middle)
(define-key viper-vi-basic-map "\C-]" 'vimpulse-jump-to-tag-at-point)
(define-key viper-vi-basic-map "\C-t" 'pop-tag-mark)
(define-key viper-vi-basic-map "]" nil) ; delete `viper-ket-function' binding
(define-key viper-vi-basic-map "]P" 'vimpulse-Put-and-indent)
(define-key viper-vi-basic-map "]p" 'vimpulse-put-and-indent)
(define-key viper-vi-basic-map "=" 'vimpulse-indent)
(define-key viper-vi-basic-map "+" 'vimpulse-previous-line-skip-white)
(define-key viper-vi-basic-map "_" 'vimpulse-next-line-skip-white)
(define-key viper-vi-basic-map "#" 'vimpulse-search-backward-for-symbol-at-point)
(define-key viper-vi-basic-map "*" 'vimpulse-search-forward-for-symbol-at-point)
(define-key viper-vi-basic-map "<" 'vimpulse-shift-left)
(define-key viper-vi-basic-map ">" 'vimpulse-shift-right)
(define-key viper-vi-basic-map "~" 'vimpulse-invert-char)
(define-key viper-vi-basic-map "\"" 'vimpulse-read-register)
(define-key viper-vi-basic-map "/" 'vimpulse-search-forward)
(define-key viper-vi-basic-map "?" 'vimpulse-search-backward)
(define-key viper-vi-kbd-map "/" nil)

;; Visual bindings
(define-key viper-vi-basic-map "v" 'vimpulse-visual-toggle-char)
(define-key viper-vi-basic-map "V" 'vimpulse-visual-toggle-line)
(define-key viper-vi-basic-map "\C-v" 'vimpulse-visual-toggle-block)
(define-key viper-vi-basic-map "gv" 'vimpulse-visual-restore)

;; map undo and redo
(define-key viper-vi-basic-map "u" 'undo)
(cond
 ((fboundp 'undo-tree-redo)
  (define-key viper-vi-basic-map "\C-r" 'undo-tree-redo))
 ((fboundp 'redo)
  (define-key viper-vi-basic-map "\C-r" 'redo)))

;; window manipulation
(defvar vimpulse-window-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-w" 'other-window)
    (define-key map "w" 'other-window)
    (define-key map "o" 'delete-other-windows)
    (define-key map "c" 'delete-window)
    (define-key map "s" 'split-window-vertically)
    (define-key map "v" 'split-window-horizontally)
    (when (fboundp 'windmove-left)
      (define-key map "h" 'windmove-left)
      (define-key map "j" 'windmove-down)
      (define-key map "k" 'windmove-up)
      (define-key map "l" 'windmove-right))
    map)
  "Keymap for window-related commands.
Equivalent to Vim's C-w prefix.")

(define-key viper-vi-basic-map "\C-w" vimpulse-window-map)

;;; Insert mode keys

;; Vim-like completion keys
(define-key viper-insert-basic-map "\C-p" 'vimpulse-abbrev-expand-before)
(define-key viper-insert-basic-map "\C-n" 'vimpulse-abbrev-expand-after)
(define-key viper-insert-basic-map "\C-x\C-p" 'vimpulse-expand-line)
(define-key viper-insert-basic-map "\C-x\C-n" 'vimpulse-expand-line)
(define-key viper-insert-basic-map [delete] 'delete-char) ; <delete> key
;; make ^[ work
(define-key viper-insert-basic-map (kbd "ESC") 'viper-exit-insert-state)

;;; "

(defun vimpulse-read-register (&optional register command)
  "Use COMMAND with REGISTER.
  If called interactively, read REGISTER and COMMAND from keyboard."
  (interactive)
  (setq register (or register (read-char)))
  (when (viper-valid-register register)
    (setq command (or command (key-binding (read-key-sequence nil))))
    (when (commandp command)
      (let ((this-command command)
            (viper-use-register register))
        (call-interactively command)))))

;;; g0, g$

(defun vimpulse-beginning-of-visual-line (arg)
  "Go to beginning of `visual-line-mode' line."
  (interactive "p")
  (if (and (boundp 'visual-line-mode) visual-line-mode)
      (beginning-of-visual-line arg)
    ;; `move-beginning-of-line' instead of `beginning-of-line'
    ;; handles longlines-mode properly
    (move-beginning-of-line arg)))

(defun vimpulse-end-of-visual-line (arg)
  "Go to end of `visual-line-mode' line."
  (interactive "p")
  (if (and (boundp 'visual-line-mode) visual-line-mode)
      (end-of-visual-line arg)
    ;; `move-end-of-line' instead of `end-of-line'
    ;; handles longlines-mode properly
    (move-end-of-line arg))
  (unless (bolp)
    (backward-char)))

;;; gg

(defun vimpulse-goto-first-line (arg)
  "Go to ARGth line (first line by default)."
  (interactive "P")
  (let ((val (viper-P-val arg))
        (com (viper-getCom arg)))
    (when (eq com ?c) (setq com ?C))
    (viper-move-marker-locally 'viper-com-point (point))
    (viper-deactivate-mark)
    (push-mark nil t)
    (cond
     ((null val)
      (goto-char (point-min)))
     (t
      (viper-goto-line val)))
    (when com
      (viper-execute-com 'vimpulse-goto-line val com))))

;;; gb

(defun vimpulse-beginning-of-Word-p ()
  (save-excursion
    (or (bobp)
        (when (viper-looking-at-alpha)
          (backward-char)
          (not (viper-looking-at-alpha))))))

(defun vimpulse-end-of-previous-word (arg)
  "Move point to end of previous word."
  (interactive "P")
  (viper-leave-region-active)
  (let ((val (viper-p-val arg))
        (com (viper-getcom arg)))
    (when com
      (viper-move-marker-locally 'viper-com-point (point)))
    (unless (vimpulse-beginning-of-Word-p)
      (viper-backward-Word 1))
    (viper-backward-Word val)
    (viper-end-of-Word '(1 . ?r))
    (unless com
      (backward-char))
    (when com
      (viper-execute-com 'viper-end-of-word val com))))

;;; gd

(defun vimpulse-goto-definition ()
  "Go to definition or first occurrence of symbol under cursor."
  (interactive)
  (let ((str (vimpulse-search-string (point) 'symbol))
        ientry ipos)
    (cond
     ((string= str "")
      (error "No string under cursor"))
     ;; if imenu is available, try it
     ((or (fboundp 'imenu--make-index-alist)
          (load "imenu" t))
      (setq ientry
            (condition-case nil
                (and (fboundp 'imenu--make-index-alist)
                     (imenu--make-index-alist))
              (error nil)))
      (setq ientry (assoc str ientry))
      (setq ipos (cdr ientry))
      (unless (markerp ipos)
        (setq ipos (cadr ientry)))
      (cond
       ;; imenu found a position, so go there and
       ;; highlight the occurrence
       ((and (markerp ipos)
             (eq (marker-buffer ipos) (current-buffer)))
        (vimpulse-search-for-symbol nil ipos str))
       ;; imenu failed, so just go to first occurrence in buffer
       (t
        (vimpulse-search-for-symbol nil (point-min)))))
     ;; no imenu, so just go to first occurrence in buffer
     (t
      (vimpulse-search-for-symbol nil (point-min))))))

(defun vimpulse-jump-to-tag-at-point ()
  (interactive)
  (let ((tag (thing-at-point 'word)))
    (find-tag tag)))

;;; gi

(defun vimpulse-resume-insert (arg)
  "Insert at previous insert position."
  (interactive "P")
  (when (markerp vimpulse-exit-point)
    (goto-char vimpulse-exit-point))
  (viper-insert arg))

;;; +, _

(defun vimpulse-previous-line-skip-white (&optional arg)
  "Go ARG lines backward and to the first non-blank character."
  (interactive "P")
  (let ((val (viper-p-val arg))
        (com (viper-getcom arg)))
    (when com
      (viper-move-marker-locally 'viper-com-point (point)))
    (forward-line (- val))
    (back-to-indentation)
    (when com
      (viper-execute-com 'vimpulse-previous-line-nonblank val com))))

(defun vimpulse-next-line-skip-white (&optional arg)
  "Go ARG lines forward and to the first non-blank character."
  (interactive "P")
  (let ((val (viper-p-val arg))
        (com (viper-getcom arg)))
    (when com
      (viper-move-marker-locally 'viper-com-point (point)))
    (forward-line val)
    (back-to-indentation)
    (when com
      (viper-execute-com 'vimpulse-next-line-nonblank val com))))

;;; *, #

(defun vimpulse-search-string (&optional pos thing backward regexp)
  "Find something to search for near POS or point.
THING is a `thing-at-point', default `symbol'.
BACKWARD, if t, specifies reverse direction.
REGEXP, if t, means the string is `regexp-quote'd.
Returns the empty string if nothing is found."
  (save-excursion
    (setq pos (or pos (point))
          thing (or thing 'symbol))
    (goto-char pos)
    (let ((str (thing-at-point thing)))
      ;; if there's nothing under point, go forwards
      ;; (or backwards) to find it
      (while (and (not str) (or (and backward (not (bobp)))
                                (and (not backward) (not (eobp)))))
        (if backward (backward-char) (forward-char))
        (setq str (thing-at-point 'symbol)))
      (setq str (or str ""))
      ;; no text properties, thank you very much
      (set-text-properties 0 (length str) nil str)
      (when regexp
        (setq str (regexp-quote str)))
      str)))

(defun vimpulse-search-for-symbol (&optional backward pos search)
  "Search forwards or backwards for the symbol under point.
If BACKWARD is t, search in the reverse direction.
SEARCH is a regular expression to use for searching instead of
the symbol under point; it is wrapped in \"\\\\_<\" and \"\\\\_>\".
POS specifies an alternative position to search from. Note that
if POS is specified and at the beginning of a match, that match
is highlighted rather than skipped past."
  (setq search (or search (vimpulse-search-string
                           (point) 'symbol backward t)))
  (cond
   ((string= search "")
    (error "No string under cursor"))
   (t
    (setq viper-s-string  (concat "\\_<" search "\\_>")
          viper-s-forward (not backward))
    (cond
     (pos
      (unless (region-active-p)
        (push-mark nil t))
      (goto-char pos)
      (cond
       ((looking-at search)
        (save-excursion
          (search-forward search))
        (viper-flash-search-pattern))
       (t
        (viper-search viper-s-string (not backward) 1)
        (unless (region-active-p)
          (pop-mark)))))
     (t
      (viper-search viper-s-string (not backward) 1))))))

(defun vimpulse-search-forward-for-symbol-at-point ()
  (interactive)
  (cond
   ((and (stringp viper-s-string)
         (not (string= viper-s-string ""))
         (memq last-command
               '(vimpulse-search-backward-for-symbol-at-point
                 vimpulse-search-forward-for-symbol-at-point)))
    (setq viper-s-forward t)
    (viper-search-next 1))
   (t
    (vimpulse-search-for-symbol))))

(defun vimpulse-search-backward-for-symbol-at-point ()
  (interactive)
  (cond
   ((and (stringp viper-s-string)
         (not (string= viper-s-string ""))
         (memq last-command
               '(vimpulse-search-backward-for-symbol-at-point
                 vimpulse-search-forward-for-symbol-at-point)))
    (setq viper-s-forward nil)
    (viper-search-next 1))
   (t
    (vimpulse-search-for-symbol t))))

;;; Auto-indent

(defun vimpulse-autoindent ()
  "Auto Indentation, Vim-style."
  (interactive)
  (let ((col (current-indentation)))
    (if viper-preserve-indent
        (setq viper-preserve-indent nil)
      (setq viper-current-indent col))
    ;; don't leave whitespace lines around
    (if (memq last-command
              '(viper-autoindent
                viper-open-line viper-Open-line
                viper-replace-state-exit-cmd))
        (indent-to-left-margin))
    (when viper-auto-indent
      (setq viper-cted t)
      (if (and viper-electric-mode
               (not (memq major-mode
                          '(fundamental-mode
                            text-mode
                            paragraph-indent-text-mode))))
          (if (fboundp 'comment-indent-new-line)
              (comment-indent-new-line)
            (newline-and-indent))
        (newline)
        (indent-to col)))))

(defun vimpulse-Put-and-indent (&optional arg)
  "Put before point/line and indent to current line.
Doesn't indent with a prefix argument."
  (interactive "P")
  (viper-Put-back nil)
  (unless arg
    (indent-region (region-beginning) (region-end))))

(defun vimpulse-put-and-indent (&optional arg)
  "Put after point/line and indent to current line.
Doesn't indent with a prefix argument."
  (interactive "P")
  (viper-put-back nil)
  (unless arg
    (indent-region (region-beginning) (region-end))))

(defalias 'viper-autoindent 'vimpulse-autoindent)

;;; C-o, C-i

(defadvice set-mark (after vimpulse activate)
  "Clear `vimpulse-mark-list'."
  (mapc (lambda (marker)
          (set-marker marker nil))
        vimpulse-mark-list)
  (setq vimpulse-mark-list nil))

(defadvice push-mark (after vimpulse activate)
  "Clear `vimpulse-mark-list'."
  (mapc (lambda (marker)
          (set-marker marker nil))
        vimpulse-mark-list)
  (setq vimpulse-mark-list nil))

(defun vimpulse-jump-backward (arg)
  "Go to older position in jump list.
\\<viper-vi-basic-map>To go the other way, \
press \\[vimpulse-jump-forward]."
  (interactive "p")
  (let ((current-pos (make-marker)) i)
    (unless vimpulse-mark-list
      (move-marker current-pos (point))
      (add-to-list 'vimpulse-mark-list current-pos))
    (dotimes (arg arg)
      (setq current-pos (make-marker))
      ;; skip past duplicate entries in the mark ring
      (setq i (length mark-ring))
      (while (progn (move-marker current-pos (point))
                    (let (vimpulse-mark-list)
                      ;; protect `vimpulse-mark-list'
                      (set-mark-command 0))
                    (setq i (1- i))
                    (and (= (point) current-pos) (> i 0))))
      ;; Already there?
      (move-marker current-pos (point))
      (unless (= (car vimpulse-mark-list) current-pos)
        (setq vimpulse-mark-list
              (cons current-pos vimpulse-mark-list))))))

(defun vimpulse-jump-forward (arg)
  "Go to newer position in jump list.
\\<viper-vi-basic-map>To go the other way, \
press \\[vimpulse-jump-backward]."
  (interactive "p")
  (let (current-pos next-pos)
    (dotimes (arg arg)
      (setq current-pos (car vimpulse-mark-list)
            next-pos (cadr vimpulse-mark-list))
      (when next-pos
        ;; protect `vimpulse-mark-list'
        (let (vimpulse-mark-list)
          (push-mark current-pos t nil))
        (unless (eq (marker-buffer next-pos) (current-buffer))
          (switch-to-buffer (marker-buffer next-pos)))
        (goto-char next-pos)
        (setq vimpulse-mark-list (cdr vimpulse-mark-list))))))

(when vimpulse-want-C-i-like-Vim
  (define-key viper-vi-basic-map "\C-i" 'vimpulse-jump-forward))
(define-key viper-vi-basic-map "\C-o" 'vimpulse-jump-backward)

;;; Replace backspace

(defcustom vimpulse-backspace-restore t
  "Whether Backspace restores the original text in Replace mode.
On by default."
  :group 'vimpulse
  :type  'boolean)

(defun vimpulse-replace-pre-command ()
  "Remember the character under point."
  (cond
   (viper-replace-minor-mode
    (unless (assq (point) vimpulse-replace-alist)
      (add-to-list 'vimpulse-replace-alist
                   (cons (point) (char-after)))))
   ;; if not in Replace mode, remove itself
   (t
    (remove-hook 'pre-command-hook 'vimpulse-replace-pre-command))))

(add-hook 'viper-replace-state-hook
          (lambda ()
            (setq vimpulse-replace-alist nil)
            (vimpulse-replace-pre-command)
            (add-hook 'pre-command-hook
                      'vimpulse-replace-pre-command)))

(defun vimpulse-replace-backspace ()
  "Restore character under cursor.
If `vimpulse-backspace-restore' is nil,
call `viper-del-backward-char-in-replace' instead."
  (interactive)
  (cond
   (vimpulse-backspace-restore
    (backward-char)
    (let ((oldchar (cdr (assq (point) vimpulse-replace-alist))))
      (when oldchar
        (save-excursion
          (delete-char 1)
          (insert oldchar)))))
   (t
    (viper-del-backward-char-in-replace))))

(defadvice viper-adjust-keys-for (after vimpulse activate)
  "Map <backspace> to `vimpulse-replace-backspace' in Replace mode."
  (define-key viper-replace-map [backspace] 'vimpulse-replace-backspace))

(defvar dabbrev--last-abbrev-location)
(defvar dabbrev--last-abbreviation)
(defvar dabbrev--last-direction)
(defvar dabbrev--last-expansion)
(defvar dabbrev--last-expansion-location)

(defun vimpulse-abbrev-expand-before ()
  "Expand to the nearest preceding word.
Search forwards if a match isn't found."
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (dabbrev-expand nil)))

;; Getting dabbrev to search forwards first and then backwards
;; is tricky, because (dabbrev-expand -1) just fails when it
;; doesn't find a following match.
(defun vimpulse-abbrev-expand-after ()
  "Expand to the nearest following word.
Search backwards if a match isn't found."
  (interactive)
  ;; back up global variables
  (let ((abbrev (and (boundp 'dabbrev--last-abbreviation)
                     dabbrev--last-abbreviation))
        (abbrev-loc (and (boundp 'dabbrev--last-abbrev-location)
                         dabbrev--last-abbrev-location))
        (expansion (and (boundp 'dabbrev--last-expansion)
                        dabbrev--last-expansion))
        (expansion-loc (and (boundp 'dabbrev--last-expansion-location)
                            dabbrev--last-expansion-location)))
    ;; expand in same direction as previously, initially forward
    (if (minibufferp)
        (minibuffer-complete)
      (condition-case nil
          (if (eq last-command this-command)
              (dabbrev-expand nil)
            (setq dabbrev--last-direction -1)
            (dabbrev-expand -1))
        ;; restore dabbrev variables if version < 23.2
        (error (progn
                 (when (version< emacs-version "23.2")
                   (setq dabbrev--last-abbreviation abbrev
                         dabbrev--last-abbrev-location abbrev-loc
                         dabbrev--last-expansion expansion
                         dabbrev--last-expansion-location expansion-loc))
                 (setq dabbrev--last-direction 1)
                 (dabbrev-expand nil) nil))))))

(defun vimpulse-expand-line (&optional arg)
  "Expand a whole line."
  (interactive "P")
  (let ((hippie-expand-try-functions-list
         '(try-expand-line
           try-expand-line-all-buffers)))
    (hippie-expand arg)))

(provide 'vimpulse-misc-keybindings)
