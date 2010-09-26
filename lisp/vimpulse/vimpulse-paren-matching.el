;;;; Paren matching

;; When highlighting matching parentheses, Emacs matches the closing
;; parenthesis before the cursor, instead of under it (like in Vim).
;; This code provides an alternate parenthesis matching function
;; used when Viper is in vi (command) mode, so that the parenthesis
;; under the cursor is matched. This makes it possible to visually
;; inspect a closing parenthesis at the end of the line.
;;
;; In Insert mode, Emacs' scheme is deemed best and kept as is.
;;
;; This code is LOADED BY DEFAULT.
;; To avoid loading it, set `vimpulse-enhanced-paren-matching' to nil
;; in your .emacs before loading Vimpulse.

(require 'vimpulse-dependencies)        ; vimpulse-setq etc.

(declare-function vimpulse-delete-overlay "vimpulse-utils" (overlay))

(defvar show-paren-delay)
(defvar vimpulse-paren-overlay-open nil
  "Overlay used to highlight the opening paren.")

(defvar vimpulse-paren-overlay-close nil
  "Overlay used to highlight the closing paren.")

;; Load and enable paren.el if available.
(unless (featurep 'paren)
  (condition-case nil
      (require 'paren)
    (error nil)))
(and (fboundp 'show-paren-mode)
     (not (vimpulse-custom-value-p 'show-paren-mode))
     ;; fast paren-matching
     (vimpulse-setq show-paren-delay 0)
     (show-paren-mode 1))

(defun vimpulse-paren-open-p (&optional pos)
  "Return t if the character at point (or POS) is an opening paren."
  (setq pos (or pos (point)))
  (let ((class (syntax-after pos)))
    (when class
      (setq class (syntax-class class))
      (= class 4))))

(defun vimpulse-paren-close-p (&optional pos)
  "Return t if the character at point (or POS) is an closing paren."
  (setq pos (or pos (point)))
  (let ((class (syntax-after pos)))
    (when class
      (setq class (syntax-class class))
      (= class 5))))

(defun vimpulse-paren-match (&optional pos)
  "Return the position of possible matching paren at point (or POS).
If not a paren, return `not-a-paren'. If not found, return nil."
  (setq pos (or pos (point)))
  (condition-case nil
      (cond
       ((vimpulse-paren-open-p pos)
        (1- (scan-sexps pos 1)))
       ((vimpulse-paren-close-p pos)
        (scan-sexps (1+ pos) -1))
       (t
        'not-a-paren))
    (error nil)))

(defun vimpulse-paren-match-p (pos1 pos2)
  "Return t if POS1 and POS2 are matching characters.
Checks the characters at position POS1 and POS2 and returns t
if they are matching characters (in a paren-match meaning),
nil otherwise."
  (let ((class1 (car (syntax-after pos1)))
        (match1 (cdr (syntax-after pos1)))
        (class2 (car (syntax-after pos2)))
        (match2 (cdr (syntax-after pos2))))
    (or (eq match1 (char-after pos2))
        (eq match2 (char-after pos1))
        (eq match1 match2))))

(defun vimpulse-paren-highlight (face &optional pos)
  "Highlight the paren at point (or POS) with FACE."
  (setq pos (or pos (point)))
  (let ((ovl (if (vimpulse-paren-open-p pos)
                 vimpulse-paren-overlay-open
               vimpulse-paren-overlay-close)))
    (viper-overlay-put ovl 'face face)
    (viper-move-overlay ovl pos (1+ pos))))

;; FIXME: this description sucks
(defun vimpulse-paren-highlight-pair (&optional pos)
  "Highlight paren pair.
Highlights the paren at point (or POS) and eventual matching
or mismatched paren."
  (setq pos (or pos (point)))
  (let ((match (vimpulse-paren-match pos)))
    (cond
     ((not match)
      (vimpulse-paren-highlight 'show-paren-mismatch pos))
     ((eq match 'not-a-paren)
      (vimpulse-delete-overlay vimpulse-paren-overlay-open)
      (vimpulse-delete-overlay vimpulse-paren-overlay-close))
     ((/= pos (vimpulse-paren-match match))
      (vimpulse-paren-highlight 'show-paren-mismatch pos))
     ((vimpulse-paren-match-p pos match)
      (vimpulse-paren-highlight 'show-paren-match pos)
      (vimpulse-paren-highlight 'show-paren-match match))
     (t
      (vimpulse-paren-highlight 'show-paren-mismatch pos)
      (vimpulse-paren-highlight 'show-paren-mismatch match)))))

(defadvice show-paren-function (around vimpulse-paren activate)
  "Use custom highlighting if `vimpulse-enhanced-paren-matching' is t."
  ;; define overlays if they don't exist
  (cond
   (vimpulse-enhanced-paren-matching
    (unless (viper-overlay-live-p vimpulse-paren-overlay-open)
      (setq vimpulse-paren-overlay-open
            (viper-make-overlay (point) (point) nil t nil)
            vimpulse-paren-overlay-close
            (viper-make-overlay (point) (point) nil t nil))
      (vimpulse-delete-overlay vimpulse-paren-overlay-open)
      (vimpulse-delete-overlay vimpulse-paren-overlay-close))
    (cond
     ;; Viper not in Insert, Replace or Emacs state
     ((and (not (eq viper-current-state 'insert-state))
           (not (eq viper-current-state 'replace-state))
           (not (eq viper-current-state 'emacs-state))
           show-paren-mode viper-mode)
      ;; safely delete the overlays used by `show-paren-function'
      ;; and call our custom function instead
      (and (viper-overlay-live-p show-paren-overlay)
           (vimpulse-delete-overlay show-paren-overlay))
      (and (viper-overlay-live-p show-paren-overlay-1)
           (vimpulse-delete-overlay show-paren-overlay-1))
      (vimpulse-paren-highlight-pair))
     ;; Viper in Insert mode
     (t
      ;; delete the overlays used by our custom function
      (vimpulse-delete-overlay vimpulse-paren-overlay-open)
      (vimpulse-delete-overlay vimpulse-paren-overlay-close)
      ad-do-it)))
   (t
    ad-do-it)))

(provide 'vimpulse-paren-matching)
